"""A single bounded, evidence-only table tool over MCP stdio; standard library only."""
import json
from pathlib import Path
import re
import sys

MAX_EVIDENCE_BYTES = 100_000
MAX_REQUEST_BYTES = 8192
MAX_CALLS = 2
MAX_ROWS = 1000
MAX_COLUMNS = 32

TOOL = {
    "name": "count_rows",
    "description": (
        "Count rows in one plain Markdown table in a supplied evidence source. "
        "Give its exact source ID and a where object of column names to values. "
        "All filters must match (AND); matching is case-sensitive after trimming "
        "cell whitespace. Use {} to count all rows. Do not copy the table. "
        "At most two calls per answer; malformed or unsupported tables return errors."
    ),
    "inputSchema": {
        "type": "object",
        "properties": {
            "source": {"type": "string", "maxLength": 512},
            "where": {
                "type": "object", "maxProperties": MAX_COLUMNS,
                "additionalProperties": {"type": "string", "maxLength": 512},
            },
        },
        "required": ["source", "where"],
        "additionalProperties": False,
    },
    "annotations": {"readOnlyHint": True, "openWorldHint": False},
}


def cells(line: str) -> list[str]:
    line = line.strip()
    if line.startswith("|"):
        line = line[1:]
    if line.endswith("|"):
        line = line[:-1]
    return [cell.strip() for cell in line.split("|")]


def parse_table(text: str) -> tuple[list[str], list[list[str]]]:
    """Accept one plain pipe table; reject ambiguity rather than guess."""
    lines = text.splitlines()
    separators = [i for i, line in enumerate(lines) if "|" in line and
                  all(re.fullmatch(r":?-{3,}:?", cell) for cell in cells(line))]
    if len(separators) != 1 or separators[0] == 0:
        raise ValueError("Source must contain exactly one plain Markdown table with a header separator.")
    start = separators[0]
    header = cells(lines[start - 1])
    if (not 1 <= len(header) <= MAX_COLUMNS or any(not name for name in header)
            or len(set(header)) != len(header)
            or len(cells(lines[start])) != len(header)):
        raise ValueError("Table needs unique nonempty headers and matching separator columns (at most 32).")
    end = start + 1
    while end < len(lines) and lines[end].strip() and "|" in lines[end]:
        end += 1
    # Deliberately do not interpret code blocks, escapes, or inline code as table syntax.
    if any("```" in line or "~~~" in line for line in lines) or any(
            "\\" in line or "`" in line for line in lines[start - 1:end]):
        raise ValueError("Only plain table cells are supported; no code fences, backticks or escapes.")
    rows = [cells(line) for line in lines[start + 1:end]]
    if len(rows) > MAX_ROWS or any(len(row) != len(header) for row in rows):
        raise ValueError("Table rows must match the header width and number at most 1000.")
    return header, rows


class Counter:
    def __init__(self, evidence: list[dict[str, str]]) -> None:
        if not isinstance(evidence, list) or len(json.dumps(evidence).encode()) > MAX_EVIDENCE_BYTES:
            raise ValueError("Counting evidence must be a list of at most 100000 serialized bytes.")
        self.evidence: dict[str, str] = {}
        for passage in evidence:
            if (not isinstance(passage, dict) or set(passage) != {"source", "text"}
                    or not isinstance(passage["source"], str)
                    or not 1 <= len(passage["source"]) <= 512
                    or not isinstance(passage["text"], str)
                    or passage["source"] in self.evidence):
                raise ValueError("Counting evidence needs unique source IDs and text strings.")
            self.evidence[passage["source"]] = passage["text"]
        self.calls = 0

    def call(self, name: str, arguments: dict) -> dict:
        self.calls += 1  # Failed attempts consume the same budget.
        try:
            if self.calls > MAX_CALLS:
                raise ValueError("Two-call limit reached; no further counting is available.")
            if name != "count_rows":
                raise ValueError("Only count_rows is available.")
            if not isinstance(arguments, dict) or set(arguments) != {"source", "where"}:
                raise ValueError("Provide exactly source and where.")
            source, where = arguments["source"], arguments["where"]
            if not isinstance(source, str) or source not in self.evidence:
                raise ValueError("Unknown source ID; use an exact source ID from supplied evidence.")
            if (not isinstance(where, dict) or len(where) > MAX_COLUMNS
                    or any(not isinstance(key, str) or not isinstance(value, str)
                           or len(value) > 512 for key, value in where.items())):
                raise ValueError("where must map at most 32 column names to string values of at most 512 characters.")
            header, rows = parse_table(self.evidence[source])
            if any(key not in header for key in where):
                raise ValueError("Unknown filter column; use an exact table header.")
            filters = [(header.index(key), value) for key, value in where.items()]
            result = {"source": source, "rows_examined": len(rows),
                      "count": sum(all(row[i] == value for i, value in filters) for row in rows)}
            return {"content": [{"type": "text", "text": json.dumps(result, ensure_ascii=False)}],
                    "isError": False}
        except ValueError as error:
            return {"content": [{"type": "text", "text": str(error)}], "isError": True}


def serve(counter: Counter) -> None:
    """Minimal MCP 2025-06-18: initialization, ping, tool discovery and calls."""
    for raw in iter(lambda: sys.stdin.buffer.readline(MAX_REQUEST_BYTES + 1), b""):
        if len(raw) > MAX_REQUEST_BYTES:
            return  # Close the transport instead of accumulating an oversized request.
        request = None
        try:
            request = json.loads(raw)
            if not isinstance(request, dict) or request.get("jsonrpc") != "2.0":
                raise ValueError("Expected a JSON-RPC object.")
            if "id" not in request:
                continue  # Notifications, including initialized and cancelled, have no reply.
            method = request.get("method")
            if method == "initialize":
                result = {"protocolVersion": "2025-06-18", "capabilities": {"tools": {}},
                          "serverInfo": {"name": "evidence_counter", "version": "1.0"}}
            elif method == "ping":
                result = {}
            elif method == "tools/list":
                result = {"tools": [TOOL]}
            elif method == "tools/call":
                params = request.get("params", {})
                if not isinstance(params, dict):
                    raise ValueError("Expected object params.")
                result = counter.call(params.get("name"), params.get("arguments", {}))
            else:
                print(json.dumps({"jsonrpc": "2.0", "id": request["id"],
                                  "error": {"code": -32601, "message": "Method not found"}}), flush=True)
                continue
            response = {"jsonrpc": "2.0", "id": request["id"], "result": result}
        except (ValueError, RecursionError) as error:
            response = {"jsonrpc": "2.0", "id": request.get("id") if isinstance(request, dict) else None,
                        "error": {"code": -32600, "message": str(error)[:200]}}
        print(json.dumps(response, ensure_ascii=False), flush=True)


if __name__ == "__main__":
    # The host supplies this fixed snapshot path, never the model's tool arguments.
    with Path(sys.argv[1]).open("rb") as snapshot:
        data = snapshot.read(MAX_EVIDENCE_BYTES + 1)
    if len(data) > MAX_EVIDENCE_BYTES:
        raise ValueError("Counting evidence exceeds 100000 bytes.")
    serve(Counter(json.loads(data)))
