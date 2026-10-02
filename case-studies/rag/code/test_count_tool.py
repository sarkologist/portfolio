import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

from count_tool import Counter, MAX_EVIDENCE_BYTES, MAX_REQUEST_BYTES, parse_table


TABLE = """Stock available today.
| Item | Site | Ready |
| --- | :--- | ---: |
| Screw | North | yes |
| Nut | South | yes |
| Bolt | North | no |
| Washer | North | yes |
"""


class CountToolTests(unittest.TestCase):
    def counter(self, text=TABLE):
        return Counter([{"source": "stock.md:L1-L7", "text": text}])

    def test_counts_all_and_conjunction_without_domain_logic(self):
        counter = self.counter()
        for where, expected in (({}, 4), ({"Site": "North", "Ready": "yes"}, 2)):
            result = counter.call("count_rows", {"source": "stock.md:L1-L7", "where": where})
            self.assertFalse(result["isError"])
            self.assertEqual(json.loads(result["content"][0]["text"]), {
                "source": "stock.md:L1-L7", "rows_examined": 4, "count": expected})

    def test_plain_cells_case_sensitive_and_empty_table(self):
        for table, where, expected in (
            ("A | B\n--- | ---\n  yes  |  3  \nYes | 4", {"A": "yes"}, 1),
            ("A | B\n--- | ---\nyes | 3", {"A": "YES"}, 0),
            ("A | B\n--- | ---", {}, 0),
        ):
            result = self.counter(table).call("count_rows", {"source": "stock.md:L1-L7", "where": where})
            self.assertFalse(result["isError"])
            self.assertEqual(json.loads(result["content"][0]["text"])["count"], expected)

    def test_rejects_ambiguous_and_malformed_tables(self):
        for table in (
            "No table.", TABLE + "\n" + TABLE,
            "A | A\n--- | ---\nx | y", "A | B\n--- | ---\nx | y | z",
            "A | B\n--- | --- | ---\nx | y", "A | \n--- | ---\nx | y",
            "```\n" + TABLE + "```", "A | B\n--- | ---\nx\\|y | z",
            "A | B\n--- | ---\n`x` | z",
            "A | B\n--- | ---\n" + "x | y\n" * 1001,
        ):
            with self.subTest(table=table[:80]), self.assertRaises(ValueError):
                parse_table(table)

    def test_only_supplied_sources_and_valid_filters(self):
        for arguments in (
            {"source": "/etc/passwd", "where": {}},
            {"source": "stock.md:L1-L7", "where": {"missing": "yes"}},
            {"source": "stock.md:L1-L7", "where": {"Ready": True}},
            {"source": "stock.md:L1-L7", "where": {}, "code": "print(1)"},
            {"source": "stock.md:L1-L7", "where": []},
            {"source": [], "where": {}},
        ):
            self.assertTrue(self.counter().call("count_rows", arguments)["isError"])

    def test_failed_attempts_consume_two_call_budget(self):
        counter = self.counter()
        self.assertTrue(counter.call("shell", {})["isError"])
        self.assertTrue(counter.call("count_rows", {})["isError"])
        result = counter.call("count_rows", {"source": "stock.md:L1-L7", "where": {}})
        self.assertTrue(result["isError"])
        self.assertIn("Two-call limit", result["content"][0]["text"])

    def test_snapshot_bounds_and_duplicate_sources(self):
        for evidence in (
            [{"source": "a", "text": "x" * MAX_EVIDENCE_BYTES}],
            [{"source": "a", "text": "x"}] * 2,
            [{"source": "a", "text": 123}],
        ):
            with self.assertRaises(ValueError):
                Counter(evidence)

    def test_stdio_handshake_discovery_call_budget_and_oversized_input(self):
        with tempfile.TemporaryDirectory() as directory:
            snapshot = Path(directory) / "evidence.json"
            snapshot.write_text(json.dumps([{"source": "stock.md:L1-L7", "text": TABLE}]))
            messages = [
                {"id": 1, "method": "initialize", "params": {"protocolVersion": "2025-06-18"}},
                {"method": "notifications/initialized"},
                {"id": 2, "method": "tools/list"},
            ]
            messages.extend({"id": i, "method": "tools/call", "params": {
                "name": "count_rows", "arguments": {"source": "stock.md:L1-L7", "where": {"Site": "North"}}
            }} for i in range(3, 6))
            payload = "\n".join(json.dumps({"jsonrpc": "2.0", **m}) for m in messages) + "\n"
            command = [sys.executable, "-I", "-S", str(Path(__file__).with_name("count_tool.py")), str(snapshot)]
            result = subprocess.run(command, input=payload, capture_output=True, text=True, timeout=5, check=True)
            replies = [json.loads(line) for line in result.stdout.splitlines()]
            self.assertEqual(len(replies), 5)
            self.assertEqual(replies[0]["result"]["protocolVersion"], "2025-06-18")
            self.assertEqual([t["name"] for t in replies[1]["result"]["tools"]], ["count_rows"])
            self.assertEqual(json.loads(replies[2]["result"]["content"][0]["text"])["count"], 3)
            self.assertTrue(replies[-1]["result"]["isError"])
            result = subprocess.run(command, input="x" * (MAX_REQUEST_BYTES + 1),
                                    capture_output=True, text=True, timeout=5, check=True)
            self.assertEqual(result.stdout, "")


if __name__ == "__main__":
    unittest.main()
