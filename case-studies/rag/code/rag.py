"""Inspectable retrieval, context selection and optional Codex generation."""
import argparse
import json
from pathlib import Path

from retrieval import Chunk, retrieve
from context_selection import build_prompt
from generation import DEFAULT_MODEL, DEFAULT_REASONING, generate

ROOT = Path(__file__).resolve().parent


def load_chunks(directory: Path) -> list[Chunk]:
    """Blank-line paragraphs and separate headings with exact source lines."""
    if not directory.is_dir():
        raise ValueError(f"not a directory: {directory}")
    chunks: list[Chunk] = []
    for path in sorted(directory.rglob("*.md")):
        lines = path.read_text(encoding="utf-8").splitlines()
        start: int | None = None
        paragraph: list[str] = []
        for number, line in enumerate(lines + [""], 1):
            if not line.strip() or line.startswith("#"):
                if paragraph:
                    source = f"{path.relative_to(directory).as_posix()}:L{start}-L{number - 1}"
                    chunks.append(Chunk(source, "\n".join(paragraph)))
                    start, paragraph = None, []
                if line.startswith("#"):
                    source = f"{path.relative_to(directory).as_posix()}:L{number}-L{number}"
                    chunks.append(Chunk(source, line))
            else:
                if start is None:
                    start = number
                paragraph.append(line)
    return chunks


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--docs", type=Path, default=ROOT / "examples" / "conflict")
    commands = parser.add_subparsers(dest="command", required=True)
    for name in ("retrieve", "prompt", "answer"):
        command = commands.add_parser(name)
        command.add_argument("--top-k", type=int, default=3)
        command.add_argument("question")
        if name in ("prompt", "answer"):
            command.add_argument("--max-chars", type=int, default=6000)
            command.add_argument("--count-tables", action="store_true",
                                 help="allow bounded counting of supplied Markdown tables")
        if name == "answer":
            command.add_argument("--model", default=DEFAULT_MODEL)
            command.add_argument("--reasoning", choices=("low", "medium", "high", "xhigh", "max"),
                                 default=DEFAULT_REASONING)
    args = parser.parse_args()
    try:
        chunks = load_chunks(args.docs)
        if not chunks:
            raise ValueError("no nonempty Markdown passages found")
        if args.top_k < 1:
            raise ValueError("top-k must be positive")
        if not args.question.strip():
            raise ValueError("question must not be blank")
        result = retrieve(args.question, chunks, args.top_k)
        if args.command in ("prompt", "answer"):
            prompt = build_prompt(args.question, result.parent_documents, args.max_chars,
                                  count_tables=args.count_tables)
            print(generate(prompt, args.model, args.reasoning, count_tables=args.count_tables)
                  if args.command == "answer" else prompt)
        else:
            print(json.dumps({
                "child_hits": [{"score": score, "source": chunk.source,
                                "text": chunk.text} for score, chunk in result.child_hits],
                "parent_documents": [{
                    "document": parent.document,
                    "passages": [{"source": chunk.source, "text": chunk.text}
                                 for chunk in parent.chunks],
                } for parent in result.parent_documents],
            }, indent=2))
    except (ValueError, OSError) as error:
        parser.error(str(error))


if __name__ == "__main__":
    main()
