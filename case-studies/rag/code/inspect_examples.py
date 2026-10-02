"""Offline evidence checks and recorded-score summary; never calls a model."""
import hashlib
import json
from pathlib import Path

from context_selection import build_prompt
from count_tool import Counter
from historical import retrieval_before_links
from rag import load_chunks
from retrieval import retrieve

ROOT = Path(__file__).resolve().parent
QUESTION = "How many listed members may operate the laser?"


def payload(prompt):
    return json.loads(prompt.split("\n", 1)[1])


def linked_condition():
    chunks = load_chunks(ROOT / "examples/linked-condition")
    question = "Can members renew drill loans?"
    before = payload(build_prompt(question, retrieval_before_links.retrieve(question, chunks, 2).parent_documents))
    after = payload(build_prompt(question, retrieve(question, chunks, 2).parent_documents))
    assert not before["excluded_by_budget"] and not after["excluded_by_budget"]
    assert [p["source"] for p in before["evidence"]] == ["borrowing.md:L1-L1"]
    assert [p["source"] for p in after["evidence"]] == ["borrowing.md:L1-L1", "conditions.md:L1-L1"]
    return {"before": before["evidence"], "after": after["evidence"]}


def conflict_prompt(count_tables=False):
    parents = retrieve(QUESTION, load_chunks(ROOT / "examples/conflict"), 2).parent_documents
    return build_prompt(QUESTION, parents, count_tables=count_tables)


def conflict_count():
    data = payload(conflict_prompt())
    assert data["excluded_by_budget"] == []
    assert [p["source"] for p in data["evidence"]] == ["summary.md:L1-L1", "roster.md:L1-L20"]
    assert "9 listed members" in data["evidence"][0]["text"]
    result = Counter(data["evidence"]).call("count_rows", {
        "source": "roster.md:L1-L20", "where": {"Trained": "yes", "Steward present": "yes"}})
    assert not result["isError"]
    count = json.loads(result["content"][0]["text"])
    assert count["rows_examined"] == 16 and count["count"] == 10
    return count


def verify_records():
    """Verify captured inputs and recompute summaries from recorded manual scores.

    This does not regrade model answers or attest provider identity/date.
    """
    provenance = json.loads((ROOT / "PROVENANCE.json").read_text())
    for name, metadata in provenance["files"].items():
        assert hashlib.sha256((ROOT / name).read_bytes()).hexdigest() == metadata["packaged_sha256"], name
    batches = json.loads((ROOT / "evidence/records.json").read_text())["batches"]
    evidence = payload(conflict_prompt())["evidence"]
    summaries = {}
    for batch in batches:
        prompts = batch.get("prompts") or {"single": batch["prompt"]}
        hashes = batch.get("prompt_sha256")
        for key, prompt in prompts.items():
            digest = hashlib.sha256(prompt.encode()).hexdigest()
            if hashes:
                assert digest == (hashes[key] if isinstance(hashes, dict) else hashes)
            assert payload(prompt)["evidence"] == evidence
        if batch["id"] in ("reasoning_comparison", "low_counting_comparison", "medium_counting_comparison"):
            assert prompts.get("False", prompts.get("single")) == conflict_prompt()
            if "True" in prompts:
                assert prompts["True"] == conflict_prompt(True)
            groups = {}
            for run in batch["runs"]:
                label = run.get("reasoning", batch.get("reasoning")) + (" with counting" if run.get("count_tables") else " without counting")
                stats = groups.setdefault(label, {"attempts": 0, "completed": 0, "timeouts": 0, "correct_count": 0, "conflict_with_citations": 0})
                stats["attempts"] += 1
                if run["answer"] is None:
                    assert run["manual_assessment"]["status"] == "timeout"
                    stats["timeouts"] += 1
                    continue
                stats["completed"] += 1
                score = run["manual_assessment"]
                stats["correct_count"] += bool(score.get("roster_count_correct", score.get("count_correct")))
                stats["conflict_with_citations"] += bool(score["conflict_disclosed"] and score["both_sources_cited"])
                for call in run.get("tool_calls", []):
                    assert call["arguments"] == {"source": "roster.md:L1-L20", "where": {"Trained": "yes", "Steward present": "yes"}}
                    assert call["results"] == [{"source": "roster.md:L1-L20", "rows_examined": 16, "count": 10}]
            summaries[batch["id"]] = groups
    return summaries


if __name__ == "__main__":
    print(json.dumps({"linked_condition": linked_condition(), "deterministic_count": conflict_count(),
                      "recorded_manual_score_summaries": verify_records()}, indent=2))
