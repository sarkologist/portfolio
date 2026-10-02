"""Focused offline checks for the two case-study examples."""
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

from inspect_examples import conflict_count, conflict_prompt, linked_condition, payload, verify_records
from rag import load_chunks
from retrieval import retrieve
from context_selection import build_prompt

ROOT = Path(__file__).resolve().parent


class ExampleTests(unittest.TestCase):
    def test_historical_and_fixed_linked_condition(self):
        self.assertIn("approval", linked_condition()["after"][1]["text"])

    def test_rule_and_condition_in_one_parent_control(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "borrowing.md").write_text("Drill renewal.\n\nApproval required.\n")
            from historical import retrieval_before_links
            parents = retrieval_before_links.retrieve("Drill renewal?", load_chunks(root), 2).parent_documents
            self.assertIn("Approval required.", payload(build_prompt("Drill renewal?", parents))["evidence"][1]["text"])

    def test_one_hop_does_not_follow_unrelated_or_second_hop_files(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "borrowing.md").write_text("Drill renewal: [terms](conditions.md).\n")
            (root / "conditions.md").write_text("Approval required. See [details](details.md).\n")
            (root / "details.md").write_text("Bring identification.\n")
            self.assertEqual([p.document for p in retrieve("Drill renewal?", load_chunks(root), 2).parent_documents],
                             ["borrowing.md", "conditions.md"])

    def test_complete_conflicting_context_and_deterministic_count(self):
        self.assertEqual(conflict_count()["count"], 10)

    def test_recorded_inputs_and_summaries(self):
        scores = verify_records()
        low = scores["low_counting_comparison"]
        self.assertEqual(low["low without counting"]["correct_count"], 2)
        self.assertEqual(low["low without counting"]["conflict_with_citations"], 0)
        self.assertEqual(low["low with counting"]["correct_count"], 3)
        self.assertEqual(low["low with counting"]["conflict_with_citations"], 1)
        self.assertEqual(low["low with counting"]["timeouts"], 1)
        self.assertEqual(scores["reasoning_comparison"]["high without counting"]["conflict_with_citations"], 3)
        self.assertEqual(scores["medium_counting_comparison"]["medium with counting"]["conflict_with_citations"], 1)

    def test_cli_defaults_to_case_study_corpus(self):
        result = subprocess.run([sys.executable, "-B", str(ROOT / "rag.py"), "prompt",
                                 "How many listed members may operate the laser?", "--top-k", "2"],
                                capture_output=True, text=True, check=True, timeout=5)
        self.assertEqual(payload(result.stdout), payload(conflict_prompt()))

    def test_counting_prompt_does_not_change_evidence(self):
        self.assertEqual(payload(conflict_prompt())["evidence"], payload(conflict_prompt(True))["evidence"])


if __name__ == "__main__":
    unittest.main()
