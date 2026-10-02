import json
import os
from pathlib import Path
import subprocess
from typing import Any
import unittest
from unittest.mock import patch

from generation import generate
from context_selection import build_prompt
from retrieval import Chunk, expand_parents


class GenerationTests(unittest.TestCase):
    def test_count_tool_snapshot_and_narrow_event_allowlist(self) -> None:
        chunk = Chunk("table.md:L1-L3", "A | B\n--- | ---\nyes | yes")
        parents = expand_parents([(1, chunk)], [chunk, Chunk("private", "not supplied")])
        prompt = build_prompt("Count rows", parents, count_tables=True)
        events = [{"type": "item.completed", "item": {
            "type": "mcp_tool_call", "server": "evidence_counter", "tool": "count_rows",
            "arguments": {"source": chunk.source, "where": {}}, "status": "completed"}}]

        def run(command, **kwargs):
            self.assertEqual(json.loads((Path(kwargs["cwd"]) / "evidence.json").read_text()),
                             [{"source": chunk.source, "text": chunk.text}])
            self.assertIn('mcp_servers.evidence_counter.enabled_tools=["count_rows"]', command)
            self.assertIn("mcp_servers.evidence_counter.required=true", command)
            self.assertIn("mcp_servers.evidence_counter.tool_timeout_sec=2", command)
            self.assertIn("features.shell_tool=false", command)
            Path(command[command.index("--output-last-message") + 1]).write_text("One row.")
            return subprocess.CompletedProcess(command, 0, "\n".join(json.dumps(e) for e in events), "")

        calls = []
        with patch("generation.subprocess.run", side_effect=run):
            self.assertEqual(generate(prompt, count_tables=True, tool_calls=calls), "One row.")
            self.assertEqual(calls, [events[0]["item"]])
            for server, tool in (("other", "count_rows"), ("evidence_counter", "shell")):
                events[0]["item"].update(server=server, tool=tool)
                with self.assertRaisesRegex(ValueError, "unexpected tool"):
                    generate(prompt, count_tables=True)

    def test_tool_prompt_preserves_evidence_and_conflict_requirement(self) -> None:
        chunk = Chunk("table", "A | B\n--- | ---\nx | y")
        parents = expand_parents([(1, chunk)], [chunk])
        plain = json.loads(build_prompt("count?", parents).split("\n", 1)[1])
        enabled = json.loads(build_prompt("count?", parents, count_tables=True).split("\n", 1)[1])
        requirements = enabled["application_instructions"]["answer_requirements"]
        self.assertIn("count_rows", requirements[0])
        requirements[0] = plain["application_instructions"]["answer_requirements"][0]
        self.assertEqual(plain, enabled)

    def test_prompt_and_subscription_isolation(self) -> None:
        def run(command: list[str], **kwargs: Any) -> subprocess.CompletedProcess[str]:
            self.assertEqual(kwargs["input"], "evidence prompt")
            self.assertEqual(list(Path(kwargs["cwd"]).iterdir()), [])
            self.assertNotIn("CODEX_API_KEY", kwargs["env"])
            self.assertNotIn("OPENAI_API_KEY", kwargs["env"])
            self.assertIn('forced_login_method="chatgpt"', command)
            self.assertIn('model_reasoning_effort="high"', command)
            self.assertIn("suppress_unstable_features_warning=true", command)
            self.assertEqual(command[command.index("--model") + 1], "gpt-6-luna")
            self.assertIn("--ignore-user-config", command)
            self.assertIn("--ephemeral", command)
            self.assertEqual(command[-1], "-")
            Path(command[command.index("--output-last-message") + 1]).write_text("Three days [a:L1-L1].")
            return subprocess.CompletedProcess(command, 0, json.dumps({
                "type": "item.completed", "item": {"type": "agent_message", "text": "answer"}}), "")

        with patch.dict(os.environ, {"CODEX_API_KEY": "test", "OPENAI_API_KEY": "test"}), \
                patch("generation.subprocess.run", side_effect=run):
            self.assertEqual(generate("evidence prompt"), "Three days [a:L1-L1].")

    def test_failure_empty_and_tool_outputs_are_rejected(self) -> None:
        for result in (
            subprocess.CompletedProcess([], 1, "", "model unavailable"),
            subprocess.CompletedProcess([], 0, "", ""),
            subprocess.CompletedProcess([], 0, json.dumps({"type": "turn.failed"}), ""),
            subprocess.CompletedProcess([], 0, json.dumps({
                "type": "item.started", "item": {"type": "command_execution"}}), ""),
        ):
            with self.subTest(result=result), patch("generation.subprocess.run", return_value=result):
                with self.assertRaises(ValueError):
                    generate("prompt")

    def test_missing_cli_and_timeout(self) -> None:
        for error in (FileNotFoundError(), subprocess.TimeoutExpired("codex", 180)):
            with self.subTest(error=error), patch("generation.subprocess.run", side_effect=error):
                with self.assertRaises(ValueError):
                    generate("prompt")

    def test_error_item_preserves_diagnostic(self) -> None:
        event = {"type": "item.completed", "item": {
            "type": "error", "message": "reasoning effort is unsupported"}}
        result = subprocess.CompletedProcess([], 0, json.dumps(event), "")
        with patch("generation.subprocess.run", return_value=result):
            with self.assertRaisesRegex(ValueError, "reasoning effort is unsupported"):
                generate("prompt")


if __name__ == "__main__":
    unittest.main()
