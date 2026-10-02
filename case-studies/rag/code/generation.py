"""Model generation through the installed, ChatGPT-authenticated Codex CLI."""
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile

from count_tool import Counter

DEFAULT_MODEL = "gpt-6-luna"
DEFAULT_REASONING = "high"


def generate(
    prompt: str, model: str = DEFAULT_MODEL, reasoning: str = DEFAULT_REASONING,
    *, count_tables: bool = False, tool_calls: list[dict] | None = None,
) -> str:
    """Send the evidence prompt over stdin; never give Codex the repository cwd."""
    env = os.environ.copy()
    for key in ("CODEX_API_KEY", "OPENAI_API_KEY"):
        env.pop(key, None)
    with tempfile.TemporaryDirectory(prefix="rag-generation-") as directory:
        output = Path(directory) / "answer.txt"
        command = [
            "codex", "exec", "--ignore-user-config", "--ephemeral",
            "--skip-git-repo-check", "--sandbox", "read-only", "--json",
            "--color", "never", "--model", model,
            "--output-last-message", str(output),
        ]
        settings = {
            "model_provider": '"openai"',
            "forced_login_method": '"chatgpt"',
            "model_reasoning_effort": json.dumps(reasoning),
            "approval_policy": '"never"',
            "web_search": '"disabled"',
            "project_doc_max_bytes": "0",
            "suppress_unstable_features_warning": "true",
            "features.shell_tool": "false",
            "features.multi_agent": "false",
            "features.apps": "false",
            "features.plugins": "false",
            "features.memories": "false",
            "features.hooks": "false",
            "features.browser_use": "false",
            "features.computer_use": "false",
            "features.image_generation": "false",
            "features.skip_host_skill_discovery": "true",
        }
        if count_tables:
            evidence = json.loads(prompt.split("\n", 1)[1])["evidence"]
            Counter(evidence)  # Validate bounds before launching a model call.
            snapshot = Path(directory) / "evidence.json"
            snapshot.write_text(json.dumps(evidence), encoding="utf-8")
            settings.update({
                "mcp_servers.evidence_counter.command": json.dumps(sys.executable),
                "mcp_servers.evidence_counter.args": json.dumps([
                    "-I", "-S", str(Path(__file__).with_name("count_tool.py").resolve()),
                    str(snapshot),
                ]),
                "mcp_servers.evidence_counter.enabled_tools": '["count_rows"]',
                "mcp_servers.evidence_counter.required": "true",
                "mcp_servers.evidence_counter.tool_timeout_sec": "2",
            })
        for key, value in settings.items():
            command.extend(["-c", f"{key}={value}"])
        command.append("-")
        try:
            result = subprocess.run(command, input=prompt, cwd=directory,
                                    env=env, capture_output=True, text=True,
                                    encoding="utf-8", timeout=180)
        except FileNotFoundError as error:
            raise ValueError("Codex CLI not found; install it and run codex login") from error
        except subprocess.TimeoutExpired as error:
            raise ValueError("Codex generation timed out after 180 seconds") from error
        if result.returncode:
            raise ValueError(f"Codex generation failed: {result.stderr.strip() or result.stdout.strip()}")
        for line in result.stdout.splitlines():
            event = json.loads(line)
            if event.get("type") in ("error", "turn.failed"):
                raise ValueError(f"Codex generation failed: {event}")
            if event.get("type", "").startswith("item."):
                item = event.get("item", {})
                item_type = item.get("type")
                if item_type == "error":
                    raise ValueError(f"Codex generation failed: {item.get('message', item)}")
                if (count_tables and item_type == "mcp_tool_call"
                        and item.get("server") == "evidence_counter"
                        and item.get("tool") == "count_rows"):
                    if event["type"] == "item.completed" and tool_calls is not None:
                        tool_calls.append(item)
                    continue
                if item_type not in ("agent_message", "reasoning"):
                    raise ValueError(f"Codex used an unexpected tool/item: {item_type}; answer rejected")
        answer = output.read_text(encoding="utf-8").strip() if output.exists() else ""
        if not answer:
            raise ValueError("Codex returned no final answer")
        return answer
