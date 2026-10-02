# Code companion for the RAG case study

[Read the story](../README.md) · [Recorded experiment evidence](evidence/README.md)

Only the two examples in the story are included: a missing linked renewal condition, and a laser roster whose count conflicts with a summary. This is a small synthetic learning prototype, not a deployed application. Python 3.10+; no third-party Python packages.

## Start offline

Run from this directory:

```sh
python3 -B inspect_examples.py
python3 -B -m unittest -v
```

No credentials, network or model calls are needed. The inspector shows:

- **Before/after link retrieval:** the original pre-fix retriever omits the approval condition; the current retriever follows one local link and includes it. Neither prompt loses evidence to budgeting. This tests evidence coverage, not a generated answer.
- **Conflicting context:** both sources reach the prompt. The roster has 16 rows, 10 meeting both conditions; the summary says 9. The counter independently returns 10.
- **Recorded comparisons:** prompt hashes and supplied evidence are checked, and summaries are recomputed from the retained manual assessments. This is not automatic answer regrading or independent attestation of historical runs.

Inspect the prompts themselves:

```sh
python3 -B rag.py --docs examples/linked-condition prompt 'Can members renew drill loans?' --top-k 2
python3 -B rag.py prompt 'How many listed members may operate the laser?' --top-k 2
python3 -B rag.py prompt 'How many listed members may operate the laser?' --top-k 2 --count-tables
```

The CLI defaults to the included conflicting-count corpus. `--docs` comes before the command; other options come after it.

## What to read

| File | Role |
| --- | --- |
| [retrieval.py](retrieval.py) | Lexical child search, parent expansion and one-hop local-link traversal |
| [historical/retrieval_before_links.py](historical/retrieval_before_links.py) | Actual pre-fix retriever, retained to reproduce the evidence omission |
| [context_selection.py](context_selection.py) | Whole-parent character budgeting and cited-answer/conflict instructions |
| [count_tool.py](count_tool.py) | Bounded deterministic table counting over selected evidence; minimal MCP stdio transport |
| [generation.py](generation.py) | Codex CLI boundary and optional counter exposure |
| [rag.py](rag.py) | Original document loader and narrowly adapted example CLI |
| [inspect_examples.py](inspect_examples.py) | Offline example checks and recorded manual-score aggregation |
| [evidence/records.json](evidence/records.json) | Curated prompts, answers, settings and relevant tool results |

The original workshop vocabulary aliases are retained inside the retriever so the source remains authentic; unrelated workshop fixtures and exercises are excluded. Check aliases before using another domain.

## Optional live generation

The following commands **send the question and selected synthetic passages to OpenAI and consume subscription usage**. They require an installed Codex CLI, ChatGPT authentication (`codex login`) and access to the requested model/settings. Historical calls used Codex CLI 0.156.1; the adapter requires `--ignore-user-config`. Future availability and answers may differ; there is no silent model fallback.

```sh
python3 -B rag.py answer 'How many listed members may operate the laser?' --top-k 2 --reasoning low
python3 -B rag.py answer 'How many listed members may operate the laser?' --top-k 2 --reasoning low --count-tables
python3 -B rag.py answer 'How many listed members may operate the laser?' --top-k 2 --reasoning high
```

Tools are disabled by default. With counting enabled, the prompt directs table-count questions to use `count_rows`; this changes both tool availability and the instruction. The tool only filters exact table cells in the selected evidence. It does not decide source precedence or whether the answer must disclose disagreement. The adapter's tool restrictions are application controls, not an OS security boundary.

## Source and packaging provenance

[PROVENANCE.json](PROVENANCE.json) pins the private development source and original/packaged file hashes. Those historical commit IDs belong to the development repository, not this portfolio's Git history.

- Retrieval, context selection, generation, counting and their existing adapter/counter tests are unchanged from source revision `8c641d604741a5dedd64c3beeaa6ad34958cca0f`.
- Historical retrieval is unchanged from `acd42a3c539968599b55b1a4f4bc87ddc1a3871e`, before link expansion was added.
- `rag.py` keeps the original loader and retrieve/prompt/answer path, changes the default corpus, and removes the unrelated development evaluator. The focused inspector and example tests are new packaging helpers, not the code that generated the historical answers.
- Recorded calls predate this packaging and include earlier implementations; captured hashes, dirty-tree status where recorded and prompts preserve those distinctions. The inspector verifies that the final tools-off/tools-on prompts still match the corresponding repeated-comparison inputs. The initial discretionary-tool pilot retains its different historical prompt.

Raw agent sessions, CLI diagnostics/thread identifiers, credentials, machine-specific paths, unrelated probes and private Git history are not included. No model calls were made during packaging. No new licence is assigned by this companion.
