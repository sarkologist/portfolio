# Recorded evidence for the two case study examples

[Code and offline checks](../README.md) · [Case study](../../README.md) · [Curated records](records.json)

The renewal example is reproducible offline using the retained pre-fix and current retrievers. The generation evidence consists of **27 recorded attempts: 26 final answers and one timeout**, organised as follows:

| Record group | Attempts | Purpose |
| --- | --- | --- |
| Original observation | 1 | Correct roster count with omitted source conflict |
| Low/high reasoning comparison | 6 | Three repeats per reasoning setting, tools off |
| Discretionary-tool pilot | 6 | Three tools-off and three enabled calls; the model skipped the tool |
| Direct-instruction development probe | 1 | Tool used after stronger instruction, but conflict omitted |
| Final low-effort counting comparison | 7 | Three completed answers per condition, plus a timeout and retry |
| Medium-effort counting comparison | 6 | Three completed answers per condition |

The original observation and development stages are not pooled into the repeated-comparison scores. Calls alternated configurations within each repeated batch. Historical dates/settings belong to these records, not to a claim that the configurations were all evaluated in one simultaneous benchmark.

## Repeated comparisons

The scores below are computed from the retained **manual assessment fields**, not from a newly automated answer grader. A conflict success requires both explicit disclosure and both source citations.

| Batch | Configuration | Correct count | Conflict with citations | Completed / attempts |
| --- | --- | --- | --- | --- |
| Reasoning | Low without counting | 2/3 | 1/3 | 3/3 |
| Reasoning | High without counting | 3/3 | 3/3 | 3/3 |
| Low counting | Without counting | 2/3 | 0/3 | 3/3 |
| Low counting | With counting | 3/3 | 1/3 | 3/4 |
| Medium counting | Without counting | 0/3 | 0/3 | 3/3 |
| Medium counting | With counting | 3/3 | 1/3 | 3/3 |

The low-enabled timeout was 180 seconds and was retried once. No final answer or partial tool trace was exposed for that attempt; the empty trace is not proof the tool was unused. It is retained, not silently dropped from the attempt count.

## Check the evidence without a model

From the code directory:

```sh
python3 -B inspect_examples.py
python3 -B -m unittest -v
```

The inspector verifies the selected source snapshot hashes, every recorded prompt hash where captured, the exact evidence in those prompts, and the deterministic roster count. It also reconstructs the final repeated-comparison prompts and recomputes their summaries from manual labels. Read the saved final answers to assess those labels independently.

Records were curated by retaining final answers, relevant counter calls, exact prompts, assessment fields, settings, source/record hashes and captured provenance. Tool envelopes were reduced to the tool name, arguments, returned JSON and completion status; CLI diagnostics and thread IDs were excluded. Original absolute file paths in hash keys were made relative. Hashes support integrity, not independent verification of backend identity or timestamps. The development probe did not capture a source revision, so none is assigned to it.

This is one selected synthetic failure with small repeat counts. Counting-enabled configurations also change the instruction. No fresh model calls, held-out evaluation, general reliability claim or high-effort tool comparison is included.
