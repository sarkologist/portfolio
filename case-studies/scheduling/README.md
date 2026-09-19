# Scheduling: using agents to find a counterexample

[Portfolio](../../README.md) · [Source](https://github.com/sarkologist/scheduling-llm) · [Recorded evidence](https://github.com/sarkologist/scheduling-llm/blob/main/evidence/README.md) · [Protocol and results](https://github.com/sarkologist/scheduling-llm/blob/main/experiments/paired-v1/RESULTS.md)

## At a glance

**Question:** when should an LLM solve a scheduling problem itself, and when should it translate the request for ordinary code to solve?

**My contribution:** helped make the task precise, proposed using a stronger agent adversarially to find failures, and directed the investigation from a counterexample to a broader comparison. Agents wrote the implementation and carried out the experiments and statistical analysis.

**Result:** on 100 fresh synthetic requests at the same deliberately low-capability model setting, direct scheduling succeeded on 63; interpretation followed by finite search succeeded on all 100. The result supports that division of responsibility within the tested domain—not a claim of perfect reliability or a production-ready assistant.

## From an exercise to a testable question

The work sample began with a small scheduling task: turn a natural-language request into a valid schedule, a clarification question, or a justified no-solution result.

I helped clarify the task for agent execution: whole-hour boundaries, an explicit output contract with validation, and self-contained instructions for each stage.

With at most four meetings, two rooms and six hourly slots, ordinary code could check answers and exhaust possible schedules. Another model’s approval was not the correctness test.

Two approaches were available:

```text
Direct:  request → LLM proposes a schedule → checker
Staged:  request → LLM extracts constraints → finite search → checker
```

Staging was already proposed in the brief. My move was to actively challenge the scheduler rather than rely on passing examples.

## Use an agent to search for the failure

I proposed employing a stronger agent adversarially: give it the job of constructing scheduling problems likely to expose mistakes in the smaller model.

Agents implemented the harness: adversarial constraints became requests, while attack hypotheses and reference answers stayed hidden from the scheduler. Code checked the responses.

The first six-case pilot found no errors. I then asked to reduce the scheduler’s capability. With unchanged requests and prompts, the next run reduced Luna’s reasoning setting from `low` to `none`.

It failed on the first case.

## A concrete counterexample

There was one room and a six-hour window. Every meeting lasted two hours:

| Meeting | Allowed start hours |
| --- | --- |
| A | 0 or 2 |
| B | 0 |
| C | 2 or 4 |

B also had to finish before C began. The model claimed there was no solution; its explanation treated C as though it could only occupy hours 2–4, overlooking the permitted start at hour 4.

A valid schedule was straightforward to exhibit:

```text
Hour:  0       2       4       6
       |   B   |   A   |   C   |
```

There was now a specific false claim and a checkable witness refuting it—not just an impression of unreliability. The explanation did not establish the model’s internal reasoning.

## Does the alternative fail too?

I asked whether the interpretation-first approach would also fail on this example. At the same `none` reasoning setting, it preserved all the constraints—including both start options for C. Finite search then produced the valid schedule above.

That was promising, but the example had been selected because the direct approach failed. I asked for enough evidence to compare the approaches beyond that one case.

Agents prepared a fixed paired experiment: 100 fresh synthetic problems, with the dataset, prompts, code and analysis protocol frozen before live outcomes. Both approaches used the same requested model and reasoning setting. Separately, the known counterexample was repeated 30 times per approach; those repetitions were not counted as new problems.

## What the comparison showed

| Evaluation | Direct scheduling | Interpretation + finite search |
| --- | --- | --- |
| 100 fresh synthetic problems | 63 successes | 100 successes |
| 30 new invocations of the selected counterexample | 0 successes | 30 successes |

All 100 fresh extractions preserved the reference constraints. **A correct solver can still solve the wrong problem if the LLM mistranslates the request**, so interpretation and final behaviour were checked separately.

The 37-point improvement came with more output tokens and about 1.2 seconds higher mean pipeline latency in that run.

The main qualification is deliberate: we had reduced the model's reasoning capability to expose a failure regime. The experiment does not show that staging beats a stronger model, or offers the best quality for a given cost. Requests were explicit, template-rendered descriptions of synthetic constraints; ambiguous language and real calendars were not tested. Zero observed staged failures is not proof that none can occur.

## My role

I shaped the question and chose how to challenge the system, then asked for evidence beyond the first counterexample. Agents supplied the code, generated cases, ran the experiments and implemented the statistical analysis. I did not invent the staged architecture or manually implement the system.

The work sample shows how I use agents to turn a technical concern into an executable investigation, with concrete evidence and a clear boundary on the conclusion. It ends at an evaluated pipeline, rather than expanding into a calendar product.

## Inspect the evidence

- [Actual counterexample responses](https://github.com/sarkologist/scheduling-llm/blob/main/evidence/counterexample.json) and [request/reference](https://github.com/sarkologist/scheduling-llm/blob/main/cases/dev/adversarial/01-greedy-trap.json).
- [Frozen protocol](https://github.com/sarkologist/scheduling-llm/blob/main/experiments/paired-v1/PROTOCOL.md) and [results with uncertainty](https://github.com/sarkologist/scheduling-llm/blob/main/experiments/paired-v1/RESULTS.md).
- [Model input boundary](https://github.com/sarkologist/scheduling-llm/blob/main/scheduling_llm/provider.py), [independent checker and search](https://github.com/sarkologist/scheduling-llm/blob/main/scheduling_llm/checker.py), and [interpretation/final-behaviour grading](https://github.com/sarkologist/scheduling-llm/blob/main/scheduling_llm/experiment.py).
- [Offline regrading](https://github.com/sarkologist/scheduling-llm/blob/main/evidence/README.md): recompute the comparison from 260 recorded final responses without credentials or new model calls. This is recorded evidence, not a hosted demo.

The source release is a reviewed snapshot; frozen experiment files are preserved, while private development history and raw agent sessions are excluded. See the repository for provenance and reproduction limits.
