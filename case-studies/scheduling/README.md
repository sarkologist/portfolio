# Scheduling, told through the questions

[Portfolio](../../README.md) · [Source](https://github.com/sarkologist/scheduling-llm) · [Recorded evidence](https://github.com/sarkologist/scheduling-llm/blob/main/evidence/README.md) · [Protocol and results](https://github.com/sarkologist/scheduling-llm/blob/main/experiments/paired-v1/RESULTS.md)

## At a glance

**Question:** when should an LLM solve a scheduling problem itself, and when should it translate the request for ordinary code to solve?

**My contribution:** the questions. I fixed what the task was, confirmed what the staged design committed the model to, proposed adversarial search by a stronger agent, asked to weaken the scheduler until it failed, asked whether the alternative survived the counterexample, asked for evidence beyond one case, and questioned whether the comparison was fair and the evidence clean. Agents wrote the implementation and carried out the experiments and statistical analysis.

**Result:** on 100 fresh synthetic requests at the same deliberately low-capability model setting, direct scheduling succeeded on 63; interpretation followed by finite search succeeded on all 100. The result supports that division of responsibility within the tested domain, not perfect reliability or a production-ready assistant.

**How this is written:** the questions below are quoted as I typed them to the coding agent, in order, from the development session logs. Each is followed by what its answer changed.

The problem was small: turn a natural-language meeting request into a valid schedule, a clarifying question, or a justified "no schedule exists". At most four meetings, two rooms, six hourly slots. An agent had written the brief and a scaffold, and the model under test was a cheap, fast one (OpenAI's `gpt-5.6-luna`, called Luna below). What follows is the sequence of questions I asked while working out what the problem was and how to know whether it had been solved. The agents wrote the code and ran the experiments; the questions below are mine, quoted as I typed them.

## Day one: what is the thing, exactly?

The scaffold came with a specification I did not fully understand.

> what are these slots?
>
> what is meant by slice?
>
> do we have examples? it is not clear what the inputs or outputs should look like

A specification that two readers can read differently is not yet a problem statement. I made the first fixing decision myself, so that everything downstream had a unit to stand on:

> we need to decide the granularity of duration. let's just work with hour boundaries for now

The examples that followed were readable, but readability was not the point:

> ok the examples are nice but i think we should explicitly declare the expected shape of the output json. we should build a validator which check that the types are correct and the values fall within expected ranges

Once a validator exists, "did the model answer correctly" stops being a matter of reading its output and becomes a matter of running code. Everything later rests on that.

## The question that fixed what the model is for

The brief had already proposed two ways to use the model: ask it for a schedule directly, or ask it only to extract the constraints and let ordinary code search for a schedule. Reading the prompts, I wanted to confirm that in the second design the model's role ended at interpretation:

> given the output of the interpretation step, the problem can be solved deterministically and optimally right? so the LLM is constrained to the interpretation?

It did, with one correction: the specification defined feasibility, not optimisation, so exhaustive search guarantees a valid schedule or a proof that none exists, and nothing more is asked of it. The point of confirming this is the corollary I was relying on: the solver is only as correct as the interpretation it is given. If the model drops a constraint, a correct solver returns a perfect answer to the wrong question. So interpretation fidelity and final validity had to be measured separately.

The brief had proposed the staged design; this question pinned down what it commits to. The model is responsible for translation; search and checking belong to code; and the evaluation has two independent parts. Everything after this is a consequence of taking that seriously.

A smaller question the same afternoon came from the same instinct, keeping the thing under test minimal:

> seems like we are passing more context to the model than is necessary ... the spec should remain for development, not runtime purposes

## Day two: find where it breaks

A system that passes its examples has told you about the examples. I wanted to see it fail, so I knew what the staged design was protecting against:

> let's use a stronger model to adversarially surface mistakes by the scheduler model.

A stronger agent wrote six problems designed to trip the scheduler. The scheduler got all six right: two valid schedules, four correct "no solution" results. The tempting reading was that the model was fine. I took the other one, that the adversary had not yet found the regime where it fails:

> let's lower the capability of the scheduler model until we see mistakes. we may have to resort to using local models to make them stupid enough, but pause and let me know before we proceed with that

Turning the model's reasoning setting from `low` to `none`, with the same six requests and prompts, broke it on the first case. One room, a six-hour window, three two-hour meetings: A may start at 0 or 2, B only at 0, C at 2 or 4, and B must finish before C starts. The model declared it impossible. Its explanation treated C as if it could only start at 2, overlooking the start at 4. A valid schedule exists:

```text
Hour:  0       2       4       6
       |   B   |   A   |   C   |
```

I later asked what the example was really about:

> what is the key idea behind this example

A failed greedy choice is not a proof that no solution exists. Place A first at its earliest start and B, which can only start at 0, is blocked. Start from the least flexible meeting and everything fits. That is exactly the kind of search a model does badly and a few lines of exhaustive code do perfectly.

## Does the alternative survive the same case?

A counterexample against one design is only interesting if the other design handles it:

> nice. do both the baseline and staged-interpretation models fail it?
>
> oh btw do we already have a finite solver implemented?

I did not know whether the solver existed yet. It did, written by the agent earlier, not yet wired to the interpretation output. At the same weakened setting, interpretation preserved every constraint, including both start options for C, and finite search produced the valid schedule.

One case, selected because the baseline failed on it, proves little about reliability. So:

> ok conduct an experiment and build up enough statistical confidence

The agents froze 100 fresh synthetic problems, both prompts, the code and an analysis protocol before any outcome was observed, and ran both approaches at the same model and setting. Direct scheduling succeeded on 63 of 100; interpretation plus finite search on 100 of 100, with every extraction matching its reference constraints. Thirty fresh repetitions of the known counterexample were 0 of 30 against 30 of 30.

## Day three: is the comparison fair, and is the evidence clean?

The number is only as good as the comparison behind it, so the next questions were about what could make it misleading.

> the staged model is also luna with reasoning none right? and that means no chain of thought?

Yes: same model, same setting, zero reasoning tokens on all 260 calls. The improvement came from changing what the model was asked to do, not from giving it more thinking.

> what are held-outs?
>
> what guardrails are in place to prevent the held-out from being used for development
>
> were the consumed held-out cases designed specifically for the experiment we ran?

The answers became the limitations section. Hashes of the frozen files were checked before, during and after the run, so the experiment could not finish on altered prompts or data. But a hash proves the files were unchanged, not that nobody learned from reading them. And the case generator had been designed after we had seen the greedy-trap failure: the 100 cases were fresh, but their distribution was informed by what we already knew. The result is a claim about this synthetic distribution at a deliberately weakened setting, not about real calendars or about staging beating a stronger model.

## Questions I asked to understand what I was judging

Some questions in the logs are the ones you ask when a word means nothing to you yet:

> what do these test?
>
> how can i run the validator on model output
>
> McNemar, what is it?
>
> what is bonferroni adjustment?

I cannot judge a result I do not understand, so when I notice a gap I ask rather than let it pass. The agent chose the paired test; I asked what it was, and then whether the comparison it supported was fair. The same sensitivity to what has not yet been established is what drove the questions about held-outs and the weakened model.

## The last question

When the agent drafted a CV line for the project, it led with the score: 100 of 100 against 63 of 100.

> but is this really the takeaway or selling point of the project?

It is not. The takeaway is the division of responsibility: let the model translate, let code search and check, and measure the two separately. The number is evidence that the division did what it should in the regime where the model fails.

## What I did and did not do

I chose the project, fixed the granularity, asked for an explicit contract and validator, asked what the staged design committed the model to, proposed adversarial search by a stronger agent, asked to weaken the scheduler until it failed, asked whether the alternative survived the counterexample, asked for evidence beyond one case, and questioned the fairness and cleanliness of the comparison. Agents drafted the brief and scaffold, wrote the contracts, validator, solver and harness, generated the cases, designed the frozen protocol, ran the calls and did the statistical analysis. I did not invent the staged architecture, write the implementation, or originate the statistical method.

## Inspect the evidence

- [Actual counterexample responses](https://github.com/sarkologist/scheduling-llm/blob/main/evidence/counterexample.json) and [request/reference](https://github.com/sarkologist/scheduling-llm/blob/main/cases/dev/adversarial/01-greedy-trap.json).
- [Frozen protocol](https://github.com/sarkologist/scheduling-llm/blob/main/experiments/paired-v1/PROTOCOL.md) and [results with uncertainty](https://github.com/sarkologist/scheduling-llm/blob/main/experiments/paired-v1/RESULTS.md).
- [Model input boundary](https://github.com/sarkologist/scheduling-llm/blob/main/scheduling_llm/provider.py), [independent checker and search](https://github.com/sarkologist/scheduling-llm/blob/main/scheduling_llm/checker.py), and [interpretation/final-behaviour grading](https://github.com/sarkologist/scheduling-llm/blob/main/scheduling_llm/experiment.py).
- [Offline regrading](https://github.com/sarkologist/scheduling-llm/blob/main/evidence/README.md): recompute the comparison from 260 recorded final responses without credentials or new model calls. This is recorded evidence, not a hosted demo.

The source release is a reviewed snapshot; frozen experiment files are preserved, while private development history and raw agent sessions are excluded. See the repository for provenance and reproduction limits.
