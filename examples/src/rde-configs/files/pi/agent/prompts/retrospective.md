---
description: Analyze interaction history for reusable insights and suggest efficiency improvements
---
Review the full interaction history of this session and identify useful reusable insights. Look for patterns such as:

- Tasks that required many trial-and-error iterations but wouldn't have if more context about the environment, OS, or available tools had been known upfront.
- Jobs that took a lot of time, compute, or steps but could have been simplified with better preparation or tooling.
- Repeated questions or lookups that could be answered once and recorded.
- Workarounds for missing tools, packages, or capabilities.
- Any other friction points or inefficiencies.

For each insight you find, evaluate which action would best capture it:

1. **Record to AGENTS.md** — Add environment facts, project conventions, or behavioral notes that would help future sessions start with the right context.
2. **Create a new SKILL.md** — If a multi-step procedure was discovered that could be packaged as a reusable agent skill, draft it following the agentskills.io specification.
3. **Create a new extension for the coding agent** — If a custom tool or automation would eliminate repeated manual steps, outline it.
4. **Improve environment / OS / tools** — If installing a package, configuring a tool, or changing the system setup would prevent future friction, recommend it.
5. **Something else**.

After your analysis, prepare a structured report for the user with the following sections:

## Session Retrospective

### Insights Found
List each insight with a short description of the friction observed.

### Recommended Actions
For each insight, state the recommended action (AGENTS.md entry, new skill, new extension, or environment change) and provide a concrete draft or implementation plan.

### Summary
A brief overview of expected efficiency gains for the next session.

Then ask the user which recommendations they'd like you to apply now.
