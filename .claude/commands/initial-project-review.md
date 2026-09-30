---
description: Full onboarding review — understand the design, identify risks, and propose small improvements. Use when joining a project for the first time or revisiting after a long absence.
allowed-tools: Read, Bash
---

# Initial Project Review

Use this command when joining a project for the first time or revisiting after a long absence.
It is designed to bring you up to speed on the codebase, identify risks, and suggest small improvements.

Do not modify any files.

---

## Part 1: Understand the Design

Please begin by explaining the current state of this project:

1. **Overall purpose** — What does this project do? Who uses it? What problem does it solve?
2. **Directory structure** — How is the code organized? What is the responsibility of each major directory and file?
3. **Main execution flow** — How does data flow from entry point(s) to output(s)? What are the critical paths?
4. **Dependencies and integrations** — What external libraries, services, or APIs does it depend on? How tightly coupled are they?
5. **Key assumptions and constraints** — What foundational assumptions does the code rely on? (e.g., data format, timing, ordering, scale, environment.)
6. **Current state** — Is this mature production code, active development, or in maintenance mode?

---

## Part 2: Identify Issues and Opportunities

After explaining the design, provide a concise assessment:

### Issues

Look for:

- **Correctness risks** — bugs, missing edge case handling, logic errors
- **Reliability risks** — error handling, failure modes, data loss, silent failures
- **Maintainability concerns** — unclear code, missing documentation, tight coupling, technical debt
- **Operational risks** — performance, scalability, security, monitoring gaps
- **Documentation gaps** — missing setup instructions, unclear APIs, outdated examples

### Opportunities

Look for:

- Ways to **simplify the code** without losing clarity or correctness
- Ways to **improve maintainability** (better names, smaller functions, clearer structure)
- Ways to **improve reliability** (better error handling, validation, test coverage)
- Ways to **improve documentation** (examples, edge cases, assumptions)

### Distinguish by Priority

For each issue or opportunity, rank it:

- **High-priority** — correctness bug, security risk, data loss, missing critical test coverage
- **Medium-priority** — clarity, maintainability, reliability improvement, documentation gap
- **Low-priority** — style, minor optimization, nice-to-have improvement

---

## Part 3: Suggest Small Improvements

Based on your assessment above, propose **5–10 small, low-risk improvement tasks**.

For each task, provide:

- **Title** — brief, actionable name
- **Why it matters** — what benefit does it provide? (reliability, clarity, documentation, maintenance)
- **Estimated effort** — Small / Medium / Large
- **Risk level** — Low / Medium / High
- **Implementation hint** — a 1–2 sentence sketch of how to do it

**Favor improvements that:**
- Increase reliability
- Improve clarity or discoverability
- Improve documentation
- Simplify maintenance
- Reduce future risk

**Avoid:**
- Large refactors
- Architectural changes
- Dependency additions
- Optimizations (unless there's a demonstrated performance problem)

---

## Do Not

- Propose major rewrites unless you discover a clear, critical problem.
- Assume the codebase needs refactoring; small incremental improvements are preferred.
- Suggest architectural changes without strong justification.
- Recommend adding dependencies unless there's significant value.
