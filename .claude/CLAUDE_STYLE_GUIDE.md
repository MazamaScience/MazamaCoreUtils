# CLAUDE_STYLE_GUIDE.md

Cross-project working principles for AI assistants (and human contributors)
collaborating on software.

This file is **tech-stack-independent and portable**: it captures *how* we want
work done, not the details of any one repository. It is intended to be copied
into multiple projects unchanged. Project-specific facts (language, build tools,
data conventions, public API) live in each repository's own `CLAUDE.md`.

---

## General Philosophy

The maintainer values clarity, correctness, and long-term maintainability over
novelty or abstraction.

- Prefer simple, understandable code over clever solutions.
- Favor maintainability over optimization unless performance is a demonstrated
  problem.
- Prefer explicit code over metaprogramming or excessive abstraction.
- Small, incremental improvements are preferred over large rewrites.
- Existing working code should be respected and changed conservatively.
- Avoid introducing complexity unless there is a clear benefit.
- Avoid introducing dependencies unless they provide significant value.

## Documentation and Comments

- Write code for future maintainers.
- Public functions should have clear API documentation describing parameters,
  return values, and units.
- Include explanatory comments describing intent, not just implementation.
- For domain-specific algorithms, document the reasoning and any external
  specifications the code follows.
- Code readability and clear documentation matter — not all contributors are
  professional software engineers.

## Refactoring

- Do not propose architectural rewrites unless specifically requested.
- Preserve existing behavior unless a bug or design issue has been identified.
- When suggesting refactoring, explain the expected benefit.
- Prefer minimal diffs that are easy to review.
- Small improvements are generally preferred over large rewrites.

## Error Handling

- Failures should be detected and reported clearly.
- Avoid silent failures.
- Error messages should provide useful context (e.g. validate input types and
  explain what was expected).
- Reliability is more important than elegance.

## Communication Style

- Explain recommendations before making substantial changes.
- Distinguish between required fixes and optional improvements.
- Identify risks and tradeoffs.
- Rank recommendations by priority.
- Do not assume a rewrite is desired.

## Review Priorities

When reviewing code, prioritize recommendations in this order:

1. **Correctness** — logic errors, edge cases, algorithmic mistakes.
2. **Reliability** — operational robustness and backward compatibility.
3. **Maintainability**
4. **Readability**
5. **Performance**

Performance optimizations should generally be proposed only after correctness
and maintainability concerns have been addressed.
