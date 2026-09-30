---
description: Review all documentation layers for completeness, consistency, and alignment with source code. Catches drift between inline comments and user-facing docs.
allowed-tools: Read, Bash
---

Please review all documentation in this project for completeness and alignment.

## Scope

Review the documentation layers for this project:

1. **Inline documentation** — roxygen2 comments in `R/*.R` (parameters,
   return values, examples, `@section` blocks)
2. **Generated reference** — `man/*.Rd` (must match the roxygen comments; run
   `devtools::document()`) and `NAMESPACE`
3. **Markdown** — `README.md`, `NEWS.md`, `CLAUDE.md`,
   `.claude/CLAUDE_ARCHITECTURE.md`
4. **Vignettes** — `vignettes/*.Rmd` (cache management, date parsing, error
   handling, logging)
5. **Website config and output** — `_pkgdown.yml` (every exported topic listed
   in a `reference:` section) and `docs/` (generated; check freshness only)
6. **Cross-project docs** — `.claude/CLAUDE_STYLE_GUIDE.md`
7. **Skills** — `.claude/skills/*/SKILL.md`

## Checks for Alignment

- **README template drift:** If this project has a `.claude/skills/generating-r-readme/`
  skill, compare the top-level `README.md` against its `TEMPLATE.md`. Flag
  sections that are missing, sections that no longer reflect what the repo
  actually has, and any leftover placeholder text (bracketed fields, `TODO`
  markers) that was never filled in. Separately, for any section the skill
  marks as fixed content rather than repo-specific fact (e.g. Development →
  Ending a Work Session), flag any deviation from the template's exact
  wording — being present but reworded is drift too, not just being missing.
  If this skill isn't present in the project, skip this check.
- **Function signatures:** Do exported function names, parameters, and return
  shapes in README and CLAUDE.md match the inline documentation in source files?
- **Breaking changes:** Are breaking changes listed in the changelog also
  reflected in updated inline docs and README examples?
- **Renamed symbols:** When a function or type is renamed, verify the old name
  does not appear in Markdown documentation (other than in changelog history).
- **Units and conventions:** Do units, data format requirements, and
  missing-value conventions described in README/CLAUDE match the inline docs?
- **Examples:** Do inline doc examples match the usage patterns shown in README?
- **Missing prerequisites:** Does README clearly state any required setup,
  runtime dependencies, or environment assumptions?

## Identify

- Missing or incomplete inline documentation (missing parameters, return types,
  units, or examples)
- Stale inline documentation (references renamed functions or outdated behavior)
- Inconsistent conventions between Markdown files and inline docs
- Missing examples or unclear usage instructions
- Confusing or contradictory sections
- Outdated information (especially after version bumps or renames)
- Links or cross-references that are broken or dangling

## Provide

Suggested improvements, with:
- File and line number (or section name)
- What the drift or gap is
- How to fix it
- Priority (high / medium / low)

Do not modify any files.
