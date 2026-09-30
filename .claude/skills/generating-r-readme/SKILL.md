---
name: generating-r-readme
description: Use when creating or updating the top-level README.md for an R package, to align it with R package and GitHub documentation best practices.
---

# Generating an R Package README

## Overview

This skill creates or updates the top-level `README.md` for an R package using
the standardized structure in `TEMPLATE.md`, next to this file.

The README is primarily **user-facing package documentation**. Its purpose is to
help someone encountering the package understand:

1. What problem does this package solve?
2. How do I install it?
3. How do I perform a basic task with it?
4. Where do I go for more detailed documentation or help?

Do not turn the README into a repository operations manual. Development,
deployment, release, architecture, and AI-assistant workflow documentation
belong elsewhere unless they are directly relevant to package users.

**Core rule:** every factual claim in the finished README must be traceable to
something you actually read in this repository or to authoritative package
metadata such as CRAN.

Do not guess what a package does from its name or from conventions used by
other packages.

A bracketed placeholder left unfilled, or filled with a guess, is worse than
omitting that material entirely.

**The output is a README, not a diff against the template.** A reader should
never see evidence of the generation process: no placeholder brackets, no
comments about omitted template sections, and no references to this skill or
template.

## README source file

Before editing, determine how the README is maintained.

- If the repository has `README.Rmd`, treat it as the source of truth. Edit
  `README.Rmd`, not the generated `README.md`, unless explicitly instructed
  otherwise.
- If the repository has only `README.md`, edit `README.md` directly.
- Do not introduce `README.Rmd` merely because it is common R-package practice.
  Converting the README workflow is a separate project decision and should not
  happen implicitly while updating documentation.
- If `README.Rmd` generates figures under `man/figures/` or another location,
  preserve the repository's existing convention.

When both `README.Rmd` and `README.md` exist, verify whether `README.md` appears
to be generated before making changes.

## Primary sources to inspect

Inspect the package itself before drafting the README.

| README content            | Primary sources to inspect                                              |
| ------------------------- | ----------------------------------------------------------------------- |
| Package name and purpose  | `DESCRIPTION`, existing README, package-level documentation             |
| CRAN status and version   | `DESCRIPTION`, CRAN metadata or existing valid CRAN links               |
| Package website           | `DESCRIPTION`, `_pkgdown.yml`, existing README links                    |
| Installation instructions | CRAN status, repository hosting, existing installation documentation    |
| Exported functions        | `NAMESPACE`, `R/`, generated documentation in `man/`                    |
| Function families         | `R/`, `NAMESPACE`, vignettes, package-level documentation               |
| Basic example             | Existing examples, vignettes, tests, example datasets                   |
| Included datasets         | `data/`, `R/data.R`, corresponding documentation                        |
| Dependencies              | `DESCRIPTION`                                                           |
| Compiled code             | `src/`, `DESCRIPTION`, build configuration                              |
| Vignettes                 | `vignettes/`, `DESCRIPTION`                                             |
| Tests                     | `tests/`, package configuration                                         |
| CI badges                 | `.github/workflows/` or other actual CI configuration                   |
| Coverage badge            | Existing coverage service configuration                                 |
| License                   | `DESCRIPTION`, `LICENSE`, `LICENSE.md`                                  |
| Citation information      | `inst/CITATION`, `DESCRIPTION`                                          |
| Support/community links   | Repository issue/discussion configuration and existing documented links |

Prefer package metadata and source code over prose that may have become stale.

## Recommended README structure

The default ordering is:

1. Package name
2. Badges
3. Short package description
4. Installation
5. Example
6. Package/function overview
7. Background or design rationale, when useful
8. Documentation and help
9. Funding, acknowledgements, citation, or license information when appropriate

This is a default, not a requirement. Use judgment when the package has a
strong reason for a different ordering.

The first screenful should normally tell a prospective user what the package
does and give them enough information to decide whether it is relevant.

## Package name and badges

Use the package name as the top-level heading:

```markdown
# PackageName
```

Place useful badges immediately below it.

Only include badges backed by real services or repository configuration.
Examples may include:

- CRAN version/status
- CRAN downloads
- `R CMD check`
- test coverage
- package lifecycle

Do not add decorative or hypothetical badges.

Do not duplicate information merely to have more badges.

## Package description

Follow the badges with one or two short paragraphs describing:

- what the package does;
- the problem or domain it serves;
- important design characteristics that distinguish it from alternatives, when
  relevant.

Use ordinary prose, not a code block.

Prefer wording supported by `DESCRIPTION` and package documentation, but do not
blindly copy an awkward `DESCRIPTION` sentence when clearer user-facing prose
already exists in the repository.

Avoid implementation detail unless it matters to package users.

## Installation

Provide copyable R installation commands.

If the package is on CRAN, show CRAN installation first:

```r
install.packages("PackageName")
```

If a development version is intentionally supported from GitHub, show it
separately using the installation mechanism actually documented or depended
upon by the repository.

Do not advertise installation sources that do not exist.

Do not add prerequisite installation commands for `remotes`, `pak`, or
`devtools` unless useful to the intended audience.

## Example

Include one small, representative example whenever the package supports one.

A good README example should:

- demonstrate a common package use case;
- be understandable without reading the API reference first;
- use real exported functions;
- use included or easily created data;
- remain short enough to scan quickly.

Prefer showing the package solving a problem over listing function calls with
no context.

If the README is generated from `README.Rmd`, examples may execute and include
their output or plots. If the repository maintains `README.md` manually, do not
introduce generated output merely to imitate an R Markdown README.

Do not invent example APIs from function names alone. Verify signatures and
behavior from source or documentation.

## Package overview

Describe the major capabilities of the package after the reader has seen its
purpose and a basic example.

For a package with related function families, grouped bullets are usually more
useful than an exhaustive flat function inventory.

For example:

- rolling statistics;
- filtering and outlier detection;
- domain-specific calculations;
- utilities supporting those calculations.

Mention representative exported functions when that helps users navigate the
API.

Do not duplicate complete function reference documentation in the README.
Detailed argument and return-value documentation belongs in function help and
the pkgdown reference.

## Background and ecosystem context

A `Background`, `Motivation`, or similar section is useful when users benefit
from understanding why the package exists or how it differs from alternatives.

If competing or related R packages are discussed:

- verify that package names and links are correct;
- describe differences factually rather than marketing the package;
- focus on the design need this package addresses;
- keep detailed comparisons in a vignette when they become lengthy.

Domain-specific motivation is valuable when it explains package design.

## Documentation and help

When they actually exist, link users to resources such as:

- pkgdown package website;
- introductory or topical vignettes;
- function reference;
- GitHub Issues;
- GitHub Discussions.

Do not add a generic "Additional Resources" section merely to preserve template
shape.

Use descriptive link text rather than raw URLs where practical.

## Citation, license, funding, and acknowledgements

These are optional README elements because authoritative package metadata often
exists elsewhere.

Include them when they are useful to package users.

Examples:

- a citation link when academic or professional citation is expected;
- a brief funding acknowledgement;
- a concise license statement;
- acknowledgements important to the project's provenance.

Do not reproduce long legal or citation metadata that already has an
authoritative source in the package.

## Development information

The README may briefly point contributors toward `CONTRIBUTING.md` or
development documentation if such files exist.

Do not add internal development procedures, AI-assistant commands, release
checklists, deployment instructions, environment variables, repository
architecture, or local developer setup unless the package README is explicitly
intended to serve that purpose.

In particular, do not include project-specific Claude workflows merely because
they exist in the repository.

## Formatting for skimmability

- **No emoji** unless the existing project explicitly uses them as part of its
  documentation style.
- Use a single `#` heading for the package name.
- Prefer ordinary Markdown headings without bold formatting inside them.
- Use fenced code blocks with `r` for R examples.
- Keep introductory prose concise.
- Group related functions rather than producing long undifferentiated lists.
- Prefer one representative example over many small examples.
- Link to detailed documentation rather than reproducing it.
- Avoid unnecessary horizontal rules between every section.
- Use consistent R style in examples.
- Preserve established project terminology and naming.

## Common mistakes

- **Writing for maintainers instead of package users.** The README is primarily
  the package's front door.
- **Leading with history instead of purpose.** Tell users what the package does
  before explaining why it was created.
- **Putting prose in a code block.** Descriptions belong in normal Markdown.
- **Documenting every exported function.** Group capabilities and delegate
  details to reference documentation.
- **Inventing badges.** A badge must correspond to a real service or workflow.
- **Guessing installation commands.** Verify that CRAN and GitHub installation
  paths actually exist.
- **Guessing function behavior from names.** Read source or documentation.
- **Treating `DESCRIPTION` as README prose.** It is authoritative metadata, but
  the README should still read naturally.
- **Adding Node/application concepts to an R package.** Ports, runtime servers,
  environment configuration, deployment infrastructure, and application startup
  commands usually do not belong here.
- **Forcing every template section into the README.** Omit sections that do not
  benefit users.
- **Leaving placeholders behind.** The finished README must contain only
  verified, publishable content.
- **Editing generated `README.md` when `README.Rmd` is the source.** Determine
  the repository's README workflow first.
