---
description: Bump version, update changelog, run tests, commit, tag and hand off to developer for push and publish.
allowed-tools: Read, Edit, Bash
---

Prepare this project for a new release. Follow each step in order and stop
immediately if any step fails.

## Step 1 — Determine the new version

Read the current version from the `Version:` field of `DESCRIPTION`. Compute the next
version (patch, minor, or major — ask the developer if unclear). Show both
versions before continuing.

## Step 2 — Summarize changes since the last release

Run:

```
git log $(git describe --tags --abbrev=0)..HEAD --oneline
```

Read the output and identify the meaningful changes. Group them into:
- Bug fixes
- New features or behavior changes
- Documentation or tooling changes

Use this as the basis for the changelog entry in Step 4.

## Step 3 — Bump the version

Edit the `Version:` field of `DESCRIPTION` to update the version to the new value.
Do not change anything else in the file.

## Step 4 — Update the changelog

Add a new entry at the top of `NEWS.md` using this format:

```
# MazamaCoreUtils <new-version>

- <short bulleted description of change 1>
- <short bulleted description of change 2>
```

Keep bullets concise (one line each). Use plain language. Do not copy commit
messages verbatim — summarize the intent and effect of each change.

Mark breaking changes explicitly: **Breaking:** ...

## Step 5 — Rebuild documentation

Run:

```
Rscript -e 'devtools::document()'
```

If it fails, stop and report the error.

## Step 6 — Run tests

Run:

```
Rscript -e 'devtools::test()'
Rscript -e 'MazamaCoreUtils::check_slower()'
```

If any tests fail, or `check_slower()` reports errors or warnings, stop and
report them. Do not proceed.

## Step 7 — Commit and tag

If Steps 5 and 6 both passed, run:

```
git commit -a -m "Bump to <new-version>: <one-line summary of changes>"
git tag <new-version>
```

The commit message summary should be concise (under 72 characters) and describe
what changed functionally, not just "bump version".

## Step 8 — Handoff to developer

Print the following instructions exactly, substituting the real version number
and the correct push/publish commands for this project:

---

**Release `<new-version>` is staged. To publish, run:**

```
git push
git push origin <new-version>
```

Then submit the package to CRAN (e.g. `devtools::release()` or
`devtools::submit_cran()`) from R.

**Do not submit to CRAN until `git push` has succeeded.**

---

Do not run `git push` or any publish command yourself under any circumstances.
