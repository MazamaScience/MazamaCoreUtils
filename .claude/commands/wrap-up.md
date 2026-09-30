---
description: End-of-session checklist — run tests, build, docs, and summarize git status. Run before pushing to a remote or publishing.
allowed-tools: Read, Bash
---

Please run the end-of-session checklist for this project:

1. Run `devtools::test()` (e.g. `Rscript -e 'devtools::test()'`) and confirm all
   tests pass. If any fail, report them and stop.
2. Run `Rscript -e 'MazamaCoreUtils::check_fast()'` (or
   `devtools::check(vignettes = FALSE)`) to verify the package checks cleanly
   with no new errors, warnings or notes.
3. Run `Rscript -e 'devtools::document()'` to regenerate `man/` and `NAMESPACE`,
   then report whether it changed any files. Do not rebuild the pkgdown site
   (`docs/`) unless I ask.
4. Run `git status` and summarize any uncommitted changes.
5. If there are staged or unstaged changes, show a `git diff` summary and ask
   whether I want to commit them.
6. Report the current `Version:` from `DESCRIPTION` and the most recent entry in
   `NEWS.md`, and note whether `NEWS.md` covers the changes made this session.

Do not commit, push, or modify any files unless I explicitly ask.
