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
   then report whether it changed any files.
4. Rebuild the website in `docs/` from the current documentation, and report
   any errors or warnings. If it fails, stop and report the error.

   ```
   Rscript -e 'pkgdown::build_site()'
   ```

   pkgdown renders `CLAUDE.md` (an AI project guide) to `docs/CLAUDE.html` and
   lists it in `sitemap.xml`. Delete both after every build, and confirm they are
   gone:

   ```
   rm -f docs/CLAUDE.html
   grep -v 'CLAUDE.html' docs/sitemap.xml > docs/sitemap.tmp && mv docs/sitemap.tmp docs/sitemap.xml
   ```

5. Run `git status` and summarize any uncommitted changes, noting which are
   regenerated files (`man/`, `NAMESPACE`, `docs/`).
6. If there are staged or unstaged changes, show a `git diff` summary and ask
   whether I want to commit them.
7. Report the current `Version:` from `DESCRIPTION` and the most recent entry in
   `NEWS.md`, and note whether `NEWS.md` covers the changes made this session.

Regenerating documentation (steps 3 and 4) is expected to change files. Do not
commit, push, or make any other changes unless I explicitly ask.
