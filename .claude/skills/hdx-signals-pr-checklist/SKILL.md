---
name: hdx-signals-pr-checklist
description: Runs hdx-signals' required pre-PR checks (version bump, CHANGES.md entry, lint) before opening a pull request in this repo. Use whenever creating, or about to create, a PR against OCHA-DAP/hdx-signals - including draft PRs.
---

## Why this exists

`hdx-signals` CI has two checks that fail the PR if skipped, and a third
that's easy to break without noticing:

- **`check-changes`** (`.github/workflows/check_changes.yml` running
  `src/repo/check_changes.R`): fails unless `.signals-version` in this
  branch is strictly greater than the version on `main`, **and**
  `CHANGES.md` has a matching `## <version> (<date>)` section.
- **`lint`** (`.github/workflows/lint.yaml`): runs `lintr::lint_dir()`
  with `LINTR_ERROR_ON_LINT=true`, which fails the job on **any** lint,
  not just errors.
- **Pinned lint tooling**: the workflow installs `lintr`/`box.linters` at
  the exact versions pinned in `renv.lock`, not "latest" - a fresh CRAN
  release of either package can otherwise break lint independently of any
  code change (this happened with lintr 3.4.0, which crashes
  `indentation_linter` on this repo's code with `replacement has length
  zero`). If you ever touch `.github/workflows/lint.yaml`, keep it pinned
  to the `renv.lock` versions rather than reverting to `install.packages()`.

## Instructions

Run this before opening (or updating a draft into) a PR:

1. **Bump `.signals-version`.**
   - Read the version on `main`:
     `curl -s https://raw.githubusercontent.com/OCHA-DAP/hdx-signals/main/.signals-version`
   - Your branch's `.signals-version` must be strictly greater (as an
     R `numeric_version`, e.g. `0.6.0.0 > 0.5.0.0`). Bump the version
     component that matches the size of the change (patch-level bump
     for a small fix, minor for a feature) - follow the existing pattern
     in `CHANGES.md`'s history.

2. **Add a `CHANGES.md` entry.**
   - Insert a new section right after the `# Changes` header, above the
     previous top entry:
     ```
     ## <new version> (<DD Month YYYY>)
     - <bullet summary of what changed and why>
     ```
   - The date **must** parse with R's `as.Date(format = "%d %B %Y")`
     (e.g. `24 July 2026`, not `2026-07-24` or `Jul 24 2026`).
   - The version string in the heading must exactly match
     `.signals-version` (dots, no `v` prefix).
   - Summarize the actual diff - don't just restate the PR title.

3. **Lint locally with the pinned versions before pushing**, not
   whatever's on `.libPaths()` by default - a locally-installed "latest"
   lintr can pass locally and still crash in CI, or vice versa. From the
   project root, with `renv`'s library on `.libPaths()`:
   ```r
   .libPaths(c("renv/library/windows/R-4.4/x86_64-w64-mingw32", .libPaths()))
   lintr::lint_dir()
   ```
   Fix anything reported. If `lint_dir()` throws an R error (not just
   returns lints) instead of returning a lint list, that's the
   crash-on-latest-CRAN-release failure mode above, not a real lint - do
   not "fix" it by editing arbitrary code; check `renv.lock` still
   matches what's installed and pinned in the workflow instead.

4. Only then create the PR (draft or otherwise).

## Judgment calls

- If dozens of pre-existing, clearly-false-positive lints show up in
  files you didn't touch (e.g. `box_usage_linter` flagging a `module$fn()`
  call where `fn` is genuinely exported), that's a known, longstanding,
  repo-wide issue unrelated to your change - don't silently rewrite
  unrelated files to chase a fully green lint run. Flag it in the PR
  description instead of guessing at a fix.
- Don't bump the version or add a CHANGES.md entry for changes that are
  purely conversational/no-diff (e.g. answering a question) - only do
  this when there's an actual code diff going into the PR.
