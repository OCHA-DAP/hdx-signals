# `feat/sea5-anomaly-indicator` — state of the work

Review date: 11 September 2026. Branch last touched 27 July 2026.

## TL;DR

The branch adds a new HDX Signals indicator, `sea5_anomaly`, driven by ECMWF
SEAS5 seasonal precipitation forecasts. The indicator skeleton and a global
choropleth map are written; the data plumbing is not finished. **It is not
runnable as it stands** — `src/utils/cloud_storage.R` calls a
`container_seas5()` helper that was never written, which is exactly where the
"WIP: committing to resume work later" commit stops.

Beyond that blocker: `alert()` is missing the `extreme_case` column that real
monitoring runs require, the indicator is unregistered in the repo's central
indicator metadata and in the triage workflow, lint and the CHANGES gate will
both fail, there is no real plot or AI summary, and the map predates the August
2026 HDX redesign that has since landed on `main`. Call it roughly half done:
the shape is right, the wiring is not.

## Branch facts

| | |
|---|---|
| Branch | `feat/sea5-anomaly-indicator` (local and `origin`) |
| Branched from `main` at | `97d49fd`, 21 May 2026 |
| Commits ahead of `main` | 4 |
| Commits behind `main` | 4 |
| Unpushed | 1 — the WIP commit `3cb6616` is local only |
| Related issue | [#343](https://github.com/OCHA-DAP/hdx-signals/issues/343) "Add map to sea5_anomaly indicator" — still **open** |
| Related PR | [#344](https://github.com/OCHA-DAP/hdx-signals/pull/344) — merged, but into *this branch*, not `main` |

Commits, newest first:

```
3cb6616  2026-07-27  WIP: committing to resume work later        (cloud_storage.R only)
472903c  2026-07-10  Feat: add global choropleth map (#344)
01cd751              feat: add workflow for sea5
5656664              feat:wip add indicator
```

Nothing from this branch has reached `main`: `grep -rn "sea5" src/ .github/` on
`main` returns nothing.

## What has been built

433 lines added across 10 files.

### The indicator module — `src/indicators/sea5_anomaly/`

Follows the repo's box-module indicator convention (`__init__.R` re-exporting
`utils/*`, `indicator_id`, and a `generate_signals()` call guarded by
`is.null(box::name())`). Dry-run filter is `c("ETH", "SOM")`.

| File | State | Notes |
|---|---|---|
| `__init__.R` | Done | Exports all seven functions plus `indicator_id`. Passes `validate_indicator()`, which requires `indicator_id`, `raw`, `wrangle`, `alert`, `info`, `summary`. |
| `utils/raw_sea5_anomaly.R` | **Incomplete** | One-liner reading `output/sea5_anomaly/raw.parquet`, but it does **not** pass `container = "seas5"`, so it currently reads from the default `prod` container — inconsistent with the whole point of the `cloud_storage.R` change. |
| `utils/wrangle_sea5_anomaly.R` | Done (pass-through) | Returns `df_raw` unchanged. Fine if the upstream `ds-seas5-skill` output is already alert-ready. |
| `utils/alert_sea5_anomaly.R` | **Incomplete** | Alerts where `p_5rp > 0.2`. Emits all seven `alerts_template_base` columns with the right types (`alert_level_numeric = 1L` integer, `value` double) — but **omits `extreme_case`**, which `filter_alerts.R:104`/`:111` references on every real monitoring run. See blocker (2). Only ever sets level 1, i.e. every SEAS5 signal is "Medium concern"; there is no level-2 path. The 20% threshold is not documented or justified anywhere on the branch. |
| `utils/info_sea5_anomaly.R` | **Wrong link** | `hdx_url` and `other_urls` are `NA`. The `source_url` points at the **ERA5 reanalysis** page, not SEAS5 seasonal forecasts, while the prose says "ECMWF SEA5 seasonal forecasts". |
| `utils/summary_sea5_anomaly.R` | **Placeholder** | `summary_long = NA`, `summary_source = NA`; `summary_short` is just the alert `title`. No `prompts/` folder, so no AI summary — the email location block will render without a narrative. |
| `utils/plot_sea5_anomaly.R` | **Placeholder** | `sea5_anomaly_plot()` returns `NULL`. This is safe — `create_images.R:101` handles a `NULL` return by emitting `NA` id/url — so the email just carries no chart. Per issue #343 this may be intentional (map only). |
| `utils/map_sea5_anomaly.R` | Substantially done | 201 lines, the real work on this branch. See below. |

### The map (the bulk of the work, PR #344)

`map_sea5_anomaly.R` builds a **global** choropleth reproducing the
`ds-seas5-skill` reference map:

- Five anomaly categories from `forecast_percentile`, cut at the 10th/90th
  (10-year RP) and 33rd/67th (3-year RP) percentiles, plus `off_season` and
  `not_monitored`, on a drier-to-wetter colour strip.
- Forecast skill from `pearson_r` (>= 0.5 high, >= 0.3 moderate, else low) drawn
  as a `ggpattern` solid/stripe/crosshatch overlay on the fill.
- Trimester label computed from the alert date — a July forecast prints `ASO`,
  as issue #343 asks.
- Fixed 10x5in canvas via `settings = "plot"`, bypassing per-country
  `iso3_map_settings.json`. All `create_images()` arguments used are valid.

Note the map reads `forecast_percentile`, `pearson_r`, `is_rainy` from
`df_raw`, whereas `alert()` reads `p_5rp`. Both come from the same parquet; the
schema of that file has not been verified against either consumer on this
branch.

### Infrastructure

- `.github/workflows/monitor_sea5_anomaly.yaml` — copied from the other monitor
  workflows, wired to `Rscript ./src/indicators/sea5_anomaly/__init__.R`. It has
  **no `schedule:` block** (every monitor except `who_cholera` has one), so it is
  manual-dispatch only.
- `src/utils/cloud_storage.R` — `"seas5"` added as a fourth container option to
  `read_az_file()`, `update_az_file()`, `az_file_detect()` and `get_container()`,
  to reach the shared `projects` container that `ds-seas5-skill` writes to.

## Blockers and gaps, in the order they will bite

1. **`container_seas5()` does not exist.** `get_container()` dispatches
   `seas5 = container_seas5()` at `src/utils/cloud_storage.R:181`, but only
   `container_prod()`, `container_dev()` and `container_wfp()` are defined
   (lines 222/236/252). Any call with `container = "seas5"` errors. This is the
   WIP cut-off — resume here. It needs the `projects` container name and
   presumably a dev SAS token.

   Related: **nothing in this repo produces `output/sea5_anomaly/raw.parquet`.**
   `raw()` assumes `ds-seas5-skill` publishes it. That pipeline's schedule and
   output contract are an external dependency that has not been pinned down.
2. **`alert()` does not set `extreme_case`.** `filter_alerts.R:104` and `:111`
   reference the column on the ongoing-monitoring path (`HS_DRY_RUN=FALSE`,
   `HS_FIRST_RUN=FALSE`), so a real run dies with `object 'extreme_case' not
   found`. Dry runs pass, which is why this has not surfaced. Every other
   indicator sets it — `FALSE` in jrc/acled/idmc/acaps/wfp, `phase == "phase5"`
   in ipc. (`who_cholera` also omits it; that is a latent bug there, not a
   precedent.)
3. **`raw()` never asks for the `seas5` container** — see (1). Fix together with
   it, and confirm which container/path `ds-seas5-skill` actually publishes to.
4. **`sea5_anomaly` is not in `src-static/update_indicator_mapping.R`.** That
   script hardcodes every indicator's `mc_interest` / `mc_tag` / `mc_folder` /
   `indicator_subject` / `static_segment` / `banner_url` / `data_source` and
   writes `input/indicator_mapping.parquet`. Without a row there:
   - `caption$caption()` (`src/images/plots/caption.R:32`) resolves
     `data_source` to `character(0)` — the map caption breaks;
   - `save_image.R:30` and `:117` cannot resolve the Mailchimp folder;
   - `create_campaigns.R:234` has no segment to send to.
   This requires a **new Mailchimp static segment / interest to be created**, so
   it is a coordination task, not just a code edit.
5. **The branch is 4 commits behind `main`, including the 2025 HDX redesign**
   (`f00cfb9`, 13 August 2026, PR #348), which rewrote `map_theme.R`,
   `theme_signals.R`, `plot_ts.R`, the email components, and added
   `src/images/plots/hdx_signals_palette.R`. The SEAS5 map was written in July,
   before that, and hardcodes its own hexes (`#5A5A5A` boundaries, `#e2e8e8`
   neutral) instead of the redesign tokens (`map_boundary = "#C4D0D1"`,
   `hairline = "#D8E0E1"`). `map_theme()`'s signature is unchanged, so the rebase
   should be mechanical, but the map will look off-brand next to every other
   Signals image until the palette is aligned.
6. **Boundaries come from Natural Earth, not the repo's boundary source.** The
   map calls `rnaturalearth::ne_countries(scale = "medium")` and joins on
   `iso_a3`; every other map in the repo loads OCHA boundaries via
   `src/images/maps/sf_adm0.R`. Two consequences:
   - `rnaturalearthdata` (required for `scale = "medium"`) is **not in
     `renv.lock`** — `rnaturalearth` and `ggpattern` are, but the data package is
     not, so the call fails in CI. Same on `main` and on the branch.
   - Natural Earth's `iso_a3` is `-99` for several territories (France, Norway,
     Kosovo among them), which silently drops them to `not_monitored`.
   - The map calls `caption$caption(..., map = FALSE)`, so the standard UN
     boundary disclaimer ("The boundaries and names shown... do not imply
     official endorsement") is **omitted from a world map published by a UN
     product**. Worth resolving deliberately before anything is sent.
7. **Lint CI will fail.** Every other indicator's `__init__.R` is listed under
   `exclusions:` in `.lintr` (lines 18-25) because `box.linters` flags the
   `[...]` re-export pattern; `sea5_anomaly` is not there. Separately,
   `map_sea5_anomaly.R`'s UPPER_CASE constants (`SEA5_R_MOD`, `ANOMALY_LEVELS`,
   `SKILL_PATTERNS`, ...) violate the repo's `object_name_linter(styles =
   "snake_case")`.
8. **`sea5_anomaly` is not in the `triage_signals.yaml` `INDICATOR_ID` choice
   list** (lines 45-56), so signals could be generated but not triaged from the
   Actions UI.
9. **No `CHANGES.md` entry and no `.signals-version` bump.** `check_changes.yml`
   runs `src/repo/check_changes.R` on every PR to `main` and hard-fails if the
   branch version is not greater than main's and if `CHANGES.md` has no section
   for it. Current version is 0.6.1.1.
10. **No schedule, no README.** The missing `schedule:` block also means Slack
    monitoring reports "No scheduled update" — `check_signals_updates.R:165`
    filters workflow runs on `event == "schedule"`. And `who_cholera` carries a
    `README.md` documenting its methodology for non-publicly-subscribable
    indicators; there is nothing describing the `p_5rp > 0.2` rule.

    No per-indicator tests exist anywhere in the repo, so sea5 is not behind on
    that front — `TESTING.md` flags `src/indicators` as untested in general.

Two more allowlists matter only if manual info is ever attached to SEAS5
signals: `src/utils/validate_manual_info.R:17-26` (unlisted ids are silently
dropped) and `src/utils/get_manual_info.R:90-105` (an unlisted id falls through
the `if/else if` chain and returns `NULL`).

## Open questions to answer before resuming

- Where does `ds-seas5-skill` publish `raw.parquet` — which container, which
  path, which columns, on what schedule? Everything in (1) and (3) depends on
  the answer, as does whether `wrangle()` can stay a pass-through.
- Is a chart in scope at all, or is the map the only image (issue #343 suggests
  map-only)? If map-only, delete the plot placeholder rather than ship a stub.
- Is an AI summary wanted? If yes, this needs a `prompts/` folder
  (`system.txt`, `short.txt`, `long.txt`) like `jrc_agricultural_hotspots`.
- Should SEAS5 ever raise a "High concern" (level 2) alert, and on what
  threshold?
- Does this indicator get its own public Mailchimp subscription (new segment), or
  is it internal like `who_cholera`?

## Suggested resume order

1. Confirm the SEAS5 blob location and schema, then write `container_seas5()`
   and pass `container = "seas5"` in `raw()`. Run a local dry run against
   ETH/SOM — that alone gets the branch from "errors immediately" to "produces
   something you can look at".
2. Add `extreme_case` to `alert()`, so a non-dry run can get past
   `filter_alerts()`.
3. Add `rnaturalearthdata` to `renv.lock` (if Natural Earth stays) and the
   `.lintr` exclusion for the new `__init__.R`, plus snake_case the map
   constants. These are what CI will complain about first.
4. Rebase on `main` and re-point the map at `hdx_signals_palette` tokens.
5. Decide the boundary question (OCHA `sf_adm0` vs Natural Earth) and turn the
   caption's `map = TRUE` on so the UN disclaimer is printed.
6. Add the `sea5_anomaly` row to `update_indicator_mapping.R` once the Mailchimp
   segment exists, and the `INDICATOR_ID` option in `triage_signals.yaml`.
7. Fix the ERA5/SEAS5 `source_url`, decide on plot and summary, add a cron to the
   workflow.
8. Bump `.signals-version` plus a `CHANGES.md` entry (hard PR gate), then open
   the PR to `main` and close issue #343.
