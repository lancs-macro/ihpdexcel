# ihpdexcel

Builds the downloadable Excel workbook of international house price indices from the IHPD (via the `ihpdr` package, `kvasilopoulos/ihpdr` on GitHub). `create-excel.R` / `create-excel-fun.R` do the main build, `download-all.R` fetches source data, `app.R` is a small Shiny viewer, `versions/` and `comparisons/` hold prior outputs.

## Environment

- R version: managed with `rig` (currently 4.6.1 - `rig default 4.6.1`)
- Dependencies: managed with `rv` (`rproject.toml` + `rv.lock`), not renv
  - Setup: `rv sync`
  - Add a dependency: `rv add <pkg>`
- Format: `air format .` (check only: `air format --check .`)
- Lint: `jarl check .` (autofix: `jarl check . --fix`)

### Rules
- Never use `install.packages()` or `renv::*` - all dependency changes go through `rv add`/`rv remove`.
- Run `air format .` then `jarl check .` before committing.
- No `# nolint` comments - jarl uses `# jarl-ignore <rule>: <reason>` on the line before the flagged code instead.

## Commands

- Build the workbook: `Rscript create-excel.R`
- Run the Shiny viewer: `Rscript -e 'shiny::runApp()'`

<!-- knowledge:start -->
## Knowledge wiki
This repo's wiki lives in `./wiki` (committed with the code; overview: `wiki/ihpdexcel.md`). It is linked into the
shared vault at `./kb` (-> C:\Users\User\knowledge), where `kb/CLAUDE.md` holds the rules (frontmatter, wikilinks, INGEST/QUERY/LINT).
- Session start: read `kb/inbox/ihpdexcel.md`, act on items, move them under `## Done`.
- Write the wiki to the repo wiki standard: `kb/REPO-WIKI.md` (layout, page types, frontmatter, links). Scaffold pages with `python kb/scripts/new_page.py ihpdexcel architecture|decision|runbook|integration|concept "<Title>"`.
- Update the wiki page that describes a behaviour in the same change that alters it; then run `python kb/scripts/reindex.py` and `python kb/scripts/lint.py`.
- Find things with `python kb/scripts/search.py "<terms>"` before browsing `kb/wiki/INDEX.md`. Never edit `kb/raw/`.
<!-- knowledge:end -->
