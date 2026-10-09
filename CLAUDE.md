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
