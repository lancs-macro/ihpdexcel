# ihpdexcel

Builds the downloadable Excel workbook of international house price indices from the IHPD (via the `ihpdr` package, `kvasilopoulos/ihpdr` on GitHub). `create-excel.R` / `create-excel-fun.R` do the main build, `download-all.R` fetches source data, `app.R` is a small Shiny viewer, `versions/` and `comparisons/` hold prior outputs.

## Environment

Dependencies are pinned with `renv` (installer backend: `pak`, enabled via `.Rprofile`).

- Restore: `Rscript -e 'renv::restore()'`
- Add a dependency: `Rscript -e 'renv::install("pkg"); renv::snapshot()'`
- After changing dependencies: `Rscript -e 'renv::snapshot()'`

## Commands

- Build the workbook: `Rscript create-excel.R`
- Run the Shiny viewer: `Rscript -e 'shiny::runApp()'`
