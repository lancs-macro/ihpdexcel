---
title: ihpdexcel
type: project
status: active
created: 2026-10-27
updated: 2026-10-29
sources: []
scope: housing-observatory
tags: [project, backend]
verified_commit: fdf9cc1fbda939b7a969ac270940c6a83232567f
files: [create-excel.R, create-excel-fun.R, download-all.R, app.R, template, versions]
---
# ihpdexcel

Builds the downloadable Excel workbook of international house price indices for the International Housing Observatory.

Part of [[concepts/housing-observatory]].

## Stack
R, using the `ihpdr` package (`kvasilopoulos/ihpdr`) to download and tidy data, so nothing is fetched by hand. R 4.6.1 via `rig`; dependencies with `rv` (`rproject.toml` + `rv.lock`), never `install.packages()` or `renv`; format with `air format .`, lint with `jarl check .` before committing (repo `CLAUDE.md`).

## Run and test
- Build the workbook: `Rscript create-excel.R` (functions in `create-excel-fun.R`)
- Fetch everything: `download-all.R`
- Shiny viewer: `Rscript -e 'shiny::runApp()'` (`app.R`)

## Layout
`template/` the Excel template; `versions/` output named `hpta<YY><QQ>.xlsx` (latest `hpta2602.xlsx`), with `nominal/` and `old-real/` variants; `comparisons/`; `data/`.

## Unknowns
- Where the workbook is published for users: not documented here.

Related: [[concepts/housing-observatory]], [[conventions]], [[inbox/ihpdexcel]].
