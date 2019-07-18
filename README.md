
<!-- README.md is generated from README.Rmd. Please edit that file -->

# excel-iho

<!-- badges: start -->

<!-- badges: end -->

The goal of excel-iho provides exuberance excel-reports for the IHO. It
sources an excel template from the `template` dir and store the output
to `versions` dir. It utilizes the api from `{ihpdr}`, to download and
tidy data, so there is no need to fetch data manually.

``` r
source("create-excel.R")
#> Saving `hpta1901.xlsx` to `versions/hpta1901.xlsx`
```

# Download

You can download the whole repo, doing the following:

``` r
download.file("https://github.com/kvasilopoulos/excel-iho/archive/master.zip",
              destfile = "excel-iho.zip")
unzip(zipfile = "excel-iho.zip")
unlink("excel-iho.zip", force = TRUE)
```
