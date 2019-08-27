
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ihpdexcel

<!-- badges: start -->

<!-- badges: end -->

The goal of {ihpdexcel} is to create excel-reports for the International
Housing Observatory. It sources an excel template from the `template`
dir and store the output to `versions` dir, as “hpta%Year%Quarter.xlsx”.
It utilizes the api from `{ihpdr}`, to download and tidy data, so there
is no need to fetch data manually.

``` r
source("create-excel.R")
#> Saving `hpta1901.xlsx` to `versions/hpta1901.xlsx`
```

# Download

You can download the whole repo easily with the {usethis} package, just
do the following:

``` r
# install.packages("usethis")
usethis::use_course("kvasilopoulos/ihpdexcel")
```
