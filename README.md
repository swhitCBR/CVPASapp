
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{CVPASapp}`

## Installation

The CVPAS app code is housed within an R package. To run the app locally
you will neeed to navigate to the root directory ‘CVPASapp’ then run the
following code to:

minor

``` r
fs::dir_tree(type="directory")
.
├── data
├── dev
├── inst
│   └── app
│       └── www
│           ├── about
│           ├── images
│           │   ├── png
│           │   └── svg
│           │       └── basic route schematic
│           ├── main
│           ├── met_and_ref
│           ├── modal
│           ├── sidebar
│           └── unused
│               └── old_MDS file
├── man
├── R
├── rsconnect
│   └── shinyapps.io
│       └── swhit-cbr
└── tests
    └── testthat
```

## Running the app

``` r

# loads all of the required functions into your environment
devtools::load_all("CVPASapp")

launches the application
CVPASapp::run_app()
```

This README has been compiled on the

``` r
Sys.time()
#> [1] "2026-06-08 10:12:35 PDT"
```
