
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{CVPASapp}`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Installation

You can install the development version of `{CVPASapp}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
CVPASapp::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2026-04-21 15:33:30 PDT"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ══ Documenting ═════════════════════════════════════════════════════════════════
#> ℹ Installed roxygen2 version (7.3.3) doesn't match required (7.1.1)
#> ✖ `check()` will not re-document this package
#> ── R CMD check results ──────────────────────────────── CVPASapp 0.0.0.9000 ────
#> Duration: 19s
#> 
#> ❯ checking DESCRIPTION meta-information ... WARNING
#>   Non-standard license specification:
#>     What license is it under?
#>   Standardizable: FALSE
#> 
#> ❯ checking code files for non-ASCII characters ... WARNING
#>   Found the following file with non-ASCII characters:
#>     R/utils_ui.R
#>   Portable packages must use only ASCII characters in their R code and
#>   NAMESPACE directives, except perhaps in comments.
#>   Use \uxxxx escapes for other characters.
#>   Function 'tools::showNonASCIIfile' can help in finding non-ASCII
#>   characters in files.
#> 
#> ❯ checking dependencies in R code ... WARNING
#>   '::' or ':::' imports not declared from:
#>     'shinyjs' 'usethis'
#>   Namespaces in Imports field not imported from:
#>     'bslib' 'markdown'
#>     All declared Imports should be used.
#> 
#> ❯ checking top-level files ... NOTE
#>   Non-standard file/directory found at top level:
#>     'RUN_ME_FIRST_golem_app_setup.R'
#> 
#> 0 errors ✔ | 3 warnings ✖ | 1 note ✖
#> Error: R CMD check found WARNINGs
```

``` r
covr::package_coverage()
#> CVPASapp Coverage: 37.59%
#> R/app_config.R: 0.00%
#> R/app_server.R: 0.00%
#> R/app_ui.R: 0.00%
#> R/mod_about_page.R: 0.00%
#> R/mod_main_page.R: 0.00%
#> R/run_app.R: 0.00%
#> R/utils_golem_init_wrap.R: 0.00%
#> R/utils_ui.R: 0.00%
#> R/golem_utils_server.R: 100.00%
#> R/golem_utils_ui.R: 100.00%
```
