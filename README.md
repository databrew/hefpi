
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hefpi <a href='https://github.com/databrew/hepfi'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

`hefpi` is a web application for the visual exploration of the World
Bank’s Health Equity and Financial Protection Indicators. It is
currently under active development.

## Installation

You can install hefpi from github by running the following:

``` r
devtools::install_github('databrew/hefpi')
```

## Development

To work on developing this package, you’ll likely want to focus on the
following files:

  - `R/app_server.R`: The server-side code
  - `R/app_ui.R`: The user interface
  - `dev/run_dev.R`: The code you’ll use to run the app locally after
    making changes

## Deploy

To deploy on a shiny server, simply place an `app.R` file in a folder as
one normally would do, and populate that file with the following lines:

``` r
library(hefpi)
run_app()
```

## Reproducing this package

In order to build this package and run the application correctly, one
should:

1.  Clone this repository: `git clone
    https://github.com/databrew/hefpi`.  
2.  Populate the `data-raw/from_web` directory with the following two
    files (supplied directly from the WB): (a) `hefpi_full_database.dta`
    and (b) `Indicator_description.xlsx`.
3.  Populate the `data-raw/from_website` directory with the following
    three files (taken from the HEFPI website): (a) `HEFPICountry.csv`,
    (b) `HEFPIData.csv`, and (c) `HEFPISeries.csv`.
4.  Run the script in the same directory to generate R-compatible data
    files: `Rscript raw_data.R`
5.  Run `Rscript dev/run_dev.R`
