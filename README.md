
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gcalendr <img src='man/figures/logo.svg' align="right" height="139" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/andrie/gcalendr.svg?branch=master)](https://travis-ci.org/andrie/gcalendr)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/gcalendr)](https://cran.r-project.org/package=gcalendr)
<!-- badges: end -->

This package enables you to read events from google calendar.

## Installation

The package is not yet on CRAN.

You can install the development version of `gcalendr` from github using:

``` r
# install.packages("devtools")
devtools::install_github("andrie/gcalendr")
```

## Example

Use this example to authenticate, list available calendars and view
events:

``` r
## Set up google oauth permissions
## This will prompt you to specify an account
gcalendr_auth()

## To specify a specific account, provide your email address
gcalendr_auth("apdevries@gmail.com")


## Retrieve tibble of available calenders
calendar_ids <- get_gcal_list()
calendar_ids

## Retrieve tibble of events from a specific calendar

my_cal_id <- "apdevries@gmail.com"
events <- get_gcal_events(my_cal_id, days_in_past = 90, days_in_future = 90)
events
```
