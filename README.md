
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gcalendr <img src='man/figures/logo.svg' align="right" height="139" />

This package enables you to read events from google calendar.

## Installation

The package is not yet on CRAN.

You can install the development version of `gcalendr` from github using:

``` r
# install.packages("devtools")
devtools::install_github("andrie/gcalendr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# Set up google oauth permissions
google_token <- get_google_token()

# Get tibble of available calenders
calendar_ids <- get_gcal_list(google_token)
calendar_ids

# Retrieve tibble of events from a calendar
# By default maximum 250 results are returned

my_cal_id <- calendar_ids[7, "id"]
events <- get_gcal_events(my_cal_id, google_token, days_in_past = 90, days_in_future = 90)
events
```
