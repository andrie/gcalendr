# gcalendr

This package enables you to read events from google calendar

## Installation

You can install gcalendr from github with:


``` r
# install.packages("devtools")
devtools::install_github("andrie/gcalendr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# Set up google oauth permissions
google_token <- get_google_token()

# Get list of available calenders
calendar_ids <- get_gcal_list(google_token)
calendar_ids

# Retrieve list of events from a calendar
events <- get_gcal_events(calendar_ids[1], google_token)
events
```
