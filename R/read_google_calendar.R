# Set Environment ----

# library(httr)
# library(dplyr)
# library(purrr)
# library(rlang)
#
# library(memoise)

GET <- memoise(httr::GET)

#' Get google token for authentication.
#'
#' @family gcal functions
#' @export
get_google_token <- function(){
  google_app <- oauth_app(
    'GOOGLE_APIS',
    key = "115354746897-98g2v3hvasb4q1p0p4adp37ophpqqo7l.apps.googleusercontent.com",
    secret = "0qh9MxOXWzaKIX016Jv_mVqQ"
  )
  google_token <- oauth2.0_token(
    oauth_endpoints('google'), google_app,
    scope = 'https://www.googleapis.com/auth/calendar.readonly'
  )
}


#' Get list of calendars from google.
#'
#' @param google_token token obtained from [get_google_token]
#'
#' @return character vector with calendar ids
#' @family gcal functions
#' @export
get_gcal_list <- function(google_token){
  r <- GET("https://www.googleapis.com/calendar/v3/users/me/calendarList",
           config(token = google_token))

  r %>%
    content() %>%
    pluck("items") %>%
    map_chr("id")
}


utils::globalVariables(c("attendee_count", "internal_count"))


#' Read list of google calendar events.
#'
#' @inheritParams get_gcal_list
#'
#' @param id calendar id, obtained from [get_gcal_list()]
#' @param max_results Maximum number of results retrieved per query
#' @param days_in_past Restrict results to date range, number of days in past
#' @param days_in_future Restrict results to date range, number of days into future
#'
#' @references https://developers.google.com/google-apps/calendar/v3/reference/events/list
#' @family gcal functions
#' @export
get_gcal_events <- function(id, google_token, max_results = 100, days_in_past = 90, days_in_future = 90){
  message("Reading calendar", id)
  time_min <- Sys.time() - days_in_past * 24 * 3600
  time_max <- Sys.time() + days_in_future * 24 * 3600

  time_min <- strftime(time_min, tz = "UTC", "%Y-%m-%dT%H:%00%00Z")
  time_max <- strftime(time_max, tz = "UTC", "%Y-%m-%dT%H:%00%00Z")

  url <- sprintf(
    'https://www.googleapis.com/calendar/v3/calendars/%s/events?maxResults=%d&timeMin=%s&timeMax=%s&orderBy=startTime&singleEvents=true',
    id, max_results, time_min, time_max
  )
  r <- GET(url, config(token = google_token))

  events <- r %>% content() %>% pluck("items")

  # If there's more than one page of results
  # while (!is.null(content(req)$nextPageToken)) {
  #   req <- GET(paste0('https://www.googleapis.com/calendar/v3/calendars/',
  #                     cals[i], '/events?maxResults=2500&orderBy=startTime&',
  #                     'singleEvents=true&pageToken=',
  #                     content(req)$nextPageToken),
  #              config(token = google_token))
  #   events_list <- lapply(content(req)$items,
  #                         function (x) {
  #                           x[['calendar']] <- cals[i]
  #                           x})
  #   events_list_all <- append(events_list_all, events_list)
  # }

  events %>%
    map_df(~suppressWarnings(squash(.))) %>%
    bind_rows()
}



utils::globalVariables(c("date", "dateTime", "summary", "description", "attendees"))
utils::globalVariables(c("attendee_count", "internal_count"))

extract_meetings <- function(events){
  events %>%
    map(~squash_if(is.data.frame)) %>%
    bind_cols() %>%
    select(date, dateTime, summary, description, attendees) %>%
    mutate(
      date = if_else(!is.na(date), lubridate::date(date), lubridate::date(dateTime)),
      attendee_count = map_int(events$attendees, ~length(.$email)),
      internal_count = map_int(events$attendees, ~sum(grepl("@rstudio.com", .$email))),
      external = attendee_count > internal_count,
      internal = attendee_count == internal_count
    ) %>%
    select(
      -one_of("dateTime")
    )

}


