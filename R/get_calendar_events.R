call_calendar_events <- function(id, token = calendar_token(), time_min, time_max, max_results = 250){
  # api = "https://developers.google.com/calendar/v3/reference/calendars"

  time_min <- strftime(time_min, tz = "UTC", "%Y-%m-%dT%H:%M:00Z")
  time_max <- strftime(time_max, tz = "UTC", "%Y-%m-%dT%H:%M:00Z")

  req <- request_generate(
    "calendar.events.list",
    token = token,
    params = list(
      timeMin = time_min,
      timeMax = time_max,
      calendarId = id,
      maxResults = max_results,
      singleEvents = TRUE
    )
  )
  r <- gargle::request_make(req)
  httr::stop_for_status(r)
  r <- content(r)

  items <- r$items


  # Retrieve more results if necessary

  retrieved_more <- FALSE
  while (!is.null(r[["nextPageToken"]])) {
    retrieved_more <- TRUE
    message(".", appendLF = FALSE)
    req <- request_generate(
      "calendar.events.list",
      token = token,
      params = list(
        timeMin = time_min,
        timeMax = time_max,
        calendarId = id,
        pageToken = r[["nextPageToken"]]
      )
    )

    r <- request_make(req)
    httr::stop_for_status(r)
    r <- content(r)
    new_items <- r$items
    items <- append(items, new_items)
  }
  items
}


#' Retrieve google calendar events.
#'
#' This returns a tibble of events for a specific calendar.
#'
#' The resulting tibble has nested list columns for:
#'
#' * `attendees`
#' * `creator`
#' * `organizer`
#'
#' To unnest these columns, use [tidyr::unnest()]
#'
#' @inheritParams calendar_list
#'
#' @param id calendar id, obtained from [calendar_list()]
#' @param days_in_past Restrict results to date range, number of days in past
#' @param days_in_future Restrict results to date range, number of days into
#'   future
#' @param now Reference time stamp, defaulting to [Sys.time()]
#' @param max_results Used internally to specify the maximum number of results
#'   retrieved per "page" of the query.
#'
#' @references
#'   https://developers.google.com/google-apps/calendar/v3/reference/events/list
#' @family calendar functions
#' @export
calendar_events <- function(id, token = calendar_token(),
                            days_in_past = 90, days_in_future = 90,
                            now = Sys.Date(), max_results = 250){
  message("Reading calendar ", id)
  time_min <- now - days_in_past
  time_max <- now + days_in_future

  items <- call_calendar_events(
    id = id,
    token = token,
    time_min = time_min,
    time_max = time_max,
    max_results = max_results
  )

  convert_items_to_events(items)
  }
