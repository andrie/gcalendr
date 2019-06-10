call_gcal_api <- function(id, token = gcalendr_token(), time_min, time_max, max_results = 250){
  # api = "https://www.googleapis.com/calendar/v3/calendars"
  # url <- sprintf(
  #   "%s/%s/events?maxResults=%d&timeMin=%s&timeMax=%s&orderBy=startTime&singleEvents=true",
  #   api, id, max_results, time_min, time_max
  # )
  # r <- GET(url, config(token = google_token))

  time_min <- strftime(time_min, tz = "UTC", "%Y-%m-%dT%H:%M:00Z")
  time_max <- strftime(time_max, tz = "UTC", "%Y-%m-%dT%H:%M:00Z")

  req <- request_generate(
    "calendar.events.list",
    token = token,
    params = list(
      timeMin = time_min,
      timeMax = time_max,
      calendarId = id
    )
  )
  r <- request_make(req)
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

    # new_url <- sprintf("%s&pageToken=%s", url, r[["nextPageToken"]])
    # r <- GET(new_url, config(token = google_token))
    r <- request_make(req)
    httr::stop_for_status(r)
    r <- content(r)
    new_items <- r$items
    items <- append(items, new_items)
  }
  items
}


#' Read list of google calendar events.
#'
#' @inheritParams get_gcal_list
#'
#' @param id calendar id, obtained from [get_gcal_list()]
#' @param days_in_past Restrict results to date range, number of days in past
#' @param days_in_future Restrict results to date range, number of days into future
#' @param now Reference time stamp, defaulting to [Sys.time()]
#' @param max_results Maximum number of results retrieved per query
#'
#' @references https://developers.google.com/google-apps/calendar/v3/reference/events/list
#' @family gcal functions
#' @export
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr rename
get_gcal_events <- function(id, token = gcalendr_token(),
                            days_in_past = 90, days_in_future = 90,
                            now = Sys.time(), max_results = 250){
  message("Reading calendar ", id)
  time_min <- now - days_in_past * 24 * 3600
  time_max <- now + days_in_future * 24 * 3600

  items <- call_gcal_api(
    id = id,
    token = token,
    time_min = time_min,
    time_max = time_max,
    max_results = max_results
  )

  convert_items_to_events(items)
  }
