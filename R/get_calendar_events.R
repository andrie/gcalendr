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

  # drop_attendees <- function(x){
  #   x %>%
  #     discard(., ~vec_depth(.) >= 3) %>%
  #     rename_embedded_lists() %>%
  #     squash_without_warning()
  # }

  # events <- items %>%
  #   map_dfr(drop_attendees) %>%
  #   bind_rows()

  single_column <- function(x, varname) {
    z <- x[[varname]]
    if(is.null(z)) z <- NA
    z
  }


  events <- items %>%
    map_dfr(~tibble(
      kind = single_column(., "kind"),
      etag = single_column(., "etag"),
      id = single_column(., "id"),
      status = single_column(., "status"),
      htmlLink = single_column(., "htmlLink"),
      created = single_column(., "created"),
      updated = single_column(., "updated"),
      summary = single_column(., "summary"),
      location = single_column(., "location"),
      # creator = single_column(., "creator"),
      # organizer = single_column(., "organizer"),
      start = single_column(., "start"),
      end = single_column(., "end"),
      recurrence = single_column(., "recurrence"),
      iCalUID = single_column(., "iCalUID"),
      sequence = single_column(., "sequence"),
      # attendees = single_column(., "attendees"),
      extendedProperties = single_column(., "extendedProperties"),
      reminders = single_column(., "reminders"),
      description = single_column(., "description"),
      recurringEventId = single_column(., "recurringEventId"),
      originalStartTime = single_column(., "originalStartTime"),
      transparency = single_column(., "transparency"),
      guestsCanModify = single_column(., "guestsCanModify"),
      visibility = single_column(., "visibility"),
      guestsCanSeeOtherGuests = single_column(., "guestsCanSeeOtherGuests"),
      guestsCanInviteOthers = single_column(., "guestsCanInviteOthers"),
      privateCopy = single_column(., "privateCopy"),
      source = single_column(., "source"),
      attachments = single_column(., "attachment")
    ))

  extract_nested_column <- function(x, varname){
    y <- x[[varname]] %>% map_dfr(., flatten)
    bind_cols(id = rep(x$id, nrow(y)), y)
  }



  deep_column <- function(x, varname) {
    z <- x %>%
      map_dfr(extract_nested_column, varname) %>%
      dplyr::as_tibble() %>%
      group_by(id) %>%
      nest() %>%
      dplyr::as_tibble()

    names(z) <- c("id", varname)
    z
  }

  attendees <- items %>% deep_column("attendees")

  creator <-
    items %>%
    map_dfr(~tibble(
      creator = .[["creator"]] %>% .[["email"]] %||% NA,
      creator_self = .[["creator"]] %>% .[["self"]] %||% NA,
      id = .[["id"]]
    )) %>%
    group_by(id) %>%
    nest() %>%
    as_tibble() %>%
    rename(creator = "data")



  organizer <-
    items %>%
    map_dfr(~tibble(
      organizer = .[["organizer"]] %>% .[["email"]] %||% NA,
      organizer_self = .[["organizer"]] %>% .[["self"]] %||% NA,
      id = .[["id"]]
    )) %>%
    group_by(id) %>%
    nest() %>%
    as_tibble() %>%
    rename(organizer = "data")

  events %>%
    # select(-one_of(c("attendees", "creator", "organizer"))) %>%
    left_join(attendees, by = "id") %>%
    left_join(creator, by = "id") %>%
    left_join(organizer, by = "id") %>%
    mutate(
      created        = transform_or_na(., "created", as_datetime),
      updated        = transform_or_na(., "updated", as_datetime),
      start_date     = transform_or_na(., "start_date", as.Date),
      end_date       = transform_or_na(., "end_date", as.Date),
      start_dateTime = transform_or_na(., "start_dateTime", as_datetime),
      end_dateTime   = transform_or_na(., "end_dateTime", as_datetime)
    )
}
