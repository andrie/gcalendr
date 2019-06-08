# GET <- memoise(httr::GET)


#' Get list of calendars from google.
#'
#' @param token token obtained from [get_google_token]
#'
#' @return character vector with calendar ids
#' @family gcal functions
#' @export
get_gcal_list <- function(token){

  req <- generate_request("calendar.calendarList.list", token = token)
  r <- request_make(req)
  httr::stop_for_status(r)
  r <- content(r)

  null_to_na <- function(x){
    ifelse(is.null(x), NA, x)
  }

  r %>%
    pluck("items") %>%
    map_dfr(
      ~list(
        id = .[["id"]],
        summary = .[["summary"]],
        description = .[["description"]]  %>% null_to_na()
        )
      )

}


# declare variables for R CMD check
utils::globalVariables(c("attendee_count", "internal_count"))


embedded_lists <- function(x){
  el <- x %>% map_lgl(is.list)
  el[el] %>% names()
}

rename_embedded_lists <- function(x){
  el <- embedded_lists(x)
  x[el] <- lapply(seq_along(el), function(i){
    el_i_name <- names(x[el][i])
    this <- x[el][[i]]
    names(this) <- paste(el_i_name, names(this), sep = "_")
    this
  })
  x
}

squash_without_warning <- function(x)suppressWarnings(squash(x))


utils::globalVariables(c(".", "created", "updated", "start_date", "end_date", "end_dateTime", "start_dateTime"))


call_gcal_api <- function(id, google_token, time_min, time_max, max_results = 250){
  api = "https://www.googleapis.com/calendar/v3/calendars"
  time_min <- strftime(time_min, tz = "UTC", "%Y-%m-%dT%H:%M:00Z")
  time_max <- strftime(time_max, tz = "UTC", "%Y-%m-%dT%H:%M:00Z")
  url <- sprintf(
    "%s/%s/events?maxResults=%d&timeMin=%s&timeMax=%s&orderBy=startTime&singleEvents=true",
    api, id, max_results, time_min, time_max
  )
  r <- GET(url, config(token = google_token))
  httr::stop_for_status(r)
  r <- content(r)

  items <- r$items


  # Retrieve more results if necessary

  retrieved_more <- FALSE
  while (!is.null(r[["nextPageToken"]])) {
    retrieved_more <- TRUE
    message(".", appendLF = FALSE)
    new_url <- sprintf("%s&pageToken=%s", url, r[["nextPageToken"]])
    r <- GET(new_url, config(token = google_token))
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
get_gcal_events <- function(id, google_token, days_in_past = 90, days_in_future = 90, now = Sys.time(), max_results = 250){
  message("Reading calendar ", id)
  time_min <- now - days_in_past * 24 * 3600
  time_max <- now + days_in_future * 24 * 3600

  items <- call_gcal_api(
    id = id,
    google_token = google_token,
    time_min = time_min,
    time_max = time_max,
    max_results = max_results
  )

  drop_attendees <- function(x){
    x %>%
      discard(., ~vec_depth(.) == 3) %>%
      rename_embedded_lists() %>% squash_without_warning()
  }
  keep_attendees <- function(x){
    att <- x %>% pluck("attendees") %>% map_dfr(., flatten)
    bind_cols(id = rep(x$id, nrow(att)), att)
  }

  events <- left_join(
    items %>% map_dfr(drop_attendees) %>% bind_rows(),
    items %>% map_dfr(keep_attendees) %>%
      group_by(id) %>%
      nest() %>%
      dplyr::rename(attendees = "data"),
    by = "id"
  )

  events %>%
    mutate(
      created        = transform_or_na(., "created", as_datetime),
      updated        = transform_or_na(., "updated", as_datetime),
      start_date     = transform_or_na(., "start_date"),
      end_date       = transform_or_na(., "end_date"),
      start_dateTime = transform_or_na(., "start_dateTime", as_datetime),
      end_dateTime   = transform_or_na(., "end_dateTime", as_datetime)
    )
}

transform_or_na <- function(df, varname, fn = as_date) {
  if (varname %in% names(df)) df %>% dplyr::pull(!!varname) %>% fn() else fn(NA)
}


# declare variables for R CMD check
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


