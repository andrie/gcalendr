# GET <- memoise(httr::GET)


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

  r <- content(r)

  # `%||%` <- function(lhs, rhs){
  #   if_else (!is.null(lhs), lhs, rhs )
  # }

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
get_gcal_events <- function(id, google_token, max_results = 250, days_in_past = 90, days_in_future = 90){
  message("Reading calendar ", id)
  time_min <- Sys.time() - days_in_past * 24 * 3600
  time_max <- Sys.time() + days_in_future * 24 * 3600

  time_min <- strftime(time_min, tz = "UTC", "%Y-%m-%dT%H:%M:00Z")
  time_max <- strftime(time_max, tz = "UTC", "%Y-%m-%dT%H:%M:00Z")

  api = "https://www.googleapis.com/calendar/v3/calendars"
  url <- sprintf(
    "%s/%s/events?maxResults=%d&timeMin=%s&timeMax=%s&orderBy=startTime&singleEvents=true",
    api, id, max_results, time_min, time_max
  )
  r <- GET(url, config(token = google_token))
  r <- content(r)

  items <- r$items


  # Retrieve more results if necessary

  retrieved_more <- FALSE
  while (!is.null(r[["nextPageToken"]])) {
    retrieved_more <- TRUE
    message(".", appendLF = FALSE)
    new_url <- sprintf("%s&pageToken=%s", url, r[["nextPageToken"]])
    r <- GET(new_url, config(token = google_token))
    r <- content(r)
    new_items <- r$items
    items <- append(items, new_items)
  }
  if (retrieved_more) message()


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
      nest(.key = "attendees"),
    by = "id"
  )

  events %>%
    mutate(
      created = transform_or_na(., "created", as_datetime),
      updated = transform_or_na(., "updated", as_datetime),
      start_date = transform_or_na(., "start_date"),
      end_date = transform_or_na(., "end_date"),
      start_dateTime = as_datetime(start_dateTime),
      end_dateTime = as_datetime(end_dateTime)
    )
}

transform_or_na <- function(df, varname, fn = as_date) {
  if (varname %in% names(df)) df %>% pull(!!varname) %>% fn() else fn(NA)
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


