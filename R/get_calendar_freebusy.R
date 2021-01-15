#' return availability for a list of calendars
#'
#' @param token token obtained from [calendar_token()]
#' @param ids a character vector of calendars or groups to query
#' @param time_min min date
#' @param time_max max date
#' @return a ti
calendar_freebusy <- function(
  ids,
  time_min = NULL,
  time_max = NULL,
  token = calendar_token()
  ) {

  time_min <- fmt_time(time_min)
  time_max <- fmt_time(time_max)

  req <- request_generate(
    endpoint = "calendar.freebusy.query",
    token = token,
    params = list(
      timeMin = time_min,
      timeMax = time_max,
      items = enlist(ids)
    )
    )
  r <- request_make(req)
  httr::stop_for_status(r)
  r <- content(r)

  calendars <- tibble(
    id  = names(r$calendars),
    cal = r$calendars
    )

  extract_freebusy <- function(x) x[[1]]$busy

  # auto-unpack cols:
  # https://github.com/tidyverse/dplyr/issues/2326#issuecomment-546302847
  calendars %>%
    group_by(id) %>%
    summarise(
      as_tibble(
        as.list(
          bind_rows(extract_freebusy(cal))
          )
        ),
      .groups = "drop"
      )
  }

