#' Create a calendar event
#'
#' This function allows you to create a calendar event.
#'
#'
#' @param id calendar id, obtained from [calendar_list()]
#' @param start_time the event start time formatted "yyyy-mm-dd hh:mm:ss". Hours, minutes, seconds, optional.
#' @param end_time the event start time formatted "yyyy-mm-dd hh:mm:ss". Hours, minutes, seconds, optional.
#' @param attendees character vector of email addresses of event attendees. Optional.
#' @param description description of the event. This should be a character vector. Optional.
#' @importFrom lubridate ceiling_date minutes now
#' @export

create_event <- function(id, attendees, description, start_time = ceiling_date(now(), "30minutes"), end_time = start_time + minutes(30)) {

  time_start <- strftime(start_time, tz = "UTC", "%Y-%m-%dT%H:%M:00Z")
  time_end <- strftime(end_time, tz = "UTC", "%Y-%m-%dT%H:%M:00Z")

  req <- request_generate("calendar.events.insert",
                          params = list(calendarId = id,
                                        attendees = list(list("email" = list(attendees)),
                                                         "responseStatus" = "needsAction"),
                                        description = description,
                                        start = list(dateTime = time_start),
                                        end =  list(dateTime = time_end)))

  r <- request_make(req, encode = "json")
  r
}


# time_start <- strftime(now() + minutes(5), tz = "UTC", "%Y-%m-%dT%H:%M:00Z")
# time_end <- strftime(now() + minutes(20), tz = "UTC", "%Y-%m-%dT%H:%M:00Z")
#
# attendees <- c("josiah@rstudio.com")
# description <- "this is a test"
# id <- "josiah.parry@gmail.com"
# req <- request_generate("calendar.events.insert",
#                         params = list(calendarId = id,
#                                       attendees = list(list("email" = list(attendees)),
#                                                        "responseStatus" = "needsAction"),
#                                       description = description,
#                                       start = list(dateTime = time_start),
#                                       end =  list(dateTime = time_end)))
#
# r <- request_make(req, encode = "json")
# r
