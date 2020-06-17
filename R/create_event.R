#' Create a Google calendar event
#'
#' This function allows you to create a calendar event.
#'
#'
#' @param id calendar id, obtained from [calendar_list()]
#' @param start_time the event start time formatted "yyyy-mm-dd hh:mm:ss". Hours, minutes, seconds, optional.
#' @param end_time the event start time formatted "yyyy-mm-dd hh:mm:ss". Hours, minutes, seconds, optional.
#' @param attendees character vector of email addresses of event attendees. Optional.
#' @param description description of the event. This should be a character vector. Optional.
#' @param timezone See \code{?timezones}.
#' @export

create_event <- function(id, attendees, description, start_time, end_time, timezone = Sys.timezone()) {

  time_start <- strftime(start_time, tz = Sys.timezone(), "%Y-%m-%dT%H:%M:00Z")
  time_end <- strftime(end_time, tz = Sys.timezone(), "%Y-%m-%dT%H:%M:00Z")

  req <- request_generate("calendar.events.insert",
                          params = list(calendarId = id,
                                        attendees = list(list("email" = list(attendees)),
                                                         "responseStatus" = "needsAction"),
                                        description = description,
                                        start = list(dateTime = time_start),
                                        end =  list(dateTime = time_end)))

  request_make(req, encode = "json")

}

