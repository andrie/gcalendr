#' Create a Google calendar event
#'
#' This function allows you to create a calendar event.
#'
#'
#' @param id calendar id, obtained from [calendar_list()]
#' @param title Title of the event. Optional.
#' @param description description of the event. This should be a character vector. Optional.
#' @param attendees character vector of email addresses of event attendees. Optional.
#' @param start_time the event start time formatted "yyyy-mm-dd hh:mm:ss". Hours, minutes, seconds, optional.
#' @param duration duration of calendar event in minutes. Optional.
#' @param timezone See \code{?timezones}.
#' @export

create_event <- function(id, title, description, attendees, start_time, duration, timezone = Sys.timezone()) {


  time_start <- strftime(start_time, tz = timezone, "%Y-%m-%dT%H:%M:00Z")

  time_end<- strftime(as.POSIXct(start_time, tz = timezone) + duration * 60, tz = timezone, "%Y-%m-%dT%H:%M:00Z")



  req <- request_generate("calendar.events.insert",
                          params = list(calendarId = id,
                                        attendees = list(list("email" = list(attendees)),
                                                         "responseStatus" = "needsAction"),
                                        description = description,
                                        summary = title,
                                        start = list(dateTime = time_start),
                                        end =  list(dateTime = time_end)))

  request_make(req, encode = "json")

}

