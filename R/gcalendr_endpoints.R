
#' List Calendar endpoints
#'
#' Returns a list of selected calendar API v3 endpoints, as stored inside the
#' googlecalendar package. The names of this list (or the `id` sub-elements) are
#' the nicknames that can be used to specify an endpoint in
#' [request_generate()]. For each endpoint, we store its nickname or `id`, the
#' associated HTTP verb, the `path`, and details about the parameters. This list
#' is derived programmatically from the
#' [calendar API v3 Discovery Document](https://www.googleapis.com/discovery/v1/apis/calendar/v3/rest).
#'
#' @param i The name(s) or integer index(ices) of the endpoints to return.
#'   Optional. By default, the entire list is returned.
#'
#' @return A list containing some or all of the subset of the calendar API v3
#'   endpoints that are used internally by `gcalendr`.
#' @export
#'
#' @examples
#' str(calendar_endpoints(), max.level = 2)
#' calendar_endpoints("calendar.events.list")
#' calendar_endpoints(4)
calendar_endpoints <- function(i = NULL) {
  is_expose <- function(x) inherits(x, "expose")
  if (is.null(i) || is_expose(i)) {
    i <- seq_along(.endpoints)
  }
  .endpoints[i]
}
