#' Functions that are no longer used.
#'
#' These functions were available in a very early version of the package.
#' Please use these replacement functions instead:
#'  * [calendar_auth()] and [calendar_token()]
#'  * [calendar_list()]
#'  * [calendar_events()]
#'
#' @param google_app No longer used
#' @rdname gcalendr-defunct
#' @export
get_google_token <- function(google_app){
  .Defunct("calendar_token", package = "gcalendr")
}

#' @inheritParams calendar_list
#' @rdname gcalendr-defunct
#' @export
get_gcal_list <- function(token = calendar_token()){
  .Defunct("calendar_list", package = "gcalendr")
}

#' @inheritParams calendar_events
#' @rdname gcalendr-defunct
#' @export
get_gcal_events <- function(id, token = calendar_token(),
                            days_in_past = 90, days_in_future = 90,
                            now = Sys.Date(), max_results = 250)
  {
  .Defunct("calendar_events", package = "gcalendr")
}
