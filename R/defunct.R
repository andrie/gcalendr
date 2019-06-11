#' Functions that are no longer used.
#'
#' Please use [calendar_auth()] and [calendar_token()] instead.
#'
#' @param google_app No longer used
#' @rdname gcalendr-defunct
#' @export
get_google_token <- function(google_app){
  .Defunct("calendar_token", package = "gcalendr")
}

#' Please use [calendar_list] instead.
#'
#' @inheritParams calendar_list
#' @rdname gcalendr-defunct
#' @export
get_gcal_list <- function(token = calendar_token()){
  .Defunct("calendar_list", package = "gcalendr")
}

#' Please use [calendar_events] instead.
#'
#' @inheritParams calendar_events
#' @rdname gcalendr-defunct
#' @export
get_gcal_events <- function(id, token = calendar_token(),
                            days_in_past = 90, days_in_future = 90,
                            now = Sys.Date(), max_results = 250)
  {
  .Defunct("calendar_events", package = "gcalendr")
}
