#' Defunct.
#'
#' Please use [gcalendr_auth()] and [gcalendr_token()] instead.
#'
#' @param google_app No longer used
#' @rdname gcalendr-defunct
#' @export
get_google_token <- function(google_app){
  .Defunct("gcalendr_token", package = "gcalendr")
}
