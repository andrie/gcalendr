
#' Get google token for authentication.
#'
#' @param google_app A valid oauth app, e.g. as provided by [httr::oauth_app].  Defaults to an app that will ask for permission to view your google calendars.  You can use the built-in app, or create your own.
#' @family gcal functions
#' @export
get_google_token <- function(google_app){
  if (missing(google_app) || is.null(google_app)) {
    google_app <- gcalendr_app
  }
  google_token <- oauth2.0_token(
    oauth_endpoints('google'), google_app,
    scope = 'https://www.googleapis.com/auth/calendar.readonly'
  )
}


options(gargle_quiet = FALSE)
get_google_token <- function(email = NULL, google_app = NULL){
  if (missing(google_app) || is.null(google_app)) {
    google_app <- gcalendr_app
  }
  gargle::token_fetch(
    scopes = 'https://www.googleapis.com/auth/calendar.readonly',
    app = google_app,
    email = email
  )
}
