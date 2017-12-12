rstudio_app <- oauth_app(
  'GOOGLE_APIS',
  key = "115354746897-98g2v3hvasb4q1p0p4adp37ophpqqo7l.apps.googleusercontent.com",
  secret = "0qh9MxOXWzaKIX016Jv_mVqQ"
)

#' Get google token for authentication.
#'
#' @param google_app A valid oauth app, e.g. as provided by [httr::oauth_app].  Defaults to an app that will ask for permission to view your google calendars.  You can use the built-in app, or create your own.
#' @family gcal functions
#' @export
get_google_token <- function(google_app){
  if (missing(google_app) || is.null(google_app)) {
    google_app <- rstudio_app
  }
  google_token <- oauth2.0_token(
    oauth_endpoints('google'), google_app,
    scope = 'https://www.googleapis.com/auth/calendar.readonly'
  )
}
