# This file is the interface between gcalendr and the auth functionality in
# gargle.

calendar_app <- function() {
  oauth_app(
    appname = 'gcalendr-package',
    key = paste0("939459484985-fn7q750sdkpmi9a76jn8a1rtug84q5ss",
                 ".apps.googleusercontent.com"),
    secret = "KDwAZXP4mlZUodY4vcKKfCE4"
  )
}

.auth <- gargle::init_AuthState(
  package     = "gcalendr",
  app         = calendar_app(),     # YOUR PKG SHOULD USE ITS OWN APP!
  api_key     = NULL, # YOUR PKG SHOULD USE ITS OWN KEY!
  auth_active = TRUE
)

# The roxygen comments for these functions are mostly generated from data
# in this list and template text maintained in gargle.
gargle_lookup_table <- list(
  PACKAGE     = "gcalendr",
  YOUR_STUFF  = "your calendar events",
  PRODUCT     = "Google Calendar",
  API         = "Calendar API",
  PREFIX      = "calendar"
)

#' Authorize gcalendr
#'
#' @eval gargle:::PREFIX_auth_description(gargle_lookup_table)
#' @eval gargle:::PREFIX_auth_details(gargle_lookup_table)
#' @eval gargle:::PREFIX_auth_params()
#'
#' @family auth functions
#' @export
#'
#' @examples
#' \dontrun{
#' ## load/refresh existing credentials, if available
#' ## otherwise, go to browser for authentication and authorization
#' calendar_auth()
#'
#' ## see user associated with current token
#' calendar_user()
#'
#' ## force use of a token associated with a specific email
#' calendar_auth(email = "jenny@example.com")
#' calendar_user()
#'
#' ## force a menu where you can choose from existing tokens or
#' ## choose to get a new one
#' calendar_auth(email = NA)
#'
#' ## use a 'read only' scope, so it's impossible to edit or delete files
#' calendar_auth(
#'   scopes = "https://www.googleapis.com/auth/calendar.readonly"
#' )
#'
#' ## use a service account token
#' calendar_auth(path = "foofy-83ee9e7c9c48.json")
#' }
calendar_auth <- function(
  email = gargle::gargle_oauth_email(),
  path = NULL,
  scopes = "https://www.googleapis.com/auth/calendar.readonly",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL)
{
  cred <- gargle::token_fetch(
    scopes = scopes,
    app = calendar_oauth_app(),
    email = email,
    path = path,
    package = "gcalendr",
    cache = cache,
    use_oob = use_oob,
    token = token
  )
  if (!inherits(cred, "Token2.0")) {
    stop(
      "Can't get Google credentials.\n",
      "Are you running gcalendr in a non-interactive session? Consider:\n",
      "  * `calendar_deauth()` to prevent the attempt to get credentials.\n",
      "  * Call `calendar_auth()` directly with all necessary specifics.\n",
      call. = FALSE
    )
  }
  .auth$set_cred(cred)
  .auth$set_auth_active(TRUE)

  invisible()
}

#' Clear current token
#'
#' Clears any currently stored token. The next time gcalendr needs a token, the
#' token acquisition process starts over, with a fresh call to [calendar_auth()]
#' and, therefore, internally, a call to [gargle::token_fetch()]. Unlike some
#' other packages that use gargle, gcalendr is not usable in a de-authorized
#' state. Therefore, calling `calendar_deauth()` only clears the token, i.e. it
#' does NOT imply that subsequent requests are made with an API key in lieu of a
#' token.
#'
#' @family auth functions
#' @export
#' @examples
#' \dontrun{
#' calendar_deauth()
#' }
calendar_deauth <- function() {
  .auth$clear_cred()
  invisible()
}

#' Produce configured token
#'
#' @eval gargle:::PREFIX_token_description(gargle_lookup_table)
#' @eval gargle:::PREFIX_token_return()
#'
#' @family low-level API functions
#' @export
#' @examples
#' \dontrun{
#' req <- request_generate(
#'   "drive.files.get",
#'   list(fileId = "abc"),
#'   token = calendar_token()
#' )
#' req
#' }
calendar_token <- function() {
  if (isFALSE(.auth$auth_active)) {
    return(NULL)
  }
  if (!calendar_has_token()) {
    calendar_auth()
  }
  httr::config(token = .auth$cred)
}

#' Is there a token on hand?
#'
#' Reports whether gcalendr has stored a token, ready for use in downstream
#' requests. Exists mostly for protecting examples that won't work in the
#' absence of a token.
#'
#' @return Logical.
#' @export
#'
#' @examples
#' calendar_has_token()
calendar_has_token <- function() {
  inherits(.auth$cred, "Token2.0")
}

#' Edit and view auth configuration
#'
#' @eval gargle:::PREFIX_auth_configure_description(gargle_lookup_table, .has_api_key = FALSE)
#' @eval gargle:::PREFIX_auth_configure_params(.has_api_key = FALSE)
#' @eval gargle:::PREFIX_auth_configure_return(gargle_lookup_table, .has_api_key = FALSE)
#'
#' @family auth functions
#' @export
#' @examples
#' # see the current user-configured OAuth app (probaby `NULL`)
#' calendar_oauth_app()
#'
#' if (require(httr)) {
#'
#'   # store current state, so we can restore
#'   original_app <- calendar_oauth_app()
#'
#'   # bring your own app via client id (aka key) and secret
#'   google_app <- httr::oauth_app(
#'     "my-awesome-google-api-wrapping-package",
#'     key = "123456789.apps.googleusercontent.com",
#'     secret = "abcdefghijklmnopqrstuvwxyz"
#'   )
#'   calendar_auth_configure(app = google_app)
#'
#'   # confirm current app
#'   calendar_oauth_app()
#'
#'   # restore original state
#'   calendar_auth_configure(app = original_app)
#'   calendar_oauth_app()
#' }
#'
#' \dontrun{
#' # bring your own app via JSON downloaded from GCP Console
#' bq_auth_configure(
#'   path = "/path/to/the/JSON/you/downloaded/from/gcp/console.json"
#' )
#' }
#'
calendar_auth_configure <- function(app, path) {
  if (!xor(missing(app), missing(path))) {
    stop("Must supply exactly one of `app` and `path`", call. = FALSE)
  }
  if (!missing(path)) {
    stopifnot(is_string(path))
    app <- gargle::oauth_app_from_json(path)
  }
  stopifnot(is.null(app) || inherits(app, "oauth_app"))

  .auth$set_app(app)
}

#' @export
#' @rdname calendar_auth_configure
calendar_oauth_app <- function() .auth$app

#' Get info on current user
#'
#' Reveals the email address of the user associated with the current token. If
#' no token has been loaded yet, this function does not initiate auth.
#'
#' @seealso [gargle::token_userinfo()], [gargle::token_email()],
#'   [gargle::token_tokeninfo()]
#'
#' @return An email address or, if no token has been loaded, `NULL`.
#' @export
#' @examples
#' \dontrun{
#' calendar_user()
#' }
calendar_user <- function() {
  if (calendar_has_token()) {
    gargle::token_email(calendar_token())
  } else {
    NULL
  }
}
