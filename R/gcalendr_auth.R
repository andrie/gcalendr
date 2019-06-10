# This file is the interface between gcalendr and the auth functionality in
# gargle.

gcalendr_app <- function() {
  oauth_app(
    appname = 'gcalendr-package',
    key = paste0("939459484985-fn7q750sdkpmi9a76jn8a1rtug84q5ss",
                 ".apps.googleusercontent.com"),
    secret = "KDwAZXP4mlZUodY4vcKKfCE4"
  )
}

.auth <- gargle::init_AuthState(
  package     = "gcalendr",
  app         = gcalendr_app(),     # YOUR PKG SHOULD USE ITS OWN APP!
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
  PREFIX      = "gcalendr",
  AUTH_CONFIG_SOURCE = "gcalendr-package"
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
#' gcalendr_auth()
#'
#' ## see user associated with current token
#' gcalendr_user()
#'
#' ## force use of a token associated with a specific email
#' gcalendr_auth(email = "jenny@example.com")
#' gcalendr_user()
#'
#' ## force a menu where you can choose from existing tokens or
#' ## choose to get a new one
#' gcalendr_auth(email = NA)
#'
#' ## use a 'read only' scope, so it's impossible to edit or delete files
#' gcalendr_auth(
#'   scopes = "https://www.googleapis.com/auth/calendar.readonly"
#' )
#'
#' ## use a service account token
#' gcalendr_auth(path = "foofy-83ee9e7c9c48.json")
#' }
gcalendr_auth <- function(
  email = NULL,
  path = NULL,
  scopes = "https://www.googleapis.com/auth/calendar.readonly",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL)
{
  cred <- gargle::token_fetch(
    scopes = scopes,
    app = gcalendr_oauth_app(),
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
      "  * `gcalendr_deauth()` to prevent the attempt to get credentials.\n",
      "  * Call `gcalendr_auth()` directly with all necessary specifics.\n",
      call. = FALSE
    )
  }
  .auth$set_cred(cred)
  .auth$set_auth_active(TRUE)

  invisible()
}

#' Suspend authorization
#'
#' @eval gargle:::PREFIX_deauth_description(gargle_lookup_table)
#'
#' @family auth functions
#' @export
#' @examples
#' \dontrun{
#' gcalendr_deauth()
#' gcalendr_user()
#' public_file <-
#'   gcalendr_get(as_id("1Hj-k7NpPSyeOR3R7j4KuWnru6kZaqqOAE8_db5gowIM"))
#' gcalendr_download(public_file)
#' }
gcalendr_deauth <- function() {
  .auth$set_auth_active(FALSE)
  .auth$set_cred(NULL)
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
#'   token = gcalendr_token()
#' )
#' req
#' }
gcalendr_token <- function() {
  if (isFALSE(.auth$auth_active)) {
    return(NULL)
  }
  if (!gcalendr_has_token()) {
    gcalendr_auth()
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
#' gcalendr_has_token()
gcalendr_has_token <- function() {
  inherits(.auth$cred, "Token2.0")
}

#' View or edit auth config
#'
#' @eval gargle:::PREFIX_auth_config_description(gargle_lookup_table)
#' @eval gargle:::PREFIX_auth_config_params_except_key(gargle_lookup_table)
#'
#' @family auth functions
#' @export
#' @examples
#' ## this will print current config
#' gcalendr_auth_config()
#'
#' \dontrun{
#' if (require(httr)) {
#'   ## bring your own app via client id (aka key) and secret
#'   google_app <- httr::oauth_app(
#'     "my-awesome-google-api-wrapping-package",
#'     key = "123456789.apps.googleusercontent.com",
#'     secret = "abcdefghijklmnopqrstuvwxyz"
#'   )
#'   gcalendr_auth_config(app = google_app)
#' }
#'
#' ## bring your own app via JSON downloaded from Google Developers Console
#' gcalendr_auth_config(
#'   path = "/path/to/the/JSON/you/downloaded/from/google/dev/console.json"
#' )
#' }
#'
gcalendr_auth_config <- function(
  app = NULL,
  path = NULL
)
{
  stopifnot(is.null(app) || inherits(app, "oauth_app"))
  stopifnot(is.null(path) || is_string(path))

  if (!is.null(app) && !is.null(path)) {
    stop_glue("Don't provide both 'app' and 'path'. Pick one.")
  }

  if (is.null(app) && !is.null(path)) {
    app <- gargle::oauth_app_from_json(path)
  }
  if (!is.null(app)) {
    .auth$set_app(app)
  }

  .auth
}


#' @export
#' @rdname gcalendr_auth_config
gcalendr_oauth_app <- function() .auth$app
