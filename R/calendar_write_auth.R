#' Authorize Google Calendar with write permissions
#'
#' This is a convenience function as an alternative to \code{calendar_auth(scopes = "https://www.googleapis.com/auth/calendar.events")}
#'
#' @inheritParams calendar_auth
#' @export
calendar_write_auth <- function(
  email = gargle::gargle_oauth_email(),
  path = NULL,
  scopes = "https://www.googleapis.com/auth/calendar.events",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL)
{
  cred <- gargle::token_fetch(
    scopes = scopes,
    app = calendar_oauth_app() %||% calendar_app(),
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

