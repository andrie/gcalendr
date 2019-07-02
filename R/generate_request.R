stop_glue <- function (..., .sep = "", .envir = parent.frame(), call. = FALSE,
          .domain = NULL)
{
  stop(glue::glue(..., .sep = .sep, .envir = .envir), call. = call., domain = .domain)
}

#' Build a request for the Google Calendar API
#'
#' @description Build a request, using knowledge of the [Calendar v3
#'   API](https://developers.google.com/calendar/v3/reference/) from its
#'   [Discovery
#'   Document](https://www.googleapis.com/discovery/v1/apis/calendar/v3/rest).
#'   Most users should, instead, use higher-level wrappers that facilitate
#'   common tasks, such as reading calendar lists or calendar entries. The
#'   functions here are intended for internal use and for programming around the
#'   Calendar API.
#'
#' @description `request_generate()` lets you provide the bare minimum of input.
#'   It takes a nickname for an endpoint and:
#'
#'   * Uses the API spec to look up the `path`, `method`, and base URL.
#'
#'   * Checks `params` for validity and completeness with respect to the
#'   endpoint. Separates parameters into those destined for the body, the query,
#'   and URL endpoint substitution (which is also enacted).
#'
#' @param endpoint Character. Nickname for one of the selected Calendar v3 API
#'   endpoints built into `gcalendr`. Learn more in [calendar_endpoints()].
#'
#' @param params Named list. Parameters destined for endpoint URL substitution,
#'   the query, or the body.
#'
#' @param token Calendar token.
#'
#' @return `list()`\cr Components are `method`, `path`, `query`, `body`,
#'   `token`, and `url`, suitable as input for [request_make()].
#' @export
#' @family low-level API functions
#' @seealso [gargle::request_develop()], [gargle::request_build()]
#'
#'
#' @examples
#' \dontrun{
#' req <- request_generate(
#'   "drive.files.get",
#'   list(fileId = "abc"),
#'   token = calendar_token()
#' )
#' req
#' }
request_generate <- function(endpoint = character(),
                             params = list(),
                             token = calendar_token()) {
  ept <- .endpoints[[endpoint]]
  if (is.null(ept)) {
    stop_glue("\nEndpoint not recognized:\n  * {endpoint}")
  }

  req <- gargle::request_develop(endpoint = ept, params = params)

  gargle::request_build(
    method = req$method,
    path = req$path,
    params = req$params,
    body = req$body,
    token = token
  )
}
