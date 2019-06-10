#' Get list of calendars from google.
#'
#'
#' @param token token obtained from [gcalendr_token()]
#'
#' @return Tibble with columns for `id`, `summary` and `description`.
#' @family calendar functions
#'
#' @export
get_gcal_list <- function(token = gcalendr_token()){

  req <- request_generate("calendar.calendarList.list", token = token)
  r <- request_make(req)
  httr::stop_for_status(r)
  r <- content(r)

  null_to_na <- function(x){
    ifelse(is.null(x), NA, x)
  }

  r %>%
    pluck("items") %>%
    map_dfr(
      ~list(
        id = .[["id"]],
        summary = .[["summary"]],
        description = .[["description"]]  %>% null_to_na()
      )
    )

}
