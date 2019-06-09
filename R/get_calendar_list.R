#' Get list of calendars from google.
#'
#' @param token token obtained from [get_google_token]
#'
#' @return character vector with calendar ids
#' @family gcal functions
#'
#' @importFrom gargle request_make
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
