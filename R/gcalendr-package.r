#' gcalendr
#'
#' You can read Google calendar data using the  Google Calendar API
#'
#' Access your calendar:
#'
#' * [get_gcal_list()]
#' * [get_gcal_events()]
#'
#' Authenticate
#'
#' * [gcalendr_auth()]
#' * [gcalendr_token()]
#'
#' @name gcalendr
#' @aliases gcalendr-package
#' @docType package
#'
#' @importFrom dplyr '%>%' bind_cols select mutate if_else bind_rows one_of left_join group_by rename
#' @importFrom httr GET content oauth_app config oauth_endpoints oauth2.0_token
#' @importFrom purrr map map_chr map_int map_lgl map_dfr
#' @importFrom purrr pluck discard pluck
#' @importFrom rlang flatten is_string %||%
#' @importFrom lubridate as_date as_datetime
#' @importFrom tidyr nest
#' @importFrom tibble tibble as_tibble
#' @importFrom gargle  request_make
#'
NULL
