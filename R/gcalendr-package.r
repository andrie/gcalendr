#' gcalendr
#'
#' Important functions:
#'
#' * [get_google_token()]
#' * [get_gcal_list()]
#' * [get_gcal_events()]
#'
#' @name gcalendr
#' @docType package
#' @importFrom httr GET content oauth_app config oauth_endpoints oauth2.0_token
#' @importFrom purrr map map_chr map_int map_lgl map_dfr
#' @importFrom purrr pluck discard pluck vec_depth
#' @importFrom rlang flatten squash
#' @importFrom dplyr '%>%' bind_cols select mutate if_else bind_rows one_of left_join group_by
# @importFrom memoise memoise
#' @importFrom lubridate as_date as_datetime
#' @importFrom tidyr nest
#'
NULL
