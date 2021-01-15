#' convert character vector of ids to named list
#'
#' @param x a character vector of calendar ids
#'
#' @return a list character vectors, each with name "id"
enlist <- function(x) lapply(x, function(y) list(id = y))

#' format time for calendar API
#'
#' @param time a POSIXct or POSIXlt object
#' @param tz an IANA timezone designation
#'
#' @return an RFC 3339 formatted character vector
fmt_time <- function(date, zone = "UTC") {
  strftime(x = date, tz = zone, "%Y-%m-%dT%H:%M:00Z")
}
