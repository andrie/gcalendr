if (gargle:::secret_can_decrypt("gcalendr")) {
  json <- gargle:::secret_read("gcalendr", "gcalendr-testing.json")
  calendar_auth(path = rawToChar(json))
}

skip_if_no_token <- function() {
  testthat::skip_if_not(calendar_has_token(), "No Calendar token")
}
