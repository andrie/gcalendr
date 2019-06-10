if (interactive()) library(testthat)
library(dplyr)
library(tidyr)

context("gcalendr")

if (gargle:::secret_can_decrypt("gcalendr")) {
  my_cal_id <- "apdevries@gmail.com"

  token <- gargle:::secret_read("gcalendr", "gmail-token")
  gcalendr_auth("apdevries@gmail.com", path = rawToChar(token))

  expect_true(gcalendr_has_token())

  calendar_ids <- get_gcal_list()
  expect_is(calendar_ids, "data.frame")
  expect_equal(ncol(calendar_ids), 3)


  events <- get_gcal_events(my_cal_id, days_in_past = 7, days_in_future = 7, now = as.Date("2019-01-01"))
  expect_is(events, "data.frame")

  attendees <-
    events %>%
    select(id, attendees) %>%
    tidyr::unnest(attendees)

  creator <-
    events %>%
    select(id, creator) %>%
    tidyr::unnest(creator)

  organizer <-
    events %>%
    select(id, organizer) %>%
    tidyr::unnest(organizer)


  expect_is(attendees, "data.frame")
  expect_equal(ncol(attendees), 7)

  expect_is(creator, "data.frame")
  expect_equal(ncol(creator), 4)

  expect_is(organizer, "data.frame")
  expect_equal(ncol(organizer), 4)

}
