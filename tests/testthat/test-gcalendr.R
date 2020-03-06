test_that("Can read events", {
  skip_if_no_token()

  expect_is(calendar_user(), "character")
  expect_is(calendar_app(), "oauth_app")

  calendar_ids <- calendar_list()
  expect_is(calendar_ids, "data.frame")
  expect_equal(ncol(calendar_ids), 3)

  my_cal_id <- "apdevries@gmail.com"
  items <- call_calendar_events(
    my_cal_id,
    time_min = as.Date("2019-01-01") - 90,
    time_max = as.Date("2019-01-01") + 90
  )

  calendar_ids <- calendar_list()

  expect_is(calendar_ids, "data.frame")
  expect_equal(ncol(calendar_ids), 3)
  expect_is(items, "list")

  events <- calendar_events(
    my_cal_id,
    days_in_past = 90,
    days_in_future = 90,
    now = as.Date("2019-01-01"),
    max_results = 25
  )

  expect_is(events, "data.frame")
  expect_equal(length(items), nrow(events))
  expect_true(nrow(events) > 0)

  attendees <-
    events %>%
    select(id, attendees) %>%
    tidyr::unnest(attendees)

  creator <-
    events %>%
    select(id, starts_with("creator"))

  organizer <-
    events %>%
    select(id, dplyr::starts_with("organizer"))

  expect_is(attendees, "data.frame")
  expect_equal(ncol(attendees), 7)

  expect_is(creator, "data.frame")
  expect_equal(ncol(creator), 4)

  expect_is(organizer, "data.frame")
  expect_equal(ncol(organizer), 4)

  expect_null(
    calendar_deauth()
  )
})
