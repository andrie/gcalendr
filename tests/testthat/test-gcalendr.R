context("Read calendar events")

if (Sys.getenv("GCALENDR_PASSWORD") != "" && gargle:::secret_can_decrypt("gcalendr")) {
  test_that("Can read events", {

    secret <- gargle:::secret_read("gcalendr", "gmail-token")
    token <- gargle::credentials_service_account(path = rawToChar(secret))
    calendar_auth("apdevries@gmail.com", path = token, use_oob = TRUE)

    expect_true(calendar_has_token())

    calendar_ids <- calendar_list()
    expect_is(calendar_ids, "data.frame")
    expect_equal(ncol(calendar_ids), 3)


    my_cal_id <- "apdevries@gmail.com"
    items <- call_calendar_events(my_cal_id,
                                  time_min = as.Date("2019-01-01") - 90,
                                  time_max = as.Date("2019-01-01") + 90
    )

    expect_is(items, "list")

    events <- calendar_events(my_cal_id,
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
}
