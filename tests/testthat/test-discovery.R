context("API endpoints")

test_that("Can decode endpoints", {
  ep <- gcalendr_endpoints()
  expect_is(ep, "list")

  ep <- gcalendr_endpoints("calendar.calendarList.list")
  expect_is(ep, "list")

})


context("Defunct")
test_that("Error on defunct", {
  expect_error(get_google_token(), "Use 'gcalendr_token' instead.")
})
