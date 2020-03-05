test_that("Can decode endpoints", {
  ep <- calendar_endpoints()
  expect_is(ep, "list")

  ep <- calendar_endpoints("calendar.calendarList.list")
  expect_is(ep, "list")

})

