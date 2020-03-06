expect_defunct <- function(expr, message){
  z <- tryCatch(
    eval(expr),
    error = function(e)e,
    message = function(m)m
  )
  if (getRversion() > "3.5.3") {
    expect_is(z, "defunctError")
  } else {
    expect_is(z, "simpleError")
  }
  expect_match(z$message, message)
}

test_that("Error on defunct", {
  expect_defunct(
    get_google_token(),
    "Use 'calendar_token' instead."
  )

  expect_defunct(
    get_gcal_list(),
    "Use 'calendar_list' instead."
  )

  expect_defunct(
    get_gcal_events(),
    "Use 'calendar_events' instead."
  )

})
