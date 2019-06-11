context("Defunct functions")

expect_defunct <- function(expr, message){
  z <- tryCatch(
    eval(expr),
    error = function(e)e,
    message = function(m)m
  )
  expect_is(z, "defunctError")
  expect_match(z$message, message)
}

test_that("Error on defunct", {
  expect_defunct(
    get_google_token(),
    "Use 'calendar_token' instead."
  )
})
