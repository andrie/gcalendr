.onLoad <- function(libname, pkgname) {
  .auth <<- gargle::init_AuthState(
    package     = "gcalendr",
    auth_active = TRUE
  )
}