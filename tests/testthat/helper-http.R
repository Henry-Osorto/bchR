# Helpers for HTTP-layer tests ------------------------------------------------

.bchr_with_test_key <- function(code) {
  old <- Sys.getenv("BCH_API_KEY", unset = NA_character_)
  key <- paste0(
    "bchR-test-secret-",
    Sys.getpid(),
    "-not-a-real-key"
  )

  on.exit({
    if (is.na(old)) {
      Sys.unsetenv("BCH_API_KEY")
    } else {
      Sys.setenv(BCH_API_KEY = old)
    }
  }, add = TRUE)

  Sys.setenv(BCH_API_KEY = key)

  force(code)
}


.bchr_with_no_test_key <- function(code) {
  old <- Sys.getenv("BCH_API_KEY", unset = NA_character_)

  on.exit({
    if (is.na(old)) {
      Sys.unsetenv("BCH_API_KEY")
    } else {
      Sys.setenv(BCH_API_KEY = old)
    }
  }, add = TRUE)

  Sys.unsetenv("BCH_API_KEY")
  force(code)
}
