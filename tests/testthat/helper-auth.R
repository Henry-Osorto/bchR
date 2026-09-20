# Test helpers for authentication --------------------------------------------

with_bch_api_key <- function(value = NULL, code) {
  old <- Sys.getenv("BCH_API_KEY", unset = NA_character_)

  on.exit({
    if (is.na(old)) {
      Sys.unsetenv("BCH_API_KEY")
    } else {
      Sys.setenv(BCH_API_KEY = old)
    }
  }, add = TRUE)

  if (is.null(value)) {
    Sys.unsetenv("BCH_API_KEY")
  } else {
    Sys.setenv(BCH_API_KEY = value)
  }

  force(code)
}

new_test_secret <- function() {
  # Clearly synthetic and generated only for the current test process.
  paste0("bchR-test-secret-", Sys.getpid(), "-not-a-real-key")
}
