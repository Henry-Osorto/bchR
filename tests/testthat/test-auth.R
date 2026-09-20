test_that("bch_has_api_key() detects missing, empty, whitespace and valid values", {
  with_bch_api_key(NULL, {
    expect_false(bch_has_api_key())
  })

  with_bch_api_key("", {
    expect_false(bch_has_api_key())
  })

  with_bch_api_key("   ", {
    expect_false(bch_has_api_key())
  })

  with_bch_api_key(new_test_secret(), {
    expect_true(bch_has_api_key())
  })
})

test_that("bch_api_key() fails safely when the key is unavailable", {
  with_bch_api_key(NULL, {
    expect_error(
      bch_api_key(),
      "bch_set_api_key\\(\\)",
      fixed = FALSE
    )
  })
})

test_that("bch_api_key() returns the configured key only to package code", {
  secret <- new_test_secret()

  with_bch_api_key(secret, {
    expect_identical(bch_api_key(), secret)
  })
})

test_that("a new .Renviron file is created with exactly one key declaration", {
  secret <- new_test_secret()
  renviron <- tempfile(pattern = "bchR-Renviron-")
  on.exit(unlink(renviron, force = TRUE), add = TRUE)

  expect_false(file.exists(renviron))

  expect_true(
    .bch_store_api_key(
      key = secret,
      renviron_file = renviron,
      set_session = FALSE
    )
  )

  expect_true(file.exists(renviron))

  x <- readLines(renviron, warn = FALSE)
  expect_equal(
    sum(grepl("^[[:space:]]*BCH_API_KEY[[:space:]]*=", x)),
    1L
  )

  con <- file(renviron, open = "rb")
  on.exit(close(con), add = TRUE)
  bytes <- readBin(con, what = "raw", n = file.info(renviron)$size)
  expect_identical(tail(bytes, 1L), as.raw(0x0a))
})

test_that("existing .Renviron content is preserved and duplicate keys are replaced", {
  secret <- new_test_secret()
  renviron <- tempfile(pattern = "bchR-Renviron-")
  on.exit(unlink(renviron, force = TRUE), add = TRUE)

  writeLines(
    c(
      "OTHER_VAR=alpha",
      "# BCH_API_KEY=commented-value-is-not-active",
      "BCH_API_KEY=old-one",
      "   BCH_API_KEY = old-two",
      "",
      "SECOND_VAR=beta"
    ),
    renviron,
    useBytes = TRUE
  )

  .bch_store_api_key(
    key = secret,
    renviron_file = renviron,
    set_session = FALSE
  )

  x <- readLines(renviron, warn = FALSE)

  expect_true("OTHER_VAR=alpha" %in% x)
  expect_true("SECOND_VAR=beta" %in% x)
  expect_true("# BCH_API_KEY=commented-value-is-not-active" %in% x)
  expect_true("" %in% x)
  expect_equal(
    sum(grepl("^[[:space:]]*BCH_API_KEY[[:space:]]*=", x)),
    1L
  )
})

test_that("storing a key makes it available immediately in the current session", {
  secret <- new_test_secret()
  renviron <- tempfile(pattern = "bchR-Renviron-")
  on.exit(unlink(renviron, force = TRUE), add = TRUE)

  with_bch_api_key(NULL, {
    .bch_store_api_key(
      key = secret,
      renviron_file = renviron,
      set_session = TRUE
    )

    expect_true(bch_has_api_key())
    expect_identical(Sys.getenv("BCH_API_KEY", unset = ""), secret)
  })
})

test_that("line breaks in a key are rejected to prevent .Renviron injection", {
  expect_error(
    .bch_validate_api_key_value("abc\nSECOND_VAR=unsafe"),
    "cannot contain line breaks"
  )
})

test_that("write failures produce a safe error and do not expose the key", {
  secret <- new_test_secret()
  missing_parent <- file.path(
    tempdir(),
    paste0("bchR-missing-parent-", Sys.getpid()),
    ".Renviron"
  )

  expect_false(dir.exists(dirname(missing_parent)))

  err <- tryCatch(
    {
      .bch_store_api_key(
        key = secret,
        renviron_file = missing_parent,
        set_session = FALSE
      )
      NULL
    },
    error = identity
  )

  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "Could not write BCH_API_KEY")
  expect_false(grepl(secret, conditionMessage(err), fixed = TRUE))
})

test_that("non-interactive package attachment is silent and never prompts", {
  old_askpass <- options(
    askpass = function(prompt) stop("prompt should not be called")
  )
  on.exit(options(old_askpass), add = TRUE)

  with_bch_api_key(NULL, {
    expect_silent(.bch_on_attach(is_interactive = FALSE))
    expect_false(bch_has_api_key())
  })
})

test_that("interactive attachment explains registration and securely configures a missing key", {
  secret <- new_test_secret()
  renviron <- tempfile(pattern = "bchR-Renviron-startup-")
  on.exit(unlink(renviron, force = TRUE), add = TRUE)

  old_r_environ_user <- Sys.getenv("R_ENVIRON_USER", unset = NA_character_)
  old_askpass <- options(askpass = function(prompt) secret)

  on.exit({
    options(old_askpass)
    if (is.na(old_r_environ_user)) {
      Sys.unsetenv("R_ENVIRON_USER")
    } else {
      Sys.setenv(R_ENVIRON_USER = old_r_environ_user)
    }
  }, add = TRUE)

  Sys.setenv(R_ENVIRON_USER = renviron)

  with_bch_api_key(NULL, {
    msg <- capture.output(
      .bch_on_attach(is_interactive = TRUE, prompt_for_key = TRUE),
      type = "message"
    )
    msg <- paste(msg, collapse = "\n")

    expect_match(msg, "https://bchapi-am.developer.azure-api.net/", fixed = TRUE)
    expect_match(msg, "Primary key", fixed = TRUE)
    expect_true(file.exists(renviron))
    expect_true(bch_has_api_key())
    expect_identical(Sys.getenv("BCH_API_KEY", unset = ""), secret)
    expect_false(grepl(secret, msg, fixed = TRUE))
  })
})

test_that("interactive attachment tolerates a cancelled password prompt", {
  old_askpass <- options(askpass = function(prompt) NULL)
  on.exit(options(old_askpass), add = TRUE)

  with_bch_api_key(NULL, {
    expect_message(
      .bch_on_attach(is_interactive = TRUE, prompt_for_key = TRUE),
      "No API key was entered"
    )
    expect_false(bch_has_api_key())
  })
})

test_that("interactive attachment can disable automatic prompting", {
  old_askpass <- options(
    askpass = function(prompt) stop("prompt should not be called")
  )
  on.exit(options(old_askpass), add = TRUE)

  with_bch_api_key(NULL, {
    msg <- capture.output(
      .bch_on_attach(is_interactive = TRUE, prompt_for_key = FALSE),
      type = "message"
    )
    msg <- paste(msg, collapse = "\n")
    expect_match(msg, "Automatic API-key prompting is disabled", fixed = TRUE)
    expect_false(bch_has_api_key())
  })
})

test_that("package attachment is silent and never prompts when a key is configured", {
  old_askpass <- options(
    askpass = function(prompt) stop("prompt should not be called")
  )
  on.exit(options(old_askpass), add = TRUE)

  with_bch_api_key(new_test_secret(), {
    expect_silent(.bch_on_attach(is_interactive = TRUE, prompt_for_key = TRUE))
  })
})

test_that("askpass-based prompting does not require RStudio", {
  old <- options(askpass = function(prompt) NULL)
  on.exit(options(old), add = TRUE)

  # Because the prompt is cancelled, the public function must not reach the
  # file-writing stage and therefore cannot touch ~/.Renviron.
  expect_message(
    result <- bch_set_api_key(),
    "No API key was entered"
  )
  expect_false(result)
})



test_that("bch_set_api_key() stores securely without exposing the key", {
  secret <- new_test_secret()
  renviron <- tempfile(pattern = "bchR-Renviron-")
  on.exit(unlink(renviron, force = TRUE), add = TRUE)

  old_r_environ_user <- Sys.getenv("R_ENVIRON_USER", unset = NA_character_)
  old_askpass <- options(askpass = function(prompt) secret)

  on.exit({
    options(old_askpass)
    if (is.na(old_r_environ_user)) {
      Sys.unsetenv("R_ENVIRON_USER")
    } else {
      Sys.setenv(R_ENVIRON_USER = old_r_environ_user)
    }
  }, add = TRUE)

  Sys.setenv(R_ENVIRON_USER = renviron)

  with_bch_api_key(NULL, {
    msg <- capture.output(
      result <- bch_set_api_key(),
      type = "message"
    )

    expect_true(result)
    expect_true(file.exists(renviron))
    expect_true(bch_has_api_key())
    expect_identical(Sys.getenv("BCH_API_KEY", unset = ""), secret)
    expect_false(any(grepl(secret, msg, fixed = TRUE)))
  })
})

test_that("bch_redact_url() removes both query credentials and session secrets", {
  secret <- new_test_secret()

  with_bch_api_key(secret, {
    input <- paste0(
      "Request failed: https://example.test/path?formato=Json&clave=",
      secret,
      "&other=1"
    )

    output <- bch_redact_url(input)

    expect_false(grepl(secret, output, fixed = TRUE))
    expect_match(output, "clave=<redacted>", fixed = TRUE)
  })
})

test_that("redaction works even when BCH_API_KEY is not set", {
  with_bch_api_key(NULL, {
    output <- bch_redact_url(
      "https://example.test/path?clave=some-value&other=1"
    )

    expect_identical(
      output,
      "https://example.test/path?clave=<redacted>&other=1"
    )
  })
})

test_that("user-facing errors and messages never contain the configured secret", {
  secret <- new_test_secret()

  with_bch_api_key(secret, {
    attach_msg <- capture.output(
      .bch_on_attach(is_interactive = TRUE),
      type = "message"
    )

    expect_false(any(grepl(secret, attach_msg, fixed = TRUE)))
  })

  with_bch_api_key(NULL, {
    err <- tryCatch(bch_api_key(), error = identity)
    expect_false(grepl(secret, conditionMessage(err), fixed = TRUE))
  })
})
