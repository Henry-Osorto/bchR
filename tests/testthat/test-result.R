testthat::test_that("empty bch_result has five stable tibble components", {
  x <- bch_result()

  testthat::expect_s3_class(x, "bch_result")
  testthat::expect_identical(
    names(x),
    c("data", "metadata", "provenance", "problems", "column_map")
  )
  testthat::expect_true(all(vapply(x, inherits, logical(1), "tbl_df")))
  testthat::expect_identical(nrow(bch_data(x)), 0L)
  testthat::expect_identical(nrow(bch_metadata(x)), 0L)
  testthat::expect_identical(nrow(bch_provenance(x)), 0L)
  testthat::expect_identical(nrow(bch_problems(x)), 0L)
  testthat::expect_identical(nrow(bch_column_map(x)), 0L)
  testthat::expect_silent(validate_bch_result(x))
})

testthat::test_that("high-level constructor coerces data frames and accessors work", {
  retrieved_at <- as.POSIXct("2026-09-09 12:00:00", tz = "UTC")
  x <- bch_result(
    data = data.frame(indicator_id = "609", value = 4.2),
    metadata = data.frame(
      indicator_id = "609",
      indicator_description = "Inflation"
    ),
    provenance = data.frame(
      retrieved_at = retrieved_at,
      source = "Banco Central de Honduras",
      endpoint = "https://bchapi-am.azure-api.net/api/v1/indicadores/609/cifras",
      schema_version = "prototype-1",
      from_cache = FALSE
    ),
    problems = data.frame(
      indicator_id = "610",
      endpoint = "https://bchapi-am.azure-api.net/api/v1/indicadores/610/cifras",
      class = "bch_error_http_not_found",
      message = "Indicator was not found.",
      attempts = 1L
    ),
    column_map = data.frame(
      output_column = "inflation",
      indicator_id = "609"
    )
  )

  testthat::expect_s3_class(bch_data(x), "tbl_df")
  testthat::expect_identical(bch_metadata(x)$indicator_id, "609")
  testthat::expect_identical(bch_provenance(x)$retrieved_at, retrieved_at)
  testthat::expect_identical(bch_problems(x)$attempts, 1L)
  testthat::expect_identical(bch_column_map(x)$output_column, "inflation")
})

testthat::test_that("required schemas and component types are enforced", {
  testthat::expect_error(
    bch_result(metadata = tibble::tibble(name = "Inflation")),
    class = "bch_invalid_result"
  )
  testthat::expect_error(
    bch_result(provenance = tibble::tibble()),
    class = "bch_invalid_result"
  )

  x <- bch_result()
  x$data <- data.frame(value = 1)
  testthat::expect_error(
    validate_bch_result(x),
    class = "bch_invalid_result"
  )

  y <- unclass(bch_result())
  y$extra <- tibble::tibble()
  class(y) <- c("bch_result", "list")
  testthat::expect_error(
    validate_bch_result(y),
    class = "bch_invalid_result"
  )
})

testthat::test_that("cross-component uniqueness invariants are enforced", {
  testthat::expect_error(
    bch_result(
      metadata = tibble::tibble(indicator_id = c("609", "609"))
    ),
    class = "bch_invalid_result"
  )

  testthat::expect_error(
    bch_result(
      column_map = tibble::tibble(
        output_column = c("inflation", "inflation"),
        indicator_id = c("609", "610")
      )
    ),
    class = "bch_invalid_result"
  )
})

testthat::test_that("provenance and problems reject unsafe endpoints", {
  testthat::expect_error(
    bch_result(
      provenance = tibble::tibble(
        retrieved_at = as.POSIXct("2026-09-09", tz = "UTC"),
        source = "Banco Central de Honduras",
        endpoint = "https://example.test/cifras?clave=secret",
        schema_version = "prototype-1",
        from_cache = FALSE
      )
    ),
    class = "bch_unsafe_endpoint"
  )

  testthat::expect_error(
    bch_result(
      problems = tibble::tibble(
        indicator_id = "609",
        endpoint = "https://example.test/cifras?clave=secret",
        class = "bch_error_http_auth",
        message = "Authentication failed.",
        attempts = 1L
      )
    ),
    class = "bch_unsafe_endpoint"
  )
})

testthat::test_that("problem attempts are positive whole numbers", {
  make_problems <- function(attempts) {
    tibble::tibble(
      indicator_id = "609",
      endpoint = "https://example.test/cifras",
      class = "bch_error_network",
      message = "Temporary network failure.",
      attempts = attempts
    )
  }

  testthat::expect_error(
    bch_result(problems = make_problems(NA_integer_)),
    class = "bch_invalid_result"
  )
  testthat::expect_error(
    bch_result(problems = make_problems(0L)),
    class = "bch_invalid_result"
  )
  testthat::expect_error(
    bch_result(problems = make_problems(1.5)),
    class = "bch_invalid_result"
  )
})

testthat::test_that("conversions intentionally return only primary data", {
  data <- tibble::tibble(indicator_id = "609", value = 4.2)
  x <- bch_result(data = data)

  testthat::expect_identical(tibble::as_tibble(x), data)
  testthat::expect_identical(as.data.frame(x), as.data.frame(data))
  testthat::expect_false(inherits(as.data.frame(x), "bch_result"))
})

testthat::test_that("print summarizes every component and partial problems", {
  x <- bch_result(
    data = tibble::tibble(value = 4.2),
    problems = tibble::tibble(
      indicator_id = "610",
      endpoint = "https://example.test/cifras",
      class = "bch_error_http_not_found",
      message = "Indicator was not found.",
      attempts = 1L
    )
  )

  output <- testthat::capture_output(print(x))
  testthat::expect_match(output, "<bch_result>", fixed = TRUE)
  testthat::expect_match(output, "metadata:", fixed = TRUE)
  testthat::expect_match(output, "1 partial problem recorded", fixed = TRUE)
  testthat::expect_match(output, "Data:", fixed = TRUE)
  testthat::expect_identical(invisible(print(x)), x)
})

testthat::test_that("condition helpers expose stable package classes", {
  condition <- testthat::expect_error(
    bch_abort(
      "Invalid input.",
      class = "invalid_argument",
      status = 400L,
      call = NULL
    ),
    class = "bch_invalid_argument"
  )

  testthat::expect_s3_class(condition, "bch_error")
  testthat::expect_identical(condition$status, 400L)
})
