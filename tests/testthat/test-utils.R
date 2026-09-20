testthat::test_that("shared logical validation is strict", {
  testthat::expect_silent(.bch_validate_flag(TRUE, "x"))
  testthat::expect_silent(.bch_validate_flag(FALSE, "x"))
  testthat::expect_error(.bch_validate_flag(NA, "x"), "x must be TRUE or FALSE")
  testthat::expect_error(.bch_validate_flag(c(TRUE, FALSE), "x"))
  testthat::expect_error(.bch_validate_flag(1, "x"))
})


testthat::test_that("provenance helper never carries the API key", {
  .bchr_with_test_key({
    secret <- Sys.getenv("BCH_API_KEY")
    x <- data.frame(a = 1)

    out <- .bch_add_provenance(
      x,
      path = c("indicadores", "609", "cifras"),
      indicator_id = "609",
      retrieved_at = as.POSIXct("2026-09-19 12:00:00", tz = "UTC")
    )

    testthat::expect_identical(attr(out, "indicator_id"), "609")
    testthat::expect_s3_class(attr(out, "retrieved_at"), "POSIXct")
    endpoint <- attr(out, "api_endpoint")
    testthat::expect_false(grepl("clave=", endpoint, fixed = TRUE))
    testthat::expect_false(grepl(secret, endpoint, fixed = TRUE))
  })
})
