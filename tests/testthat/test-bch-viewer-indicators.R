testthat::test_that("viewer validates all logical arguments", {
  args <- c(
    "progress",
    "show_search",
    "striped",
    "bordered",
    "compact",
    "highlight",
    "full_width",
    "open_browser"
  )

  for (arg in args) {
    call_args <- list(progress = FALSE)
    call_args[[arg]] <- NA

    testthat::expect_error(
      do.call(bch_viewer_indicators, call_args),
      paste0(arg, " must be TRUE or FALSE"),
      fixed = TRUE
    )
  }
})


testthat::test_that("viewer validates page_size before calling the API", {
  bad_values <- list(
    0,
    -1,
    NA_real_,
    Inf,
    2.5,
    c(10, 15),
    "15"
  )

  for (value in bad_values) {
    testthat::expect_error(
      bch_viewer_indicators(
        progress = FALSE,
        page_size = value
      ),
      "page_size must be a single positive whole number",
      fixed = TRUE
    )
  }
})


testthat::test_that("empty catalogue data are rejected by the viewer layer", {
  testthat::expect_error(
    .bch_validate_viewer_data(data.frame()),
    "No BCH indicators are available to display",
    fixed = TRUE
  )
})


testthat::test_that("viewer data require the six standard catalogue columns", {
  data <- .bchr_viewer_catalogue()
  data$Descripcion <- NULL

  testthat::expect_error(
    .bch_validate_viewer_data(data),
    "Descripcion",
    fixed = TRUE
  )
})


testthat::test_that("viewer uses bch_get_indicators as its backend and returns HTML", {
  .bchr_with_test_key({
    httr2::local_mocked_responses(
      httr2::response_json(
        status_code = 200L,
        body = .bchr_viewer_records(6L)
      )
    )

    out <- bch_viewer_indicators(
      progress = FALSE,
      page_size = 15
    )

    testthat::expect_true(
      htmltools::is.browsable(out)
    )

    rendered <- htmltools::renderTags(out)$html

    testthat::expect_match(
      rendered,
      "Catálogo de indicadores del Banco Central de Honduras",
      fixed = TRUE
    )
    testthat::expect_match(
      rendered,
      "Indicadores disponibles: 6",
      fixed = TRUE
    )
    testthat::expect_match(
      rendered,
      "Generado con bchR",
      fixed = TRUE
    )
  })
})


testthat::test_that("viewer configuration keeps Spanish labels and six columns", {
  data <- .bchr_viewer_catalogue()

  out <- .bch_build_indicator_viewer(
    data = data,
    show_search = TRUE,
    striped = TRUE,
    bordered = FALSE,
    compact = FALSE,
    highlight = TRUE,
    full_width = TRUE,
    page_size = 15L
  )

  testthat::expect_true(
    htmltools::is.browsable(out)
  )

  rendered <- htmltools::renderTags(out)$html

  testthat::expect_match(rendered, "Descripción", fixed = TRUE)
  testthat::expect_match(rendered, "Periodicidad", fixed = TRUE)
  testthat::expect_match(rendered, "Correlativo del grupo", fixed = TRUE)
  testthat::expect_match(rendered, "Buscar", fixed = TRUE)
})


testthat::test_that("description cells are configured for wrapping", {
  columns <- .bch_viewer_column_definitions()

  testthat::expect_true("Descripcion" %in% names(columns))

  # Render a small viewer and verify that the wrapping styles reach the widget
  # specification rather than relying on internal colDef object structure.
  out <- .bch_build_indicator_viewer(
    data = .bchr_viewer_catalogue(),
    show_search = TRUE,
    striped = TRUE,
    bordered = FALSE,
    compact = FALSE,
    highlight = TRUE,
    full_width = TRUE,
    page_size = 15L
  )

  rendered <- htmltools::renderTags(out)$html
  testthat::expect_match(rendered, "whiteSpace", fixed = TRUE)
  testthat::expect_match(rendered, "overflowWrap", fixed = TRUE)
})


testthat::test_that("open_browser saves HTML without launching a real browser in tests", {
  .bchr_with_test_key({
    httr2::local_mocked_responses(
      httr2::response_json(
        status_code = 200L,
        body = .bchr_viewer_records(4L)
      )
    )

    opened <- NULL
    old_options <- options(
      bchR.browser_fun = function(url) {
        opened <<- url
        invisible(TRUE)
      }
    )
    on.exit(options(old_options), add = TRUE)

    out <- bch_viewer_indicators(
      progress = FALSE,
      open_browser = TRUE
    )

    testthat::expect_true(htmltools::is.browsable(out))
    testthat::expect_true(is.character(opened))
    testthat::expect_length(opened, 1L)
    testthat::expect_true(file.exists(opened))
    testthat::expect_identical(
      tolower(tools::file_ext(opened)),
      "html"
    )
  })
})


testthat::test_that("optional logos are not required", {
  testthat::expect_null(
    .bch_optional_viewer_image(
      "this-logo-does-not-exist.png"
    )
  )

  header <- .bch_viewer_header(10L)
  rendered <- htmltools::renderTags(header)$html

  testthat::expect_match(rendered, "bchR", fixed = TRUE)
  testthat::expect_match(
    rendered,
    "Banco Central de Honduras",
    fixed = TRUE
  )
})


testthat::test_that("a representative large catalogue can build a browsable viewer", {
  # This does not assert wall-clock timing because CI hardware varies. The
  # dedicated benchmark script measures construction time and HTML size using
  # the live catalogue. This fixture exercises the client-side data shape at a
  # non-trivial scale during ordinary tests.
  data <- .bchr_viewer_catalogue(5000L)

  out <- .bch_build_indicator_viewer(
    data = data,
    show_search = TRUE,
    striped = TRUE,
    bordered = FALSE,
    compact = FALSE,
    highlight = TRUE,
    full_width = TRUE,
    page_size = 15L
  )

  testthat::expect_true(htmltools::is.browsable(out))
})
