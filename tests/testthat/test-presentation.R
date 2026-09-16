presentation_catalog <- function() {
  data.frame(
    indicator_id = c("609", "700"),
    indicator_code = c("SYNTHETIC_INFLATION", "SYNTHETIC_GDP"),
    indicator_description = c("Synthetic inflation", "Synthetic output"),
    frequency = c("monthly", "quarterly"),
    group_code = c("prices", "activity"),
    stringsAsFactors = FALSE
  )
}

presentation_series <- function() {
  data.frame(
    indicator_id = "609", indicator_description = "Synthetic inflation",
    date = as.Date(c("2024-01-01", "2024-02-01", "2024-04-01", "2024-05-01")),
    value = c(1, 2, 4, 5), frequency = "monthly", unit = "Synthetic percent",
    stringsAsFactors = FALSE
  )
}

testthat::test_that("catalogue viewer escapes untrusted strings and works without external assets", {
  testthat::skip_if_not_installed("htmltools")
  catalog <- presentation_catalog()
  catalog$indicator_description[[1L]] <- "<script>alert('unsafe')</script>"
  catalog$group_code[[1L]] <- "\"><img src=x onerror=alert(1)>"
  html <- as.character(bch_view_indicators(catalog, page_size = 1))
  testthat::expect_match(html, "&lt;script&gt;", fixed = TRUE)
  testthat::expect_false(grepl("<script>alert", html, fixed = TRUE))
  testthat::expect_false(grepl("<img src=x", html, fixed = TRUE))
  testthat::expect_false(grepl("<script[^>]+src=", html))
  testthat::expect_false(grepl("innerHTML", html, fixed = TRUE))
  testthat::expect_match(html, 'aria-live="polite"', fixed = TRUE)
  testthat::expect_match(html, 'scope="col"', fixed = TRUE)
  testthat::expect_match(html, "datos proporcionados por el usuario", fixed = TRUE)
  testthat::expect_match(html, "SYNTHETIC_INFLATION", fixed = TRUE)
  script <- sub(".*<script>(.*)</script>.*", "\\1", gsub("\n", "", html))
  testthat::expect_false(grepl("SYNTHETIC_INFLATION", script, fixed = TRUE))
  testthat::expect_false(grepl("unsafe", script, fixed = TRUE))
})

testthat::test_that("viewer offers accessible local sorting and ID copying", {
  testthat::skip_if_not_installed("htmltools")
  html <- as.character(bch_view_indicators(presentation_catalog()))
  testthat::expect_equal(length(gregexpr('data-sort-index="[0-4]"', html)[[1L]]), 5L)
  testthat::expect_match(html, 'aria-sort="none"', fixed = TRUE)
  testthat::expect_match(html, 'aria-label="Ordenar por ID"', fixed = TRUE)
  testthat::expect_match(html, 'data-copy-id="609"', fixed = TRUE)
  testthat::expect_match(html, 'aria-label="Copiar ID 609"', fixed = TRUE)
  testthat::expect_match(html, 'data-role="copy-value"', fixed = TRUE)
  testthat::expect_match(html, 'readonly="readonly"', fixed = TRUE)
  testthat::expect_match(html, "clipboard.writeText(value)", fixed = TRUE)
  testthat::expect_match(html, "use Ctrl+C o Comando+C", fixed = TRUE)
  testthat::expect_match(html, "collator.compare(a.cells[column],b.cells[column])", fixed = TRUE)
  testthat::expect_match(html, "fragment.appendChild(entry.row)", fixed = TRUE)
})

testthat::test_that("viewer limits DOM updates to the current page and retains a no-JavaScript table", {
  testthat::skip_if_not_installed("htmltools")
  catalog <- presentation_catalog()
  html <- as.character(bch_view_indicators(catalog, page_size = 1))
  # The static document contains every row for progressive enhancement.
  testthat::expect_match(html, "<td>609</td>", fixed = TRUE)
  testthat::expect_match(html, "<td>700</td>", fixed = TRUE)
  testthat::expect_match(html, "<noscript>", fixed = TRUE)
  # Interactive renders batch only a page of existing, escaped DOM nodes.
  testthat::expect_match(html, "var fragment=document.createDocumentFragment();", fixed = TRUE)
  testthat::expect_match(html,
    "filtered.slice(page*size,(page+1)*size).forEach(function(entry){fragment.appendChild(entry.row);});",
    fixed = TRUE)
  testthat::expect_match(html, "body.replaceChildren(fragment);", fixed = TRUE)
  testthat::expect_false(grepl("body.appendChild(entry.row)", html, fixed = TRUE))
  testthat::expect_false(grepl("rows.forEach(function(row){row.hidden", html, fixed = TRUE))
})

testthat::test_that("viewer can restrict text search to a selected field", {
  testthat::skip_if_not_installed("htmltools")
  html <- as.character(bch_view_indicators(presentation_catalog()))
  testthat::expect_match(html, 'data-role="search-field"', fixed = TRUE)
  for (option in c('<option value="-1">Todos los campos</option>',
                   '<option value="0">ID</option>', '<option value="1">Codigo</option>',
                   '<option value="2">Descripcion</option>', '<option value="4">Grupo</option>')) {
    testthat::expect_match(html, option, fixed = TRUE)
  }
  testthat::expect_match(html, "column<0?entry.text:entry.foldedCells[column]", fixed = TRUE)
  testthat::expect_match(html, "searchField.addEventListener('change',filter)", fixed = TRUE)
  testthat::expect_match(html, "function filter(){copyStatus.textContent='';copyManual.hidden=true;", fixed = TRUE)
})

testthat::test_that("viewer preserves readable columns inside a horizontal mobile scroll region", {
  testthat::skip_if_not_installed("htmltools")
  html <- as.character(bch_view_indicators(presentation_catalog()))
  testthat::expect_match(html, ".bch-catalog table{width:100%;min-width:760px;", fixed = TRUE)
  testthat::expect_match(html,
    ".bch-catalog .bch-table-wrap{max-width:100%;min-width:0;max-height:65vh;overflow:auto;",
    fixed = TRUE)
  testthat::expect_match(html, 'class="bch-table-wrap" tabindex="0" role="region"', fixed = TRUE)
})

testthat::test_that("viewer rejects refresh with supplied data and supports empty catalogs", {
  testthat::skip_if_not_installed("htmltools")
  catalog <- presentation_catalog()
  testthat::expect_error(bch_view_indicators(catalog, refresh = TRUE), class = "bch_invalid_argument")
  testthat::expect_error(bch_view_indicators(catalog, unused = 1), class = "bch_invalid_argument")
  testthat::expect_error(bch_view_indicators(catalog, page_size = 0), class = "bch_invalid_argument")
  testthat::expect_error(bch_view_indicators(data.frame(id = 1)), class = "bch_invalid_argument")
  testthat::expect_s3_class(bch_view_indicators(catalog[FALSE, ]), "shiny.tag")
})

testthat::test_that("viewer preserves source and retrieval date from result provenance", {
  testthat::skip_if_not_installed("htmltools")
  x <- bch_result(data = presentation_catalog(), provenance = data.frame(
    retrieved_at = as.POSIXct("2026-09-14 12:00:00", tz = "UTC"),
    source = "Synthetic catalogue fixture", endpoint = "https://example.invalid/catalogue",
    schema_version = "fixture", from_cache = FALSE
  ))
  html <- as.character(bch_view_indicators(x))
  testthat::expect_match(html, "Synthetic catalogue fixture", fixed = TRUE)
  testthat::expect_match(html, "2026-09-14 12:00 UTC", fixed = TRUE)
})

testthat::test_that("attribution prefixes are not duplicated for cached provenance", {
  x <- bch_result(data = presentation_catalog(), provenance = data.frame(
    retrieved_at = as.POSIXct("2026-09-14 12:00:00", tz = "UTC"),
    source = c("Fuente: Banco Central de Honduras", "Banco Central de Honduras"),
    endpoint = "https://example.invalid/catalogue", schema_version = "fixture",
    from_cache = TRUE
  ))
  source <- .bch_presentation_source(x)$source
  testthat::expect_identical(source, "Fuente: Banco Central de Honduras")
})

testthat::test_that("plots break lines at absent periods without modifying inputs", {
  testthat::skip_if_not_installed("ggplot2")
  x <- presentation_series()
  original <- x
  plot <- bch_plot_series(x)
  testthat::expect_s3_class(plot, "ggplot")
  testthat::expect_identical(x, original)
  testthat::expect_equal(length(unique(plot$data$.bch_segment)), 2L)
  built <- ggplot2::ggplot_build(plot)
  testthat::expect_equal(length(unique(built$data[[1L]]$group)), 2L)
  testthat::expect_equal(nrow(built$data[[1L]]), 4L)
  testthat::expect_match(plot$labels$caption, "datos proporcionados por el usuario", fixed = TRUE)
})

testthat::test_that("plot defaults use readable dates and a viridis colour scale", {
  testthat::skip_if_not_installed("ggplot2")
  plot <- bch_plot_series(presentation_series())
  testthat::expect_equal(plot$theme$axis.text.x$angle, 30)
  testthat::expect_equal(plot$theme$axis.text.x$hjust, 1)
  testthat::expect_gte(as.numeric(plot$theme$plot.margin)[[2L]], 20)
  expected <- ggplot2::scale_colour_viridis_d(begin = 0.1, end = 0.8)
  testthat::expect_equal(plot$scales$get_scales("colour")$palette(3), expected$palette(3))
})

testthat::test_that("missing values break lines and unknown frequency is points only", {
  testthat::skip_if_not_installed("ggplot2")
  x <- presentation_series()
  x$date <- as.Date(c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01"))
  x$value[[2L]] <- NA_real_
  plot <- bch_plot_series(x)
  line <- plot$layers[[1L]]$data
  testthat::expect_equal(nrow(line), 2L)
  testthat::expect_equal(line$value, c(4, 5))
  x$frequency <- "unknown"
  plot <- bch_plot_series(x)
  testthat::expect_equal(nrow(plot$layers[[1L]]$data), 0L)
  testthat::expect_equal(nrow(ggplot2::ggplot_build(plot)$data[[2L]]), 4L)
})

testthat::test_that("plots use metadata and separate panels for distinct units", {
  testthat::skip_if_not_installed("ggplot2")
  x <- presentation_series()
  second <- x
  second$indicator_id <- "700"
  second$indicator_description <- "Synthetic production"
  x <- rbind(x, second)
  x$unit <- NULL
  result <- bch_result(data = x, metadata = data.frame(
    indicator_id = c("609", "700"), unit = c("Synthetic percent", "Synthetic index")
  ))
  plot <- bch_plot_series(result)
  testthat::expect_s3_class(plot$facet, "FacetWrap")
  testthat::expect_equal(length(unique(ggplot2::ggplot_build(plot)$layout$layout$PANEL)), 2L)
  testthat::expect_equal(unique(bch_plot_series(result, id = "609")$data$indicator_id), "609")
  testthat::expect_error(bch_plot_series(result, id = "missing"), class = "bch_unknown_indicator")
})

testthat::test_that("custom plot captions retain attribution from provenance", {
  testthat::skip_if_not_installed("ggplot2")
  x <- bch_result(data = presentation_series(), provenance = data.frame(
    retrieved_at = as.POSIXct("2026-09-14 12:00:00", tz = "UTC"),
    source = "Synthetic source", endpoint = "https://example.invalid/series",
    schema_version = "fixture", from_cache = FALSE
  ))
  caption <- bch_plot_series(x, caption = "Custom explanation")$labels$caption
  testthat::expect_match(caption, "Custom explanation", fixed = TRUE)
  testthat::expect_match(caption, "Fuente: Synthetic source", fixed = TRUE)
  testthat::expect_match(caption, "2026-09-14 12:00 UTC", fixed = TRUE)
})

testthat::test_that("plots reject ambiguous dates, duplicate periods and implicit transformations", {
  testthat::skip_if_not_installed("ggplot2")
  x <- presentation_series()
  testthat::expect_error(bch_plot_series(x, mode = "growth"), class = "bch_invalid_argument")
  testthat::expect_error(bch_plot_series(x, unused = TRUE), class = "bch_invalid_argument")
  x$date <- as.character(x$date)
  testthat::expect_error(bch_plot_series(x), class = "bch_invalid_date")
  x <- presentation_series()
  x$date[[2L]] <- as.Date("2024-01-15")
  testthat::expect_error(bch_plot_series(x), class = "bch_conflicting_duplicate")
  x <- presentation_series()
  x$unit[[2L]] <- "Other unit"
  testthat::expect_error(bch_plot_series(x), class = "bch_invalid_argument")
})

testthat::test_that("daily, quarterly and annual gaps follow calendar periods", {
  testthat::skip_if_not_installed("ggplot2")
  calendars <- list(
    daily = c("2024-01-01", "2024-01-02", "2024-01-04", "2024-01-05"),
    quarterly = c("2023-10-01", "2024-01-01", "2024-07-01", "2024-10-01"),
    annual = c("2021-01-01", "2022-01-01", "2024-01-01", "2025-01-01")
  )
  for (frequency in names(calendars)) {
    x <- presentation_series()
    x$date <- as.Date(calendars[[frequency]])
    x$frequency <- frequency
    plot <- bch_plot_series(x)
    testthat::expect_equal(length(unique(plot$data$.bch_segment)), 2L)
  }
  singleton <- presentation_series()[1L, ]
  plot <- bch_plot_series(singleton)
  testthat::expect_equal(nrow(plot$layers[[1L]]$data), 0L)
  testthat::expect_equal(nrow(ggplot2::ggplot_build(plot)$data[[2L]]), 1L)
})

testthat::test_that("consecutive missing values and unsorted rows cannot bridge a gap", {
  testthat::skip_if_not_installed("ggplot2")
  x <- presentation_series()
  x$date <- as.Date(c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01"))
  x$value <- c(1, NA_real_, NA_real_, 4)
  plot <- bch_plot_series(x[c(4, 2, 1, 3), ])
  testthat::expect_identical(plot$data$.bch_date, sort(x$date))
  testthat::expect_equal(nrow(plot$layers[[1L]]$data), 0L)
  x$value <- NA_real_
  testthat::expect_error(bch_plot_series(x), class = "bch_invalid_argument")
})
