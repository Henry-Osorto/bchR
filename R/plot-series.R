# Time-series graphics ------------------------------------------------------

.bch_plot_period_index <- function(date, frequency) {
  year <- as.integer(format(date, "%Y"))
  month <- as.integer(format(date, "%m"))
  switch(frequency,
    daily = as.numeric(date),
    monthly = year * 12L + month,
    quarterly = year * 4L + (month - 1L) %/% 3L,
    annual = year,
    rep(NA_real_, length(date))
  )
}

.bch_plot_date <- function(data) {
  out <- rep(as.Date(NA), nrow(data))
  candidates <- intersect(c("period_start", "reference_date", "date"), names(data))
  for (column in candidates) {
    value <- data[[column]]
    if (inherits(value, "POSIXt")) value <- as.Date(value, tz = "UTC")
    if (!inherits(value, "Date")) {
      bch_abort("Plot dates must be Date or POSIXt vectors; parse text dates explicitly first.",
                "invalid_date")
    }
    fill <- is.na(out) & !is.na(value)
    out[fill] <- value[fill]
    if (!anyNA(out)) break
  }
  if (!length(candidates) || anyNA(out)) {
    bch_abort("Plot data need non-missing `period_start`, `reference_date` or `date` values.",
              "invalid_date")
  }
  out
}

.bch_plot_metadata <- function(data, metadata, field, default) {
  value <- if (field %in% names(data)) data[[field]] else rep(NA_character_, nrow(data))
  if (!is.atomic(value) || !is.null(dim(value))) {
    bch_abort(sprintf("`%s` must be an atomic vector.", field), "invalid_argument")
  }
  value <- as.character(value)
  missing <- is.na(value) | !nzchar(value)
  if (is.data.frame(metadata) && field %in% names(metadata)) {
    matched <- match(as.character(data$indicator_id), as.character(metadata$indicator_id))
    value[missing] <- as.character(metadata[[field]][matched[missing]])
  }
  value[is.na(value) | !nzchar(value)] <- default
  value
}

#' Plot observed indicator series without implicit transformations
#'
#' Uses supplied long-form observations only; this function never downloads
#' data. Annual, quarterly, monthly and daily series are split at absent
#' periods and missing values. Series with unknown frequency are shown as
#' points because continuity cannot be established. Different units use
#' separate panels. Metadata can supply descriptions, frequency and units.
#' A colour-vision-friendly viridis palette and angled date labels are used
#' by default. The returned plot can be further customized with `ggplot2`.
#'
#' @param x A long-form `bch_result` or data frame with `indicator_id`, numeric
#'   `value` and Date columns `period_start`, `reference_date` or `date`.
#' @param id Optional indicator ID or vector of IDs to select.
#' @param title,subtitle Optional plain-text plot labels.
#' @param caption Optional text placed before the source and retrieval date.
#'   Attribution is always retained; data frames without provenance are
#'   labelled as user-provided data.
#' @param mode Currently only `"level"`: display values exactly as supplied,
#'   with no growth-rate calculation, aggregation or interpolation.
#' @param ... Reserved for future use. Must be empty.
#' @return A `ggplot2` plot. The plot's data include explicit segment IDs so
#'   gaps remain visible. No changes are made to the input.
#' @export
bch_plot_series <- function(
    x,
    id = NULL,
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    mode = "level",
    ...) {
  .bch_presentation_dependency("ggplot2", "bch_plot_series")
  if (!identical(mode, "level") || length(list(...))) {
    bch_abort("Only `mode = 'level'` and empty `...` are supported.", "invalid_argument")
  }
  for (argument in c("title", "subtitle", "caption")) {
    .bch_presentation_text(get(argument), argument, allow_null = TRUE)
  }
  if (inherits(x, "bch_result")) validate_bch_result(x)
  provenance <- .bch_presentation_source(x)
  metadata <- if (inherits(x, "bch_result")) x$metadata else NULL
  data <- if (inherits(x, "bch_result")) x$data else x
  if (!is.data.frame(data) || !all(c("indicator_id", "value") %in% names(data))) {
    bch_abort("`x` must contain long-form `indicator_id` and `value` columns.", "invalid_argument")
  }
  if (!is.atomic(data$indicator_id) || anyNA(data$indicator_id) ||
      any(!nzchar(as.character(data$indicator_id)))) {
    bch_abort("`indicator_id` must contain non-missing IDs.", "invalid_argument")
  }
  data$indicator_id <- as.character(data$indicator_id)
  if (!is.null(id)) {
    if (!is.atomic(id) || !length(id) || anyNA(id)) {
      bch_abort("`id` must be a non-empty vector of IDs.", "invalid_argument")
    }
    id <- unique(as.character(id))
    if (any(!id %in% data$indicator_id)) {
      bch_abort("One or more selected IDs are absent from `x`.", "unknown_indicator")
    }
    data <- data[data$indicator_id %in% id, , drop = FALSE]
  }
  if (!nrow(data) || !is.numeric(data$value) || any(is.infinite(data$value)) ||
      all(is.na(data$value))) {
    bch_abort("Plot data need at least one finite numeric value; infinite values are unsupported.",
              "invalid_argument")
  }
  data$.bch_date <- .bch_plot_date(data)
  data$.bch_frequency <- .bch_plot_metadata(data, metadata, "frequency", "unknown")
  data$.bch_unit <- .bch_plot_metadata(data, metadata, "unit", "Unidad no especificada")
  data$.bch_description <- .bch_plot_metadata(data, metadata, "indicator_description", "")
  allowed <- c("daily", "monthly", "quarterly", "annual", "unknown")
  if (any(!data$.bch_frequency %in% allowed)) {
    bch_abort("Use canonical daily/monthly/quarterly/annual/unknown frequency labels.",
              "unsupported_frequency")
  }
  data <- data[order(data$indicator_id, data$.bch_date), , drop = FALSE]
  data$.bch_segment <- NA_character_
  data$.bch_label <- NA_character_
  indices <- split(seq_len(nrow(data)), data$indicator_id)
  for (rows in indices) {
    frequencies <- unique(data$.bch_frequency[rows])
    units <- unique(data$.bch_unit[rows])
    if (length(frequencies) != 1L || length(units) != 1L) {
      bch_abort("Each indicator must have one consistent frequency and unit.", "invalid_argument")
    }
    dates <- data$.bch_date[rows]
    index <- .bch_plot_period_index(dates, frequencies)
    unknown <- identical(frequencies, "unknown")
    duplicate_key <- if (unknown) as.numeric(dates) else index
    if (anyDuplicated(duplicate_key)) {
      bch_abort("An indicator has multiple observations in the same plotting period.",
                "conflicting_duplicate")
    }
    values <- data$value[rows]
    gap <- if (unknown) rep(TRUE, length(rows)) else
      c(TRUE, diff(index) != 1 | is.na(values[-length(values)]) | is.na(values[-1L]))
    data$.bch_segment[rows] <- paste(data$indicator_id[rows], cumsum(gap), sep = ":")
    descriptions <- unique(data$.bch_description[rows])
    descriptions <- descriptions[nzchar(descriptions)]
    label <- if (length(descriptions)) descriptions[[1L]] else data$indicator_id[rows[[1L]]]
    data$.bch_label[rows] <- paste0(label, " [", data$indicator_id[rows[[1L]]], "]")
  }
  finite <- !is.na(data$value)
  segment_size <- table(data$.bch_segment[finite])
  line_data <- data[finite & data$.bch_segment %in% names(segment_size)[segment_size >= 2L], , drop = FALSE]
  if (is.null(title)) title <- "Evolucion de indicadores"
  if (is.null(subtitle)) {
    subtitle <- paste("Valores originales. Frecuencia:",
      paste(sort(unique(data$.bch_frequency)), collapse = ", "))
  }
  caption <- paste(c(caption, provenance$source, provenance$retrieved), collapse = "\n")

  # Local bindings keep NSE symbols explicit for package checks.
  .bch_date <- .bch_label <- .bch_segment <- .bch_unit <- value <- NULL
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = .bch_date, y = value, colour = .bch_label)) +
    ggplot2::geom_line(data = line_data, ggplot2::aes(group = .bch_segment), linewidth = 0.65) +
    ggplot2::geom_point(size = 1.7, na.rm = TRUE) +
    ggplot2::scale_colour_viridis_d(begin = 0.1, end = 0.8) +
    ggplot2::labs(title = title, subtitle = subtitle, x = NULL,
      y = if (length(unique(data$.bch_unit)) == 1L) unique(data$.bch_unit) else "Valor",
      colour = "Indicador", caption = caption) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom", plot.caption = ggplot2::element_text(hjust = 0),
                   plot.margin = ggplot2::margin(t = 12, r = 22, b = 12, l = 14),
                   axis.text.x = ggplot2::element_text(angle = 30, hjust = 1, vjust = 1,
                     margin = ggplot2::margin(t = 5)),
                   panel.grid.minor = ggplot2::element_blank())
  if (length(unique(data$.bch_unit)) > 1L) {
    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(.bch_unit), scales = "free_y")
  }
  plot
}
