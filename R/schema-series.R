# Series schema --------------------------------------------------------------

.bch_value_scalar <- function(x, row) {
  if (is.null(x) || (length(x) == 1L && is.atomic(x) && is.na(x))) {
    return(list(raw = NA_character_, value = NA_real_))
  }

  if (is.numeric(x) && length(x) == 1L && is.finite(x)) {
    return(list(
      raw = format(x, scientific = FALSE, trim = TRUE, digits = 17L),
      value = as.double(x)
    ))
  }

  decimal <- paste0(
    "^[+-]?(?:",
    "[0-9]+(?:\\.[0-9]*)?",
    "|\\.[0-9]+",
    ")(?:[eE][+-]?[0-9]+)?$"
  )

  if (is.character(x) &&
      length(x) == 1L &&
      !is.na(x) &&
      grepl(decimal, x, perl = TRUE)) {
    value <- suppressWarnings(as.numeric(x))
    if (is.finite(value)) {
      return(list(raw = x, value = value))
    }
  }

  bch_abort(
    sprintf(
      "BCH series row %d field `Valor` is not a finite number using the documented decimal grammar.",
      row
    ),
    "invalid_value",
    resource = "series",
    row = row,
    field = "Valor"
  )
}

.bch_series_frequency_labels <- function(x, frequency_raw) {
  n <- length(x)
  supplied <- NULL
  if (!is.null(frequency_raw)) {
    if (!is.character(frequency_raw) ||
        !length(frequency_raw) %in% c(1L, n)) {
      bch_abort(
        "`frequency_raw` must be NULL or a character vector of length 1 or the number of observations.",
        "invalid_frequency"
      )
    }
    supplied <- rep(frequency_raw, length.out = n)
  }

  has_frequency <- vapply(
    x,
    function(record) "Periodicidad" %in% names(record),
    logical(1)
  )

  if (!any(has_frequency)) {
    return(if (is.null(supplied)) rep(NA_character_, n) else supplied)
  }
  if (!all(has_frequency)) {
    bch_abort(
      "`Periodicidad` is present in only part of the BCH series payload.",
      "schema_error",
      resource = "series",
      rows = which(!has_frequency),
      missing_fields = "Periodicidad"
    )
  }

  observed <- vapply(seq_along(x), function(i) {
    .bch_text_scalar(
      x[[i]]$Periodicidad,
      "Periodicidad",
      "series",
      i
    )
  }, character(1))

  if (!is.null(supplied)) {
    mismatch <- is.na(supplied) | supplied != observed
    if (any(mismatch)) {
      bch_abort(
        "The series `Periodicidad` disagrees with the supplied catalog frequency.",
        "frequency_mismatch", resource = "series", rows = which(mismatch),
        frequency_raw = observed[mismatch],
        catalog_frequency_raw = supplied[mismatch]
      )
    }
    return(supplied)
  }
  observed
}

.bch_series_empty <- function() {
  tibble::tibble(
    observation_id = character(),
    indicator_id = character(),
    indicator_code = character(),
    indicator_description = character(),
    date_raw = character(),
    reference_datetime = as.POSIXct(character(), tz = "UTC"),
    reference_date = as.Date(character()),
    frequency_raw = character(),
    frequency = character(),
    period_start = as.Date(character()),
    period_end = as.Date(character()),
    period_label = character(),
    value_raw = character(),
    value = double(),
    missing_code = character(),
    is_transformed = logical(),
    extra = list()
  )
}

#' Parse observations returned by a BCH series endpoint
#'
#' Validates a list produced from a JSON array, parses `Fecha` as UTC, parses
#' `Valor` with an explicit decimal grammar, preserves raw representations and
#' extra fields, and enforces one non-conflicting value per indicator-period.
#'
#' @param x A list of named lists, one per observation.
#' @param expected_id Optional requested indicator ID used for consistency
#'   validation.
#' @param frequency_raw Optional unmodified BCH frequency label, of length one
#'   or the number of observations. If omitted, `Periodicidad` must be present
#'   in every observation for strict frequency processing. If both sources
#'   are present, their raw labels must match exactly; discrepancies abort.
#' @param unknown_frequency Whether an unverified frequency should abort or be
#'   retained as `"unknown"` with missing period boundaries.
#' @return A chronologically ordered tibble with canonical and raw fields.
#' @keywords internal
.bch_parse_series <- function(
    x,
    expected_id = NULL,
    frequency_raw = NULL,
    unknown_frequency = c("error", "keep")) {
  unknown_frequency <- match.arg(unknown_frequency)
  .bch_validate_record_array(x, "series")

  if (length(x) == 0L) {
    return(.bch_series_empty())
  }

  required <- c(
    "Id",
    "IndicadorId",
    "Nombre",
    "Descripcion",
    "Fecha",
    "Valor"
  )
  known <- c(required, "Periodicidad")
  frequency_labels <- .bch_series_frequency_labels(x, frequency_raw)
  frequency <- .bch_normalize_frequency(
    frequency_labels,
    unknown = unknown_frequency
  )

  expected_id_parsed <- NULL
  if (!is.null(expected_id)) {
    expected_id_parsed <- .bch_id_scalar(
      expected_id,
      "expected_id",
      "request",
      1L
    )
  }

  rows <- lapply(seq_along(x), function(i) {
    record <- x[[i]]
    .bch_require_fields(record, required, "series", i)

    indicator_id <- .bch_id_scalar(
      record$IndicadorId,
      "IndicadorId",
      "series",
      i
    )
    if (!is.null(expected_id_parsed) &&
        !identical(indicator_id, expected_id_parsed)) {
      bch_abort(
        sprintf(
          "BCH series row %d belongs to indicator '%s', not requested indicator '%s'.",
          i,
          indicator_id,
          expected_id_parsed
        ),
        "indicator_mismatch",
        resource = "series",
        row = i,
        indicator_id = indicator_id,
        expected_id = expected_id_parsed
      )
    }

    date_raw <- .bch_text_scalar(
      record$Fecha,
      "Fecha",
      "series",
      i
    )
    value <- .bch_value_scalar(record$Valor, i)

    list(
      observation_id = .bch_id_scalar(record$Id, "Id", "series", i),
      indicator_id = indicator_id,
      indicator_code = .bch_text_scalar(
        record$Nombre,
        "Nombre",
        "series",
        i
      ),
      indicator_description = .bch_text_scalar(
        record$Descripcion,
        "Descripcion",
        "series",
        i,
        allow_missing = TRUE
      ),
      date_raw = date_raw,
      reference_datetime = .bch_parse_datetime_utc(date_raw),
      frequency_raw = frequency_labels[[i]],
      frequency = frequency[[i]],
      value_raw = value$raw,
      value = value$value,
      missing_code = NA_character_,
      is_transformed = FALSE,
      extra = .bch_extra_fields(record, known)
    )
  })

  reference_datetime <- as.POSIXct(
    vapply(rows, function(row) unclass(row$reference_datetime), numeric(1)),
    origin = "1970-01-01",
    tz = "UTC"
  )
  reference_date <- as.Date(reference_datetime, tz = "UTC")
  frequency <- vapply(rows, `[[`, character(1), "frequency")
  periods <- .bch_period_fields(reference_date, frequency)

  data <- tibble::tibble(
    observation_id = vapply(rows, `[[`, character(1), "observation_id"),
    indicator_id = vapply(rows, `[[`, character(1), "indicator_id"),
    indicator_code = vapply(rows, `[[`, character(1), "indicator_code"),
    indicator_description = vapply(
      rows,
      `[[`,
      character(1),
      "indicator_description"
    ),
    date_raw = vapply(rows, `[[`, character(1), "date_raw"),
    reference_datetime = reference_datetime,
    reference_date = reference_date,
    frequency_raw = vapply(rows, `[[`, character(1), "frequency_raw"),
    frequency = frequency,
    period_start = periods$period_start,
    period_end = periods$period_end,
    period_label = periods$period_label,
    value_raw = vapply(rows, `[[`, character(1), "value_raw"),
    value = vapply(rows, `[[`, numeric(1), "value"),
    missing_code = vapply(rows, `[[`, character(1), "missing_code"),
    is_transformed = vapply(rows, `[[`, logical(1), "is_transformed"),
    extra = lapply(rows, `[[`, "extra")
  )

  # Build exact timestamp keys from verified ISO text, not a formatted POSIXct:
  # formatting can discard fractional seconds according to `digits.secs`, and
  # double precision cannot distinguish every fractional timestamp. Trailing
  # fractional zeroes are immaterial, so .1Z and .100Z denote the same instant.
  timestamp_key <- data$date_raw
  fractional <- grepl("\\.[0-9]+Z$", timestamp_key)
  timestamp_key[fractional] <- sub("0+Z$", "Z", timestamp_key[fractional])
  timestamp_key[fractional] <- sub("\\.Z$", "Z", timestamp_key[fractional])
  known_period <- data$frequency != "unknown"
  period_key <- ifelse(
    known_period,
    as.character(data$period_start),
    timestamp_key
  )
  key <- paste(data$indicator_id, period_key, sep = "\r")
  duplicate_keys <- unique(key[duplicated(key)])
  keep <- rep(TRUE, nrow(data))

  comparison_fields <- setdiff(names(data), "observation_id")
  comparison <- data[comparison_fields]
  comparison$date_raw <- timestamp_key
  for (duplicate_key in duplicate_keys) {
    locations <- which(key == duplicate_key)
    reference <- as.list(comparison[locations[[1L]], , drop = FALSE])
    same <- vapply(
      locations[-1L],
      function(j) {
        identical(reference, as.list(comparison[j, , drop = FALSE]))
      },
      logical(1)
    )

    if (any(!same)) {
      bch_abort(
        sprintf(
          "Conflicting observations were returned for indicator '%s' and period '%s'.",
          data$indicator_id[locations[[1L]]],
          period_key[locations[[1L]]]
        ),
        "conflicting_duplicate",
        resource = "series",
        indicator_id = data$indicator_id[locations[[1L]]],
        period = period_key[locations[[1L]]],
        rows = locations
      )
    }

    keep[locations[-1L]] <- FALSE
  }

  if (any(!keep)) {
    .bch_warn_duplicate_collapsed(
      "series",
      ids = unique(data$indicator_id[!keep]),
      n_removed = sum(!keep)
    )
    data <- data[keep, , drop = FALSE]
    timestamp_key <- timestamp_key[keep]
  }

  # Equal-width ISO fractions also give exact chronological ordering when
  # distinct raw instants round to the same POSIXct double.
  fraction_width <- pmax(0L, nchar(timestamp_key) - 21L)
  timestamp_order <- paste0(
    sub("Z$", "", timestamp_key), ifelse(fraction_width == 0L, ".", ""),
    strrep("0", max(fraction_width) - fraction_width)
  )
  order_rows <- order(
    data$indicator_id,
    timestamp_order,
    data$observation_id,
    method = "radix"
  )
  data[order_rows, , drop = FALSE]
}
