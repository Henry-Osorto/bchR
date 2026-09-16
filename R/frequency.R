# Frequency and time parsing -------------------------------------------------

.bch_verified_frequency_map <- c(
  "Diario" = "daily"
)

#' Normalize verified BCH frequency labels
#'
#' Only labels whose spelling and semantics have been verified against an
#' official BCH response belong in `.bch_verified_frequency_map`. Unknown
#' labels can either abort or be retained as the canonical value `"unknown"`.
#'
#' @param x A character vector containing unmodified `Periodicidad` values.
#' @param unknown Either `"error"` or `"keep"`.
#' @return A character vector with the same length as `x`.
#' @keywords internal
.bch_normalize_frequency <- function(x, unknown = c("error", "keep")) {
  unknown <- match.arg(unknown)

  if (!is.character(x)) {
    bch_abort(
      "`x` must be a character vector of BCH frequency labels.",
      "invalid_frequency"
    )
  }

  if (length(x) == 0L) {
    return(character())
  }

  out <- unname(.bch_verified_frequency_map[x])
  is_unknown <- is.na(x) | !nzchar(x) | is.na(out)

  if (any(is_unknown) && identical(unknown, "error")) {
    labels <- unique(x[is_unknown])
    labels[is.na(labels)] <- "<NA>"
    labels[!nzchar(labels)] <- "<empty>"

    bch_abort(
      paste0(
        "Unverified BCH frequency label(s): ",
        paste(sprintf("'%s'", labels), collapse = ", "),
        ". Use `unknown = \"keep\"` only when raw dates are sufficient."
      ),
      "unknown_frequency",
      frequency_raw = labels
    )
  }

  out[is_unknown] <- "unknown"
  names(out) <- names(x)
  out
}

#' Parse BCH timestamps without local-time conversion
#'
#' @param x A character vector in the verified ISO 8601 UTC representation,
#'   for example `"2024-03-09T00:00:00Z"`.
#' @return A `POSIXct` vector with time zone `UTC`.
#' @keywords internal
.bch_parse_datetime_utc <- function(x) {
  if (!is.character(x)) {
    bch_abort(
      "BCH `Fecha` values must be character strings.",
      "invalid_date"
    )
  }

  if (length(x) == 0L) {
    return(as.POSIXct(character(), tz = "UTC"))
  }

  iso_utc <- paste0(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}T",
    "(?:[01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\\.[0-9]+)?Z$"
  )
  has_valid_shape <- !is.na(x) & grepl(iso_utc, x, perl = TRUE)

  parsed <- rep(as.POSIXct(NA, tz = "UTC"), length(x))
  if (any(has_valid_shape)) {
    parsed[has_valid_shape] <- as.POSIXct(
      strptime(
        x[has_valid_shape],
        format = "%Y-%m-%dT%H:%M:%OSZ",
        tz = "UTC"
      ),
      tz = "UTC"
    )
  }
  attr(parsed, "tzone") <- "UTC"

  round_trip <- format(parsed, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  invalid <- !has_valid_shape | is.na(parsed) |
    (!is.na(parsed) & round_trip != substr(x, 1L, 19L))
  if (any(invalid)) {
    bad <- unique(x[invalid])
    bad[is.na(bad)] <- "<NA>"

    bch_abort(
      paste0(
        "Invalid BCH `Fecha` value(s). Expected ISO 8601 UTC such as ",
        "'2024-03-09T00:00:00Z': ",
        paste(sprintf("'%s'", bad), collapse = ", "),
        "."
      ),
      "invalid_date",
      values = bad,
      locations = which(invalid)
    )
  }

  parsed
}

#' Build period fields for verified frequencies
#'
#' @param date A `Date` or `POSIXt` vector.
#' @param frequency A canonical frequency vector. Currently only `"daily"`
#'   has a verified period rule; `"unknown"` produces missing period fields.
#' @return A tibble with `period_start`, `period_end`, and `period_label`.
#' @keywords internal
.bch_period_fields <- function(date, frequency) {
  if (inherits(date, "POSIXt")) {
    date <- as.Date(date, tz = "UTC")
  }
  if (!inherits(date, "Date")) {
    bch_abort(
      "`date` must inherit from `Date` or `POSIXt`.",
      "invalid_date"
    )
  }
  if (anyNA(date)) {
    bch_abort(
      "`date` cannot contain missing values.",
      "invalid_date"
    )
  }

  n <- length(date)
  if (length(frequency) == 1L && n != 1L) {
    frequency <- rep(frequency, n)
  }
  if (!is.character(frequency) || length(frequency) != n) {
    bch_abort(
      "`frequency` must be a character vector of length 1 or `length(date)`.",
      "invalid_frequency"
    )
  }

  unsupported <- !is.na(frequency) &
    !frequency %in% c("daily", "unknown")
  if (any(unsupported)) {
    bch_abort(
      paste0(
        "Period rules are not yet verified for: ",
        paste(unique(frequency[unsupported]), collapse = ", "),
        "."
      ),
      "unsupported_frequency",
      frequency = unique(frequency[unsupported])
    )
  }

  period_start <- as.Date(rep(NA_character_, n))
  period_end <- as.Date(rep(NA_character_, n))
  period_label <- rep(NA_character_, n)

  is_daily <- !is.na(frequency) & frequency == "daily"
  period_start[is_daily] <- date[is_daily]
  period_end[is_daily] <- date[is_daily]
  period_label[is_daily] <- format(date[is_daily], "%Y-%m-%d")

  tibble::tibble(
    period_start = period_start,
    period_end = period_end,
    period_label = period_label
  )
}
