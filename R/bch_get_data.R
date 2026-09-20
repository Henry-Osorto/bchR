# BCH indicator-data retrieval -----------------------------------------------

# Standard fields observed in the BCH indicator-data endpoint.
.bch_indicator_data_columns <- c(
  "Id",
  "IndicadorId",
  "Nombre",
  "Descripcion",
  "Fecha",
  "Valor"
)


#' Signal an internal BCH data-processing error
#'
#' @param message User-facing message.
#' @param subclass Specific bchR data condition subclass.
#'
#' @return This function does not return; it signals an error.
#' @noRd
.bch_data_abort <- function(message, subclass) {
  classes <- unique(c(subclass, "bchR_data_error", "error", "condition"))

  condition <- structure(
    list(
      message = message,
      call = NULL
    ),
    class = classes
  )

  stop(condition)
}


#' Signal an internal BCH data warning
#'
#' @param message User-facing message.
#' @param subclass Specific warning subclass.
#'
#' @return `NULL`, invisibly, after signaling a warning.
#' @noRd
.bch_data_warn <- function(message, subclass = "bchR_data_warning") {
  classes <- unique(c(subclass, "bchR_data_warning", "warning", "condition"))

  condition <- structure(
    list(
      message = message,
      call = NULL
    ),
    class = classes
  )

  warning(condition)
  invisible(NULL)
}


#' Validate and canonicalize a BCH indicator ID
#'
#' Numeric IDs must be positive, finite whole numbers representable as an R
#' integer. Character IDs may contain surrounding whitespace and leading zeros,
#' but otherwise must consist only of decimal digits and represent a positive
#' integer. The canonical return value is character so it can be appended to an
#' API path without numeric formatting ambiguity.
#'
#' @param indicator_id Numeric or character BCH indicator ID.
#'
#' @return Canonical positive integer ID as a character scalar.
#' @noRd
.bch_validate_indicator_id <- function(indicator_id) {
  invalid <- function() {
    stop(
      paste0(
        "indicator_id must be a single positive whole-number BCH indicator ID, ",
        "supplied as numeric or character."
      ),
      call. = FALSE
    )
  }

  if (length(indicator_id) != 1L || is.na(indicator_id)) {
    invalid()
  }

  if (is.numeric(indicator_id) && !is.logical(indicator_id)) {
    if (
      !is.finite(indicator_id) ||
        indicator_id <= 0 ||
        indicator_id != floor(indicator_id) ||
        indicator_id > .Machine$integer.max
    ) {
      invalid()
    }

    return(as.character(as.integer(indicator_id)))
  }

  if (is.character(indicator_id)) {
    indicator_id <- trimws(indicator_id)

    if (!nzchar(indicator_id) || !grepl("^[0-9]+$", indicator_id)) {
      invalid()
    }

    # Remove leading zeros without converting through floating point first.
    canonical <- sub("^0+", "", indicator_id)
    if (!nzchar(canonical)) {
      invalid()
    }

    # IDs currently fit comfortably in an R integer. Keeping this explicit
    # protects against accidental path values that cannot represent an ID.
    numeric_id <- suppressWarnings(as.numeric(canonical))
    if (
      is.na(numeric_id) ||
        !is.finite(numeric_id) ||
        numeric_id > .Machine$integer.max
    ) {
      invalid()
    }

    return(as.character(as.integer(numeric_id)))
  }

  invalid()
}


#' Convert parsed BCH indicator data into a data frame
#'
#' Supports the tabular result normally produced by `httr2`/`jsonlite` as well
#' as an unsimplified JSON array represented as a list of records. Nested or
#' non-scalar record fields are rejected rather than flattened silently.
#'
#' @param x Parsed BCH response.
#'
#' @return Base `data.frame`.
#' @noRd
.bch_data_as_data_frame <- function(x) {
  if (is.data.frame(x)) {
    return(
      as.data.frame(
        x,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    )
  }

  if (is.matrix(x)) {
    return(
      as.data.frame(
        x,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    )
  }

  if (is.list(x) && length(x) == 0L) {
    return(data.frame())
  }

  # Named equal-length vectors: column-oriented JSON simplification.
  if (
    is.list(x) &&
      !is.null(names(x)) &&
      length(names(x)) == length(x) &&
      all(!is.na(names(x))) &&
      all(nzchar(names(x)))
  ) {
    out <- tryCatch(
      as.data.frame(
        x,
        stringsAsFactors = FALSE,
        check.names = FALSE
      ),
      error = function(e) NULL
    )

    if (!is.null(out)) {
      return(out)
    }
  }

  # Unsimplified JSON array: list of scalar records.
  if (
    is.list(x) &&
      length(x) > 0L &&
      all(vapply(x, is.list, logical(1)))
  ) {
    record_names <- lapply(x, names)

    if (
      all(vapply(record_names, function(nm) !is.null(nm), logical(1))) &&
        all(vapply(record_names, function(nm) all(nzchar(nm)), logical(1)))
    ) {
      all_names <- unique(unlist(record_names, use.names = FALSE))

      rows <- lapply(
        x,
        function(record) {
          row <- setNames(vector("list", length(all_names)), all_names)

          for (nm in all_names) {
            value <- record[[nm]]

            if (is.null(value) || length(value) == 0L) {
              row[[nm]] <- NA
              next
            }

            if (!is.atomic(value) || length(value) != 1L) {
              .bch_data_abort(
                paste0(
                  "The BCH indicator-data response contains a nested or ",
                  "non-scalar field and cannot be represented safely as a data frame."
                ),
                "bchR_data_conversion_error"
              )
            }

            row[[nm]] <- value
          }

          as.data.frame(
            row,
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
        }
      )

      out <- tryCatch(
        do.call(rbind, rows),
        error = function(e) NULL
      )

      if (!is.null(out)) {
        rownames(out) <- NULL
        return(out)
      }
    }
  }

  .bch_data_abort(
    "The BCH indicator-data response could not be converted to a data frame.",
    "bchR_data_conversion_error"
  )
}


#' Validate the observed BCH indicator-data schema
#'
#' Missing standard columns are treated as a breaking schema change. Additional
#' columns are preserved and reported so changes in the upstream API are not
#' hidden from users.
#'
#' @param data Data frame returned by `.bch_data_as_data_frame()`.
#'
#' @return `data` with standard fields first and extra fields afterwards.
#' @noRd
.bch_validate_indicator_data_schema <- function(data) {
  if (!is.data.frame(data)) {
    .bch_data_abort(
      "The BCH indicator-data response is not a data frame.",
      "bchR_data_conversion_error"
    )
  }

  if (anyDuplicated(names(data))) {
    duplicated_names <- unique(names(data)[duplicated(names(data))])

    .bch_data_abort(
      paste0(
        "The BCH indicator-data response contains duplicated column names: ",
        paste(duplicated_names, collapse = ", "),
        ". The BCH API schema may have changed."
      ),
      "bchR_data_schema_error"
    )
  }

  missing_columns <- setdiff(
    .bch_indicator_data_columns,
    names(data)
  )

  extra_columns <- setdiff(
    names(data),
    .bch_indicator_data_columns
  )

  if (length(missing_columns) > 0L) {
    observed <- if (length(names(data)) > 0L) {
      paste(names(data), collapse = ", ")
    } else {
      "<none>"
    }

    .bch_data_abort(
      paste0(
        "The BCH indicator-data schema does not match the structure expected ",
        "by bchR. Missing column(s): ",
        paste(missing_columns, collapse = ", "),
        ". Observed column(s): ",
        observed,
        ". The BCH API schema may have changed."
      ),
      "bchR_data_schema_error"
    )
  }

  if (length(extra_columns) > 0L) {
    .bch_data_warn(
      paste0(
        "The BCH indicator-data response contains new/unexpected column(s): ",
        paste(extra_columns, collapse = ", "),
        ". They were preserved after the standard bchR data columns."
      ),
      "bchR_data_schema_warning"
    )
  }

  data[
    c(
      .bch_indicator_data_columns,
      extra_columns
    )
  ]
}


#' Trim character metadata without changing official content
#'
#' @param data Validated BCH indicator data.
#'
#' @return Data frame with leading/trailing whitespace removed from character
#'   fields. Missing values are preserved.
#' @noRd
.bch_trim_indicator_data <- function(data) {
  factor_columns <- vapply(data, is.factor, logical(1))
  if (any(factor_columns)) {
    data[factor_columns] <- lapply(data[factor_columns], as.character)
  }

  character_columns <- vapply(data, is.character, logical(1))
  if (any(character_columns)) {
    data[character_columns] <- lapply(
      data[character_columns],
      function(x) {
        out <- trimws(x)
        out[is.na(x)] <- NA_character_
        out
      }
    )
  }

  data
}


#' Validate the indicator identity returned by the API
#'
#' @param x Returned `IndicadorId` vector.
#' @param requested_id Canonical requested indicator ID.
#'
#' @return `TRUE`, invisibly.
#' @noRd
.bch_validate_returned_indicator_id <- function(x, requested_id) {
  non_missing <- !is.na(x)

  if (!any(non_missing)) {
    return(invisible(TRUE))
  }

  observed <- vapply(
    x[non_missing],
    function(value) {
      tryCatch(
        .bch_validate_indicator_id(value),
        error = function(e) NA_character_
      )
    },
    character(1)
  )

  if (anyNA(observed)) {
    .bch_data_abort(
      paste0(
        "The BCH Web API returned an invalid IndicadorId value. ",
        "The upstream response may have changed."
      ),
      "bchR_data_indicator_mismatch_error"
    )
  }

  unexpected <- unique(observed[observed != requested_id])

  if (length(unexpected) > 0L) {
    .bch_data_abort(
      paste0(
        "The BCH Web API returned data for an indicator different from the ",
        "requested indicator ", requested_id, "."
      ),
      "bchR_data_indicator_mismatch_error"
    )
  }

  invisible(TRUE)
}


#' Parse BCH dates conservatively
#'
#' The observed API uses ISO date-time strings such as
#' `YYYY-MM-DDT00:00:00`. The calendar-date component is retained. Missing
#' source values remain missing; malformed non-missing values are errors.
#'
#' @param x BCH `Fecha` field.
#'
#' @return A `Date` vector.
#' @noRd
.bch_parse_indicator_dates <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }

  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }

  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (!is.character(x)) {
    .bch_data_abort(
      "The BCH Fecha field is not in a supported date format.",
      "bchR_data_date_error"
    )
  }

  source_na <- is.na(x)
  x <- trimws(x)
  non_missing <- !source_na

  out <- as.Date(rep(NA_character_, length(x)))

  if (!any(non_missing)) {
    return(out)
  }

  iso_like <- grepl(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}($|T|[[:space:]])",
    x[non_missing]
  )

  date_part <- substr(x[non_missing], 1L, 10L)
  parsed <- as.Date(date_part, format = "%Y-%m-%d")

  invalid <- !iso_like | is.na(parsed)

  if (any(invalid)) {
    positions <- which(non_missing)[invalid]
    shown <- paste(utils::head(positions, 5L), collapse = ", ")
    suffix <- if (length(positions) > 5L) ", ..." else ""

    .bch_data_abort(
      paste0(
        "The BCH Fecha field contains malformed non-missing date value(s) ",
        "at row(s): ", shown, suffix, "."
      ),
      "bchR_data_date_error"
    )
  }

  out[non_missing] <- parsed
  out
}


#' Convert BCH values to numeric without silently losing data
#'
#' Missing values are preserved. Character values are accepted only when every
#' non-missing value can be converted unambiguously by R to a finite numeric
#' value. Invalid strings are errors rather than silently becoming `NA`.
#'
#' @param x BCH `Valor` field.
#'
#' @return Numeric vector.
#' @noRd
.bch_parse_indicator_values <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (is.numeric(x) && !is.logical(x)) {
    out <- as.numeric(x)
    invalid <- !is.na(out) & !is.finite(out)

    if (any(invalid)) {
      .bch_data_abort(
        "The BCH Valor field contains non-finite numeric value(s).",
        "bchR_data_value_error"
      )
    }

    return(out)
  }

  if (is.character(x)) {
    source_na <- is.na(x)
    cleaned <- trimws(x)

    parsed <- suppressWarnings(as.numeric(cleaned))
    invalid <- !source_na & (
      !nzchar(cleaned) |
        is.na(parsed) |
        !is.finite(parsed)
    )

    if (any(invalid)) {
      positions <- which(invalid)
      shown <- paste(utils::head(positions, 5L), collapse = ", ")
      suffix <- if (length(positions) > 5L) ", ..." else ""

      .bch_data_abort(
        paste0(
          "The BCH Valor field contains non-numeric non-missing value(s) ",
          "at row(s): ", shown, suffix, "."
        ),
        "bchR_data_value_error"
      )
    }

    parsed[source_na] <- NA_real_
    return(parsed)
  }

  .bch_data_abort(
    "The BCH Valor field is not numeric and cannot be converted safely.",
    "bchR_data_value_error"
  )
}


#' Detect potential duplicate observations without removing them
#'
#' Exact duplicates are evaluated across all returned columns. Substantive
#' duplicates use the core observed BCH fields except the technical observation
#' `Id`: `IndicadorId`, `Nombre`, `Descripcion`, `Fecha`, and `Valor`.
#' Repeated dates are also reported because they matter for downstream
#' time-series analysis, but no assumption is made that every repeated date is
#' necessarily an error.
#'
#' @param data Cleaned and typed BCH indicator data.
#'
#' @return `TRUE` invisibly. Potential duplicates trigger a warning while all
#'   rows remain unchanged.
#' @noRd
.bch_warn_indicator_duplicates <- function(data) {
  if (nrow(data) < 2L) {
    return(invisible(TRUE))
  }

  exact_duplicate <- duplicated(data)

  substantive_columns <- c(
    "IndicadorId",
    "Nombre",
    "Descripcion",
    "Fecha",
    "Valor"
  )

  substantive_duplicate <- duplicated(
    data[substantive_columns]
  )

  substantive_only <- substantive_duplicate & !exact_duplicate

  non_missing_dates <- data$Fecha[!is.na(data$Fecha)]
  repeated_date_count <- 0L

  if (length(non_missing_dates) > 1L) {
    date_counts <- table(non_missing_dates)
    repeated_date_count <- sum(date_counts > 1L)
  }

  exact_count <- sum(exact_duplicate)
  substantive_count <- sum(substantive_only)

  if (
    exact_count > 0L ||
      substantive_count > 0L ||
      repeated_date_count > 0L
  ) {
    .bch_data_warn(
      paste0(
        "Potential duplicate BCH observations were detected: ",
        exact_count, " exact duplicate row(s); ",
        substantive_count,
        " substantive duplicate observation(s) with the same core fields ",
        "but a differing technical/other field; ",
        repeated_date_count, " repeated date(s). ",
        "All ", nrow(data), " row(s) were preserved."
      ),
      "bchR_data_duplicate_warning"
    )
  }

  invisible(TRUE)
}


#' Clean, type, and order BCH indicator data
#'
#' @param data Validated BCH data frame.
#' @param requested_id Canonical requested indicator ID.
#'
#' @return Cleaned base data frame ordered chronologically by `Fecha`. Missing
#'   dates, if present in the source, are retained at the end.
#' @noRd
.bch_clean_indicator_data <- function(data, requested_id) {
  data <- .bch_trim_indicator_data(data)

  .bch_validate_returned_indicator_id(
    data$IndicadorId,
    requested_id
  )

  data$Fecha <- .bch_parse_indicator_dates(data$Fecha)
  data$Valor <- .bch_parse_indicator_values(data$Valor)

  # Stable chronological ordering. The original relative order is preserved
  # for rows sharing the same date; source-missing dates remain at the end.
  original_order <- seq_len(nrow(data))
  order_index <- order(
    data$Fecha,
    original_order,
    na.last = TRUE
  )

  data <- data[order_index, , drop = FALSE]
  rownames(data) <- NULL

  .bch_warn_indicator_duplicates(data)

  data
}


#' Retrieve data for a BCH indicator
#'
#' @description
#' Downloads all observations currently returned by the Banco Central de
#' Honduras (BCH) Web API for a single indicator and performs only the minimal
#' transformations needed to provide a consistent analysis-ready data frame.
#'
#' `bch_get_data()` delegates authentication, URL construction, retries, HTTP
#' validation, and JSON parsing to the package's internal HTTP client. Users do
#' not pass `BCH_API_KEY` directly. Configure it first with
#' [bch_set_api_key()].
#'
#' @param indicator_id A single positive BCH indicator ID, supplied as a
#'   numeric whole number or a character string containing decimal digits.
#'   Surrounding whitespace and leading zeros in character IDs are accepted and
#'   canonicalized.
#' @param progress Logical. If `TRUE` (default), download and processing
#'   messages are displayed. Must be a single non-missing logical value.
#'
#' @return A base `data.frame` containing, at minimum, the standard BCH fields
#'   `Id`, `IndicadorId`, `Nombre`, `Descripcion`, `Fecha`, and `Valor`.
#'   `Fecha` is returned as `Date`, `Valor` as numeric, and observations are
#'   ordered chronologically in ascending order. Additional fields introduced
#'   by the BCH API are preserved after the standard fields and trigger a schema
#'   warning.
#'
#'   The result includes secret-free provenance attributes:
#'   `retrieved_at`, `package_version`, `indicator_id`, and `api_endpoint`.
#'
#' @details
#' Character fields are trimmed only at their leading and trailing boundaries;
#' official BCH labels and descriptions are otherwise unchanged. Missing values
#' supplied by the API are preserved.
#'
#' The function does **not** filter dates, truncate the sample, interpolate
#' missing observations, calculate lags or transformations, infer a time-series
#' frequency, convert the result to `ts`, or fit statistical models.
#'
#' Potential duplicate observations are never removed silently. Exact duplicate
#' rows, substantively duplicated observations that differ only in a technical
#' or other field, and repeated dates trigger a warning; every upstream row is
#' preserved. This policy is deliberate because a repeated date alone is not
#' sufficient evidence that an official observation should be discarded.
#'
#' A missing standard field is treated as a breaking upstream schema change.
#' New additional fields are preserved and reported rather than being silently
#' discarded.
#'
#' @seealso [bch_get_indicators()], [bch_set_api_key()]
#'
#' @examples
#' \dontrun{
#' inflation <- bch_get_data(609)
#'
#' head(inflation)
#' tail(inflation)
#' str(inflation)
#'
#' # Character IDs are also accepted
#' inflation <- bch_get_data("609", progress = FALSE)
#'
#' # Secret-free provenance
#' attr(inflation, "indicator_id")
#' attr(inflation, "retrieved_at")
#' attr(inflation, "api_endpoint")
#' }
#'
#' @export
bch_get_data <- function(indicator_id, progress = TRUE) {
  requested_id <- .bch_validate_indicator_id(indicator_id)
  .bch_validate_flag(progress, "progress")

  if (isTRUE(progress)) {
    message(
      paste0(
        "Downloading BCH indicator ",
        requested_id,
        "..."
      )
    )
  }

  raw_data <- bch_get(
    path = c(
      "indicadores",
      requested_id,
      "cifras"
    ),
    simplify_vector = TRUE
  )

  retrieved_at <- Sys.time()

  if (isTRUE(progress)) {
    message("Processing dates and values...")
  }

  data <- .bch_data_as_data_frame(raw_data)

  if (nrow(data) == 0L) {
    .bch_data_abort(
      paste0(
        "The BCH Web API returned no observations for indicator ",
        requested_id,
        "."
      ),
      "bchR_empty_data_error"
    )
  }

  data <- .bch_validate_indicator_data_schema(data)
  data <- .bch_clean_indicator_data(data, requested_id)

  data <- .bch_add_provenance(
    data,
    path = c(
      "indicadores",
      requested_id,
      "cifras"
    ),
    indicator_id = requested_id,
    retrieved_at = retrieved_at
  )

  if (isTRUE(progress)) {
    message(
      paste0(
        "Finished downloading ",
        .bch_format_count(nrow(data)),
        " observations."
      )
    )
  }

  data
}
