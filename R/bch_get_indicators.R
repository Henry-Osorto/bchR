# BCH indicator catalogue ----------------------------------------------------

.bch_indicator_catalogue_columns <- c(
  "Id",
  "Nombre",
  "Descripcion",
  "Periodicidad",
  "Grupo",
  "CorrelativoGrupo"
)


#' Signal an indicator-catalogue error
#'
#' @param message Secret-free user-facing message.
#' @param subclass Specific `bchR` condition subclass.
#'
#' @return This function does not return; it signals an error.
#' @noRd
.bch_catalogue_abort <- function(message, subclass) {
  condition <- structure(
    list(message = message, call = NULL),
    class = unique(c(subclass, "bchR_catalogue_error", "error", "condition"))
  )

  stop(condition)
}


#' Signal a non-breaking indicator-catalogue schema warning
#'
#' @param message Secret-free user-facing message.
#' @param subclass Specific `bchR` warning subclass.
#'
#' @return `NULL`, invisibly, after signaling a warning.
#' @noRd
.bch_catalogue_warn <- function(message,
                                subclass = "bchR_catalogue_schema_warning") {
  condition <- structure(
    list(message = message, call = NULL),
    class = unique(c(subclass, "bchR_catalogue_warning", "warning", "condition"))
  )

  warning(condition)
  invisible(NULL)
}


#' Convert a parsed BCH catalogue response to a data frame
#'
#' `bch_get()` normally returns a data frame for the current BCH JSON catalogue
#' because JSON vector simplification is enabled. This helper nevertheless
#' accepts a small set of equivalent tabular representations so that a benign
#' parser representation change does not force the public function to duplicate
#' conversion logic.
#'
#' Nested/non-tabular values are rejected rather than silently flattened.
#'
#' @param x Parsed JSON returned by `bch_get()`.
#'
#' @return A base `data.frame` with original column names preserved.
#' @noRd
.bch_catalogue_as_data_frame <- function(x) {
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

  # A named list of equal-length vectors is already column-oriented.
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

  # Also support an unsimplified JSON array represented as a list of records.
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
          row <- vector("list", length(all_names))
          names(row) <- all_names

          for (nm in all_names) {
            value <- record[[nm]]

            if (is.null(value) || length(value) == 0L) {
              row[[nm]] <- NA
              next
            }

            if (!is.atomic(value) || length(value) != 1L) {
              .bch_catalogue_abort(
                paste0(
                  "The BCH indicator catalogue contains a nested or non-scalar ",
                  "field and cannot be represented safely as a data frame."
                ),
                "bchR_catalogue_conversion_error"
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

  .bch_catalogue_abort(
    paste0(
      "The BCH indicator catalogue response could not be converted to a data frame."
    ),
    "bchR_catalogue_conversion_error"
  )
}


#' Validate the observed BCH indicator-catalogue schema
#'
#' Missing standard columns are treated as a breaking schema change. Additional
#' columns are preserved, but a warning is emitted so that upstream API changes
#' are not silently hidden.
#'
#' @param data A data frame returned by `.bch_catalogue_as_data_frame()`.
#'
#' @return `data`, with the six standard BCH fields first and any additional
#'   fields retained afterwards in their original relative order.
#' @noRd
.bch_validate_indicator_catalogue_schema <- function(data) {
  if (!is.data.frame(data)) {
    .bch_catalogue_abort(
      "The BCH indicator catalogue is not a data frame.",
      "bchR_catalogue_conversion_error"
    )
  }

  if (anyDuplicated(names(data))) {
    duplicated_names <- unique(names(data)[duplicated(names(data))])

    .bch_catalogue_abort(
      paste0(
        "The BCH indicator catalogue contains duplicated column names: ",
        paste(duplicated_names, collapse = ", "),
        ". This may indicate an upstream schema change."
      ),
      "bchR_catalogue_schema_error"
    )
  }

  missing_columns <- setdiff(
    .bch_indicator_catalogue_columns,
    names(data)
  )

  extra_columns <- setdiff(
    names(data),
    .bch_indicator_catalogue_columns
  )

  if (length(missing_columns) > 0L) {
    observed <- if (length(names(data)) > 0L) {
      paste(names(data), collapse = ", ")
    } else {
      "<none>"
    }

    .bch_catalogue_abort(
      paste0(
        "The BCH indicator catalogue schema does not match the structure ",
        "expected by bchR. Missing column(s): ",
        paste(missing_columns, collapse = ", "),
        ". Observed column(s): ",
        observed,
        ". The BCH API schema may have changed."
      ),
      "bchR_catalogue_schema_error"
    )
  }

  if (length(extra_columns) > 0L) {
    .bch_catalogue_warn(
      paste0(
        "The BCH indicator catalogue contains new/unexpected column(s): ",
        paste(extra_columns, collapse = ", "),
        ". They were preserved after the standard bchR catalogue columns."
      )
    )
  }

  data[
    c(
      .bch_indicator_catalogue_columns,
      extra_columns
    )
  ]
}


#' Clean the BCH indicator catalogue without changing official content
#'
#' Only leading/trailing whitespace is removed from character fields. The
#' official text is otherwise preserved. `CorrelativoGrupo` is always stored as
#' character because the observed BCH catalogue contains both numeric-looking
#' and textual/alphanumeric values.
#'
#' Row order is deliberately preserved as returned by the BCH API. `bchR` does
#' not currently have evidence that reordering the catalogue by `Id` is
#' semantically preferable to retaining the upstream order.
#'
#' @param data Validated indicator catalogue.
#'
#' @return Cleaned base `data.frame`.
#' @noRd
.bch_clean_indicator_catalogue <- function(data) {
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

  # This field cannot safely be represented as numeric: the observed catalogue
  # includes numeric-looking, alphanumeric, and descriptive values.
  if (!is.character(data$CorrelativoGrupo)) {
    data$CorrelativoGrupo <- as.character(data$CorrelativoGrupo)
  }
  data$CorrelativoGrupo <- trimws(data$CorrelativoGrupo)

  rownames(data) <- NULL
  data
}


#' Format a record count for progress messages
#'
#' @param n Non-negative row count.
#'
#' @return Character scalar using a comma as the thousands separator.
#' @noRd
.bch_format_count <- function(n) {
  format(
    as.integer(n),
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}


#' Retrieve the BCH indicator catalogue
#'
#' @description
#' Downloads the current indicator catalogue from the Banco Central de Honduras
#' (BCH) Web API and returns it as a reproducible base `data.frame`.
#'
#' `bch_get_indicators()` uses the package's internal HTTP client, so callers do
#' not construct URLs or pass `BCH_API_KEY` directly. Configure the key first
#' with [bch_set_api_key()].
#'
#' @param progress Logical. If `TRUE` (default), download and processing
#'   messages are displayed. Must be a single non-missing logical value.
#'
#' @return A base `data.frame` containing, at minimum, the standard BCH fields:
#'   `Id`, `Nombre`, `Descripcion`, `Periodicidad`, `Grupo`, and
#'   `CorrelativoGrupo`. `CorrelativoGrupo` is returned as character because
#'   the BCH catalogue contains heterogeneous values in this field. Additional
#'   fields introduced by the BCH API are preserved after the standard columns
#'   and trigger a schema warning.
#'
#'   The object includes secret-free provenance attributes:
#'   `retrieved_at`, `package_version`, and `api_endpoint`.
#'
#' @details
#' The function performs only minimal, documented cleaning: leading and
#' trailing whitespace is removed from character fields. Official BCH labels
#' and descriptions are otherwise left unchanged. Missing field values are
#' preserved.
#'
#' Row order is preserved exactly as returned by the API. The function does not
#' deduplicate, filter, translate, cache, or otherwise reorganize the catalogue.
#'
#' A missing standard field is treated as a breaking upstream schema change and
#' results in an informative error. New additional fields are preserved and
#' reported with a warning rather than being silently discarded.
#'
#' @seealso [bch_set_api_key()]
#'
#' @examples
#' \dontrun{
#' indicators <- bch_get_indicators()
#'
#' head(indicators)
#' table(indicators$Periodicidad, useNA = "ifany")
#'
#' # Suppress progress messages
#' indicators <- bch_get_indicators(progress = FALSE)
#' }
#'
#' @export
bch_get_indicators <- function(progress = TRUE) {
  .bch_validate_flag(progress, "progress")

  if (isTRUE(progress)) {
    message("Downloading BCH indicator catalogue...")
  }

  # Authentication, URL construction, timeout/retry policy, HTTP validation,
  # and JSON parsing are delegated exclusively to the internal HTTP layer.
  raw_catalogue <- bch_get(
    path = "indicadores",
    simplify_vector = TRUE
  )

  retrieved_at <- Sys.time()

  if (isTRUE(progress)) {
    message("Processing BCH indicator catalogue...")
  }

  catalogue <- .bch_catalogue_as_data_frame(raw_catalogue)

  if (nrow(catalogue) == 0L) {
    .bch_catalogue_abort(
      "The BCH Web API returned no indicators.",
      "bchR_empty_catalogue_error"
    )
  }

  catalogue <- .bch_validate_indicator_catalogue_schema(catalogue)
  catalogue <- .bch_clean_indicator_catalogue(catalogue)

  catalogue <- .bch_add_provenance(
    catalogue,
    path = "indicadores",
    retrieved_at = retrieved_at
  )

  if (isTRUE(progress)) {
    message(
      paste0(
        "Finished downloading ",
        .bch_format_count(nrow(catalogue)),
        " indicators."
      )
    )
  }

  catalogue
}
