# BCH result container -----------------------------------------------------

.bch_result_components <- c(
  "data",
  "metadata",
  "provenance",
  "problems",
  "column_map"
)

.bch_result_required_columns <- list(
  data = character(),
  metadata = "indicator_id",
  provenance = c(
    "retrieved_at",
    "source",
    "endpoint",
    "schema_version",
    "from_cache"
  ),
  problems = c("indicator_id", "endpoint", "class", "message", "attempts"),
  column_map = c("output_column", "indicator_id")
)

.bch_empty_data <- function() {
  tibble::tibble()
}

.bch_empty_metadata <- function() {
  tibble::tibble(
    indicator_id = character(),
    indicator_code = character(),
    indicator_description = character(),
    frequency = character(),
    frequency_raw = character(),
    group_code = character(),
    correlative = character()
  )
}

.bch_empty_provenance <- function() {
  tibble::tibble(
    request_id = character(),
    indicator_id = character(),
    retrieved_at = as.POSIXct(character(), tz = "UTC"),
    source = character(),
    endpoint = character(),
    schema_version = character(),
    from_cache = logical(),
    response_hash = character()
  )
}

.bch_empty_problems <- function() {
  tibble::tibble(
    indicator_id = character(),
    endpoint = character(),
    class = character(),
    message = character(),
    attempts = integer()
  )
}

.bch_empty_column_map <- function() {
  tibble::tibble(
    output_column = character(),
    indicator_id = character(),
    indicator_code = character(),
    indicator_description = character()
  )
}

#' Construct a BCH result
#'
#' A BCH result is a stable container with five tibble components. Keeping
#' observations separate from metadata and provenance avoids repeating
#' series-level information on every observation and keeps partial failures
#' machine-readable.
#'
#' @param data Observations or catalogue rows.
#' @param metadata Indicator-level metadata, indexed by `indicator_id`.
#' @param provenance Retrieval provenance. Endpoints must be sanitized and
#'   contain no credentials or query strings.
#' @param problems Structured partial failures. An empty table is retained on
#'   complete success.
#' @param column_map Mapping from wide-form output columns to indicators. It is
#'   empty for long-form output.
#'
#' @return An object of class `bch_result`.
#' @keywords internal
bch_result <- function(
    data = NULL,
    metadata = NULL,
    provenance = NULL,
    problems = NULL,
    column_map = NULL) {
  new_bch_result(
    data = .bch_as_tibble(data, "data", .bch_empty_data),
    metadata = .bch_as_tibble(metadata, "metadata", .bch_empty_metadata),
    provenance = .bch_as_tibble(
      provenance,
      "provenance",
      .bch_empty_provenance
    ),
    problems = .bch_as_tibble(problems, "problems", .bch_empty_problems),
    column_map = .bch_as_tibble(
      column_map,
      "column_map",
      .bch_empty_column_map
    )
  )
}

#' Low-level BCH result constructor
#'
#' @inheritParams bch_result
#' @param validate Whether to check all structural invariants.
#' @return An object of class `bch_result`.
#' @keywords internal
new_bch_result <- function(
    data,
    metadata,
    provenance,
    problems,
    column_map,
    validate = TRUE) {
  result <- structure(
    list(
      data = data,
      metadata = metadata,
      provenance = provenance,
      problems = problems,
      column_map = column_map
    ),
    class = c("bch_result", "list")
  )

  if (isTRUE(validate)) {
    validate_bch_result(result)
  } else if (!identical(validate, FALSE)) {
    bch_abort(
      "`validate` must be `TRUE` or `FALSE`.",
      class = "bch_invalid_argument",
      call = sys.call(-1L)
    )
  }

  result
}

#' Validate a BCH result
#'
#' @param x An object to validate.
#' @return `x`, invisibly, or an error of class
#'   `bch_invalid_result`.
#' @export
validate_bch_result <- function(x) {
  .bch_assert_bch_result(x)

  if (!is.list(x) || !identical(names(x), .bch_result_components) ||
      length(x) != length(.bch_result_components)) {
    bch_abort(
      paste0(
        "A <bch_result> must contain exactly these components in order: ",
        paste(.bch_result_components, collapse = ", "),
        "."
      ),
      class = "bch_invalid_result",
      call = sys.call(-1L)
    )
  }

  is_tbl <- vapply(
    x,
    function(component) inherits(component, "tbl_df"),
    logical(1L)
  )
  if (any(!is_tbl)) {
    bch_abort(
      sprintf(
        "Every <bch_result> component must be a tibble; invalid: %s.",
        paste(names(x)[!is_tbl], collapse = ", ")
      ),
      class = "bch_invalid_result",
      call = sys.call(-1L)
    )
  }

  for (component_name in .bch_result_components) {
    required <- .bch_result_required_columns[[component_name]]
    missing_columns <- setdiff(required, names(x[[component_name]]))
    if (length(missing_columns) > 0L) {
      bch_abort(
        sprintf(
          "`%s` is missing required column%s: %s.",
          component_name,
          if (length(missing_columns) == 1L) "" else "s",
          paste(missing_columns, collapse = ", ")
        ),
        class = "bch_invalid_result",
        component = component_name,
        missing_columns = missing_columns,
        call = sys.call(-1L)
      )
    }
  }

  .bch_validate_metadata(x$metadata)
  .bch_validate_provenance(x$provenance)
  .bch_validate_problems(x$problems)
  .bch_validate_column_map(x$column_map)

  invisible(x)
}

.bch_validate_metadata <- function(metadata) {
  ids <- metadata$indicator_id
  if (!is.character(ids) || anyNA(ids) || any(!nzchar(ids))) {
    bch_abort("`metadata$indicator_id` must contain non-missing character IDs.",
              "bch_invalid_result", component = "metadata")
  }
  duplicated_ids <- !is.na(ids) & duplicated(ids)
  if (any(duplicated_ids)) {
    bch_abort(
      "`metadata$indicator_id` must uniquely index metadata rows.",
      class = "bch_invalid_result",
      component = "metadata",
      call = sys.call(-1L)
    )
  }

  invisible(metadata)
}

.bch_validate_provenance <- function(provenance) {
  if (!inherits(provenance$retrieved_at, "POSIXct")) {
    bch_abort(
      "`provenance$retrieved_at` must be a POSIXct vector.",
      class = "bch_invalid_result",
      component = "provenance",
      call = sys.call(-1L)
    )
  }
  if (!is.logical(provenance$from_cache)) {
    bch_abort(
      "`provenance$from_cache` must be logical.",
      class = "bch_invalid_result",
      component = "provenance",
      call = sys.call(-1L)
    )
  }
  if (length(provenance$endpoint) > 0L) {
    unsafe <- is.na(provenance$endpoint) |
      grepl("[?&]", provenance$endpoint, perl = TRUE)
    if (any(unsafe)) {
      bch_abort(
        paste0(
          "`provenance$endpoint` must contain sanitized endpoints without ",
          "query strings."
        ),
        class = "bch_unsafe_endpoint",
        component = "provenance",
        call = sys.call(-1L)
      )
    }
  }

  invisible(provenance)
}

.bch_validate_problems <- function(problems) {
  if (!is.numeric(problems$attempts)) {
    bch_abort(
      "`problems$attempts` must be numeric.",
      class = "bch_invalid_result",
      component = "problems",
      call = sys.call(-1L)
    )
  }
  if (length(problems$attempts) > 0L &&
      any(!is.finite(problems$attempts))) {
    bch_abort(
      "`problems$attempts` must contain finite values.",
      class = "bch_invalid_result",
      component = "problems",
      call = sys.call(-1L)
    )
  }
  if (length(problems$attempts) > 0L &&
      any(problems$attempts < 1 | problems$attempts %% 1 != 0)) {
    bch_abort(
      "`problems$attempts` must contain positive whole numbers.",
      class = "bch_invalid_result",
      component = "problems",
      call = sys.call(-1L)
    )
  }
  if (length(problems$endpoint) > 0L &&
      any(is.na(problems$endpoint) |
          grepl("[?&]", problems$endpoint, perl = TRUE))) {
    bch_abort(
      "`problems$endpoint` must contain sanitized endpoints without queries.",
      class = "bch_unsafe_endpoint",
      component = "problems",
      call = sys.call(-1L)
    )
  }

  invisible(problems)
}

.bch_validate_column_map <- function(column_map) {
  columns <- column_map$output_column
  invalid_columns <- is.na(columns) | !nzchar(columns) | duplicated(columns)
  if (any(invalid_columns)) {
    bch_abort(
      "`column_map$output_column` must contain unique, non-missing names.",
      class = "bch_invalid_result",
      component = "column_map",
      call = sys.call(-1L)
    )
  }

  invisible(column_map)
}

#' Test whether an object is a BCH result
#'
#' @param x An object.
#' @return A single logical value.
#' @export
is_bch_result <- function(x) {
  inherits(x, "bch_result")
}

#' Extract a component from a BCH result
#'
#' @param x A `bch_result` object.
#' @return The requested tibble component.
#' @name bch_result_accessors
NULL

#' @rdname bch_result_accessors
#' @export
bch_data <- function(x) {
  .bch_result_component(x, "data")
}

#' @rdname bch_result_accessors
#' @export
bch_metadata <- function(x) {
  .bch_result_component(x, "metadata")
}

#' @rdname bch_result_accessors
#' @export
bch_provenance <- function(x) {
  .bch_result_component(x, "provenance")
}

#' @rdname bch_result_accessors
#' @export
bch_problems <- function(x) {
  .bch_result_component(x, "problems")
}

#' @rdname bch_result_accessors
#' @export
bch_column_map <- function(x) {
  .bch_result_component(x, "column_map")
}

.bch_result_component <- function(x, component) {
  validate_bch_result(x)
  x[[component]]
}

#' @export
print.bch_result <- function(x, ..., n = NULL, width = NULL) {
  validate_bch_result(x)

  cat("<bch_result>\n")
  for (component_name in .bch_result_components) {
    component <- x[[component_name]]
    cat(
      sprintf(
        "* %-10s %s x %s\n",
        paste0(component_name, ":"),
        format(nrow(component), big.mark = ",", trim = TRUE),
        format(ncol(component), big.mark = ",", trim = TRUE)
      )
    )
  }

  if (nrow(x$problems) > 0L) {
    cat(
      sprintf(
        "! %s partial %s recorded\n",
        nrow(x$problems),
        .bch_plural(nrow(x$problems), "problem")
      )
    )
  }

  if (nrow(x$data) > 0L) {
    cat("\nData:\n")
    print(x$data, ..., n = n, width = width)
  }

  invisible(x)
}

#' Convert a BCH result to a tibble
#'
#' Only the primary `data` component is returned. Use the accessors to retain
#' metadata, provenance, problems, and the wide-column map.
#'
#' @param x A `bch_result` object.
#' @param ... Unused; included for S3 compatibility.
#' @return The `data` component as a tibble.
#' @exportS3Method as_tibble bch_result
as_tibble.bch_result <- function(x, ...) {
  bch_data(x)
}

#' @export
as.data.frame.bch_result <- function(
    x,
    row.names = NULL,
    optional = FALSE,
    ...) {
  as.data.frame(
    bch_data(x),
    row.names = row.names,
    optional = optional,
    ...
  )
}
