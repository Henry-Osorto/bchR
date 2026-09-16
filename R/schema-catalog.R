# Catalog schema -------------------------------------------------------------

.bch_validate_record_array <- function(x, resource) {
  if (!is.list(x) || !is.null(names(x))) {
    bch_abort(
      sprintf("The BCH %s payload must be a JSON array of objects.", resource),
      "schema_error",
      resource = resource
    )
  }

  if (length(x) == 0L) {
    return(invisible(x))
  }

  valid <- vapply(
    x,
    function(record) {
      is.list(record) &&
        !is.null(names(record)) &&
        all(nzchar(names(record))) &&
        !anyDuplicated(names(record))
    },
    logical(1)
  )

  if (any(!valid)) {
    bch_abort(
      paste0(
        "Every element of the BCH ", resource,
        " payload must be an object with unique, non-empty field names."
      ),
      "schema_error",
      resource = resource,
      rows = which(!valid)
    )
  }

  invisible(x)
}

.bch_require_fields <- function(record, required, resource, row) {
  missing <- setdiff(required, names(record))
  if (length(missing) > 0L) {
    bch_abort(
      sprintf(
        "BCH %s row %d is missing required field(s): %s.",
        resource,
        row,
        paste(missing, collapse = ", ")
      ),
      "schema_error",
      resource = resource,
      row = row,
      missing_fields = missing
    )
  }
  invisible(record)
}

.bch_id_scalar <- function(x, field, resource, row) {
  valid_character <- is.character(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    nzchar(x) &&
    grepl("^[0-9]+$", x)

  if (valid_character) {
    return(x)
  }

  valid_numeric <- is.numeric(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    is.finite(x) &&
    x >= 0 &&
    x == floor(x) &&
    x <= 2^53 - 1

  if (valid_numeric) {
    return(sprintf("%.0f", x))
  }

  bch_abort(
    sprintf(
      "BCH %s row %d field `%s` must be a non-negative integer ID or a digit-only string.",
      resource,
      row,
      field
    ),
    "schema_error",
    resource = resource,
    row = row,
    field = field
  )
}

.bch_text_scalar <- function(
    x,
    field,
    resource,
    row,
    allow_missing = FALSE) {
  if (is.null(x) || (length(x) == 1L && is.atomic(x) && is.na(x))) {
    if (allow_missing) {
      return(NA_character_)
    }
  } else if (is.character(x) && length(x) == 1L) {
    if (allow_missing || nzchar(x)) {
      return(x)
    }
  }

  bch_abort(
    sprintf(
      "BCH %s row %d field `%s` must be a single character value%s.",
      resource,
      row,
      field,
      if (allow_missing) " or null" else ""
    ),
    "schema_error",
    resource = resource,
    row = row,
    field = field
  )
}

.bch_extra_fields <- function(record, known) {
  record[setdiff(names(record), known)]
}

.bch_warn_duplicate_collapsed <- function(resource, ids, n_removed) {
  bch_warn(
    sprintf(
      "Collapsed %d identical duplicate BCH %s row(s).",
      n_removed,
      resource
    ),
    "duplicate_collapsed",
    resource = resource,
    ids = ids,
    n_removed = n_removed,
    call = NULL
  )
}

.bch_catalog_empty <- function() {
  tibble::tibble(
    indicator_id = character(),
    indicator_code = character(),
    indicator_description = character(),
    frequency_raw = character(),
    frequency = character(),
    group_code = character(),
    correlative = character(),
    extra = list()
  )
}

#' Parse the BCH indicator catalog
#'
#' Validates a list produced from a JSON array, maps the documented BCH fields
#' to a stable schema, preserves unrecognized fields in `extra`, and keeps IDs
#' as character strings. The input order is retained.
#'
#' @param x A list of named lists, one per catalog record.
#' @param unknown_frequency Whether unverified `Periodicidad` labels should be
#'   retained as `"unknown"` or rejected.
#' @return A tibble with one row per unique indicator.
#' @keywords internal
.bch_parse_catalog <- function(
    x,
    unknown_frequency = c("keep", "error")) {
  unknown_frequency <- match.arg(unknown_frequency)
  .bch_validate_record_array(x, "catalog")

  if (length(x) == 0L) {
    return(.bch_catalog_empty())
  }

  required <- c(
    "Id",
    "Nombre",
    "Descripcion",
    "Periodicidad",
    "Grupo",
    "Correlativo"
  )

  rows <- lapply(seq_along(x), function(i) {
    record <- x[[i]]
    .bch_require_fields(record, required, "catalog", i)

    frequency_raw <- .bch_text_scalar(
      record$Periodicidad,
      "Periodicidad",
      "catalog",
      i
    )

    list(
      indicator_id = .bch_id_scalar(record$Id, "Id", "catalog", i),
      indicator_code = .bch_text_scalar(
        record$Nombre,
        "Nombre",
        "catalog",
        i
      ),
      indicator_description = .bch_text_scalar(
        record$Descripcion,
        "Descripcion",
        "catalog",
        i,
        allow_missing = TRUE
      ),
      frequency_raw = frequency_raw,
      frequency = .bch_normalize_frequency(
        frequency_raw,
        unknown = unknown_frequency
      ),
      group_code = .bch_text_scalar(
        record$Grupo,
        "Grupo",
        "catalog",
        i
      ),
      correlative = .bch_text_scalar(
        record$Correlativo,
        "Correlativo",
        "catalog",
        i,
        allow_missing = TRUE
      ),
      extra = .bch_extra_fields(record, required)
    )
  })

  ids <- vapply(rows, `[[`, character(1), "indicator_id")
  duplicated_ids <- unique(ids[duplicated(ids)])
  keep <- rep(TRUE, length(rows))

  for (id in duplicated_ids) {
    locations <- which(ids == id)
    reference <- rows[[locations[[1L]]]]
    same <- vapply(
      locations[-1L],
      function(j) identical(reference, rows[[j]]),
      logical(1)
    )

    if (any(!same)) {
      bch_abort(
        sprintf("Conflicting catalog rows were returned for indicator ID '%s'.", id),
        "conflicting_duplicate",
        resource = "catalog",
        indicator_id = id,
        rows = locations
      )
    }

    keep[locations[-1L]] <- FALSE
  }

  if (any(!keep)) {
    .bch_warn_duplicate_collapsed(
      "catalog",
      ids = unique(ids[!keep]),
      n_removed = sum(!keep)
    )
    rows <- rows[keep]
  }

  tibble::tibble(
    indicator_id = vapply(rows, `[[`, character(1), "indicator_id"),
    indicator_code = vapply(rows, `[[`, character(1), "indicator_code"),
    indicator_description = vapply(
      rows,
      `[[`,
      character(1),
      "indicator_description"
    ),
    frequency_raw = vapply(rows, `[[`, character(1), "frequency_raw"),
    frequency = vapply(rows, `[[`, character(1), "frequency"),
    group_code = vapply(rows, `[[`, character(1), "group_code"),
    correlative = vapply(rows, `[[`, character(1), "correlative"),
    extra = lapply(rows, `[[`, "extra")
  )
}
