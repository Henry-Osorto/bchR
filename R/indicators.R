# Public catalog API ---------------------------------------------------------

#' Retrieve the BCH indicator catalog
#'
#' Filters use exact IDs, group codes, and canonical frequencies. Multiple
#' filters are combined by intersection. Catalog rows are returned in both
#' `data` and `metadata`; provenance describes the catalog retrieval.
#'
#' @param id NULL or a vector of non-negative integer IDs or digit-only strings.
#'   Character IDs preserve leading zeroes.
#' @param group NULL or exact BCH group code(s).
#' @param frequency NULL or canonical frequency name(s), such as `"daily"`.
#' @param source `"auto"`, `"live"`, or `"cache"`. Cache mode never connects.
#' @param refresh Whether to refresh a resource when permitted by `source`.
#' @param max_age Maximum acceptable cache age in seconds; NULL uses defaults.
#' @param cache_dir Optional explicit cache directory.
#' @return A `bch_result` with catalog rows and retrieval provenance.
#' @export
bch_indicators <- function(id = NULL, group = NULL, frequency = NULL,
                           source = c("auto", "live", "cache"),
                           refresh = FALSE, max_age = NULL, cache_dir = NULL) {
  source <- match.arg(source)
  ids <- .bch_public_ids(id, allow_null = TRUE)
  group <- .bch_public_strings(group, "group", allow_null = TRUE)
  frequency <- .bch_public_strings(frequency, "frequency", allow_null = TRUE)
  .bch_public_flag(refresh, "refresh")

  fetched <- .bch_fetch_resource(
    resource = "catalog", source = source, refresh = refresh,
    max_age = max_age, cache_dir = cache_dir
  )
  catalog <- .bch_parse_catalog(fetched$payload, unknown_frequency = "keep")
  if (!is.null(ids)) .bch_public_known(ids, catalog$indicator_id, "id")
  if (!is.null(group)) .bch_public_known(group, catalog$group_code, "group")
  if (!is.null(frequency)) {
    .bch_public_known(frequency, catalog$frequency, "frequency")
  }
  keep <- rep(TRUE, nrow(catalog))
  if (!is.null(ids)) keep <- keep & catalog$indicator_id %in% ids
  if (!is.null(group)) keep <- keep & catalog$group_code %in% group
  if (!is.null(frequency)) keep <- keep & catalog$frequency %in% frequency
  catalog <- catalog[keep, , drop = FALSE]
  bch_result(data = catalog, metadata = catalog, provenance = fetched$provenance)
}

#' Search an indicator catalog locally
#'
#' Search terms are literal substrings, never regular expressions. Each query
#' term can match any selected field; `match` controls whether every term or
#' at least one term is required. This function does not retrieve resources.
#'
#' @param x A catalog `bch_result` or a catalog data frame.
#' @param query A non-empty character vector of literal search terms.
#' @param fields Character catalog columns to search.
#' @param match Combine query terms using `"all"` or `"any"`.
#' @param ignore_case Whether matching ignores letter case.
#' @param ignore_accents Whether matching ignores Latin accents.
#' @return A `bch_result` with matching rows and retained provenance.
#' @export
bch_search_indicators <- function(
    x, query,
    fields = c("indicator_id", "indicator_code", "indicator_description", "group_code"),
    match = c("all", "any"), ignore_case = TRUE, ignore_accents = TRUE) {
  match <- match.arg(match)
  query <- .bch_public_strings(query, "query")
  fields <- .bch_public_strings(fields, "fields")
  .bch_public_flag(ignore_case, "ignore_case")
  .bch_public_flag(ignore_accents, "ignore_accents")
  input <- .bch_public_catalog(x)
  catalog <- input$data
  if (any(!fields %in% names(catalog)) ||
      any(!vapply(catalog[intersect(fields, names(catalog))], is.character, logical(1)))) {
    bch_abort("`fields` must name character columns in the catalog.", "invalid_argument")
  }
  normalize <- function(values) {
    values[is.na(values)] <- ""
    if (ignore_accents) {
      # Explicit mappings make Spanish matching independent of OS locale.
      values <- chartr(
        "\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1\u00c1\u00c9\u00cd\u00d3\u00da\u00dc\u00d1",
        "aeiouunAEIOUUN", values
      )
      transliterated <- iconv(values, from = "UTF-8", to = "ASCII//TRANSLIT")
      use <- !is.na(transliterated)
      values[use] <- transliterated[use]
    }
    if (ignore_case) values <- tolower(values)
    values
  }
  haystacks <- lapply(catalog[fields], normalize)
  hits <- lapply(normalize(query), function(term) {
    Reduce(`|`, lapply(haystacks, grepl, pattern = term, fixed = TRUE),
           init = rep(FALSE, nrow(catalog)))
  })
  keep <- Reduce(if (match == "all") `&` else `|`, hits)
  selected <- catalog[keep, , drop = FALSE]
  bch_result(
    data = selected, metadata = selected,
    provenance = input$provenance, problems = input$problems
  )
}

.bch_public_ids <- function(x, allow_null = FALSE) {
  if (is.null(x) && allow_null) return(NULL)
  if (length(x) == 0L || is.object(x) ||
      (!is.character(x) && !is.numeric(x))) {
    bch_abort("`id` must contain integer IDs or digit-only strings.", "invalid_argument")
  }
  if (is.numeric(x)) {
    valid <- !is.na(x) & is.finite(x) & x >= 0 & x == floor(x) & x <= 2^53 - 1
    if (any(!valid)) {
      bch_abort("Numeric IDs must be non-negative, exactly representable integers.", "invalid_argument")
    }
    x <- sprintf("%.0f", x)
  }
  if (anyNA(x) || any(!grepl("^[0-9]+$", x))) {
    bch_abort("Character IDs must contain digits only, without whitespace.", "invalid_argument")
  }
  unique(unname(x))
}

.bch_public_strings <- function(x, name, allow_null = FALSE) {
  if (is.null(x) && allow_null) return(NULL)
  if (!is.character(x) || length(x) == 0L || anyNA(x) ||
      any(!nzchar(trimws(x)))) {
    bch_abort(sprintf("`%s` must contain non-empty character values.", name), "invalid_argument")
  }
  unique(unname(x))
}

.bch_public_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    bch_abort(sprintf("`%s` must be TRUE or FALSE.", name), "invalid_argument")
  }
  invisible(x)
}

.bch_public_known <- function(requested, available, name) {
  absent <- setdiff(requested, available)
  if (length(absent)) {
    bch_abort(sprintf("Unknown catalog %s value(s): %s.", name,
                      paste(absent, collapse = ", ")),
              "unknown_indicator", argument = name, values = absent)
  }
  invisible(requested)
}

.bch_public_catalog <- function(x) {
  if (is_bch_result(x)) {
    validate_bch_result(x)
    input <- x
    catalog <- x$data
  } else if (is.data.frame(x)) {
    catalog <- tibble::as_tibble(x)
    input <- bch_result(data = catalog)
  } else {
    bch_abort("`catalog` must be a catalog data frame or bch_result.", "invalid_argument")
  }
  required <- c("indicator_id", "indicator_code", "indicator_description",
                "frequency_raw", "frequency", "group_code", "correlative")
  if (any(!required %in% names(catalog)) ||
      any(!vapply(catalog[intersect(required, names(catalog))], is.character, logical(1)))) {
    bch_abort("The supplied catalog does not follow the BCH catalog schema.", "invalid_argument")
  }
  if (nrow(catalog)) {
    .bch_public_ids(catalog$indicator_id)
    if (anyDuplicated(catalog$indicator_id)) {
      bch_abort("The supplied catalog must have unique indicator IDs.", "invalid_argument")
    }
  }
  if (anyNA(catalog$frequency) || anyNA(catalog$group_code) ||
      any(!nzchar(catalog$frequency)) || any(!nzchar(catalog$group_code))) {
    bch_abort("Catalog frequency and group codes must not be missing.", "invalid_argument")
  }
  if (!identical(unname(catalog$frequency),
                 unname(.bch_normalize_frequency(catalog$frequency_raw, unknown = "keep")))) {
    bch_abort("Canonical catalog frequencies disagree with verified raw labels.", "invalid_frequency")
  }
  input$data <- catalog
  input$metadata <- catalog
  input
}
