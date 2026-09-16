# Public observations API ----------------------------------------------------

#' Retrieve BCH observations for selected indicators or groups
#'
#' Supply exactly one of `id` or `group`. A supplied catalog is used locally
#' and never triggers a catalog request. Series are retrieved sequentially.
#' The series limit is checked before any series request. `source = "cache"`
#' works without credentials and never connects to BCH.
#'
#' Dates are filtered locally, with inclusive period overlap: a period is
#' kept if its end is on/after `start` and its start is on/before `end`.
#' UTC reference dates and raw timestamps are retained in long output.
#' Unknown periods cannot be date-filtered or reshaped to wide output.
#' No aggregation, filling, frequency conversion, or imputation is performed.
#'
#' @param id Indicator ID(s), mutually exclusive with `group`.
#' @param group Exact group code(s), mutually exclusive with `id`.
#' @param catalog Optional catalog bch_result or data frame.
#' @param start,end Optional inclusive Date or ISO YYYY-MM-DD limits.
#' @param shape Long observations or wide period-aligned values.
#' @param source Resource source: `"auto"`, `"live"`, or `"cache"`.
#' @param refresh Whether to refresh resources when permitted by `source`.
#' @param on_error `"stop"` returns no partial result on failure; successful
#'   earlier requests may still have updated the local cache. `"collect"`
#'   records per-series failures in `problems` and returns available series.
#' @param max_series Maximum distinct indicators allowed in one call.
#' @param max_age Maximum cache age in seconds; NULL uses resource defaults.
#' @param unknown_frequency Error on an unverified frequency or keep raw dates.
#' @param progress Whether to report retrieval progress with messages.
#' @param cache_dir Optional explicit cache directory.
#' @return A bch_result. Wide value columns have names `id_<ID>` and are
#'   documented in `column_map`. Metadata includes all selected series,
#'   including failures; column_map includes successfully parsed series.
#' @export
bch_get_data <- function(
    id = NULL, group = NULL, catalog = NULL, start = NULL, end = NULL,
    shape = c("long", "wide"), source = c("auto", "live", "cache"),
    refresh = FALSE, on_error = c("stop", "collect"), max_series = 25L,
    max_age = NULL, unknown_frequency = c("error", "keep"),
    progress = interactive(), cache_dir = NULL) {
  shape <- match.arg(shape)
  source <- match.arg(source)
  on_error <- match.arg(on_error)
  unknown_frequency <- match.arg(unknown_frequency)
  .bch_public_flag(refresh, "refresh")
  .bch_public_flag(progress, "progress")
  if (!is.numeric(max_series) || length(max_series) != 1L || is.na(max_series) ||
      !is.finite(max_series) || max_series < 1 ||
      max_series != floor(max_series) || max_series > .Machine$integer.max) {
    bch_abort("`max_series` must be a positive integer.", "invalid_argument")
  }
  max_series <- as.integer(max_series)
  if (is.null(id) == is.null(group)) {
    bch_abort("Supply exactly one of `id` or `group`.", "invalid_argument")
  }
  ids <- .bch_public_ids(id, allow_null = TRUE)
  group <- .bch_public_strings(group, "group", allow_null = TRUE)
  start <- .bch_public_date(start, "start")
  end <- .bch_public_date(end, "end")
  if (!is.null(start) && !is.null(end) && start > end) {
    bch_abort("`start` must not be later than `end`.", "invalid_argument")
  }
  if (!is.null(ids)) .bch_public_series_limit(ids, max_series)

  catalog_result <- if (is.null(catalog)) {
    bch_indicators(source = source, refresh = refresh,
                   max_age = max_age, cache_dir = cache_dir)
  } else {
    .bch_public_catalog(catalog)
  }
  metadata <- catalog_result$data
  if (!is.null(group)) {
    .bch_public_known(group, metadata$group_code, "group")
    ids <- metadata$indicator_id[metadata$group_code %in% group]
  } else {
    .bch_public_known(ids, metadata$indicator_id, "id")
  }
  .bch_public_series_limit(ids, max_series)
  metadata <- metadata[match(ids, metadata$indicator_id), , drop = FALSE]
  if (shape == "wide") .bch_public_wide_frequency(metadata$frequency)

  observations <- list()
  provenances <- list(catalog_result$provenance)
  failures <- list()
  successful <- character()
  for (i in seq_along(ids)) {
    indicator_id <- ids[[i]]
    if (progress) message(sprintf("BCH series %d/%d: %s", i, length(ids), indicator_id))
    attempts <- 1L
    fetched <- NULL
    value <- tryCatch({
      fetched <- .bch_fetch_resource(
        resource = "series", id = indicator_id, source = source,
        refresh = refresh, max_age = max_age, cache_dir = cache_dir
      )
      attempts <- fetched$attempts
      if (unknown_frequency == "error" && metadata$frequency[[i]] == "unknown") {
        bch_abort("This series has an unverified BCH frequency label.", "unknown_frequency")
      }
      parsed <- .bch_parse_series(
        fetched$payload, expected_id = indicator_id,
        frequency_raw = metadata$frequency_raw[[i]],
        unknown_frequency = unknown_frequency
      )
      if ((!is.null(start) || !is.null(end)) &&
          (metadata$frequency[[i]] == "unknown" ||
           anyNA(parsed$period_start) || anyNA(parsed$period_end))) {
        bch_abort("Date filters require verified period boundaries.", "unknown_frequency")
      }
      keep <- rep(TRUE, nrow(parsed))
      if (!is.null(start)) keep <- keep & parsed$period_end >= start
      if (!is.null(end)) keep <- keep & parsed$period_start <= end
      parsed[keep, , drop = FALSE]
    }, error = function(cnd) cnd)
    if (!is.null(fetched)) provenances[[length(provenances) + 1L]] <- fetched$provenance
    if (inherits(value, "error")) {
      if (on_error == "stop") {
        if (inherits(value, "bch_error")) stop(value)
        # Unexpected conditions can retain transport details or secrets. Never
        # forward their messages, calls, or attached HTTP objects to callers.
        bch_abort(
          "Unexpected failure while retrieving or parsing this series.",
          "unexpected_error", indicator_id = indicator_id
        )
      }
      failures[[length(failures) + 1L]] <- .bch_public_problem(value, indicator_id, attempts)
    } else {
      observations[[length(observations) + 1L]] <- value
      successful <- c(successful, indicator_id)
    }
  }
  data <- .bch_public_bind(observations, .bch_series_empty())
  provenance <- .bch_public_bind(provenances, .bch_empty_provenance())
  problems <- .bch_public_bind(
    c(list(catalog_result$problems), failures), .bch_empty_problems()
  )
  column_map <- .bch_empty_column_map()
  if (shape == "wide") {
    mapped <- metadata[match(successful, metadata$indicator_id), , drop = FALSE]
    column_map <- tibble::tibble(
      output_column = if (nrow(mapped)) paste0("id_", mapped$indicator_id) else character(),
      indicator_id = mapped$indicator_id,
      indicator_code = mapped$indicator_code,
      indicator_description = mapped$indicator_description
    )
    data <- .bch_public_wide(data, column_map)
  }
  bch_result(data = data, metadata = metadata, provenance = provenance,
             problems = problems, column_map = column_map)
}

.bch_public_date <- function(x, name) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "Date") && length(x) == 1L && !is.na(x) &&
      is.finite(as.numeric(x)) && as.numeric(x) == floor(as.numeric(x))) return(x)
  if (is.character(x) && length(x) == 1L && !is.na(x) &&
      grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)) {
    parsed <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
    if (!is.na(parsed) && identical(format(parsed, "%Y-%m-%d"), x)) return(parsed)
  }
  bch_abort(sprintf("`%s` must be a single Date or valid YYYY-MM-DD string.", name), "invalid_argument")
}

.bch_public_series_limit <- function(ids, max_series) {
  if (length(ids) > max_series) {
    bch_abort(sprintf("Selection contains %d series, exceeding `max_series = %d`.",
                      length(ids), max_series),
              "series_limit", n_series = length(ids), max_series = max_series)
  }
  invisible(ids)
}

.bch_public_bind <- function(rows, empty) {
  rows <- rows[vapply(rows, nrow, integer(1)) > 0L]
  if (!length(rows)) return(empty)
  # Provenance permits extra columns and its optional columns need not occur
  # in a caller-supplied catalog. Preserve them while filling absent fields.
  tibble::as_tibble(do.call(vctrs::vec_rbind, rows))
}

.bch_public_problem <- function(cnd, id, attempts) {
  if (is.numeric(cnd$attempts) && length(cnd$attempts) == 1L &&
      !is.na(cnd$attempts) && is.finite(cnd$attempts) &&
      cnd$attempts >= 1 && cnd$attempts <= .Machine$integer.max &&
      cnd$attempts == floor(cnd$attempts)) attempts <- cnd$attempts
  if (!is.numeric(attempts) || length(attempts) != 1L || is.na(attempts) ||
      !is.finite(attempts) || attempts < 1 || attempts > .Machine$integer.max ||
      attempts != floor(attempts)) attempts <- 1L
  message <- if (inherits(cnd, "bch_error")) conditionMessage(cnd) else {
    "Unexpected failure while retrieving or parsing this series."
  }
  key <- Sys.getenv("BCH_API_KEY", unset = "")
  if (nzchar(key)) message <- gsub(key, "<redacted>", message, fixed = TRUE)
  tibble::tibble(
    indicator_id = id,
    endpoint = paste0(.bch_api_base_url, "/indicadores/", id, "/cifras"),
    class = class(cnd)[[1L]], message = message, attempts = as.integer(attempts)
  )
}

.bch_public_wide_frequency <- function(frequency) {
  if (length(unique(frequency)) > 1L) {
    bch_abort("Wide output requires a single frequency across selected series.", "mixed_frequency")
  }
  if (anyNA(frequency) || any(frequency == "unknown")) {
    bch_abort("Wide output requires verified period boundaries.", "unknown_frequency")
  }
  invisible(frequency)
}

.bch_public_wide <- function(data, column_map) {
  .bch_public_wide_frequency(data$frequency)
  period_fields <- c("period_start", "period_end", "period_label", "frequency")
  periods <- unique(data[period_fields])
  periods <- periods[order(periods$period_start), , drop = FALSE]
  for (i in seq_len(nrow(column_map))) {
    series <- data[data$indicator_id == column_map$indicator_id[[i]], , drop = FALSE]
    index <- match(periods$period_start, series$period_start)
    periods[[column_map$output_column[[i]]]] <- series$value[index]
  }
  periods
}
