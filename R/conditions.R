# Conditions ---------------------------------------------------------------

#' Abort with a package-specific error
#'
#' Internal helper used to expose stable condition classes without attaching
#' secrets, requests, or responses to the condition object.
#'
#' @param message A scalar character error message.
#' @param class A complete package class such as `"bch_invalid_argument"`, or
#'   a suffix such as `"invalid_argument"`. Every error also inherits from
#'   `bch_error`.
#' @param ... Named, safe diagnostic fields.
#' @param call The call to report. Use `NULL` to suppress it.
#'
#' @keywords internal
bch_abort <- function(message, class = "bch_error", ..., call = NULL) {
  if (!bch_is_scalar_string(message)) {
    stop("`message` must be a single, non-missing string.", call. = FALSE)
  }

  details <- list(...)
  .bch_validate_condition_details(details)

  condition <- structure(
    c(list(message = message, call = call), details),
    class = c(.bch_condition_classes(class, type = "error"), "error", "condition")
  )
  stop(condition)
}

#' Warn with a package-specific condition
#'
#' @inheritParams bch_abort
#' @keywords internal
bch_warn <- function(message, class = "bch_warning", ..., call = NULL) {
  if (!bch_is_scalar_string(message)) {
    stop("`message` must be a single, non-missing string.", call. = FALSE)
  }

  details <- list(...)
  .bch_validate_condition_details(details)

  condition <- structure(
    c(list(message = message, call = call), details),
    class = c(
      .bch_condition_classes(class, type = "warning"),
      "warning",
      "condition"
    )
  )
  warning(condition)
  invisible(condition)
}

.bch_condition_classes <- function(class, type) {
  if (!is.character(class) || length(class) < 1L || anyNA(class) ||
      any(!nzchar(class))) {
    stop("`class` must contain one or more non-empty strings.", call. = FALSE)
  }

  parent <- paste0("bch_", type)
  normalized <- vapply(
    class,
    function(x) {
      if (startsWith(x, "bch_")) x else paste0("bch_", x)
    },
    character(1L),
    USE.NAMES = FALSE
  )

  unique(c(normalized, parent))
}

.bch_validate_condition_details <- function(details) {
  if (length(details) == 0L) {
    return(invisible(NULL))
  }

  detail_names <- names(details)
  if (is.null(detail_names) || any(!nzchar(detail_names))) {
    stop("All diagnostic fields in `...` must be named.", call. = FALSE)
  }

  reserved <- intersect(detail_names, c("message", "call", "parent", "request",
                                       "response", "headers", "body", "key"))
  if (length(reserved) > 0L) {
    stop(
      sprintf(
        "Diagnostic fields cannot replace reserved field%s: %s.",
        if (length(reserved) == 1L) "" else "s",
        paste(reserved, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(NULL)
}
