# Shared validation helpers ---------------------------------------------------

#' Validate a scalar logical flag
#'
#' @param value Object to validate.
#' @param name Argument name used in the error message.
#'
#' @return `value`, invisibly.
#' @noRd
.bch_validate_flag <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop(
      paste0(name, " must be TRUE or FALSE."),
      call. = FALSE
    )
  }

  invisible(value)
}
