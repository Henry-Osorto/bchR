# Small internal utilities -------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

bch_is_scalar_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

.bch_as_tibble <- function(x, arg, default) {
  if (is.null(x)) {
    return(default())
  }

  tryCatch(
    tibble::as_tibble(x, .name_repair = "check_unique"),
    error = function(cnd) {
      bch_abort(
        sprintf("`%s` must be coercible to a tibble with unique columns.", arg),
        class = "bch_invalid_result",
        call = NULL
      )
    }
  )
}

.bch_assert_bch_result <- function(x, arg = "x") {
  if (!inherits(x, "bch_result")) {
    bch_abort(
      sprintf("`%s` must be a <bch_result> object.", arg),
      class = "bch_invalid_result",
      call = sys.call(-1L)
    )
  }
  invisible(x)
}

.bch_plural <- function(n, singular, plural = paste0(singular, "s")) {
  if (identical(as.double(n), 1)) singular else plural
}
