# Package startup -------------------------------------------------------------

#' Handle bchR package attachment
#'
#' If `BCH_API_KEY` is already configured, attachment is silent. If it is
#' missing in a non-interactive session, the package loads silently and API
#' functions will report the missing credential only when called. In an
#' interactive session, bchR explains how to obtain a key and, by default,
#' opens a secure password prompt.
#'
#' Automatic prompting can be disabled before attachment with
#' `options(bchR.prompt_api_key = FALSE)`. This opt-out is mainly useful for
#' development environments that are interactive but should not modify a user
#' `.Renviron` file automatically.
#'
#' @param is_interactive Logical flag, normally supplied by [interactive()].
#' @param prompt_for_key Logical. Defaults to the `bchR.prompt_api_key` option,
#'   or `TRUE` when the option is unset.
#'
#' @return `NULL`, invisibly.
#' @noRd
.bch_on_attach <- function(
    is_interactive = interactive(),
    prompt_for_key = getOption("bchR.prompt_api_key", TRUE)
) {
  if (bch_has_api_key()) {
    return(invisible(NULL))
  }

  if (!isTRUE(is_interactive)) {
    return(invisible(NULL))
  }

  packageStartupMessage(
    paste0(
      "bchR requires an API key from the Central Bank of Honduras Web API.\n\n",
      "To obtain one:\n",
      "1. Create an account at the BCH developer portal.\n",
      "2. Subscribe to the BCH API.\n",
      "3. Open your profile/subscription information.\n",
      "4. Locate the Primary key and select Show to view it.\n\n",
      "Developer portal:\n",
      "https://bchapi-am.developer.azure-api.net/"
    )
  )

  if (!isTRUE(prompt_for_key)) {
    packageStartupMessage(
      "Automatic API-key prompting is disabled. Run bch_set_api_key() when ready."
    )
    return(invisible(NULL))
  }

  # Failure to configure a key must never prevent the package from attaching.
  # Any lower-level error is already designed to be secret-free; the startup
  # hook still replaces it with a short package-level message.
  tryCatch(
    .bch_set_api_key_impl(notify = packageStartupMessage),
    error = function(e) {
      packageStartupMessage(
        paste0(
          "BCH_API_KEY could not be configured during package startup. ",
          "Run bch_set_api_key() later to try again."
        )
      )
      FALSE
    }
  )

  invisible(NULL)
}

.onAttach <- function(libname, pkgname) {
  .bch_on_attach(interactive())
}
