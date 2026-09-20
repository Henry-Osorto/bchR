# Authentication helpers ------------------------------------------------------

# Environment variable used by bchR.
.bch_api_key_envvar <- "BCH_API_KEY"

#' Check whether the BCH API key is available
#'
#' Internal helper used to determine whether `BCH_API_KEY` is present in the
#' current R session and contains a non-empty, non-whitespace value.
#'
#' @return A single logical value.
#' @noRd
bch_has_api_key <- function() {
  key <- Sys.getenv(.bch_api_key_envvar, unset = NA_character_)

  is.character(key) &&
    length(key) == 1L &&
    !is.na(key) &&
    nzchar(trimws(key))
}

#' Retrieve the BCH API key
#'
#' Internal helper for functions that need to authenticate against the BCH Web
#' API. The key is returned only to package code and is never printed.
#'
#' @return A length-one character vector containing the API key.
#' @noRd
bch_api_key <- function() {
  if (!bch_has_api_key()) {
    stop(
      paste0(
        "BCH_API_KEY is not configured. ",
        "Run bch_set_api_key() in an interactive R session, then try again."
      ),
      call. = FALSE
    )
  }

  Sys.getenv(.bch_api_key_envvar, unset = "")
}

#' Validate an API key before storing it
#'
#' @param key Candidate key.
#'
#' @return The original key, invisibly unchanged.
#' @noRd
.bch_validate_api_key_value <- function(key) {
  valid_scalar <- is.character(key) &&
    length(key) == 1L &&
    !is.na(key)

  if (!valid_scalar || !nzchar(trimws(key))) {
    stop("The BCH API key must be a non-empty character string.", call. = FALSE)
  }

  # Prevent line injection into .Renviron.
  if (grepl("[\\r\\n]", key, perl = TRUE)) {
    stop("The BCH API key cannot contain line breaks.", call. = FALSE)
  }

  key
}

#' Return the user-level .Renviron path
#'
#' @return A normalized user-level path. The file need not already exist.
#' @noRd
.bch_user_renviron <- function() {
  configured <- Sys.getenv("R_ENVIRON_USER", unset = "")

  if (nzchar(trimws(configured))) {
    return(path.expand(configured))
  }

  path.expand("~/.Renviron")
}

#' Prompt securely for the BCH API key
#'
#' Uses `askpass`, which selects an appropriate password-entry method for the
#' current platform/front-end and returns `NULL` in non-interactive sessions by
#' default.
#'
#' @return A key as a character scalar, or `NULL` when no key was entered.
#' @noRd
.bch_prompt_api_key <- function() {
  key <- askpass::askpass("Enter your BCH Primary key: ")

  if (is.null(key)) {
    return(NULL)
  }

  if (!is.character(key) || length(key) != 1L || is.na(key) ||
      !nzchar(trimws(key))) {
    return(NULL)
  }

  .bch_validate_api_key_value(key)
}

#' Encode a value for storage in .Renviron
#'
#' The R startup documentation describes `.Renviron` values as shell-like.
#' `shQuote(..., type = "sh")` protects ordinary special characters without
#' exposing the value.
#'
#' @param value A validated character scalar.
#'
#' @return A quoted character scalar suitable for the right-hand side of a
#'   `.Renviron` assignment.
#' @noRd
.bch_quote_renviron_value <- function(value) {
  .bch_validate_api_key_value(value)
  shQuote(value, type = "sh")
}

#' Write .Renviron content through a temporary file
#'
#' @param lines Character vector to write.
#' @param renviron_file Destination file.
#'
#' @return `TRUE`, invisibly.
#' @noRd
.bch_write_renviron <- function(lines, renviron_file) {
  parent <- dirname(renviron_file)

  if (!dir.exists(parent)) {
    stop("The directory containing the user .Renviron file does not exist.",
         call. = FALSE)
  }

  tmp <- tempfile(pattern = ".bchR-renviron-", tmpdir = parent)
  on.exit(unlink(tmp, force = TRUE), add = TRUE)

  writeLines(lines, con = tmp, useBytes = TRUE)

  # Best-effort restrictive permissions on Unix-like systems. Failure to chmod
  # is not fatal because some mounted or network file systems do not support it.
  if (.Platform$OS.type != "windows") {
    suppressWarnings(try(Sys.chmod(tmp, mode = "0600"), silent = TRUE))
  }

  copied <- file.copy(
    from = tmp,
    to = renviron_file,
    overwrite = TRUE,
    copy.mode = TRUE,
    copy.date = FALSE
  )

  if (!isTRUE(copied)) {
    stop("Could not replace the user .Renviron file.", call. = FALSE)
  }

  if (.Platform$OS.type != "windows") {
    suppressWarnings(try(Sys.chmod(renviron_file, mode = "0600"), silent = TRUE))
  }

  invisible(TRUE)
}

#' Store the BCH API key in a specified .Renviron file
#'
#' Internal implementation separated from the public prompt so it can be
#' tested against temporary files without touching the user's real
#' `~/.Renviron`.
#'
#' @param key API key to store.
#' @param renviron_file Path to a `.Renviron` file.
#' @param set_session Whether to also set `BCH_API_KEY` in the current session.
#'
#' @return `TRUE`, invisibly.
#' @noRd
.bch_store_api_key <- function(key,
                               renviron_file,
                               set_session = TRUE) {
  key <- .bch_validate_api_key_value(key)

  if (!is.character(renviron_file) || length(renviron_file) != 1L ||
      is.na(renviron_file) || !nzchar(renviron_file)) {
    stop("renviron_file must be a non-empty character string.", call. = FALSE)
  }

  renviron_file <- path.expand(renviron_file)

  existing <- if (file.exists(renviron_file)) {
    if (isTRUE(file.info(renviron_file)$isdir)) {
      stop("The requested .Renviron path is a directory, not a file.",
           call. = FALSE)
    }

    tryCatch(
      readLines(
        con = renviron_file,
        warn = FALSE,
        encoding = "UTF-8"
      ),
      error = function(e) {
        stop("Could not read the existing user .Renviron file.", call. = FALSE)
      }
    )
  } else {
    character()
  }

  # Remove every active declaration of BCH_API_KEY while leaving comments and
  # all unrelated variables untouched.
  key_pattern <- "^[[:space:]]*BCH_API_KEY[[:space:]]*="
  existing <- existing[!grepl(key_pattern, existing)]

  assignment <- paste0(
    .bch_api_key_envvar,
    "=",
    .bch_quote_renviron_value(key)
  )

  new_content <- c(existing, assignment)

  tryCatch(
    .bch_write_renviron(new_content, renviron_file),
    error = function(e) {
      # Deliberately do not append conditionMessage(e): lower-level errors can
      # contain user-specific details and future callers may include secrets.
      stop(
        "Could not write BCH_API_KEY to the user .Renviron file.",
        call. = FALSE
      )
    }
  )

  if (isTRUE(set_session)) {
    Sys.setenv(BCH_API_KEY = key)
  }

  invisible(TRUE)
}

#' Configure the BCH Web API key internally
#'
#' @param notify Function used for user-facing status messages. It must accept a
#'   character scalar. The public setter uses [message()], while package startup
#'   uses [packageStartupMessage()] so startup output remains suppressible.
#'
#' @return `TRUE` invisibly when the key is stored successfully; `FALSE`
#'   invisibly when no key is entered.
#' @noRd
.bch_set_api_key_impl <- function(notify = message) {
  if (!is.function(notify)) {
    stop("notify must be a function.", call. = FALSE)
  }

  key <- .bch_prompt_api_key()

  if (is.null(key)) {
    notify("No API key was entered; BCH_API_KEY was not changed.")
    return(invisible(FALSE))
  }

  # Keep the credential in the smallest practical scope. Assigning NULL does
  # not claim to erase the underlying R string from memory; it only prevents
  # this function from retaining a named reference after configuration.
  on.exit(key <- NULL, add = TRUE)

  .bch_store_api_key(
    key = key,
    renviron_file = .bch_user_renviron(),
    set_session = TRUE
  )

  key <- NULL

  notify(
    paste0(
      "BCH_API_KEY was saved to the user .Renviron file and is available ",
      "in the current R session."
    )
  )

  invisible(TRUE)
}

#' Configure the BCH Web API key
#'
#' Securely prompts for the user's Banco Central de Honduras (BCH) Web API
#' Primary key, stores it in the user-level `.Renviron` file (by default
#' `~/.Renviron`), and makes it available immediately in the current R session.
#'
#' Existing `BCH_API_KEY` declarations in the selected user `.Renviron` file
#' are replaced by a single declaration. Other environment variables and
#' comments are preserved. The key is requested with [askpass::askpass()], so
#' it is not echoed in the R console.
#'
#' @details
#' An API key can be obtained from the BCH developer portal after creating an
#' account and subscribing to the BCH API. The Primary key is available in the
#' user's subscription/profile information.
#'
#' `bch_set_api_key()` never returns or prints the API key. When successful it
#' returns `TRUE` invisibly. If the password dialog is cancelled or no key is
#' entered, it returns `FALSE` invisibly and leaves the current configuration
#' unchanged.
#'
#' @return `TRUE` invisibly when the key is stored successfully; `FALSE`
#'   invisibly when no key is entered.
#'
#' @seealso <https://bchapi-am.developer.azure-api.net/>
#'
#' @examples
#' \dontrun{
#' bch_set_api_key()
#' }
#'
#' @export
bch_set_api_key <- function() {
  .bch_set_api_key_impl(notify = message)
}

#' Redact BCH API credentials from URLs or messages
#'
#' Internal security helper intended for the HTTP layer. It removes both the
#' current session key (when available) and any value assigned to a `clave`
#' query parameter.
#'
#' @param x Character vector containing URLs or diagnostic text.
#'
#' @return A character vector with credentials replaced by `<redacted>`.
#' @noRd
bch_redact_url <- function(x) {
  if (length(x) == 0L) {
    return(character())
  }

  x <- as.character(x)

  key <- Sys.getenv(.bch_api_key_envvar, unset = "")
  if (nzchar(trimws(key))) {
    x <- gsub(key, "<redacted>", x, fixed = TRUE)
  }

  # Redact the BCH authentication query parameter even when the current session
  # does not contain the corresponding key.
  x <- gsub(
    pattern = "(?i)(clave[[:space:]]*=[[:space:]]*)[^&#[:space:]]+",
    replacement = "\\1<redacted>",
    x = x,
    perl = TRUE
  )

  x
}
