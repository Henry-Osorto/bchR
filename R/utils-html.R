# HTML helpers ---------------------------------------------------------------

#' Convert a local image to a data URI
#'
#' This helper is intentionally limited to local files. `bchR` never downloads
#' institutional or package logos when the indicator viewer is created.
#'
#' @param path Path to a local image file.
#'
#' @return A data URI character scalar, or `NULL` when `path` does not identify
#'   an existing file.
#' @noRd
.bch_img_to_data_uri <- function(path) {
  if (
    length(path) != 1L ||
      is.na(path) ||
      !nzchar(path) ||
      !file.exists(path)
  ) {
    return(NULL)
  }

  ext <- tolower(tools::file_ext(path))
  mime <- switch(
    ext,
    png = "image/png",
    jpg = "image/jpeg",
    jpeg = "image/jpeg",
    svg = "image/svg+xml",
    webp = "image/webp",
    "application/octet-stream"
  )

  base64enc::dataURI(
    file = path,
    mime = mime
  )
}


#' Locate an optional bundled viewer image
#'
#' The package does not assume permission to redistribute BCH branding. Images
#' are therefore optional: if an explicitly bundled file exists under
#' `inst/images/`, it can be embedded; otherwise the viewer renders a text-only
#' header.
#'
#' @param candidates Candidate file names under `inst/images/`, in priority
#'   order.
#'
#' @return A path character scalar, or an empty string when no candidate exists.
#' @noRd
.bch_find_viewer_image <- function(candidates) {
  if (!is.character(candidates) || length(candidates) == 0L) {
    return("")
  }

  for (candidate in candidates) {
    path <- system.file(
      "images",
      candidate,
      package = "bchR"
    )

    if (nzchar(path) && file.exists(path)) {
      return(path)
    }
  }

  ""
}


#' Return an optional bundled image as a data URI
#'
#' @param candidates Candidate file names under `inst/images/`.
#'
#' @return A data URI or `NULL`.
#' @noRd
.bch_optional_viewer_image <- function(candidates) {
  path <- .bch_find_viewer_image(candidates)

  if (!nzchar(path)) {
    return(NULL)
  }

  .bch_img_to_data_uri(path)
}


#' CSS used by the BCH indicator viewer
#'
#' @return An `htmltools` style tag.
#' @noRd
.bch_viewer_style <- function() {
  htmltools::tags$style(
    paste(
      ".bchr-viewer {",
      "  max-width: 100%;",
      "  margin: 0 auto;",
      "  background: #FFFFFF;",
      "  border: 1px solid #E2E8F0;",
      "  border-radius: 14px;",
      "  overflow: hidden;",
      "  box-shadow: 0 6px 20px rgba(15, 48, 73, 0.08);",
      "}",

      ".bchr-header {",
      "  display: flex;",
      "  justify-content: space-between;",
      "  align-items: center;",
      "  gap: 28px;",
      "  padding: 20px 24px 18px 24px;",
      "  border-top: 4px solid #173F5F;",
      "  border-bottom: 3px solid #2B7A78;",
      "  background: #FFFFFF;",
      "}",

      ".bchr-header-left {",
      "  display: flex;",
      "  align-items: center;",
      "  gap: 16px;",
      "  min-width: 0;",
      "}",

      ".bchr-header-right {",
      "  display: flex;",
      "  flex-direction: column;",
      "  align-items: flex-end;",
      "  text-align: right;",
      "  min-width: 250px;",
      "}",

      ".bchr-logo {",
      "  display: block;",
      "  max-height: 72px;",
      "  max-width: 190px;",
      "  width: auto;",
      "  height: auto;",
      "  object-fit: contain;",
      "}",

      ".bchr-title-block {",
      "  min-width: 0;",
      "}",

      ".bchr-package-name {",
      "  display: inline-block;",
      "  margin-bottom: 6px;",
      "  padding: 4px 9px;",
      "  border-radius: 8px;",
      "  background: #173F5F;",
      "  color: #FFFFFF;",
      "  font-family: Inter, 'Segoe UI', Arial, sans-serif;",
      "  font-size: 14px;",
      "  font-weight: 700;",
      "  letter-spacing: 0.02em;",
      "}",

      ".bchr-main-title {",
      "  margin: 0;",
      "  font-family: Inter, 'Segoe UI', Arial, sans-serif;",
      "  font-size: 27px;",
      "  font-weight: 700;",
      "  color: #173F5F;",
      "  line-height: 1.15;",
      "}",

      ".bchr-subtitle {",
      "  margin-top: 7px;",
      "  max-width: 760px;",
      "  font-family: Inter, 'Segoe UI', Arial, sans-serif;",
      "  font-size: 13.5px;",
      "  color: #475569;",
      "  line-height: 1.45;",
      "}",

      ".bchr-count {",
      "  font-family: Inter, 'Segoe UI', Arial, sans-serif;",
      "  font-size: 14px;",
      "  font-weight: 700;",
      "  color: #173F5F;",
      "}",

      ".bchr-source {",
      "  margin-top: 6px;",
      "  font-family: Inter, 'Segoe UI', Arial, sans-serif;",
      "  font-size: 11.5px;",
      "  color: #64748B;",
      "  line-height: 1.4;",
      "}",

      ".bchr-source a {",
      "  color: #245C78;",
      "  text-decoration: none;",
      "}",

      ".bchr-source a:hover {",
      "  text-decoration: underline;",
      "}",

      ".bchr-table-wrap {",
      "  padding: 14px 18px 10px 18px;",
      "  background: #FFFFFF;",
      "}",

      ".bchr-footer {",
      "  padding: 10px 22px 16px 22px;",
      "  font-family: Inter, 'Segoe UI', Arial, sans-serif;",
      "  font-size: 11.5px;",
      "  color: #64748B;",
      "  background: #FFFFFF;",
      "}",

      "@media (max-width: 900px) {",
      "  .bchr-header {",
      "    flex-direction: column;",
      "    align-items: flex-start;",
      "  }",
      "  .bchr-header-right {",
      "    align-items: flex-start;",
      "    text-align: left;",
      "    min-width: 0;",
      "  }",
      "  .bchr-main-title { font-size: 23px; }",
      "  .bchr-logo { max-height: 60px; max-width: 160px; }",
      "}",

      sep = "\n"
    )
  )
}


#' Build the BCH viewer header
#'
#' @param n_indicators Number of catalogue rows available.
#'
#' @return An `htmltools` tag.
#' @noRd
.bch_viewer_header <- function(n_indicators) {
  pkg_logo <- .bch_optional_viewer_image(
    c(
      "bchR_icon.png",
      "bchR_logo.png",
      "bchR_icon.svg",
      "bchR_logo.svg"
    )
  )

  institutional_logo <- .bch_optional_viewer_image(
    c(
      "bch_logo.png",
      "BCH_logo.png",
      "bch_logo.svg",
      "BCH_logo.svg"
    )
  )

  pkg_logo_tag <- if (!is.null(pkg_logo)) {
    htmltools::tags$img(
      src = pkg_logo,
      class = "bchr-logo",
      alt = "Logo de bchR"
    )
  } else {
    NULL
  }

  institutional_logo_tag <- if (!is.null(institutional_logo)) {
    htmltools::tags$img(
      src = institutional_logo,
      class = "bchr-logo",
      alt = "Identidad gr\u00e1fica del Banco Central de Honduras"
    )
  } else {
    NULL
  }

  htmltools::tags$div(
    class = "bchr-header",

    htmltools::tags$div(
      class = "bchr-header-left",
      pkg_logo_tag,
      htmltools::tags$div(
        class = "bchr-title-block",
        htmltools::tags$div(
          "bchR",
          class = "bchr-package-name"
        ),
        htmltools::tags$h1(
          "Cat\u00e1logo de indicadores del Banco Central de Honduras",
          class = "bchr-main-title"
        ),
        htmltools::tags$div(
          paste0(
            "Exploraci\u00f3n interactiva del cat\u00e1logo recuperado mediante la Web API ",
            "del Banco Central de Honduras."
          ),
          class = "bchr-subtitle"
        )
      )
    ),

    htmltools::tags$div(
      class = "bchr-header-right",
      institutional_logo_tag,
      htmltools::tags$div(
        paste0(
          "Indicadores disponibles: ",
          format(
            as.integer(n_indicators),
            big.mark = ",",
            scientific = FALSE,
            trim = TRUE
          )
        ),
        class = "bchr-count"
      ),
      htmltools::tags$div(
        "Fuente: Web API del Banco Central de Honduras",
        htmltools::tags$br(),
        htmltools::tags$a(
          href = "https://bchapi-am.developer.azure-api.net/",
          target = "_blank",
          rel = "noopener noreferrer",
          "Portal para desarrolladores"
        ),
        class = "bchr-source"
      )
    )
  )
}


#' Open a viewer without hard-wiring browser behavior in tests
#'
#' @param x Browsable HTML object.
#'
#' @return The temporary HTML path, invisibly.
#' @noRd
.bch_open_viewer <- function(x) {
  tmp <- tempfile(
    pattern = "bchR-indicators-",
    fileext = ".html"
  )

  libdir <- paste0(
    tools::file_path_sans_ext(basename(tmp)),
    "_files"
  )

  htmltools::save_html(
    x,
    file = tmp,
    libdir = libdir,
    lang = "es"
  )

  browser_fun <- getOption(
    "bchR.browser_fun",
    utils::browseURL
  )

  if (!is.function(browser_fun)) {
    stop(
      "The internal bchR browser function must be a function.",
      call. = FALSE
    )
  }

  browser_fun(tmp)
  invisible(tmp)
}
