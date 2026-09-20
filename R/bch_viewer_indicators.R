# BCH indicator viewer -------------------------------------------------------

.bch_viewer_columns <- c(
  "Id",
  "Nombre",
  "Descripcion",
  "Periodicidad",
  "Grupo",
  "CorrelativoGrupo"
)


#' Validate the default viewer page size
#'
#' @param page_size Requested rows per page.
#'
#' @return Integer page size.
#' @noRd
.bch_validate_viewer_page_size <- function(page_size) {
  valid <-
    is.numeric(page_size) &&
    length(page_size) == 1L &&
    !is.na(page_size) &&
    is.finite(page_size) &&
    page_size > 0 &&
    page_size == floor(page_size)

  if (!valid) {
    stop(
      "page_size must be a single positive whole number.",
      call. = FALSE
    )
  }

  as.integer(page_size)
}


#' Validate catalogue data before building the viewer
#'
#' @param data Indicator catalogue.
#'
#' @return The six viewer columns as a base `data.frame`.
#' @noRd
.bch_validate_viewer_data <- function(data) {
  if (!is.data.frame(data)) {
    stop(
      "bch_get_indicators() did not return a data frame.",
      call. = FALSE
    )
  }

  if (nrow(data) == 0L) {
    stop(
      "No BCH indicators are available to display.",
      call. = FALSE
    )
  }

  missing_columns <- setdiff(
    .bch_viewer_columns,
    names(data)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "The indicator catalogue cannot be displayed because it is missing: ",
        paste(missing_columns, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  out <- data[.bch_viewer_columns]
  rownames(out) <- NULL
  out
}


#' Create the reactable column specification for the BCH catalogue
#'
#' @return A named list of `reactable::colDef()` objects.
#' @noRd
.bch_viewer_column_definitions <- function() {
  header_style <- list(
    background = "#173F5F",
    color = "#FFFFFF",
    fontWeight = "700",
    fontSize = "13px",
    borderColor = "#173F5F"
  )

  list(
    Id = reactable::colDef(
      name = "ID",
      minWidth = 82,
      initWidth = 95,
      maxWidth = 135,
      align = "center",
      sticky = "left",
      rowHeader = TRUE,
      headerStyle = header_style,
      style = list(
        fontSize = "12.5px",
        fontWeight = "700",
        color = "#173F5F",
        background = "#F8FAFC"
      )
    ),

    Nombre = reactable::colDef(
      name = "Nombre",
      minWidth = 250,
      initWidth = 330,
      headerStyle = header_style,
      style = list(
        fontSize = "12.5px",
        fontWeight = "600",
        lineHeight = "1.35",
        whiteSpace = "normal",
        overflowWrap = "anywhere"
      )
    ),

    Descripcion = reactable::colDef(
      name = "Descripción",
      minWidth = 340,
      initWidth = 500,
      headerStyle = header_style,
      style = list(
        fontSize = "12.5px",
        lineHeight = "1.45",
        color = "#334155",
        whiteSpace = "normal",
        overflowWrap = "anywhere"
      )
    ),

    Periodicidad = reactable::colDef(
      name = "Periodicidad",
      minWidth = 120,
      initWidth = 135,
      align = "center",
      headerStyle = header_style,
      style = list(
        fontSize = "12.5px",
        fontWeight = "500"
      )
    ),

    Grupo = reactable::colDef(
      name = "Grupo",
      minWidth = 220,
      initWidth = 300,
      headerStyle = header_style,
      style = list(
        fontSize = "12.5px",
        lineHeight = "1.4",
        whiteSpace = "normal",
        overflowWrap = "anywhere"
      )
    ),

    CorrelativoGrupo = reactable::colDef(
      name = "Correlativo del grupo",
      minWidth = 150,
      initWidth = 175,
      align = "center",
      headerStyle = header_style,
      style = list(
        fontSize = "12.5px"
      )
    )
  )
}


#' Build the BCH indicator viewer from an already retrieved catalogue
#'
#' This internal separation keeps HTML construction testable without adding a
#' second data-access path. The public viewer still obtains its data exclusively
#' through `bch_get_indicators()`.
#'
#' @param data Indicator catalogue returned by `bch_get_indicators()`.
#' @param show_search,striped,bordered,compact,highlight,full_width Logical
#'   reactable controls.
#' @param page_size Positive integer page size.
#'
#' @return A browsable HTML object.
#' @noRd
.bch_build_indicator_viewer <- function(data,
                                        show_search,
                                        striped,
                                        bordered,
                                        compact,
                                        highlight,
                                        full_width,
                                        page_size) {
  data <- .bch_validate_viewer_data(data)

  page_options <- sort(
    unique(
      as.integer(
        c(10L, 15L, 25L, 50L, 100L, page_size)
      )
    )
  )

  table_block <- reactable::reactable(
    data,
    rownames = FALSE,
    searchable = show_search,
    filterable = FALSE,
    sortable = TRUE,
    striped = striped,
    bordered = bordered,
    highlight = highlight,
    compact = compact,
    fullWidth = full_width,
    pagination = TRUE,
    defaultPageSize = page_size,
    showPageSizeOptions = TRUE,
    pageSizeOptions = page_options,
    resizable = TRUE,
    columns = .bch_viewer_column_definitions(),
    defaultColDef = reactable::colDef(
      sortable = TRUE,
      minWidth = 100,
      align = "left",
      style = list(
        fontSize = "12.5px",
        whiteSpace = "normal"
      )
    ),
    outlined = FALSE,
    wrap = TRUE,
    showSortIcon = TRUE,
    showSortable = TRUE,
    selection = NULL,
    paginationType = "jump",
    theme = reactable::reactableTheme(
      color = "#1E293B",
      backgroundColor = "#FFFFFF",
      borderColor = "#E2E8F0",
      stripedColor = "#F8FAFC",
      highlightColor = "#EEF6F7",
      cellPadding = if (compact) "6px 8px" else "9px 10px",
      style = list(
        fontFamily = "Inter, Segoe UI, Arial, sans-serif",
        fontSize = "12.5px"
      ),
      headerStyle = list(
        fontWeight = "700"
      ),
      searchInputStyle = list(
        border = "1px solid #CBD5E1",
        borderRadius = "8px",
        padding = "8px 10px",
        fontSize = "12.5px",
        width = "min(360px, 100%)"
      ),
      pageButtonStyle = list(
        borderRadius = "8px",
        border = "1px solid #CBD5E1",
        padding = "6px 10px",
        background = "#FFFFFF"
      ),
      pageButtonHoverStyle = list(
        background = "#F1F5F9"
      ),
      pageButtonActiveStyle = list(
        background = "#173F5F",
        color = "#FFFFFF",
        border = "1px solid #173F5F"
      ),
      pageButtonCurrentStyle = list(
        background = "#173F5F",
        color = "#FFFFFF",
        border = "1px solid #173F5F"
      )
    ),
    language = reactable::reactableLang(
      searchPlaceholder = "Buscar...",
      searchLabel = "Buscar",
      noData = "No se encontraron registros",
      pageNext = "Siguiente",
      pagePrevious = "Anterior",
      pageNumbers = "{page} de {pages}",
      pageInfo = "{rowStart}\u2013{rowEnd} de {rows} registros",
      pageSizeOptions = "Mostrar {rows}",
      pageNextLabel = "Página siguiente",
      pagePreviousLabel = "Página anterior",
      pageJumpLabel = "Ir a la página",
      pageSizeOptionsLabel = "Filas por página"
    )
  )

  container <- htmltools::tags$div(
    class = "bchr-viewer",
    .bch_viewer_style(),
    .bch_viewer_header(nrow(data)),
    htmltools::tags$div(
      class = "bchr-table-wrap",
      table_block
    ),
    htmltools::tags$div(
      "Generado con bchR",
      class = "bchr-footer"
    )
  )

  htmltools::browsable(container)
}


#' Browse BCH indicators in an interactive table
#'
#' @description
#' Creates an interactive HTML viewer for the indicator catalogue of the Banco
#' Central de Honduras (BCH). The catalogue is obtained exclusively through
#' [bch_get_indicators()], so this function does not construct API URLs or
#' manage `BCH_API_KEY` directly.
#'
#' @param progress Logical. If `TRUE` (default), progress messages generated by
#'   [bch_get_indicators()] are displayed.
#' @param show_search Logical. If `TRUE` (default), enables global search over
#'   the displayed catalogue columns.
#' @param striped Logical. If `TRUE` (default), alternating row shading is used.
#' @param bordered Logical. If `TRUE`, table borders are displayed. Defaults to
#'   `FALSE`.
#' @param compact Logical. If `TRUE`, reduces table-cell padding. Defaults to
#'   `FALSE`.
#' @param highlight Logical. If `TRUE` (default), rows are highlighted on hover.
#' @param full_width Logical. If `TRUE` (default), the table uses the available
#'   width.
#' @param page_size Positive whole number. Number of rows shown per page.
#'   Defaults to `15`.
#' @param open_browser Logical. If `TRUE`, the generated viewer is saved to a
#'   temporary HTML file and opened in the system browser. Defaults to `FALSE`.
#'
#' @return A browsable HTML object containing a BCH-specific header and a
#'   `reactable` table. The same object is returned when `open_browser = TRUE`.
#'
#' @details
#' The viewer displays the standard catalogue fields `Id`, `Nombre`,
#' `Descripcion`, `Periodicidad`, `Grupo`, and `CorrelativoGrupo`, using Spanish
#' interface labels. Columns are sortable and resizable, pagination is enabled,
#' and users can change the number of rows per page.
#'
#' `Descripcion` is rendered with wrapping so long official descriptions can be
#' read without truncating their content. The viewer does not modify the
#' catalogue returned by [bch_get_indicators()].
#'
#' Logos are optional. If compatible image files have explicitly been bundled
#' under `inst/images/`, they are embedded locally as data URIs. The function
#' never downloads external images at runtime and does not require logos to be
#' present.
#'
#' The current implementation intentionally retains `reactable`, consistent
#' with the viewer design used in `CepalStatR`. The BCH catalogue is a
#' client-side table, so very large catalogues can increase HTML/widget size and
#' browser memory use. Pagination limits the number of rows rendered visibly at
#' one time, while global search still operates over the client-side catalogue.
#'
#' @seealso [bch_get_indicators()], [bch_set_api_key()]
#'
#' @examples
#' \dontrun{
#' bch_viewer_indicators()
#'
#' bch_viewer_indicators(
#'   page_size = 25,
#'   open_browser = TRUE
#' )
#'
#' bch_viewer_indicators(
#'   progress = FALSE,
#'   show_search = TRUE,
#'   compact = TRUE
#' )
#' }
#'
#' @export
bch_viewer_indicators <- function(progress = TRUE,
                                  show_search = TRUE,
                                  striped = TRUE,
                                  bordered = FALSE,
                                  compact = FALSE,
                                  highlight = TRUE,
                                  full_width = TRUE,
                                  page_size = 15,
                                  open_browser = FALSE) {
  .bch_validate_flag(progress, "progress")
  .bch_validate_flag(show_search, "show_search")
  .bch_validate_flag(striped, "striped")
  .bch_validate_flag(bordered, "bordered")
  .bch_validate_flag(compact, "compact")
  .bch_validate_flag(highlight, "highlight")
  .bch_validate_flag(full_width, "full_width")
  .bch_validate_flag(open_browser, "open_browser")

  page_size <- .bch_validate_viewer_page_size(page_size)

  data <- bch_get_indicators(
    progress = progress
  )

  out <- .bch_build_indicator_viewer(
    data = data,
    show_search = show_search,
    striped = striped,
    bordered = bordered,
    compact = compact,
    highlight = highlight,
    full_width = full_width,
    page_size = page_size
  )

  if (isTRUE(open_browser)) {
    .bch_open_viewer(out)
  }

  out
}
