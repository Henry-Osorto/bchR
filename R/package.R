#' Cliente de desarrollo para la Web API del BCH
#'
#' Cliente independiente en desarrollo. La API publica utiliza el prefijo
#' `bch_`; la validacion autenticada y la licencia de distribucion estan pendientes.
#'
#' @importFrom tibble as_tibble
#' @keywords internal
#' @examples
#' catalog <- data.frame(
#'   indicator_id = "001", indicator_code = "DEMO-001",
#'   indicator_description = "Synthetic demonstration indicator",
#'   frequency_raw = "Diario", frequency = "daily",
#'   group_code = "DEMO", correlative = "01"
#' )
#' found <- bch_search_indicators(catalog, "synthetic")
#' bch_data(found)
#' bch_provenance(found) # Empty: user-supplied demonstration, not an API request
#' if (requireNamespace("htmltools", quietly = TRUE)) {
#'   html <- bch_view_indicators(catalog)
#' }
"_PACKAGE"
