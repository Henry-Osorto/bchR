# Fixtures for bch_viewer_indicators() tests --------------------------------

.bchr_viewer_catalogue <- function(n = 6L) {
  n <- as.integer(n)

  data.frame(
    Id = seq_len(n),
    Nombre = paste("INDICADOR", seq_len(n)),
    Descripcion = paste(
      "Descripción oficial de prueba para el indicador",
      seq_len(n)
    ),
    Periodicidad = rep(
      c("Mensual", "Anual", "Trimestral", "Diario"),
      length.out = n
    ),
    Grupo = paste("GRUPO", ((seq_len(n) - 1L) %% 20L) + 1L),
    CorrelativoGrupo = as.character(seq_len(n)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}


.bchr_viewer_records <- function(n = 6L) {
  data <- .bchr_viewer_catalogue(n)
  unname(split(data, seq_len(nrow(data)))) |>
    lapply(function(row) as.list(row[1, , drop = FALSE]))
}
