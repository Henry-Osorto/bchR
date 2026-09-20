# Fixtures for bch_get_indicators() tests -----------------------------------

.bchr_indicator_records <- function(include_extra = FALSE,
                                    missing_correlative = FALSE) {
  records <- list(
    list(
      Id = 2L,
      Nombre = "  TEST-IND-2  ",
      Descripcion = "  Second indicator description  ",
      Periodicidad = " Mensual ",
      Grupo = " GROUP-A ",
      CorrelativoGrupo = " A2 "
    ),
    list(
      Id = 1L,
      Nombre = "TEST-IND-1",
      Descripcion = "First indicator description",
      Periodicidad = "Anual",
      Grupo = "GROUP-A",
      CorrelativoGrupo = "1"
    )
  )

  if (isTRUE(missing_correlative)) {
    records <- lapply(
      records,
      function(x) {
        x$CorrelativoGrupo <- NULL
        x
      }
    )
  }

  if (isTRUE(include_extra)) {
    records <- lapply(
      records,
      function(x) {
        x$NuevaColumna <- "new-value"
        x
      }
    )
  }

  records
}
