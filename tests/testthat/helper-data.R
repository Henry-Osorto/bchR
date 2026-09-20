# Fixtures for bch_get_data() tests -----------------------------------------

.bchr_data_records <- function(
    indicator_id = 609L,
    include_duplicate = FALSE,
    include_extra = FALSE,
    malformed_date = FALSE,
    missing_value = FALSE,
    mismatch_indicator = FALSE
) {
  returned_id <- if (isTRUE(mismatch_indicator)) 610L else indicator_id

  records <- list(
    list(
      Id = 103L,
      IndicadorId = returned_id,
      Nombre = "  TEST INDICATOR  ",
      Descripcion = "  Official test description  ",
      Fecha = "2024-03-01T00:00:00",
      Valor = 3.25
    ),
    list(
      Id = 101L,
      IndicadorId = returned_id,
      Nombre = "TEST INDICATOR",
      Descripcion = "Official test description",
      Fecha = "2024-01-01T00:00:00",
      Valor = 1.25
    ),
    list(
      Id = 102L,
      IndicadorId = returned_id,
      Nombre = "TEST INDICATOR",
      Descripcion = "Official test description",
      Fecha = "2024-02-01T00:00:00",
      Valor = if (isTRUE(missing_value)) NA_real_ else 2.25
    )
  )

  if (isTRUE(malformed_date)) {
    records[[2L]]$Fecha <- "2024-99-01"
  }

  if (isTRUE(include_duplicate)) {
    # Mirrors the observed BCH pattern: same substantive information, but a
    # different technical observation Id.
    records[[4L]] <- records[[3L]]
    records[[4L]]$Id <- 202L
  }

  if (isTRUE(include_extra)) {
    records <- lapply(
      records,
      function(x) {
        x$UnidadPrueba <- "index"
        x
      }
    )
  }

  records
}
