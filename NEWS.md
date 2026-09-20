# bchR 0.1.0

## Initial functional version

- Added secure configuration of `BCH_API_KEY` with `bch_set_api_key()`.
- Added retrieval and validation of the BCH indicator catalogue with
  `bch_get_indicators()`.
- Added an interactive indicator catalogue viewer with
  `bch_viewer_indicators()`.
- Added retrieval of individual indicator series with `bch_get_data()`.
- Added explicit validation of BCH catalogue and series schemas.
- Added conservative conversion of `Fecha` to `Date` and `Valor` to numeric.
- Added secret-free provenance attributes to returned data frames.
- Added HTTP status handling, transient retries, timeout controls and
  credential redaction.
- Added automated tests and cross-platform R CMD check workflows.
