# Explicit opt-in integration probe, never used by tests, examples or CI.
# Rscript --no-init-file --no-restore --no-save dev/validate-live.R --run --id=<digits>
# Do not use --vanilla if relying on your personal .Renviron.
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1L])
package_dir <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
args <- commandArgs(trailingOnly = TRUE)
id_arg <- grep("^--id=[0-9]+$", args, value = TRUE)
if (!"--run" %in% args || length(id_arg) != 1L) {
  stop("Opt-in required: supply --run and exactly one --id=<digits>.", call. = FALSE)
}
if (!nzchar(Sys.getenv("BCH_API_KEY", unset = ""))) {
  stop("Configure BCH_API_KEY locally; never paste it into a conversation.", call. = FALSE)
}
pkgload::load_all(package_dir, quiet = TRUE)
id <- sub("^--id=", "", id_arg)
catalog <- bch_indicators(source = "live", cache_dir = FALSE)
data <- bch_get_data(id = id, catalog = catalog, source = "live",
  cache_dir = FALSE, progress = FALSE, unknown_frequency = "keep")
out <- file.path(package_dir, "dev", "artifacts", "live")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
report <- list(
  executed_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  package_version = as.character(utils::packageVersion("bchR")),
  catalog_rows = nrow(bch_data(catalog)),
  catalog_columns = names(bch_data(catalog)),
  frequency_labels = sort(unique(bch_data(catalog)$frequency_raw)),
  requested_indicator_id = id, observations = nrow(bch_data(data)),
  observation_columns = names(bch_data(data)),
  provenance = bch_provenance(data),
  warning = "One successful read does not verify every frequency or pagination."
)
jsonlite::write_json(report, file.path(out, "integration-summary.json"),
                    auto_unbox = TRUE, pretty = TRUE, na = "null", POSIXt = "ISO8601")
message("Authenticated read completed. Sanitized summary saved; no raw bodies retained.")
