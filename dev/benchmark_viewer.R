# Developer benchmark for the full BCH indicator viewer ---------------------
#
# Run manually after installing/loading bchR and configuring BCH_API_KEY.
# This script is intentionally not part of the automated test suite because
# network speed and browser/CPU performance are environment-dependent.

library(bchR)

catalogue <- bch_get_indicators(progress = FALSE)

cat("Rows:", format(nrow(catalogue), big.mark = ","), "\n")
cat("Columns:", ncol(catalogue), "\n")
cat("Catalogue object size:", format(object.size(catalogue), units = "auto"), "\n")

build_time <- system.time({
  viewer <- bchR:::.bch_build_indicator_viewer(
    data = catalogue,
    show_search = TRUE,
    striped = TRUE,
    bordered = FALSE,
    compact = FALSE,
    highlight = TRUE,
    full_width = TRUE,
    page_size = 15L
  )
})

cat("Viewer construction time (seconds):\n")
print(build_time)
cat("Viewer R object size:", format(object.size(viewer), units = "auto"), "\n")

tmp <- tempfile(fileext = ".html")
libdir <- paste0(tools::file_path_sans_ext(basename(tmp)), "_files")

save_time <- system.time({
  htmltools::save_html(
    viewer,
    file = tmp,
    libdir = libdir,
    lang = "es"
  )
})

cat("HTML save time (seconds):\n")
print(save_time)
cat("Main HTML size:", format(file.info(tmp)$size, big.mark = ","), "bytes\n")
cat("HTML path:", tmp, "\n")
