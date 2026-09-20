\name{bch_viewer_indicators}
\alias{bch_viewer_indicators}
\title{Browse BCH indicators in an interactive table}
\usage{
bch_viewer_indicators(
  progress = TRUE,
  show_search = TRUE,
  striped = TRUE,
  bordered = FALSE,
  compact = FALSE,
  highlight = TRUE,
  full_width = TRUE,
  page_size = 15,
  open_browser = FALSE
)
}
\arguments{
\item{progress}{Logical. Show progress messages from \code{bch_get_indicators()}.}
\item{show_search}{Logical. Enable global search.}
\item{striped}{Logical. Use alternating row shading.}
\item{bordered}{Logical. Display table borders.}
\item{compact}{Logical. Reduce table-cell padding.}
\item{highlight}{Logical. Highlight rows on hover.}
\item{full_width}{Logical. Use the available width.}
\item{page_size}{Positive whole number of rows per page.}
\item{open_browser}{Logical. Save to temporary HTML and open the system browser.}
}
\value{
A browsable HTML object containing a BCH-specific header and a
\code{reactable} table.
}
\description{
Creates an interactive HTML viewer for the BCH indicator catalogue. Its only
data source is \code{bch_get_indicators()}.
}
\details{
Columns are sortable and resizable, pagination is enabled, and the interface is
localized in Spanish. Long descriptions wrap instead of being deliberately
truncated. Optional locally bundled logos can be embedded as data URIs; the
viewer does not download external images at runtime.
}
\examples{
\dontrun{
bch_viewer_indicators()
bch_viewer_indicators(page_size = 25, open_browser = TRUE)
}
}
\seealso{\code{\link{bch_get_indicators}}, \code{\link{bch_set_api_key}}}
