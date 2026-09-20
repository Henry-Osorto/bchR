\name{bch_get_data}
\alias{bch_get_data}
\title{Retrieve data for a BCH indicator}
\usage{
bch_get_data(indicator_id, progress = TRUE)
}
\arguments{
\item{indicator_id}{A single positive BCH indicator ID supplied as a whole-number numeric value or a character string of decimal digits.}
\item{progress}{Logical. If \code{TRUE}, download and processing messages are displayed.}
}
\value{
A base \code{data.frame} containing at minimum \code{Id}, \code{IndicadorId},
\code{Nombre}, \code{Descripcion}, \code{Fecha}, and \code{Valor}.
\code{Fecha} is a \code{Date}, \code{Valor} is numeric, and observations are
ordered chronologically. Secret-free provenance attributes are attached.
}
\description{
Downloads all observations currently returned by the BCH Web API for a single
indicator and performs only minimal documented transformations.
}
\details{
The function does not filter dates, truncate the sample, interpolate missing
observations, calculate lags, infer frequency, convert to \code{ts}, or fit
models. Potential duplicate observations are warned about and preserved rather
than removed silently.
}
\examples{
\dontrun{
inflation <- bch_get_data(609)
head(inflation)
tail(inflation)
str(inflation)
inflation <- bch_get_data("609", progress = FALSE)
}
}
\seealso{\code{\link{bch_get_indicators}}, \code{\link{bch_set_api_key}}}
