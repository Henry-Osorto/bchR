\name{bch_get_indicators}
\alias{bch_get_indicators}
\title{Retrieve the BCH indicator catalogue}
\usage{
bch_get_indicators(progress = TRUE)
}
\arguments{
\item{progress}{Logical. If \code{TRUE}, download and processing messages are displayed.}
}
\value{
A base \code{data.frame} containing at minimum \code{Id}, \code{Nombre},
\code{Descripcion}, \code{Periodicidad}, \code{Grupo}, and
\code{CorrelativoGrupo}. Secret-free provenance attributes are attached.
}
\description{
Downloads the current indicator catalogue from the Banco Central de Honduras
(BCH) Web API using the package's internal authenticated HTTP client.
}
\details{
Only leading and trailing whitespace is removed from character fields.
\code{CorrelativoGrupo} is kept as character. Row order is preserved as returned
by the API. The function does not deduplicate, filter, translate, cache, or
otherwise reorganize the catalogue.
}
\examples{
\dontrun{
indicators <- bch_get_indicators()
head(indicators)
table(indicators$Periodicidad, useNA = "ifany")
indicators <- bch_get_indicators(progress = FALSE)
}
}
\seealso{\code{\link{bch_set_api_key}}}
