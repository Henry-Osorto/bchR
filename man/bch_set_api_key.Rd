\name{bch_set_api_key}
\alias{bch_set_api_key}
\title{Configure the BCH Web API key}
\usage{
bch_set_api_key()
}
\value{
\code{TRUE} invisibly when the key is stored successfully; \code{FALSE}
invisibly when no key is entered.
}
\description{
Securely prompts for the user's Banco Central de Honduras (BCH) Web API Primary
key, stores it in the user-level \code{.Renviron} file, and makes it available
immediately in the current R session.
}
\details{
Existing \code{BCH_API_KEY} declarations in the selected user \code{.Renviron}
file are replaced by a single declaration. Other variables and comments are
preserved. The key is requested with \code{askpass::askpass()} and is not echoed
in the R console.
}
\examples{
\dontrun{
bch_set_api_key()
}
}
\seealso{
\url{https://bchapi-am.developer.azure-api.net/}
}
