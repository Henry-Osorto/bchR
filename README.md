# bchR

`bchR` is an R interface to the Web API of the Banco Central de Honduras (BCH).
The first functional version provides secure API-key configuration, access to
the indicator catalogue, an interactive catalogue viewer, and retrieval of
individual indicator series.

## Installation

Once the package is available from CRAN:

```r
install.packages("bchR")
library(bchR)
```

For a local source checkout, install it with your usual R package development
workflow (for example, `devtools::install()`).

## API key

Use of the BCH Web API requires an account and an active subscription at:

<https://bchapi-am.developer.azure-api.net/>

After registration and subscription, open the profile/subscription information
and reveal the **Primary key**.

When `library(bchR)` is called in an interactive R session and
`BCH_API_KEY` is not already available, the package explains these steps and
opens a secure password prompt. The key is stored in the user-level
`.Renviron` file and is also made available in the current R session. It is not
printed by the package.

You can configure or replace the key manually at any time:

```r
bch_set_api_key()
```

For non-interactive environments (CI, Quarto, scheduled jobs, servers), define
`BCH_API_KEY` in the environment before calling the package. Package startup
does not prompt in non-interactive sessions.

Automatic prompting in an interactive development session can be disabled
before package attachment with:

```r
options(bchR.prompt_api_key = FALSE)
library(bchR)
```

## Indicator catalogue

```r
catalogue <- bch_get_indicators()
head(catalogue)
```

The returned data frame preserves the BCH catalogue fields and includes
secret-free provenance attributes.

## Interactive catalogue viewer

```r
bch_viewer_indicators()
```

To save the viewer to a temporary HTML file and open it in the system browser:

```r
bch_viewer_indicators(open_browser = TRUE)
```

## Indicator data

```r
data <- bch_get_data(609)
head(data)
```

`Fecha` is returned as `Date`, `Valor` as numeric, and observations are ordered
chronologically. Potential duplicate observations are reported but are not
silently removed.

## Security

- Do not place API keys directly in scripts, examples, tests, or version control.
- Keep `.Renviron` out of Git.
- `bchR` does not include the authentication query parameter in provenance
  attributes.
- Public error messages are designed not to reproduce the authenticated request
  URL.
