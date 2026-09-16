# bchR

Acceso reproducible a indicadores y series estadísticas del Banco Central de Honduras desde R.

**En desarrollo:** versión `0.0.9001`. Paquete independiente; no es un producto
oficial del Banco Central de Honduras. No está publicado en CRAN y la integración
autenticada aún está pendiente de validación.

[Portal de la API](https://bchapi-am.developer.azure-api.net/) ·
[Antecedente científico](https://doi.org/10.4028/p-Br9R7i)

<!-- Activar este badge solo después de crear el repositorio y ejecutar el workflow:
[![R-CMD-check](https://github.com/Henry-Osorto/bchR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Henry-Osorto/bchR/actions/workflows/R-CMD-check.yaml)
-->

## Descripción

`bchR` reúne descubrimiento de indicadores, descarga de cifras y conservación de
metadatos/procedencia en una interfaz común. Está orientado a investigación,
docencia y análisis reproducible de estadísticas hondureñas.

El diseño mantiene continuidad conceptual con [CepalStatR](https://github.com/Henry-Osorto/CepalStatR),
pero usa nombres con prefijo `bch_` y no pretende ser un reemplazo compatible de su API.

## Funciones principales

| Función | Propósito |
|---|---|
| `bch_indicators()` | Obtener catálogo y metadatos; seleccionar IDs, grupos o frecuencia canónica. |
| `bch_search_indicators()` | Buscar localmente por texto literal, campos, mayúsculas y tildes. |
| `bch_get_data()` | Obtener una o varias series, o un grupo, con límite explícito de solicitudes. |
| `bch_view_indicators()` | Explorar el catálogo HTML: búsqueda, filtros, ordenación, páginas y copia de ID. |
| `bch_plot_series()` | Graficar datos suministrados sin interpolación ni transformación implícita. |
| `bch_cache_info()` / `bch_clear_cache()` | Inspeccionar o limpiar la caché local de forma controlada. |

## Instalación

En la carpeta de este paquete, una instalación local puede hacerse así:

```r
install.packages("remotes")       # solo si hace falta
remotes::install_local(".", dependencies = NA)
library(bchR)
```

Los componentes visuales requieren `htmltools` y `ggplot2`:

```r
install.packages(c("htmltools", "ggplot2"))
```

Una vez que el autor haya creado y publicado `Henry-Osorto/bchR`, la instalación
de desarrollo será `remotes::install_github("Henry-Osorto/bchR")`. Esa dirección
es el destino previsto, no una afirmación de disponibilidad actual. No uses
`install.packages("bchR")` esperando una versión de CRAN todavía.

## Primer ejemplo sin conexión

Este catálogo es **ficticio**, no contiene cifras ni identificadores oficiales:

```r
library(bchR)

catalogo <- data.frame(
  indicator_id = c("001", "002"),
  indicator_code = c("DEMO-01", "DEMO-02"),
  indicator_description = c("Precios simulados", "Actividad simulada"),
  frequency_raw = c("Diario", "Diario"),
  frequency = c("daily", "daily"),
  group_code = c("DEMO-PRECIOS", "DEMO-ACTIVIDAD"),
  correlative = c("01", "01")
)
resultado <- bch_search_indicators(catalogo, "precios")
bch_data(resultado)
bch_view_indicators(catalogo)  # devuelve HTML; no abre navegador ni guarda archivos

serie <- data.frame(
  indicator_id = "DEMO",
  reference_date = as.Date("2025-01-01") + 0:5,
  frequency = "daily", unit = "Indice ficticio",
  value = c(100, 101, NA, 102, 101, 103)
)
bch_plot_series(serie, title = "Demostracion con datos ficticios")
```

Para guardar explícitamente el visor o el gráfico:

```r
htmltools::save_html(bch_view_indicators(catalogo), "catalogo-demo.html")
ggplot2::ggsave("serie-demo.png", bch_plot_series(serie), width = 8, height = 5)
```

## Credencial individual del BCH

Cada usuario debe registrarse en el portal del BCH y configurar su propia clave.
El paquete **no incluye una clave compartida**. Los términos del BCH exigen
confidencialidad y uso exclusivo de las credenciales. La clave del desarrollador
tampoco debe distribuirse dentro de código, HTML, ejemplos ni GitHub.
[Condiciones de la Web API](https://www.bch.hn/marco-legal/terminos-condiciones-uso-web-api).

Guarda `BCH_API_KEY` en tu archivo personal `.Renviron` con un editor, fuera del
repositorio; puedes abrirlo con `usethis::edit_r_environ(scope = "user")` si tienes
instalado `usethis`. Reinicia R después de editarlo. Comprueba solo presencia:

```r
nzchar(Sys.getenv("BCH_API_KEY", unset = ""))
```

No imprimas `Sys.getenv("BCH_API_KEY")`, no escribas la clave en la consola y no
compartas el archivo. El valor pertenece al entorno local, no a `DESCRIPTION` ni
a las opciones públicas del paquete. El token de GitHub es una credencial distinta.

## Consultas reales: ejemplo para validar

Los siguientes comandos requieren una suscripción válida; no se ejecutan durante
los ejemplos offline o la integración continua:

```r
catalogo_bch <- bch_indicators(source = "live", cache_dir = FALSE)
candidatos <- bch_search_indicators(catalogo_bch, "producto interno bruto")
bch_data(candidatos) # revisar códigos, grupos y frecuencia antes de seleccionar

# Reemplaza el marcador por un ID que hayas revisado en el catálogo real.
id_elegido <- "ID_VERIFICADO"
datos <- bch_get_data(id = id_elegido, catalog = catalogo_bch,
  source = "live", cache_dir = FALSE, unknown_frequency = "keep")
bch_data(datos)
bch_metadata(datos)
bch_provenance(datos)
bch_problems(datos)
```

`id` y `group` son excluyentes en la descarga. `max_series = 25` protege frente a
selecciones accidentales muy grandes. Antes de descargar un grupo, revisa su
tamaño en el catálogo; no eleves el límite sin evaluar el coste y las condiciones
del servicio. Las solicitudes son secuenciales.

## Resultado y procedencia

Las consultas devuelven un `bch_result` con cinco tablas estables:

- `data`: catálogo u observaciones.
- `metadata`: metadatos por indicador.
- `provenance`: origen, recuperación, endpoint sin credencial, hash, versión del cliente y estado de caché.
- `problems`: fallos parciales estructurados; vacía si no hay problemas registrados.
- `column_map`: correspondencia de columnas cuando se solicita formato ancho.

`bch_data(x)`, `tibble::as_tibble(x)` y `as.data.frame(x)` extraen solo los datos.
Conserva `x` completo si necesitas auditar la procedencia. `on_error = "collect"`
permite resultados parciales: siempre inspecciona `bch_problems(x)` antes de analizar.
`on_error = "stop"` no devuelve un resultado parcial, pero no revierte caché ya escrita.

## Frecuencias y limitaciones actuales

Solo la equivalencia `Diario` → `daily` está implementada con respaldo del ejemplo
oficial inspeccionado. Aún falta verificar, con respuestas autenticadas, el catálogo
completo, la paginación y las reglas mensual, trimestral y anual.

Las etiquetas no verificadas se conservan como `unknown`; la descarga es estricta
por defecto. `unknown_frequency = "keep"` permite retener valores y fechas originales
sin inventar períodos. Para esas series no se permite filtrar fechas ni usar salida
ancha. No se debe interpretar una fecha completa como evidencia de frecuencia diaria.

La forma larga es predeterminada. La salida ancha exige frecuencia conocida común,
usa nombres `id_<ID>` y conserva cobertura desigual mediante `NA`. Los filtros
`start`/`end` se aplican localmente, con solapamiento inclusivo de períodos.
No hay agregación, relleno, desestacionalización o cálculo de crecimiento automático.

El gráfico admite tablas propias con frecuencia canónica explícita, separa unidades
y mantiene huecos. Frecuencia desconocida implica puntos, no líneas de continuidad.
Por ahora solo grafica niveles; panel macroeconómico, indicadores derivados y
pronósticos permanecen en la hoja de ruta, no son funcionalidades disponibles.

## Caché y reproducibilidad

La caché predeterminada está en `tools::R_user_dir("bchR", "cache")`. Puede desactivarse
con `cache_dir = FALSE`. TTL: 24 horas para catálogo y 6 horas para cifras.
`max_age` modifica ese umbral en segundos.

- `source = "cache"` nunca conecta; puede devolver contenido vencido marcado como tal.
- `source = "live"` exige una consulta nueva.
- `source = "auto"` usa caché vigente y puede recurrir a una vencida con advertencia
  ante clave ausente o fallo transitorio, no ante credencial rechazada.
- `refresh = TRUE` fuerza consulta sin respaldo silencioso.

La caché contiene respuestas públicas validadas y metadatos, no solicitudes ni
credenciales. Un hash comprueba integridad, no sustituye autenticación del origen.
Las series del BCH pueden revisarse: conserva fecha y procedencia al comparar resultados.
`bch_clear_cache()` simula la limpieza salvo que indiques `dry_run = FALSE`.

## Desarrollo y calidad

Abre `bchR.Rproj`. Con las dependencias instaladas puedes ejecutar:

```r
devtools::document()
devtools::test()
devtools::check(args = "--no-manual")
```

Las pruebas usan datos sintéticos y respuestas simuladas, sin clave del BCH.
Se incluye un workflow para Windows, Linux y macOS; su presencia no significa que
ya se haya ejecutado en GitHub. La guía inicial está en la viñeta `bchR`.

Para contribuir o informar incidencias, consulta [CONTRIBUTING.md](CONTRIBUTING.md)
y [SECURITY.md](SECURITY.md). Nunca adjuntes credenciales o respuestas privadas.

## Autoría, cita y antecedentes

Autor y mantenedor: Henry Osorto, [ORCID](https://orcid.org/0000-0002-4334-9179).
Para citar la versión instalada del software usa `citation("bchR")`; se incluyen
`inst/CITATION` y `CITATION.cff`. No se ha asignado un DOI al paquete.

Antecedente científico: Osorto y Delcid Carrasco, *Web API of the Central Bank of
Honduras: Evaluation of Its Integration in R and Perspectives for the Development
of a Specialized Package*, [DOI 10.4028/p-Br9R7i](https://doi.org/10.4028/p-Br9R7i).
Ese artículo es el antecedente, no una publicación que documente esta versión ya validada.

## Licencia y fuente de datos

El código de `bchR` se distribuye bajo la [licencia MIT](LICENSE.md).
Copyright (c) 2026 Henry Osorto. `LICENSE.md` contiene el texto completo y
`LICENSE` los datos complementarios de titularidad para el paquete R.
Esta licencia no relicencia datos, marcas, credenciales ni dependencias de terceros.

Los términos del BCH se aplican separadamente a sus datos y credenciales. En las
salidas que usen sus cifras conserva la atribución **Fuente: Banco Central de Honduras**.
No se incluyen logos oficiales ni se implica patrocinio institucional. Las demos
sintéticas se identifican como tales, sin atribuirlas al banco.

## English summary

`bchR` is an independent R development client for BCH indicator discovery,
time-series retrieval, provenance, offline caching and descriptive visualization.
Each user must supply their own local API key. Authenticated integration and
non-daily BCH frequency mappings are not yet verified. This is not a CRAN release
or an official BCH product. The package code is distributed under the MIT license;
third-party data, trademarks, credentials and dependencies retain their own terms.
