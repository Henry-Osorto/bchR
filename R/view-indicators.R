# Offline presentation helpers ---------------------------------------------

.bch_presentation_dependency <- function(package, function_name) {
  if (!requireNamespace(package, quietly = TRUE)) {
    bch_abort(
      sprintf("`%s()` requires the optional package '%s'.", function_name, package),
      "missing_dependency"
    )
  }
}

.bch_presentation_text <- function(x, argument, allow_null = FALSE) {
  if (allow_null && is.null(x)) return(invisible(x))
  if (!bch_is_scalar_string(x)) {
    bch_abort(sprintf("`%s` must be a non-empty character string.", argument),
              "invalid_argument")
  }
  invisible(x)
}

.bch_presentation_source <- function(x) {
  source <- "Fuente: datos proporcionados por el usuario"
  retrieved <- "Fecha de consulta: no disponible"
  if (inherits(x, "bch_result") && nrow(x$provenance)) {
    sources <- unique(as.character(x$provenance$source))
    sources <- sources[!is.na(sources) & nzchar(sources)]
    sources <- trimws(sub("^(Fuente\\s*:\\s*)+", "", trimws(sources),
                           ignore.case = TRUE, perl = TRUE))
    sources <- unique(sources[nzchar(sources)])
    if (length(sources)) source <- paste("Fuente:", paste(sources, collapse = "; "))
    dates <- x$provenance$retrieved_at
    dates <- dates[!is.na(dates)]
    if (length(dates)) {
      retrieved <- paste("Consulta mas reciente:",
        format(max(dates), "%Y-%m-%d %H:%M UTC", tz = "UTC"))
    }
  }
  list(source = source, retrieved = retrieved)
}

#' Explore the indicator catalogue in an offline HTML table
#'
#' Creates a self-contained, keyboard-accessible catalogue with text search
#' across all columns or a selected ID, code, description or group field,
#' frequency and group filters, column sorting, ID copying and pagination.
#' With JavaScript enabled, only the current page remains in the table DOM;
#' filtering and sorting operate on the catalogue held in memory. Without
#' JavaScript, the full catalogue remains available as an ordinary table.
#' If the browser blocks clipboard access, a selected text field allows manual
#' copying. Text from the catalogue is
#' escaped before rendering. The function returns HTML; it does not open a
#' browser, save files, or load scripts from external servers. Its independent
#' visual identity is provisional and does not use institutional logos.
#'
#' @param data A catalogue `bch_result` or data frame. If `NULL`, obtain the
#'   catalogue with [bch_indicators()].
#' @param source Catalogue source, forwarded to [bch_indicators()] only when
#'   `data` is `NULL`.
#' @param refresh Whether to refresh the catalogue. Cannot be `TRUE` when
#'   `data` is supplied.
#' @param title A plain-text heading.
#' @param page_size Number of rows displayed per page, from 1 to 500.
#' @param ... Additional arguments to [bch_indicators()]. Not accepted when
#'   `data` is supplied.
#' @return An `htmltools` tag containing inline styles, scripts and catalogue
#'   rows. Search and filters operate locally on the embedded data.
#' @export
bch_view_indicators <- function(
    data = NULL,
    source = "auto",
    refresh = FALSE,
    title = "Explorador de indicadores | bchR",
    page_size = 50L,
    ...) {
  .bch_presentation_dependency("htmltools", "bch_view_indicators")
  .bch_presentation_text(title, "title")
  .bch_presentation_text(source, "source")
  if (!is.logical(refresh) || length(refresh) != 1L || is.na(refresh)) {
    bch_abort("`refresh` must be TRUE or FALSE.", "invalid_argument")
  }
  if (!is.numeric(page_size) || length(page_size) != 1L ||
      is.na(page_size) || !is.finite(page_size) || page_size %% 1 != 0 ||
      page_size < 1 || page_size > 500) {
    bch_abort("`page_size` must be an integer between 1 and 500.", "invalid_argument")
  }
  if (!is.null(data) && (isTRUE(refresh) || length(list(...)))) {
    bch_abort("Supplied `data` cannot be combined with `refresh = TRUE` or `...`.",
              "invalid_argument")
  }
  if (is.null(data)) {
    data <- bch_indicators(source = source, refresh = refresh, ...)
  }
  if (inherits(data, "bch_result")) validate_bch_result(data)
  provenance <- .bch_presentation_source(data)
  rows <- if (inherits(data, "bch_result")) data$data else data
  required <- c("indicator_id", "indicator_description")
  if (!is.data.frame(rows) || !all(required %in% names(rows))) {
    bch_abort("`data` must contain catalogue columns `indicator_id` and `indicator_description`.",
              "invalid_argument")
  }

  fields <- c("indicator_id", "indicator_code", "indicator_description",
              "frequency", "group_code")
  visible <- lapply(fields, function(field) {
    value <- if (field %in% names(rows)) rows[[field]] else rep(NA_character_, nrow(rows))
    if (!is.atomic(value) || !is.null(dim(value))) {
      bch_abort("Displayed catalogue columns must be atomic vectors.", "invalid_argument")
    }
    value <- as.character(value)
    value[is.na(value) | !nzchar(value)] <- "No especificado"
    value
  })
  names(visible) <- fields
  tags <- htmltools::tags
  make_options <- function(x) {
    lapply(sort(unique(x)), function(value) tags$option(value = value, value))
  }
  table_rows <- lapply(seq_len(nrow(rows)), function(i) {
    tags$tr(
      `data-frequency` = visible$frequency[[i]],
      `data-group` = visible$group_code[[i]],
      lapply(visible, function(column) tags$td(column[[i]])),
      tags$td(tags$button(type = "button", class = "bch-copy-id",
        hidden = "hidden", `data-copy-id` = visible$indicator_id[[i]],
        disabled = if (identical(visible$indicator_id[[i]], "No especificado")) "disabled" else NULL,
        `aria-label` = paste("Copiar ID", visible$indicator_id[[i]]), "Copiar ID"))
    )
  })
  column_labels <- c("ID", "Codigo", "Descripcion", "Frecuencia", "Grupo")

  css <- paste0(
    ".bch-catalog{font-family:system-ui,-apple-system,Segoe UI,sans-serif;",
    "color:#18313d;background:#fff;max-width:1400px;margin:auto;padding:1.5rem;",
    "border:1px solid #cad7de;border-radius:12px;line-height:1.5}",
    ".bch-catalog *{box-sizing:border-box}.bch-catalog h2{margin:0;color:#153f53}",
    ".bch-catalog .bch-kicker{font-size:.8rem;letter-spacing:.1em;font-weight:700;",
    "color:#386272;margin:0 0 .5rem}.bch-catalog .bch-controls{display:flex;",
    "flex-wrap:wrap;gap:1rem;margin:1.5rem 0}.bch-catalog label{display:flex;",
    "flex-direction:column;gap:.35rem;flex:1;min-width:180px;font-weight:600}",
    ".bch-catalog input,.bch-catalog select,.bch-catalog button{font:inherit;",
    "padding:.65rem;border:1px solid #698797;border-radius:6px;background:#fff;color:#18313d}",
    ".bch-catalog input:focus-visible,.bch-catalog select:focus-visible,",
    ".bch-catalog button:focus-visible,.bch-catalog .bch-table-wrap:focus-visible",
    "{outline:3px solid #a14f00;outline-offset:3px}.bch-catalog button{cursor:pointer}",
    ".bch-catalog button:disabled{opacity:.5;cursor:default}",
    ".bch-catalog .bch-table-wrap{max-width:100%;min-width:0;max-height:65vh;overflow:auto;border:1px solid #cad7de}",
    ".bch-catalog table{width:100%;min-width:760px;border-collapse:collapse;font-size:.9rem}",
    ".bch-catalog th,.bch-catalog td{text-align:left;padding:.7rem;vertical-align:top;",
    "border-bottom:1px solid #dde5e9;overflow-wrap:anywhere}.bch-catalog th{background:#eaf1f5;",
    "position:sticky;top:0}.bch-catalog tbody tr:nth-child(even){background:#f6f9fa}",
    ".bch-catalog [hidden]{display:none!important}.bch-catalog .bch-pagination{display:flex;",
    "flex-wrap:wrap;align-items:center;gap:.7rem;margin-top:1rem}",
    ".bch-catalog .bch-note{font-size:.85rem;color:#475f6b}.bch-catalog caption",
    "{text-align:left;padding:.6rem;background:#f6f9fa}.bch-catalog td:first-child{font-family:monospace}",
    ".bch-catalog .bch-sort{display:flex;gap:.5rem;align-items:center;background:transparent;",
    "font-weight:700;border:0;padding:.2rem;text-align:left}.bch-catalog .bch-copy-id{white-space:nowrap}",
    ".bch-catalog .bch-copy-status{min-height:1.5em}.bch-catalog .bch-copy-manual{max-width:32rem}"
  )
  # Only this fixed, package-authored script is marked as HTML. User strings
  # reach the document exclusively via escaped tags and textContent.
  js <- paste0(
    "(function(){'use strict';var root=document.currentScript.parentElement;",
    "var pick=function(s){return root.querySelector(s);};",
    "var body=pick('tbody'),rows=Array.from(body.querySelectorAll('tr'));",
    "var fold=function(s){return s.normalize('NFD').replace(/[\\u0300-\\u036f]/g,'').toLowerCase();};",
    "var entries=rows.map(function(row,index){var cells=Array.from(row.cells).slice(0,5).map(function(cell){return cell.textContent;});",
    "return {row:row,index:index,cells:cells,foldedCells:cells.map(fold),text:fold(cells.join(' ')),",
    "frequency:row.getAttribute('data-frequency'),group:row.getAttribute('data-group')};});",
    "var search=pick('[data-role=search]'),searchField=pick('[data-role=search-field]'),freq=pick('[data-role=frequency]'),",
    "group=pick('[data-role=group]'),status=pick('[data-role=status]'),",
    "prev=pick('[data-role=previous]'),next=pick('[data-role=next]');",
    "var sortButtons=Array.from(root.querySelectorAll('[data-sort-index]'));",
    "var size=Number(root.getAttribute('data-page-size')),page=0,filtered=entries,sortColumn=-1,ascending=true;",
    "var collator=new Intl.Collator('es',{numeric:true,sensitivity:'base'});",
    "function render(){var total=filtered.length,pages=Math.max(1,Math.ceil(total/size));",
    "page=Math.min(page,pages-1);var fragment=document.createDocumentFragment();",
    "filtered.slice(page*size,(page+1)*size).forEach(function(entry){fragment.appendChild(entry.row);});",
    "body.replaceChildren(fragment);",
    "status.textContent=total+' de '+rows.length+' indicadores. Pagina '+(page+1)+' de '+pages+'.';",
    "prev.disabled=page===0;next.disabled=page>=pages-1;}",
    "function filter(){copyStatus.textContent='';copyManual.hidden=true;",
    "var terms=fold(search.value).trim().split(/\\s+/).filter(Boolean),column=Number(searchField.value);",
    "filtered=entries.filter(function(entry){var text=column<0?entry.text:entry.foldedCells[column];",
    "return terms.every(function(term){return text.includes(term);})",
    "&&(!freq.value||entry.frequency===freq.value)&&(!group.value||entry.group===group.value);});",
    "page=0;render();}var timer;search.addEventListener('input',function(){",
    "clearTimeout(timer);timer=setTimeout(filter,120);});searchField.addEventListener('change',filter);freq.addEventListener('change',filter);",
    "group.addEventListener('change',filter);prev.addEventListener('click',function(){page--;render();});",
    "next.addEventListener('click',function(){page++;render();});",
    "sortButtons.forEach(function(button){button.disabled=false;button.addEventListener('click',function(){",
    "var column=Number(button.getAttribute('data-sort-index'));ascending=column===sortColumn?!ascending:true;sortColumn=column;",
    "entries.sort(function(a,b){var order=collator.compare(a.cells[column],b.cells[column]);",
    "return order?(ascending?order:-order):a.index-b.index;});",
    "sortButtons.forEach(function(other){",
    "var selected=other===button;other.parentElement.setAttribute('aria-sort',selected?(ascending?'ascending':'descending'):'none');",
    "other.querySelector('[data-role=sort-mark]').textContent=selected?(ascending?'\u2191':'\u2193'):'\u2195';});filter();});});",
    "var copyStatus=pick('[data-role=copy-status]'),copyManual=pick('[data-role=copy-manual]'),copyValue=pick('[data-role=copy-value]');",
    "function copyFallback(value,button){copyManual.hidden=false;copyValue.value=value;copyValue.focus();copyValue.select();",
    "var copied=false;try{copied=document.execCommand('copy');}catch(error){copied=false;}",
    "if(copied){copyManual.hidden=true;button.focus();copyStatus.textContent='ID '+value+' copiado.';}",
    "else{copyStatus.textContent='Portapapeles no disponible. El ID esta seleccionado: use Ctrl+C o Comando+C para copiarlo.';}}",
    "root.querySelectorAll('[data-copy-id]').forEach(function(button){button.hidden=false;button.addEventListener('click',function(){",
    "var value=button.getAttribute('data-copy-id');copyStatus.textContent='';copyManual.hidden=true;",
    "if(navigator.clipboard&&window.isSecureContext){navigator.clipboard.writeText(value).then(function(){",
    "copyStatus.textContent='ID '+value+' copiado.';},function(){copyFallback(value,button);});}",
    "else{copyFallback(value,button);}});});",
    "pick('[data-role=controls]').hidden=false;pick('[data-role=pagination]').hidden=false;render();})();"
  )

  tags$section(
    class = "bch-catalog", `data-page-size` = as.integer(page_size),
    `aria-label` = title,
    tags$style(htmltools::HTML(css)),
    tags$p(class = "bch-kicker", "DATOS ECONOMICOS / CATALOGO"),
    tags$h2(title),
    tags$p(class = "bch-note", "Herramienta independiente en desarrollo. Identidad visual provisional."),
    tags$p(class = "bch-note", provenance$source, tags$br(), provenance$retrieved),
    tags$div(class = "bch-controls", `data-role` = "controls", hidden = "hidden",
      tags$label("Buscar texto",
        tags$input(type = "search", `data-role` = "search", placeholder = "Ejemplo: inflacion")),
      tags$label("Buscar en",
        tags$select(`data-role` = "search-field",
          tags$option(value = "-1", "Todos los campos"),
          tags$option(value = "0", "ID"), tags$option(value = "1", "Codigo"),
          tags$option(value = "2", "Descripcion"), tags$option(value = "4", "Grupo"))),
      tags$label("Frecuencia",
        tags$select(`data-role` = "frequency", tags$option(value = "", "Todas"),
                    make_options(visible$frequency))),
      tags$label("Grupo",
        tags$select(`data-role` = "group", tags$option(value = "", "Todos"),
                    make_options(visible$group_code)))
    ),
    tags$noscript(tags$p("JavaScript esta desactivado: se muestra el catalogo completo.")),
    tags$div(class = "bch-table-wrap", tabindex = "0", role = "region",
      `aria-label` = "Tabla de indicadores, desplazamiento horizontal y vertical",
      tags$table(
        tags$caption("Catalogo de indicadores. Las frecuencias y grupos conservan su identificacion de origen."),
        tags$thead(tags$tr(lapply(seq_along(column_labels), function(i) {
          tags$th(scope = "col", `aria-sort` = "none",
            tags$button(type = "button", class = "bch-sort", disabled = "disabled",
              `data-sort-index` = i - 1L, `aria-label` = paste("Ordenar por", column_labels[[i]]),
              column_labels[[i]], tags$span(`data-role` = "sort-mark", `aria-hidden` = "true", "\u2195")))
        }), tags$th(scope = "col", "Acciones"))),
        tags$tbody(table_rows)
      )
    ),
    tags$div(class = "bch-pagination", `data-role` = "pagination", hidden = "hidden",
      tags$button(type = "button", `data-role` = "previous", "Anterior"),
      tags$button(type = "button", `data-role` = "next", "Siguiente"),
      tags$p(`data-role` = "status", role = "status", `aria-live` = "polite",
             sprintf("%d indicadores.", nrow(rows)))
    ),
    tags$p(class = "bch-note bch-copy-status", `data-role` = "copy-status",
      role = "status", `aria-live` = "polite", `aria-atomic` = "true"),
    tags$label(class = "bch-copy-manual", `data-role` = "copy-manual", hidden = "hidden",
      "ID seleccionado para copiar manualmente",
      tags$input(type = "text", `data-role` = "copy-value", readonly = "readonly")),
    tags$script(htmltools::HTML(js))
  )
}
