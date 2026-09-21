#' Pick a few series out of many
#'
#' A list with one row per series: what `tsbox::ts_summary()` knows about it
#' on the left, a sparkline of its shape on the right, and a checkbox. Filter,
#' sort, tick a handful.
#'
#' @details
#' THE SPARKLINE IS THE POINT. One Census MARTS download is 168 series, and
#' their names differ by a couple of words each, so a multi-select of 168 ids
#' gives you nothing to choose on. The shape does: scrolling the list you can
#' see which series have the spike and which are flat.
#'
#' The preview cannot do this job. 168 lines on one chart draw in about three
#' milliseconds and are unreadable, so the problem is never performance.
#'
#' EACH SPARKLINE IS ONE SVG `path`, BUILT IN R. 168 charts on one panel is
#' not a thing to do; a path string is a few hundred bytes and the browser
#' draws it with no JavaScript at all. Measured on that file: 168 paths are
#' 146 KB against 334 KB to draw the same series as a chart. Each is scaled to
#' its own range, because the row answers "what shape is this" and not "how
#' big is it".
#'
#' `tsbox::ts_pick()` is what the block writes, and it is the right call for
#' two reasons beyond the selection: it keeps the order you picked in, and it
#' renames while it selects, so one control carries both the choice and the
#' legend label. A `dplyr::filter(id %in% ...)` does the selection and neither
#' of the other two.
#'
#' Picking nothing passes everything through, so a freshly connected block is
#' a no-op rather than an empty frame.
#'
#' @param series Ids to keep. Empty keeps everything.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @examples
#' new_ts_pick_block(series = c("mdeaths", "fdeaths"))
#'
#' @export
new_ts_pick_block <- function(series = character(), ...) {

  blockr.core::new_transform_block(
    function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {

          r_series <- shiny::reactiveVal(as.character(series))

          rows <- shiny::reactive(ts_pick_rows(data()))

          # The list is rebuilt when the DATA changes, never when the
          # selection does. JS owns the tick marks and reports the array
          # back; re-rendering 168 rows from our own echo would fight the
          # user's next click.
          output$list <- shiny::renderUI({
            ts_pick_ui(session$ns, rows(), shiny::isolate(r_series()))
          })

          shiny::observeEvent(input$picked, {
            picked <- as.character(unlist(input$picked))
            r_series(picked[nzchar(picked)])
          }, ignoreNULL = FALSE)

          list(
            expr = shiny::reactive({
              keep <- intersect(r_series(), vapply(rows(), `[[`,
                                                   character(1L), "id"))
              if (!length(keep)) {
                # NOT `quote(data)`. blockr.core asserts the expression is a
                # `language` object and a bare symbol is not one, so a block
                # that returns it fails with "typeof(exprs) == \"language\"
                # is not TRUE" on every evaluation. `ts_pick()` with no ids
                # is not the fallback either: it errors looking for a value
                # column.
                return(quote(base::identity(data)))
              }
              as.call(c(list(quote(tsbox::ts_pick), quote(data)),
                        as.list(keep)))
            }),
            state = list(series = r_series)
          )
        }
      )
    },
    function(id) {
      ts_gear(shiny::NS(id), shiny::uiOutput(shiny::NS(id, "list")),
              open = TRUE)
    },
    dat_val = function(data) ts_validate_series(data),
    # Nothing picked is the starting state and passes everything through.
    allow_empty_state = "series",
    class = "ts_pick_block",
    ...
  )
}

#' One row's worth of facts about each series
#' @noRd
ts_pick_rows <- function(data) {
  d <- tryCatch(suppressMessages(tsbox::ts_tbl(data)),
                error = function(e) NULL, warning = function(w) NULL)
  if (is.null(d) || !nrow(d)) {
    return(list())
  }
  nm <- names(d)
  vcol <- nm[[length(nm)]]
  tcol <- if ("time" %in% nm) "time" else nm[[length(nm) - 1L]]
  idc <- setdiff(nm, c(tcol, vcol))
  if (!length(idc)) {
    return(list())
  }

  key <- do.call(paste, c(d[idc], sep = " · "))
  parts <- split(seq_len(nrow(d)), factor(key, levels = unique(key)))

  lapply(names(parts), function(k) {
    i <- parts[[k]]
    v <- d[[vcol]][i]
    t <- d[[tcol]][i]
    ok <- !is.na(v)
    list(
      id = k,
      obs = length(i),
      start = format(min(t), "%Y-%m"),
      end = format(max(t), "%Y-%m"),
      last = if (any(ok)) v[ok][[sum(ok)]] else NA_real_,
      spark = ts_sparkline(v)
    )
  })
}

#' A series as one SVG path
#'
#' Downsampled to at most 70 points and scaled to its own range: the row is
#' asking what shape this is, not how big it is.
#'
#' @param v Values, NAs allowed.
#' @param w,h Box in user units.
#' @noRd
ts_sparkline <- function(v, w = 96, h = 20, n = 70) {
  v <- v[!is.na(v)]
  if (length(v) < 2L) {
    return("")
  }
  v <- v[round(seq(1, length(v), length.out = min(n, length(v))))]
  rng <- range(v)
  y <- if (diff(rng) == 0) rep(h / 2, length(v)) else
    h - (v - rng[[1L]]) / diff(rng) * (h - 2) - 1
  x <- seq(0, w, length.out = length(v))
  paste0("M", paste(sprintf("%.1f %.1f", x, y), collapse = "L"))
}

#' The picker list
#' @noRd
ts_pick_ui <- function(ns, rows, picked) {
  root <- ns("pick")
  if (!length(rows)) {
    return(ts_hint(paste0(
      "Nothing to pick: this input holds one unnamed series. A picker needs ",
      "an id column."
    )))
  }

  fmt <- function(x) {
    if (is.na(x)) "" else formatC(x, format = "f", digits = 1, big.mark = ",")
  }

  htmltools::tagList(
    ts_pick_dep(),
    htmltools::div(
      id = root,
      class = "blockr-ts-pick",
      `data-input` = ns("picked"),
      htmltools::div(
        class = "blockr-ts-pick__bar",
        htmltools::div(
          class = "blockr-ts-field",
          htmltools::tags$input(
            type = "search", class = "form-control blockr-ts-pick__q",
            placeholder = paste0("Filter ", length(rows), " series")
          )
        ),
        htmltools::div(
          class = "blockr-ts-field",
          htmltools::tags$select(
            class = "form-control blockr-ts-pick__sort",
            htmltools::tags$option(value = "name", "Sort by name"),
            htmltools::tags$option(value = "last", "Sort by last value"),
            htmltools::tags$option(value = "obs", "Sort by length")
          )
        ),
        htmltools::span(class = "blockr-ts-pick__count"),
        htmltools::tags$button(type = "button",
                               class = "blockr-ts-pick__link", "clear")
      ),
      htmltools::div(
        class = "blockr-ts-pick__list",
        htmltools::div(
          class = "blockr-ts-pick__head",
          htmltools::span(), htmltools::span("shape"),
          htmltools::span("series"),
          htmltools::span(class = "blockr-ts-pick__num", "obs"),
          htmltools::span("span"),
          htmltools::span(class = "blockr-ts-pick__num", "last")
        ),
        lapply(rows, function(r) {
          on <- r$id %in% picked
          htmltools::tags$label(
            class = paste("blockr-ts-pick__row", if (on) "is-on"),
            `data-id` = r$id,
            `data-q` = tolower(r$id),
            `data-last` = if (is.na(r$last)) 0 else r$last,
            `data-obs` = r$obs,
            htmltools::tags$input(type = "checkbox", checked = if (on) NA),
            htmltools::HTML(paste0(
              '<svg width="96" height="20" viewBox="0 0 96 20" fill="none" ',
              'stroke="currentColor" stroke-width="1.1" ',
              'stroke-linejoin="round" aria-hidden="true"><path d="',
              r$spark, '"/></svg>'
            )),
            # The id comes from the user's data, so it goes in as text.
            htmltools::span(class = "blockr-ts-pick__name", r$id),
            htmltools::span(class = "blockr-ts-pick__num", r$obs),
            htmltools::span(class = "blockr-ts-pick__span",
                            paste(r$start, "–", r$end)),
            htmltools::span(class = "blockr-ts-pick__num", fmt(r$last))
          )
        })
      ),
      ts_hint("Nothing ticked passes every series through.")
    ),
    htmltools::tags$script(htmltools::HTML(sprintf(
      "(function(){var f=function(){if(window.BlockrTsPick){window.BlockrTsPick.mount('%s');}else{setTimeout(f,50);}};f();})();",
      root
    )))
  )
}

#' @noRd
ts_pick_dep <- function() {
  htmltools::htmlDependency(
    name = "blockr-ts-pick",
    version = ts_asset_version(),
    src = ts_asset("js"),
    script = "ts-pick.js"
  )
}
