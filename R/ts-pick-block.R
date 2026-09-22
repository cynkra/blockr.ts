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

          # `ignoreNULL = FALSE` so that UNTICKING THE LAST ROW reaches R:
          # the browser sends an empty array and the default observer would
          # drop it, leaving the board filtered to a selection the screen no
          # longer shows.
          #
          # `ignoreInit = TRUE` is the other half of that, and without it the
          # block silently ignores its own `series` argument: at startup
          # `input$picked` is NULL, the observer fires on it, and the restored
          # selection is overwritten with nothing before a single click. The
          # symptom is a list with the right rows ticked and a block that
          # passes every series through.
          shiny::observeEvent(input$picked, {
            picked <- as.character(unlist(input$picked))
            r_series(picked[nzchar(picked)])
          }, ignoreNULL = FALSE, ignoreInit = TRUE)

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
              ts_pick_expr(keep, ts_layout(data()))
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

#' How a frame is laid out: which columns are ids, time and value
#' @noRd
ts_layout <- function(data) {
  d <- tryCatch(suppressMessages(tsbox::ts_tbl(data)),
                error = function(e) NULL, warning = function(w) NULL)
  if (is.null(d) || !nrow(d)) {
    return(NULL)
  }
  nm <- names(d)
  vcol <- nm[[length(nm)]]
  tcol <- if ("time" %in% nm) "time" else nm[[length(nm) - 1L]]
  ids <- setdiff(nm, c(tcol, vcol))
  list(ids = ids, varying = ts_varying(d, ids), time = tcol, value = vcol)
}

#' The expression the picker writes
#'
#' `tsbox::ts_pick()` MATCHES AGAINST ONE ID COLUMN, and it reads the frame's
#' columns, not our labels. A frame from an API that takes its query as
#' dimensions has one column per dimension -- `type, structure, seas_adj` --
#' so there is no single id to match and the label in our list is a composite,
#' with the pinned dimensions dropped because they identify nothing. Handing
#' that to `ts_pick()` fails with "values missing in data" after a list that
#' had looked like it worked.
#'
#' So there are two forms. A frame that is already one id, one time, one value
#' gets `ts_pick()`, which keeps the pick order and can rename. Anything else
#' gets a subset that filters on the columns that vary AND drops the ones that
#' do not, which is the work a select block was doing by hand. Base R, so a
#' document exported from the board needs nothing installed to run it.
#'
#' @param keep Ids to keep, in pick order.
#' @param lay Result of [ts_layout()].
#' @noRd
ts_pick_expr <- function(keep, lay) {
  if (is.null(lay) || !length(lay$varying)) {
    return(quote(base::identity(data)))
  }
  if (identical(lay$ids, lay$varying) && length(lay$ids) == 1L) {
    return(as.call(c(list(quote(tsbox::ts_pick), quote(data)),
                     as.list(keep))))
  }

  key <- if (length(lay$varying) == 1L) {
    call("[[", quote(data), lay$varying)
  } else {
    as.call(c(
      list(quote(paste)),
      lapply(lay$varying, function(i) call("[[", quote(data), i)),
      list(sep = " \u00b7 ")
    ))
  }
  cols <- c(lay$varying, lay$time, lay$value)
  bquote(data[.(key) %in% .(keep), .(cols), drop = FALSE])
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
  idc <- ts_varying(d, setdiff(nm, c(tcol, vcol)))
  if (!length(idc)) {
    return(list())
  }

  key <- do.call(paste, c(d[idc], sep = " \u00b7 "))
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
#'
#' A real `<table class="blockr-table">` in blockr.ui's table preview, not a
#' grid of our own: the picker reads as the table block because it IS the
#' table block's markup, and the sortable header, the sticky head and the
#' search input come with it rather than being reinvented at slightly the
#' wrong size.
#'
#' Sorting is a click on a column header, which is where a reader of a table
#' looks for it, and it never leaves the browser.
#'
#' @noRd
ts_pick_ui <- function(ns, rows, picked) {
  root <- ns("pick")
  if (!length(rows)) {
    # An empty state, not an explanation: without an id column there is
    # nothing on the screen at all, which needs saying.
    return(ts_hint("This input holds one unnamed series."))
  }

  fmt <- function(x) {
    if (is.na(x)) "" else formatC(x, format = "f", digits = 1, big.mark = ",")
  }

  th <- function(key, label, numeric = FALSE) {
    htmltools::tags$th(
      class = paste("blockr-sortable",
                    if (numeric) "blockr-td-numeric"),
      `data-sort` = key,
      scope = "col",
      label,
      htmltools::tags$span(class = "blockr-sort-icon")
    )
  }

  htmltools::tagList(
    ts_pick_dep(),
    htmltools::div(
      id = root,
      class = "blockr-ts-pick",
      `data-input` = ns("picked"),
      htmltools::div(
        class = "blockr-html-table-header blockr-ts-pick__bar",
        htmltools::div(class = "blockr-html-table-title blockr-ts-pick__count"),
        htmltools::div(
          class = "blockr-html-table-toolbar",
          htmltools::tags$input(
            type = "search", class = "blockr-search",
            placeholder = "Search\u2026",
            `aria-label` = "Search series"
          )
        )
      ),
      htmltools::div(
        class = "blockr-table-wrapper blockr-ts-pick__wrap",
        htmltools::tags$table(
          class = "blockr-table blockr-ts-pick__table",
          htmltools::tags$thead(
            htmltools::tags$tr(
              # Select all / none where the ticks are, not as a link
              # somewhere else on the row. Indeterminate while some are
              # picked, and it acts on what the filter is SHOWING, which is
              # what makes searching then ticking worth doing.
              htmltools::tags$th(
                class = "blockr-ts-pick__tick blockr-ts-pick__all",
                scope = "col", title = "Select all or none",
                htmltools::tags$input(type = "checkbox", tabindex = "-1",
                                      `aria-label` = "Select all or none")
              ),
              htmltools::tags$th(class = "blockr-ts-pick__shape", scope = "col",
                                 "shape"),
              th("name", "series"),
              th("obs", "obs", numeric = TRUE),
              th("start", "span"),
              th("last", "last", numeric = TRUE)
            )
          ),
          htmltools::tags$tbody(
            lapply(rows, function(r) {
              on <- r$id %in% picked
              htmltools::tags$tr(
                class = paste("blockr-ts-pick__row", if (on) "is-on"),
                `data-id` = r$id,
                `data-q` = tolower(r$id),
                `data-last` = if (is.na(r$last)) 0 else r$last,
                `data-obs` = r$obs,
                `data-start` = r$start,
                htmltools::tags$td(
                  class = "blockr-ts-pick__tick",
                  # NOT wrapped in a <label>. A label around a checkbox
                  # forwards activation to it, so one gesture arrives as two
                  # events and the box visibly toggles twice. The row owns
                  # the click; the box is a display of the state.
                  htmltools::tags$input(
                    type = "checkbox", tabindex = "-1",
                    checked = if (on) NA, `aria-hidden` = "true"
                  )
                ),
                htmltools::tags$td(
                  class = "blockr-ts-pick__shape",
                  htmltools::HTML(paste0(
                    '<svg width="96" height="20" viewBox="0 0 96 20" ',
                    'fill="none" stroke="currentColor" stroke-width="1.1" ',
                    'stroke-linejoin="round" aria-hidden="true"><path d="',
                    r$spark, '"/></svg>'
                  ))
                ),
                # The id comes from the user's data, so it goes in as text.
                htmltools::tags$td(class = "blockr-ts-pick__name", r$id),
                htmltools::tags$td(class = "blockr-td-numeric", r$obs),
                htmltools::tags$td(class = "blockr-ts-pick__span",
                                   paste(r$start, "\u2013", r$end)),
                htmltools::tags$td(class = "blockr-td-numeric", fmt(r$last))
              )
            })
          )
        )
      ),
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
