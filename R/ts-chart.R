#' The time series chart
#'
#' Builds the markup for one chart: a container carrying its data as JSON and
#' a script that mounts ECharts into it.
#'
#' @details
#' NOT AN HTMLWIDGET. The chart is re-created from scratch on every result,
#' inside [shiny::renderUI()], which is the one thing that cannot go stale:
#' there is no output binding holding a previous instance, no
#' `Shiny.renderContent` path to get wrong on a late-mounted panel, and no
#' widget JS served from an installed copy while `pkgload::load_all()` serves
#' the R.
#'
#' uPlot, decided 2026-09-21, having started on ECharts. 22 KB against 363 KB
#' gzipped and under 2 ms to draw against 8.7 ms on the same payload. The two
#' things that looked like they would keep ECharts, a shaded confidence band
#' and dated annotations for outliers, are native (`bands`) and twenty-four
#' lines of canvas. What uPlot does not give you is chrome: the legend and the
#' hover readout are ours, and that is the standing cost. Comparison and both
#' measurements in `_scratch/ts-charts/`.
#'
#' @name ts-chart
#' @keywords internal
NULL

#' @param d A long frame from `ts_frame()`: `id`, `time`, `value`.
#' @param toggle_id Element id of this block's chart/table switch, revealed
#'   when the chart mounts.
#' @return A `shiny.tag`.
#' @rdname ts-chart
#' @noRd
ts_chart_html <- function(d, toggle_id = NULL) {

  el <- paste0(
    "tschart-",
    paste(sample(c(letters, 0:9), 12L, replace = TRUE), collapse = "")
  )

  # COLUMNAR, WHICH IS ALSO WHAT UPLOT WANTS: one array for the time and one
  # per series, no per-point allocation. Measured against the alternatives on
  # EuStockMarkets, 1860 x 4 = 7440 points: 416 KB as records, 197 KB as
  # `[time, value]` pairs, 84 KB like this. Padding on the union of the dates
  # costs nulls, and they are cheap, four characters against eight to
  # thirteen for a value: the worst case I could build, a daily and a monthly
  # series over one span, is 94 KB against 86 KB for pairs.
  ids <- unique(d$id)
  times <- sort(unique(d$time))

  # ALIGNED ON THE UNION OF THE DATES, not on each series' own rows. Two
  # series that do not cover the same span would otherwise come out as arrays
  # of different lengths against one time axis, and every point after the
  # shorter one's end would be drawn at the wrong date.
  wide <- lapply(ids, function(i) {
    s <- d[d$id == i, , drop = FALSE]
    v <- rep(NA_real_, length(times))
    v[match(s$time, times)] <- s$value
    v
  })

  payload <- list(
    ids = as.list(as.character(ids)),
    # SECONDS. A uPlot time scale is unix seconds unless `ms: 1` is set, and
    # handing it milliseconds is silent: the series still draw, the x axis
    # loses its labels, and anything positioned by date lands off-screen.
    time = as.numeric(times) * 86400,
    series = wide
  )

  htmltools::tagList(
    ts_chart_dep(),
    htmltools::div(
      class = "blockr-ts-chart",
      id = el,
      # `na = "null"` is what draws a gap. Without it a missing value travels
      # as the string "NA" and ECharts throws the whole point away, closing
      # the gap silently.
      `data-ts` = jsonlite::toJSON(
        payload, auto_unbox = TRUE, null = "null", na = "null"
      )
    ),
    htmltools::tags$script(htmltools::HTML(sprintf(
      "(function(){ var f = function(){ if (window.BlockrTsChart) { window.BlockrTsChart.mount('%s', %s); } else { setTimeout(f, 50); } }; f(); })();",
      el, jsonlite::toJSON(toggle_id %||% "", auto_unbox = TRUE)
    )))
  )
}

#' Is this a time series, and if so what does it look like?
#'
#' ONE CONVERSION ANSWERS BOTH QUESTIONS. Asking "is this a time series" and
#' then converting it would do the same reshape twice, and worse, would ask
#' the question of a 100-row head and answer it for the whole frame: a table
#' that looks like a series in its first rows and has duplicate timestamps
#' later would pass the probe and then throw inside the chart.
#'
#' tsbox decides, and its refusals are the useful part. A real orders table,
#' one row per order and several orders a day, errors with "object contains
#' series with duplicated information" and lands here as `NULL`. A frame with
#' two numeric columns is not a series either. What survives really is one
#' value per series per period, which is what a line chart can draw.
#'
#' The `[time]: ... [value]: ...` notice tsbox prints when it picks columns is
#' suppressed: this runs on every render of every block, and the log would be
#' nothing else.
#'
#' tsbox's canonical column order is id columns first, then time, then value,
#' so the last two are known without asking. Several id columns are pasted
#' into one label, because a chart legend has one dimension.
#'
#' @param x A block result.
#' @return A data frame with `id`, `time` and `value`, or `NULL`.
#' @noRd
ts_frame <- function(x) {

  if (is.null(x) || inherits(x, "tbl_lazy")) {
    return(NULL)
  }

  d <- tryCatch(
    suppressMessages(tsbox::ts_tbl(x)),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (is.null(d) || !nrow(d) || ncol(d) < 2L) {
    return(NULL)
  }

  nm <- names(d)

  # `ts_tbl()` SUCCEEDING IS NOT THE SAME AS IT HAVING FOUND A SERIES. Handed
  # `time, value, costs` it returns all three columns unchanged: it recognised
  # `time` by name and left the rest alone. Reading the last two as time and
  # value then charts `costs` against `value`, one series per date, which is
  # both wrong and confident.
  #
  # A frame tsbox has actually resolved has its time column second to last and
  # its value last. Check that rather than trusting the call not to have
  # thrown.
  tcol <- nm[[length(nm) - 1L]]
  vcol <- nm[[length(nm)]]
  ids <- setdiff(nm, c(tcol, vcol))

  is_time <- function(x) inherits(x, c("Date", "POSIXct", "POSIXt"))

  if (!is_time(d[[tcol]]) || !is.numeric(d[[vcol]])) {
    return(NULL)
  }
  # An id that is a number or a date is another sign the frame was passed
  # through rather than reduced.
  if (length(ids) &&
      any(vapply(d[ids], function(x) is.numeric(x) || is_time(x), logical(1)))) {
    return(NULL)
  }

  tcol <- nm[[length(nm) - 1L]]
  vcol <- nm[[length(nm)]]
  idcols <- setdiff(nm, c(tcol, vcol))

  id <- if (length(idcols)) {
    do.call(paste, c(unname(lapply(d[idcols], as.character)), sep = " · "))
  } else {
    "value"
  }

  out <- data.frame(
    id = id,
    time = as.Date(d[[tcol]]),
    value = as.numeric(d[[vcol]]),
    stringsAsFactors = FALSE
  )
  out[order(out$id, out$time), , drop = FALSE]
}

#' @rdname ts-chart
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' @rdname ts-chart
#' @noRd
ts_chart_dep <- function() {
  htmltools::tagList(
    uplot_dep(),
    htmltools::htmlDependency(
      name = "blockr-ts-chart",
      version = ts_asset_version(),
      src = ts_asset("js"),
      script = c("ts-chart.js", "ts-view.js")
    ),
    htmltools::htmlDependency(
      name = "blockr-ts-chart-css",
      version = ts_asset_version(),
      src = ts_asset("css"),
      stylesheet = "ts-chart.css"
    )
  )
}

#' The version string the browser caches these assets under
#'
#' BUMP THE SUFFIX WHENEVER A FILE IN `inst/js` OR `inst/css` CHANGES. Shiny
#' serves an html dependency under a path containing its version, so a browser
#' that has seen `blockr-ts-chart-0.0.1` will keep serving its copy for as
#' long as the package version stands still, which during development is
#' always. It cost an afternoon: the chart had been taught to reveal the
#' chart/table switch and the switch stayed hidden, because the page was
#' running the previous `ts-chart.js`. Same convention as `blockr.viz`.
#'
#' @noRd
ts_asset_version <- function() {
  paste0(utils::packageVersion("blockr.ts"), ".6")
}

#' Resolve one of this package's asset directories
#'
#' `system.file()` is shimmed by pkgload inside a `load_all()`'d package's own
#' namespace, which covers the normal case, and returns `""` when an installed
#' blockr.ts sits beside the sources and the sources win the namespace. Fall
#' back to `inst/` explicitly rather than emitting a dependency with an empty
#' `src`, which serves nothing and fails silently in the browser.
#'
#' @param dir Directory under `inst/`.
#' @noRd
ts_asset <- function(dir) {
  path <- system.file(dir, package = "blockr.ts")
  if (nzchar(path)) {
    return(path)
  }
  file.path(find.package("blockr.ts"), "inst", dir)
}

#' uPlot, vendored
#'
#' 51 KB minified, MIT, and not on CRAN in any form this package could depend
#' on, so the built file lives in `inst/`. Version is uPlot's own, so a bump
#' of the library busts the browser cache by itself.
#'
#' @noRd
uplot_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "uplot",
      version = "1.6.32",
      src = ts_asset("js"),
      script = "uPlot.js"
    ),
    htmltools::htmlDependency(
      name = "uplot-css",
      version = "1.6.32",
      src = ts_asset("css"),
      stylesheet = "uPlot.css"
    )
  )
}
