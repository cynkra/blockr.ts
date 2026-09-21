#' Preview time series as a chart
#'
#' A [blockr.core::tabular_display] that draws a chart when a block's result is
#' a time series, and hands everything else to the display the app already had.
#'
#' @details
#' THE DISPATCH IS ON THE DATA, NOT ON THE BLOCK. This is the whole design. A
#' time series in blockr is any data frame tsbox can read as one, so a
#' `dplyr::filter()`, a `mutate()` or the component pulled out of a seasonal
#' adjustment all draw as charts without any of them knowing this package
#' exists.
#'
#' TSBOX DECIDES, AND ITS REFUSALS ARE THE POINT. A table of orders with a
#' date and an amount is a time series, and on a board where you asked for
#' this display you probably want to see it as one. A table with one row per
#' order and several orders a day is not, and tsbox says so: it errors with
#' "duplicated information" and the frame falls through to `fallback`. So does
#' anything with two numeric columns, and `mtcars`, `iris` and `airquality`.
#'
#' Opt in per app. This is the only switch, and a board that does not set it
#' is unaffected.
#'
#' WHERE TSBOX CANNOT TELL, THE ANSWER IS A dplyr BLOCK, not a block of ours.
#' tsbox goes on column names first and column positions second, so a
#' `blockr.dplyr::new_rename_block()` that renames a column to `time` or
#' `value` is the whole fix. That covers both of the cases worth naming: a
#' frame with two numeric columns, where tsbox will not guess which is the
#' value, and a time column it cannot recognise (a character `month` holding
#' `"2021-01"` is not read; the same column called `time` is parsed into
#' dates). A `pivot_longer()` result, `date, name, value`, needs a select to
#' put the columns back in order.
#'
#' The alternative, which this replaces, was a `ts_block` class carrying its
#' own `block_ui()` and `block_output()` methods. That put the chart on the
#' block, so a series passing through any block outside this package came out
#' as a table, and undoing the class needed a block of its own
#' (`new_ts_to_df_block()`).
#'
#' Turn it on for an app with
#' `options(blockr.tabular_display = blockr.ts::ts_display)`.
#'
#' @param fallback The display for everything else. `NULL`
#'   resolves at render time to `blockr.ui::html_table_display` when blockr.ui
#'   is installed and to [blockr.core::minimal_display] otherwise, so an app
#'   that already had the HTML table preview keeps it.
#'
#' @return `new_ts_display()` returns a `tabular_display`; `ts_display` is one
#'   over the app's own preview.
#'
#' @examples
#' if (interactive()) {
#'   options(blockr.tabular_display = blockr.ts::ts_display)
#'   blockr.core::serve(
#'     blockr.core::new_static_block(tsbox::ts_tbl(datasets::AirPassengers))
#'   )
#' }
#'
#' @export
new_ts_display <- function(fallback = NULL) {
  structure(
    list(fallback = fallback),
    class = c("ts_display", "tabular_display")
  )
}

#' @rdname new_ts_display
#' @format NULL
#' @export
ts_display <- new_ts_display()

#' The preview to keep for everything that is not a time series
#'
#' Apps in this ecosystem set `blockr.ui::html_table_display`, and replacing it
#' with the console print would be a visible downgrade on every non-series
#' block. Resolved when it is needed rather than when the package is built,
#' because whether blockr.ui is around is a property of the app.
#'
#' @param x A `ts_display`.
#' @noRd
ts_fallback <- function(x) {
  if (!is.null(x$fallback)) {
    return(x$fallback)
  }
  if (requireNamespace("blockr.ui", quietly = TRUE)) {
    return(getExportedValue("blockr.ui", "html_table_display"))
  }
  blockr.core::minimal_display
}

#' Can the fallback share our container?
#'
#' A render function has to match its output container, and blockr.core's seam
#' gives a block ONE id and ONE container, so two displays only compose when
#' both want the same kind. `uiOutput()` covers the chart and blockr.ui's
#' table; `minimal_display` wants a `verbatimTextOutput()` and cannot be
#' delegated to, which is why the branch below rebuilds its markup instead.
#'
#' Asked by building the fallback's own UI and looking at it, rather than by
#' keeping a list of which displays are which.
#'
#' @param fallback A `tabular_display`.
#' @noRd
shares_container <- function(fallback) {
  isTRUE(
    tryCatch(
      grepl(
        "shiny-html-output",
        paste(
          as.character(blockr.core::tabular_ui(fallback, "probe")),
          collapse = " "
        ),
        fixed = TRUE
      ),
      error = function(e) FALSE
    )
  )
}

#' @export
tabular_ui.ts_display <- function(x, id) {
  fb <- ts_fallback(x)
  htmltools::tagList(
    ts_chart_dep(),
    # The toggle sits OUTSIDE the output container on purpose. In table mode
    # the container holds the fallback's markup, which has nowhere to put a
    # control of ours, and a switch you can use to leave a view but not to
    # come back is not a switch.
    ts_view_toggle(id),
    # The fallback's own UI when it fits, so its dependencies come along. When
    # it does not fit we still need a `uiOutput()`, and the fallback's markup
    # is rebuilt in `delegate_output()`.
    if (shares_container(fb)) {
      blockr.core::tabular_ui(fb, id)
    } else {
      shiny::uiOutput(id)
    }
  )
}

#' @export
tabular_output.ts_display <- function(x, result, block, session) {

  # One conversion decides and supplies: see `ts_frame()`. Doing it here rather
  # than inside the `renderUI()` means the choice between chart and table is
  # made once per result, not on every invalidation of the render.
  d <- ts_frame(result)

  # THE SWITCH IS REVEALED BY THE CHART, NOT BY A MESSAGE FROM HERE. A block
  # that renders at startup does so before its markup is on the page, so a
  # push would land on an element that does not exist yet and nothing would
  # ever re-send it. The chart's own mount script runs when the chart is in
  # the DOM, which is exactly when the switch becomes meaningful.
  #
  # The message below only ever takes the switch away, from a block that could
  # draw and now cannot (a filter that empties the series, a mutate that turns
  # the value column into text). By then the element is there.
  if (is.null(d)) {
    ts_view_hide(session)
    return(delegate_output(ts_fallback(x), result, block, session))
  }

  if (identical(ts_view(session), "table")) {

    # A MODEL IS NOT A FRAME, so for a fitted forecast the other side of the
    # switch is the MODEL OUTPUT rather than the app's table preview, which
    # would print a list. Same shape as the seas card: the chart is the model
    # drawn, the hamburger is the model read.
    if (inherits(result, "ts_forecast")) {
      return(shiny::renderUI(fc_model_view(result)))
    }

    # WHATEVER THE CHART ACCEPTED, THE TABLE HAS TO ACCEPT TOO, or the switch
    # is a button that breaks the panel. tsbox reads `ts`, `xts`, `zoo` and
    # the rest, so `ts_frame()` resolves a plain `ts` object happily and the
    # table preview then dies on it: `table_page()` on an `AirPassengers`
    # returns "missing value where TRUE/FALSE needed". Convert at this one
    # boundary rather than teaching every fallback display about time series.
    flat <- if (is.data.frame(result)) {
      result
    } else {
      tryCatch(suppressMessages(tsbox::ts_tbl(result)), error = function(e) result)
    }
    return(delegate_output(ts_fallback(x), flat, block, session))
  }

  shiny::renderUI(ts_chart_html(d, session$ns("result_view_toggle")))
}

# The board options the fallback reads (page size and the like) have to keep
# invalidating the preview, or changing one would stop redrawing the blocks
# this display fronts.

#' @export
tabular_trigger.ts_display <- function(x, session) {
  # THIS IS WHAT MAKES THE TOGGLE WORK, and it is the designed hook rather than
  # a trick. `block_render_trigger()` runs inside the block's own output
  # observer, so reading the toggle here takes a per-block reactive dependency
  # on it; flipping it re-runs `block_output()`, which re-runs
  # `tabular_output()`, which is then free to return the chart's render OR the
  # fallback's. Deciding inside a `renderUI()` instead would not work: the
  # fallback hands back a render function, and a render function cannot be
  # returned from inside another one.
  ts_view(session)
  blockr.core::tabular_trigger(ts_fallback(x), session)
}

#' Which view this block is showing
#'
#' The output is always called `result` inside a block module, so the input the
#' toggle writes is `result_view` in the same namespace, which is also what
#' `ts_view_toggle()` builds its element id from.
#'
#' @param session The block's module session.
#' @return `"chart"` or `"table"`.
#' @noRd
ts_view <- function(session) {
  v <- tryCatch(session$input[["result_view"]], error = function(e) NULL)
  if (identical(v, "table")) "table" else "chart"
}

#' @export
tabular_options.ts_display <- function(x, ...) {
  blockr.core::tabular_options(ts_fallback(x), ...)
}

#' Hand a non-series result to the fallback display
#'
#' Calls the fallback's own renderer where the containers match, which is the
#' whole point: the app's preview comes back exactly as it was, page size,
#' sorting and all. Where they do not match there is nothing to call, so the
#' markup is rebuilt, which is a reimplementation and as fragile as it sounds.
#' There is no third option until blockr.core can compose two displays.
#'
#' @param fallback,result,block,session As passed to `tabular_output()`.
#' @noRd
delegate_output <- function(fallback, result, block, session) {

  if (shares_container(fallback)) {
    return(blockr.core::tabular_output(fallback, result, block, session))
  }

  shiny::renderUI({
    if (is.null(result)) {
      return(NULL)
    }
    dat <- as.data.frame(utils::head(result, 5L))
    val <- tryCatch(
      tibble::as_tibble(dat, .name_repair = "minimal"),
      error = function(e) dat
    )
    shiny::tags$pre(
      class = "blockr-minimal-preview",
      paste(utils::capture.output(print(val)), collapse = "\n")
    )
  })
}
