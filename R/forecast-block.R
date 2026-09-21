#' Forecast a time series
#'
#' A frame in, the fitted models out. One block for every engine: pick the
#' target series, pick what fits them, say how far ahead.
#'
#' @details
#' THE BLOCK RETURNS THE MODEL, not a frame, the same contract the seasonal
#' adjustment block has. A forecast is a point path, two or more interval
#' paths, the fitted values, the residuals and whatever the engine chose, and
#' collapsing all of that to one frame at the block boundary throws away
#' everything the second question needs.
#' [new_ts_forecast_series_block()] turns it back into a frame when something
#' downstream wants one. The preview draws the model because `ts_display`
#' dispatches on the data, so a forecast charts wherever it lands.
#'
#' ONE BLOCK, NOT ONE PER ENGINE. Target, horizon, levels and regressors are
#' the same question whatever fits the model; only the fit differs, and it is
#' a few lines. Four blocks would be four copies of the control machinery, and
#' deciding an ETS should have been an ARIMA would be a delete and two
#' reconnections rather than one click.
#'
#' THE TARGET IS ALWAYS A MULTISELECT AND THE RESULT IS ALWAYS A NAMED LIST,
#' length one when one series is picked. A control that changes the output
#' type when you touch it is a control that breaks the board downstream, which
#' is why there is no `_multi` twin of this block.
#'
#' For every engine except VAR, several targets means a loop, which costs
#' nothing. VAR treats the target set as one system, so there the multiselect
#' is the model specification rather than a convenience.
#'
#' Regressors are the one place this block asks a question it cannot answer
#' for you: a fitted regression cannot predict without values of its
#' regressors over the horizon. `"lag"` shifts them so the values needed were
#' observed; `"hold"` carries the last one forward and the resulting interval
#' treats that guess as an observation, which is why it is not the default.
#'
#' @param target Series to forecast. Empty means all of them.
#' @param engine One of `"ets"`, `"arima"`, `"theta"` or `"var"`.
#' @param h Horizon, in periods.
#' @param levels Interval levels, in per cent.
#' @param xreg Series to use as regressors, for the engines that take them.
#' @param xreg_mode `"lag"` or `"hold"`, see details.
#' @param lag Shift for `xreg_mode = "lag"`. Empty means the horizon, the
#'   only shift for which nothing has to be invented.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @examples
#' new_ts_forecast_block(engine = "arima", h = 24)
#' new_ts_forecast_block(target = c("mdeaths", "fdeaths"), engine = "var")
#'
#' @export
new_ts_forecast_block <- function(target = character(), engine = "ets",
                                  h = 12L, levels = c(80, 95),
                                  xreg = character(), xreg_mode = "lag",
                                  lag = numeric(), ...) {

  engine <- match.arg(engine, names(fc_engines()))
  xreg_mode <- match.arg(xreg_mode, c("lag", "hold"))

  blockr.core::new_transform_block(
    function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {

          r_target <- shiny::reactiveVal(as.character(target))
          r_engine <- shiny::reactiveVal(engine)
          r_h <- shiny::reactiveVal(as.integer(h))
          r_levels <- shiny::reactiveVal(as.numeric(levels))
          r_xreg <- shiny::reactiveVal(as.character(xreg))
          r_xreg_mode <- shiny::reactiveVal(xreg_mode)
          r_lag <- shiny::reactiveVal(as.numeric(lag))

          facts <- shiny::reactive(ts_facts(data()))

          # REBUILT ON THE DATA AND ON THE ENGINE, not on every pick. The
          # series lists come from the data, and the engine decides whether
          # there are regressor controls at all, so those two invalidate the
          # form. A selection does not: re-rendering a select from our own
          # echo of the user's click is the loop that eats the next one.
          output$form <- shiny::renderUI({
            fc_form(session$ns, facts()$ids %||% character(), r_engine(),
                    shiny::isolate(list(
                      target = r_target(), h = r_h(), levels = r_levels(),
                      xreg = r_xreg(), xreg_mode = r_xreg_mode(),
                      lag = r_lag()
                    )))
          })

          shiny::observeEvent(input$engine, {
            if (input$engine %in% names(fc_engines())) r_engine(input$engine)
          })
          shiny::observeEvent(input$target, {
            r_target(as.character(input$target))
          }, ignoreNULL = FALSE)
          # A cleared number input arrives as NA, and an input that is not
          # on the page yet as NULL, which `as.integer()` turns into
          # `integer(0)`. `if (!is.na(integer(0)))` is an error, not FALSE,
          # so the length is checked before the value in all three of these.
          shiny::observeEvent(input$h, {
            v <- suppressWarnings(as.integer(input$h))
            if (length(v) == 1L && !is.na(v) && v >= 1L) r_h(v)
          })
          shiny::observeEvent(input$levels, {
            v <- suppressWarnings(as.numeric(input$levels))
            if (length(v) && !anyNA(v)) r_levels(sort(unique(v)))
          }, ignoreNULL = FALSE)

          shiny::observeEvent(input$xreg, {
            r_xreg(as.character(input$xreg))
          }, ignoreNULL = FALSE)
          shiny::observeEvent(input$xreg_mode, {
            r_xreg_mode(input$xreg_mode)
          })
          shiny::observeEvent(input$lag, {
            v <- suppressWarnings(as.numeric(input$lag))
            r_lag(if (length(v) != 1L || is.na(v)) numeric() else v)
          }, ignoreNULL = FALSE)

          list(
            expr = shiny::reactive({
              ids <- facts()$ids %||% character()
              fc_call(
                target = intersect(r_target(), ids),
                engine = r_engine(),
                h = r_h(),
                levels = r_levels(),
                xreg = intersect(r_xreg(), ids),
                xreg_mode = r_xreg_mode(),
                lag = r_lag()
              )
            }),
            state = list(
              target = r_target,
              engine = r_engine,
              h = r_h,
              levels = r_levels,
              xreg = r_xreg,
              xreg_mode = r_xreg_mode,
              lag = r_lag
            )
          )
        }
      )
    },
    function(id) {
      ts_gear(shiny::NS(id), fc_css(),
              shiny::uiOutput(shiny::NS(id, "form")), open = TRUE)
    },
    dat_val = function(data) ts_validate_series(data),
    # An empty target is the starting state and means every series; empty
    # regressors mean none; an empty lag means the horizon.
    allow_empty_state = c("target", "xreg", "lag"),
    class = "ts_forecast_block",
    ...
  )
}

#' The call the block writes
#'
#' Arguments left at their default are left out, so what you read back in the
#' code panel is the choices that were made. Same rule as the transform block.
#'
#' @noRd
fc_call <- function(target, engine, h, levels, xreg, xreg_mode, lag) {

  args <- list(quote(data))

  if (length(target)) args$target <- target
  if (!identical(engine, "ets")) args$engine <- engine
  if (!identical(as.integer(h), 12L)) args$h <- as.integer(h)
  if (!identical(sort(as.numeric(levels)), c(80, 95))) {
    args$levels <- as.numeric(levels)
  }
  if (length(xreg)) {
    args$xreg <- xreg
    if (!identical(xreg_mode, "lag")) args$xreg_mode <- xreg_mode
    if (length(lag)) args$lag <- as.numeric(lag)
  }

  as.call(c(list(quote(blockr.ts::ts_forecast)), args))
}

#' The gear form
#'
#' @param ns Namespace function.
#' @param ids Series in the data.
#' @param engine Current engine.
#' @param cur Current values.
#' @noRd
fc_form <- function(ns, ids, engine, cur) {

  spec <- fc_engines()[[engine]]
  labels <- vapply(fc_engines(), `[[`, character(1L), "label")

  htmltools::tagList(
    ts_fields(
      fc_select_multi(ns("target"), "Target", ids, cur$target,
                      empty = "every series"),
      ts_select(ns("engine"), "Engine",
                stats::setNames(names(labels), unname(labels)), engine),
      fc_number(ns("h"), "Horizon", cur$h, min = 1)
    ),
    ts_fields(
      fc_select_multi(ns("levels"), "Intervals",
                      c("50", "80", "90", "95", "99"),
                      as.character(cur$levels), empty = "none"),
      if (spec$xreg) {
        fc_select_multi(ns("xreg"), "Regressors", setdiff(ids, cur$target),
                        cur$xreg, empty = "none")
      },
      if (spec$xreg && length(cur$xreg)) {
        ts_select(ns("xreg_mode"), "Over the horizon",
                  c("Lag by the horizon" = "lag",
                    "Hold the last value" = "hold"),
                  cur$xreg_mode)
      },
      if (spec$xreg && length(cur$xreg) && identical(cur$xreg_mode, "lag")) {
        fc_number(ns("lag"), "Lag",
                  if (length(cur$lag)) cur$lag else cur$h, min = 1)
      }
    ),
    ts_hint(
      spec$hint,
      if (spec$joint) {
        " The targets are fitted as one system, so picking a second one changes the model."
      } else {
        " Several targets are fitted one at a time."
      },
      if (spec$xreg && length(cur$xreg) && identical(cur$xreg_mode, "hold")) {
        htmltools::span(
          class = "blockr-ts-fc-warn",
          " Holding a regressor treats a guess as an observation: the interval is narrower than the truth."
        )
      }
    )
  )
}

#' A native multiple select
#'
#' `selectize = FALSE` throughout this package, so this is the browser's own
#' list box with the package's styling on it rather than a component from
#' somewhere else. The empty state says what empty MEANS, because "nothing
#' selected" and "every series" look identical otherwise.
#'
#' @noRd
fc_select_multi <- function(id, label, choices, selected, empty = "none") {
  htmltools::div(
    class = "blockr-ts-field blockr-ts-field--multi",
    shiny::selectInput(
      id, label, choices = choices, selected = selected,
      multiple = TRUE, selectize = FALSE, width = "100%", size = 4
    ),
    htmltools::div(
      class = "blockr-ts-fc-empty",
      if (length(selected)) "" else paste("Empty:", empty)
    )
  )
}

#' @noRd
fc_number <- function(id, label, value, min = NA) {
  htmltools::div(
    class = "blockr-ts-field blockr-ts-field--num",
    shiny::numericInput(id, label, value = value, min = min, width = "100%")
  )
}

#' Styling for the two controls this package did not already have
#'
#' Inline rather than in `inst/css`, the way `ts_view_toggle_css()` is: it is
#' two rules, and a stylesheet file carries the asset-version cache rule with
#' it for no gain.
#'
#' @noRd
fc_css <- function() {
  htmltools::tags$style(htmltools::HTML("
    .blockr-ts-field--multi { flex: 1 1 180px; }
    .blockr-ts-field--multi select { min-height: 92px; }
    .blockr-ts-field--num { flex: 0 1 110px; }
    .blockr-ts-fc-empty {
      font-size: 11px;
      color: #9ca3af;
      margin-top: 2px;
      min-height: 14px;
    }
    .blockr-ts-fc-warn { color: #b45309; }
  "))
}
