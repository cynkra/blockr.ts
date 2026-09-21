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
#' TREATS THAT GUESS AS AN OBSERVATION, so it is narrower than the truth.
#' That is why `"lag"` is the default, and why "from a second input", which
#' is the honest third answer, is the one worth building next.
#'
#' The engines: `"ets"` is exponential smoothing with the state chosen by
#' AICc, `"arima"` is `auto.arima()` and the only one here that takes
#' regressors, `"theta"` is univariate and hard to beat, and `"var"` is the
#' joint one.
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
      shiny::tagList(
        blockr.dplyr::blockr_core_js_dep(),
        blockr.dplyr::blockr_select_dep(),
        ts_gear(shiny::NS(id), fc_css(),
                shiny::uiOutput(shiny::NS(id, "form")), open = TRUE)
      )
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
      ts_select_multi(ns("target"), "Target", ids, cur$target,
                      empty = "every series"),
      ts_select(ns("engine"), "Engine",
                stats::setNames(names(labels), unname(labels)), engine),
      fc_number(ns("h"), "Horizon", cur$h, min = 1)
    ),
    ts_fields(
      ts_select_multi(ns("levels"), "Intervals",
                      c("50", "80", "90", "95", "99"),
                      as.character(cur$levels), empty = "none",
                      class = "blockr-ts-field--narrow"),
      if (spec$xreg) {
        ts_select_multi(ns("xreg"), "Regressors", setdiff(ids, cur$target),
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
    )
  )
}

#' A multi-select, the design-system one
#'
#' `Blockr.Select.multi` from blockr.dplyr, not a native `<select multiple>`.
#' The list box the first version used is the browser's own control: it cannot
#' be sized to the row it sits in, it shows a scrolling window rather than
#' what is chosen, and it does not read as anything else in a blockr board.
#' Single selects stay native here, because `ts_select()` already styles one
#' into the 42px bordered control and a native `<select>` collapsed to one
#' line is the same shape as the component.
#'
#' THE OPTIONS ARE BAKED INTO THE MOUNT, not pushed over a custom message.
#' They only change when the data or the engine changes, and those are exactly
#' what re-render this form, so the options arrive with the markup that shows
#' them. That removes the push entirely, and with it the handshake where a
#' block mounting on a deferred panel gets its options before its container
#' exists.
#'
#' Nothing is ever read back OUT of the widget: the block owns the value, the
#' widget reports changes, and `setOptions()` is never called, so there is no
#' path by which an option list can masquerade as a user's pick.
#'
#' @param id Namespaced input id the pick is written to.
#' @param label Field label.
#' @param choices Character vector.
#' @param selected Character vector.
#' @param empty What an empty selection MEANS, shown as the placeholder.
#'   The placeholder is the field's value while nothing is picked, so it says
#'   "every series" rather than describing itself: an empty target and a
#'   target of every series look identical otherwise.
#' @noRd
ts_select_multi <- function(id, label, choices, selected, empty = "none",
                            class = NULL) {

  cid <- paste0(id, "_cont")

  htmltools::div(
    class = paste("blockr-ts-field blockr-ts-field--multi", class),
    htmltools::tags$label(class = "control-label", label),
    htmltools::div(id = cid, class = "blockr-select--bordered"),
    htmltools::tags$script(htmltools::HTML(sprintf(
      "(function(){var f=function(){
         if(!window.Blockr||!Blockr.Select||!window.Shiny){return setTimeout(f,50);}
         var el=document.getElementById(%s); if(!el){return setTimeout(f,50);}
         el.innerHTML='';
         el._sel=Blockr.Select.multi(el,{options:%s,selected:%s,placeholder:%s,
           singleLine:true,
           onChange:function(v){Shiny.setInputValue(%s,v,{priority:'event'});}});
       };f();})();",
      jsonlite::toJSON(cid, auto_unbox = TRUE),
      jsonlite::toJSON(as.character(choices)),
      jsonlite::toJSON(as.character(selected)),
      jsonlite::toJSON(empty, auto_unbox = TRUE),
      jsonlite::toJSON(id, auto_unbox = TRUE)
    )))
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
    .blockr-ts-field--multi { flex: 1 1 200px; }
    /* A field that wraps onto a row of its own should not then grow to fill
       it. Two interval chips in a full-width control read as a mistake. */
    .blockr-ts-field--narrow { flex: 0 1 210px; }
    .blockr-ts-field--num { flex: 0 1 110px; }
  "))
}
