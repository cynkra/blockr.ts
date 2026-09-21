#' Turn fitted models back into a frame
#'
#' The other half of [new_ts_forecast_block()]. A model in, a long frame out:
#' the forecast with its intervals, the fitted values, the residuals, the
#' component paths, or one row of accuracy per series.
#'
#' @details
#' THIS IS WHERE THE INTERVAL CONVENTION LIVES, and putting it here rather
#' than on the forecast block is the whole reason the two are separate. A
#' block that hands out a model has the real forecast object and can read its
#' bounds directly. It is only once somebody wants the forecast as DATA that
#' an interval has to survive as something, and `(id, time, value)` has no
#' column for one.
#'
#' So it becomes more series: `sales`, `sales_lower_80`, `sales_upper_80`.
#' `ts_display` pairs them back into a band. The convention is in the id,
#' which is what makes it survive the trip: the frame can go through three
#' dplyr blocks and still draw its band, and a board that never heard of
#' forecasting draws five lines rather than failing.
#'
#' Rejected: an attribute on the frame. One `dplyr::filter()` downstream and
#' the band is gone, silently, with the lines still drawn.
#'
#' `"accuracy"` is the one output that is not a series. It is one row per
#' target, sorted worst first, because the reason to ask for it is to find
#' which series need a hand.
#'
#' The rest: `"fitted"` is in-sample, one value per observed period,
#' `"residuals"` is observed minus fitted, and `"components"` is the level,
#' trend and season paths, which ETS has and the other engines do not.
#'
#' @param what `"forecast"`, `"fitted"`, `"residuals"`, `"components"` or
#'   `"accuracy"`.
#' @param levels Which intervals to keep. Empty means all the fitted ones.
#' @param keep_history Whether the observed series comes along.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @examples
#' new_ts_forecast_series_block(what = "accuracy")
#' new_ts_forecast_series_block(levels = 95, keep_history = FALSE)
#'
#' @export
new_ts_forecast_series_block <- function(what = "forecast",
                                         levels = numeric(),
                                         keep_history = TRUE, ...) {

  what <- match.arg(what, fc_what_keys())

  blockr.core::new_transform_block(
    function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {

          r_what <- shiny::reactiveVal(what)
          r_levels <- shiny::reactiveVal(as.numeric(levels))
          r_keep <- shiny::reactiveVal(isTRUE(keep_history))

          # The levels on offer are the ones that were FITTED, read off the
          # model rather than from a fixed list. Asking for a 99 that the
          # forecast block never computed is not a question this block can
          # answer, so it is not a question it asks.
          fitted_levels <- shiny::reactive({
            v <- attr(data(), "levels")
            if (is.null(v)) numeric() else as.numeric(v)
          })

          offered <- shiny::reactive(fc_what_offered(data()))

          # A block set to "components" whose upstream engine changes to ARIMA
          # holds a pick that is no longer on offer. Falling back is right;
          # keeping it would error on every render, and the board would have
          # no way to say so. Same shape as `ts_one_of()`.
          what_now <- shiny::reactive({
            if (r_what() %in% offered()) r_what() else "forecast"
          })

          output$form <- shiny::renderUI({
            fc_series_form(session$ns, what_now(), offered(), fitted_levels(),
                           shiny::isolate(r_levels()),
                           shiny::isolate(r_keep()))
          })

          shiny::observeEvent(input$what, {
            if (input$what %in% fc_what_keys()) r_what(input$what)
          })
          shiny::observeEvent(input$levels, {
            v <- suppressWarnings(as.numeric(input$levels))
            r_levels(if (!length(v) || anyNA(v)) numeric() else sort(unique(v)))
          }, ignoreNULL = FALSE)
          shiny::observeEvent(input$keep_history, {
            r_keep(isTRUE(input$keep_history))
          })

          list(
            expr = shiny::reactive({
              fc_series_call(what_now(), intersect(r_levels(), fitted_levels()),
                             r_keep())
            }),
            state = list(
              what = r_what,
              levels = r_levels,
              keep_history = r_keep
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
    dat_val = function(data) fc_validate_model(data),
    # No levels picked means every level the model carries.
    allow_empty_state = "levels",
    class = "ts_forecast_series_block",
    ...
  )
}

#' @noRd
fc_what_keys <- function() {
  c("forecast", "fitted", "residuals", "components", "accuracy")
}

#' What THIS model can be asked for
#'
#' Only ETS carries state paths, so "Components" against an ARIMA, a Theta or
#' a VAR is a control that can only fail. Same rule the interval list already
#' follows: the choices come from the model rather than from a fixed list, so
#' a question the block cannot answer is a question it does not ask.
#'
#' `ts_forecast_frame()` still errors when it is called with `"components"`
#' directly. That is the right answer to a call written by hand; it is the
#' wrong way to find out from a dropdown.
#'
#' @param x A `ts_forecast`, or anything else.
#' @noRd
fc_what_offered <- function(x) {
  keys <- fc_what_keys()
  if (!inherits(x, "ts_forecast") || !fc_has_components(x)) {
    keys <- setdiff(keys, "components")
  }
  keys
}

#' @noRd
fc_has_components <- function(x) {
  length(x) > 0L &&
    all(vapply(x, function(f) !is.null(f$model$states), logical(1)))
}

#' @noRd
fc_what_labels <- function() {
  c(forecast = "Forecast", fitted = "Fitted", residuals = "Residuals",
    components = "Components", accuracy = "Accuracy")
}


#' @noRd
fc_series_call <- function(what, levels, keep_history) {
  args <- list(quote(data))
  if (!identical(what, "forecast")) args$what <- what
  if (length(levels)) args$levels <- as.numeric(levels)
  if (!isTRUE(keep_history)) args$keep_history <- FALSE
  as.call(c(list(quote(blockr.ts::ts_forecast_frame)), args))
}

#' @noRd
fc_series_form <- function(ns, what, offered, fitted_levels, levels, keep) {

  lab <- fc_what_labels()[offered]

  htmltools::tagList(
    ts_fields(
      ts_select(ns("what"), "Extract",
                stats::setNames(names(lab), unname(lab)), what),
      # The interval controls belong to the forecast and to nothing else, so
      # they are absent for the other four rather than present and inert.
      if (identical(what, "forecast")) {
        ts_select_multi(ns("levels"), "Intervals",
                        as.character(fitted_levels), as.character(levels),
                        empty = "every fitted level",
                        class = "blockr-ts-field--narrow")
      },
      if (identical(what, "forecast")) {
        ts_check(ns("keep_history"), "Keep the history", keep)
      }
    )
  )
}

#' Refuse anything that is not a fitted forecast, and say what makes one
#' @noRd
fc_validate_model <- function(data) {
  if (inherits(data, "ts_forecast")) {
    return(invisible(TRUE))
  }
  stop(
    "This block takes fitted models, not a frame. Connect it to a forecast ",
    "block.",
    call. = FALSE
  )
}
