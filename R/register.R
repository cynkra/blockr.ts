#' Register the time series blocks
#'
#' Four entries, where there were fourteen. The split is by INPUT SHAPE, not
#' by function: sixteen unary tsbox verbs are one block with a function
#' picker, seven ways of combining series are another, and picking a few
#' series out of many is a third because it needs a list you can read rather
#' than a select you cannot.
#'
#' @export
register_ts_blocks <- function() {
  blockr.core::register_blocks(
    c(
      "new_ts_dataset_block",
      "new_ts_transform_block",
      "new_ts_combine_block",
      "new_ts_pick_block",
      "new_ts_forecast_block",
      "new_ts_forecast_series_block"
    ),
    name = c(
      "Time series dataset",
      "Transform series",
      "Combine series",
      "Pick series",
      "Forecast",
      "Forecast series"
    ),
    description = c(
      "A built-in ts or mts object as a long frame",
      "One series in, one out: changes, index, span, lag, frequency, trend, gaps",
      "Two or more series in: side by side, append, chain-link, ratio, difference",
      "Choose a few series out of many, by shape",
      "Fit ETS, ARIMA, Theta or VAR and forecast ahead; returns the models",
      "A fitted forecast as a frame: point path, intervals, fitted, accuracy"
    ),
    category = c("input", "transform", "transform", "transform",
                 "transform", "transform"),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
