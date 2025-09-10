#' Register time series blocks
#'
#' Register the time series blocks with the blockr registry
#'
#' @export
register_ts_blocks <- function() {
  blockr.core::register_blocks(
    c("new_ts_airpassenger_block", "new_ts_dataset_block", "new_ts_pc_block", "new_ts_change_block", 
      "new_ts_frequency_block", "new_ts_select_block", "new_ts_lag_block", "new_ts_span_block",
      "new_ts_scale_block", "new_ts_forecast_block", "new_ts_decompose_block", "new_ts_pca_block"),
    name = c("AirPassengers Time Series", "Time Series Dataset Selector", "Percentage Change", "Time Series Changes", 
             "Frequency Conversion", "Series Selection", "Lag/Lead Transform", "Time Range Selection",
             "Scale & Index", "Forecast", "Decomposition", "PCA"),
    description = c(
      "Display AirPassengers time series as an interactive dygraph",
      "Select and display any built-in R time series dataset",
      "Compute percentage changes for time series data",
      "Calculate various types of changes (%, differences, YoY)",
      "Convert time series to different frequencies (aggregate/disaggregate)",
      "Select specific series from multivariate time series data",
      "Shift time series forward (lag) or backward (lead)",
      "Filter time series to a specific date range",
      "Scale, normalize, or index time series data",
      "Generate forecasts with confidence intervals",
      "Extract trend, seasonal, and remainder components",
      "Principal component analysis for multivariate series"
    ),
    category = c("data", "data", "transform", "transform", "transform", "transform", "transform", "transform",
                 "transform", "transform", "transform", "transform"),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
