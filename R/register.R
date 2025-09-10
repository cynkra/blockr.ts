#' Register time series blocks
#'
#' Register the time series blocks with the blockr registry
#'
#' @export
register_ts_blocks <- function() {
  blockr.core::register_blocks(
    c("new_ts_airpassenger_block", "new_ts_dataset_block", "new_ts_pc_block", "new_ts_change_block", "new_ts_frequency_block", "new_ts_select_block"),
    name = c("AirPassengers Time Series", "Time Series Dataset Selector", "Percentage Change", "Time Series Changes", "Frequency Conversion", "Series Selection"),
    description = c(
      "Display AirPassengers time series as an interactive dygraph",
      "Select and display any built-in R time series dataset",
      "Compute percentage changes for time series data",
      "Calculate various types of changes (%, differences, YoY)",
      "Convert time series to different frequencies (aggregate/disaggregate)",
      "Select specific series from multivariate time series data"
    ),
    category = c("data", "data", "transform", "transform", "transform", "transform"),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
