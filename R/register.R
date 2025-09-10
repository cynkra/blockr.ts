#' Register time series blocks
#'
#' Register the time series blocks with the blockr registry
#'
#' @export
register_ts_blocks <- function() {
  blockr.core::register_blocks(
    c("new_ts_airpassenger_block", "new_ts_dataset_block", "new_ts_pc_block"),
    name = c("AirPassengers Time Series", "Time Series Dataset Selector", "Percentage Change"),
    description = c(
      "Display AirPassengers time series as an interactive dygraph",
      "Select and display any built-in R time series dataset",
      "Compute percentage changes for time series data"
    ),
    category = c("data", "data", "transform"),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
