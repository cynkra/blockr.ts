#' Register time series blocks
#'
#' Register the time series data blocks with the blockr registry
#'
#' @export
register_ts_blocks <- function() {
  blockr.core::register_blocks(
    c("new_ts_airpassenger_block", "new_ts_dataset_block"),
    name = c("AirPassengers Time Series", "Time Series Dataset Selector"),
    description = c(
      "Display AirPassengers time series as an interactive dygraph",
      "Select and display any built-in R time series dataset"
    ),
    category = c("data", "data"),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
