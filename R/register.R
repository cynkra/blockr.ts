#' Register time series blocks
#'
#' Register the time series data blocks with the blockr registry
#'
#' @export
register_ts_blocks <- function() {
  blockr.core::register_blocks(
    c("new_ts_airpassenger_block"),
    name = c("AirPassengers Time Series"),
    description = c(
      "Display AirPassengers time series as an interactive dygraph"
    ),
    category = c("data"),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
