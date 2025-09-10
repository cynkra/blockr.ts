#' Register time series blocks
#'
#' Register the time series data block with the blockr registry
#'
#' @export
register_ts_blocks <- function() {
  blockr.core::register_blocks(
    c("new_ts_block"),
    name = c("Time Series Data"),
    description = c(
      "Load and transform time series data with format conversion support"
    ),
    category = c("data"),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
