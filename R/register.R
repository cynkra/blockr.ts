#' Register time series blocks
#'
#' Register the time series data block with the blockr registry
#'
#' @export
register_ts_blocks <- function() {
  blockr.core::register_blocks(
    c("new_ts_block", "new_ts_pc_block", "new_ts_plot_block"),
    name = c("Time Series Data", "Time Series Percentage Change", "Time Series Plot"),
    description = c(
      "Load and transform time series data with format conversion support",
      "Calculate percentage changes and differences in time series data",
      "Create interactive time series visualizations using tsbox and plotly"
    ),
    category = c("data", "transform", "plot"),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
