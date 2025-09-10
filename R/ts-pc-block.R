#' Time Series Percentage Change Transform Block
#'
#' A simple transform block that computes percentage changes for time series data.
#' Uses tsbox::ts_pc() to calculate the percentage change between consecutive observations.
#'
#' @param ... Additional arguments passed to new_ts_transform_block
#'
#' @details
#' This block:
#' - Takes time series data as input (from a ts data block)
#' - Applies tsbox::ts_pc() to compute percentage changes
#' - Returns data where each value is the percentage change from the previous value
#' - The first value will be NA (no previous value to compare)
#' - Works with both univariate and multivariate time series
#'
#' @export
new_ts_pc_block <- function(...) {
  new_ts_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          # Simple transform - no parameters, no UI
          list(
            expr = reactive({
              # Simply apply ts_pc to the data
              parse(text = "tsbox::ts_pc(data)")[[1]]
            }),
            state = list()  # No state needed for parameterless block
          )
        }
      )
    },
    function(id) {
      # No UI needed - just return empty tagList
      tagList()
    },
    dat_val = function(data) {
      # Validate that input is a data.frame
      stopifnot(is.data.frame(data))
    },
    class = "ts_pc_block",
    ...
  )
}