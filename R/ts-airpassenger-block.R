#' AirPassengers Time Series Data Block
#'
#' A simple time series data block that displays AirPassengers data as an interactive dygraph.
#' This block demonstrates time series visualization using tsbox and dygraphs.
#'
#' @param ... Additional arguments passed to new_ts_data_block
#'
#' @export
new_ts_airpassenger_block <- function(...) {
  new_ts_data_block(
    function(id) {
      moduleServer(
        id,
        function(input, output, session) {
          list(
            # Simple static expression - no parameters needed
            # Returns a data frame that will be converted to dygraph in block_output
            expr = reactive({
              parse(text = "tsbox::ts_tbl(datasets::AirPassengers)")[[1]]
            }),
            # No state needed for parameterless block
            state = list()
          )
        }
      )
    },
    function(id) {
      tagList(
        # Centralized responsive CSS
        ts_responsive_css(),

        div(
          class = "ts-block-container",

          # Simple info display
          div(
            class = "ts-block-help-text",
            tags$h4("AirPassengers Time Series"),
            p("Monthly totals of international airline passengers from 1949 to 1960."),
            p("This block displays the classic AirPassengers dataset as an interactive dygraph.")
          )
        )
      )
    },
    class = "ts_airpassenger_block",
    ...
  )
}
