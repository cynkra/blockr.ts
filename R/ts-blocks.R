#' Time Series Data Block with Dygraph Display
#'
#' A simple time series data block that displays AirPassengers data as an interactive dygraph.
#' This block demonstrates time series visualization using tsbox and dygraphs.
#'
#' @param ... Additional arguments passed to new_data_block
#'
#' @export
new_ts_block <- function(...) {
  blockr.core::new_data_block(
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
        div(
          class = "ts-block-container",

          # Simple CSS for clean layout
          tags$style(HTML(
            "
            .ts-block-container {
              padding: 15px;
              background: #f8f9fa;
              border-radius: 8px;
              margin-bottom: 15px;
            }
            .ts-block-info {
              color: #495057;
            }
            .ts-block-info h4 {
              margin-top: 0;
              margin-bottom: 8px;
              font-size: 16px;
            }
          "
          )),

          # Simple info display
          div(
            class = "ts-block-info",
            h4("AirPassengers Time Series"),
            p(
              "Monthly totals of international airline passengers from 1949 to 1960."
            ),
            helpText(
              "This block displays the classic AirPassengers dataset as an interactive dygraph.",
              "The data is automatically converted to a data frame format using tsbox."
            )
          )
        )
      )
    },
    class = "ts_block",
    ...
  )
}

#' @export
block_ui.ts_block <- function(id, x, ...) {
  tagList(
    dygraphs::dygraphOutput(NS(id, "result"))
  )
}

#' @export
block_output.ts_block <- function(x, result, session) {
  # This method overrides the default data table display
  # and shows a dygraph instead using tsbox::ts_dygraphs()
  dygraphs::renderDygraph({
    if (is.null(result)) {
      return(NULL)
    }
    
    # Use tsbox::ts_dygraphs to create the dygraph
    # This is where ts_dygraphs is called!
    dygraph <- tsbox::ts_dygraphs(result)
    
    # Add nice styling
    dygraph <- dygraphs::dyOptions(dygraph, 
                                    fillGraph = FALSE, 
                                    drawGrid = TRUE,
                                    colors = "#007bff")
    dygraph <- dygraphs::dyRangeSelector(dygraph, height = 20)
    
    dygraph
  })
}
