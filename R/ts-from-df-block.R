#' Convert Wide DataFrame to Time Series Long Format
#'
#' A transform block that converts wide-format data frames to long-format
#' time series data compatible with tsbox. The resulting data can be 
#' visualized as a dygraph and used with other time series blocks.
#'
#' @param ... Additional arguments passed to new_ts_transform_block
#'
#' @details
#' This block uses tsbox::ts_long() to convert wide data frames where:
#' - The time column is automatically detected (character, Date, POSIXct, etc. are all accepted)
#' - All other numeric columns are treated as separate time series
#' - The output is in long format with columns: id, time, value
#'
#' The resulting data frame is compatible with all other ts blocks and will
#' be displayed as an interactive dygraph.
#'
#' @examples
#' \dontrun{
#' # Create sample wide data
#' df_wide <- data.frame(
#'   date = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
#'   sales = rnorm(12, 100, 10),
#'   revenue = rnorm(12, 1000, 100)
#' )
#' 
#' # Convert to time series format
#' blockr.core::serve(
#'   new_ts_from_df_block(),
#'   data = list(data = df_wide)
#' )
#' }
#'
#' @export
new_ts_from_df_block <- function(...) {
  new_ts_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          # Show info about the conversion
          output$conversion_info <- renderUI({
            data_val <- data()
            if (is.null(data_val)) {
              return(helpText("No data available"))
            }
            
            # Count numeric columns (excluding time column)
            time_cols <- names(data_val)[sapply(data_val, function(x) {
              inherits(x, c("Date", "POSIXct", "POSIXlt", "POSIXt"))
            })]
            
            numeric_cols <- names(data_val)[sapply(data_val, is.numeric)]
            
            if (length(time_cols) > 0) {
              helpText(
                icon("info-circle"),
                sprintf(
                  "Converting %d series from wide to long format. Time column: %s",
                  length(numeric_cols),
                  time_cols[1]
                )
              )
            } else {
              helpText(
                icon("warning"),
                "No time column detected. tsbox::ts_long() will attempt conversion."
              )
            }
          })
          
          list(
            expr = reactive({
              # Simple conversion using ts_long
              parse(text = "tsbox::ts_long(data)")[[1]]
            }),
            state = list()
          )
        }
      )
    },
    function(id) {
      tagList(
        # Add responsive CSS
        tags$style(HTML(
          "
          .ts-from-df-container {
            width: 100%;
            margin: 0px;
            padding-bottom: 15px;
          }
          
          .ts-from-df-info {
            padding: 10px;
            background-color: #f8f9fa;
            border-radius: 4px;
            margin-bottom: 10px;
          }
          "
        )),
        div(
          class = "ts-from-df-container",
          div(
            class = "ts-from-df-info",
            tags$h4("DataFrame to Time Series Converter"),
            uiOutput(NS(id, "conversion_info")),
            helpText(
              "This block converts wide-format data frames to long-format time series data.",
              "The time column is automatically detected, and all numeric columns become separate series."
            )
          )
        )
      )
    },
    class = "ts_from_df_block",
    ...
  )
}