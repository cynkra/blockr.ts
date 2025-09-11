#' Convert Time Series Data to DataFrame
#'
#' A transform block that converts time series data to regular data frame format.
#' Provides options to output in either long format (default) or wide format.
#' This block removes the dygraph visualization and returns a standard data table.
#'
#' @param format Character string specifying output format: "long" (default) or "wide"
#' @param ... Additional arguments passed to new_transform_block
#'
#' @details
#' This block converts time series data to regular data frames:
#' - **Long format**: Preserves the tsbox long format (id, time, value columns)
#' - **Wide format**: Uses tsbox::ts_wide() to pivot series into separate columns
#'
#' Unlike other ts blocks, this block does NOT inherit from ts_block,
#' so the output is displayed as a regular data table instead of a dygraph.
#'
#' @examples
#' \dontrun{
#' # Convert time series to wide format data frame
#' blockr.core::serve(
#'   new_ts_dataset_block(dataset = "EuStockMarkets"),
#'   new_ts_to_df_block(format = "wide")
#' )
#' 
#' # Keep long format for further processing
#' blockr.core::serve(
#'   new_ts_dataset_block(dataset = "AirPassengers"),
#'   new_ts_change_block(method = "pc"),
#'   new_ts_to_df_block(format = "long")
#' )
#' }
#'
#' @export
new_ts_to_df_block <- function(format = "long", ...) {
  # Validate format parameter
  format <- match.arg(format, c("long", "wide"))
  
  # Use regular transform block (NOT ts_transform_block) to avoid dygraph rendering
  blockr.core::new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          # Reactive value for format selection
          r_format <- reactiveVal(format)
          
          # Observer for format input
          observeEvent(input$format, {
            r_format(input$format)
          })
          
          # Dynamic description based on format
          output$format_description <- renderUI({
            current_format <- r_format()
            
            desc <- switch(
              current_format,
              "long" = "Output in long format with id, time, and value columns",
              "wide" = "Output in wide format with time column and separate columns for each series",
              "Unknown format"
            )
            
            helpText(
              icon("info-circle"),
              desc
            )
          })
          
          # Show data preview info
          output$data_info <- renderUI({
            data_val <- data()
            if (is.null(data_val)) {
              return(helpText("No data available"))
            }
            
            # Check if data has multiple series
            n_series <- if ("id" %in% names(data_val)) {
              length(unique(data_val$id))
            } else {
              1
            }
            
            n_rows <- nrow(data_val)
            
            helpText(
              sprintf(
                "Input: %d rows, %d series",
                n_rows,
                n_series
              )
            )
          })
          
          list(
            expr = reactive({
              # Build expression based on selected format
              selected_format <- r_format()
              
              if (selected_format == "long") {
                # For long format, ensure data is in tsbox tibble format
                parse(text = "tsbox::ts_tbl(data)")[[1]]
              } else {
                # For wide format, use ts_wide
                parse(text = "tsbox::ts_wide(data)")[[1]]
              }
            }),
            state = list(
              format = r_format
            )
          )
        }
      )
    },
    function(id) {
      tagList(
        # Add responsive CSS
        tags$style(HTML(
          "
          .ts-to-df-container {
            width: 100%;
            margin: 0px;
            padding-bottom: 15px;
          }
          
          .ts-to-df-form-grid {
            display: grid;
            gap: 15px;
            grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
          }
          
          .ts-to-df-section {
            padding: 10px;
            background-color: #f8f9fa;
            border-radius: 4px;
          }
          
          .ts-to-df-section h4 {
            margin-top: 0;
            margin-bottom: 10px;
            font-size: 1.1rem;
            font-weight: 600;
            color: #333;
          }
          
          .ts-to-df-input-wrapper {
            margin-bottom: 10px;
          }
          "
        )),
        div(
          class = "ts-to-df-container",
          div(
            class = "ts-to-df-form-grid",
            
            # Format Selection Section
            div(
              class = "ts-to-df-section",
              tags$h4("Output Format"),
              div(
                class = "ts-to-df-input-wrapper",
                selectInput(
                  NS(id, "format"),
                  label = "Select Format:",
                  choices = c(
                    "Long format (id, time, value)" = "long",
                    "Wide format (time, series1, series2, ...)" = "wide"
                  ),
                  selected = format
                )
              ),
              uiOutput(NS(id, "format_description"))
            ),
            
            # Data Info Section
            div(
              class = "ts-to-df-section",
              tags$h4("Data Information"),
              uiOutput(NS(id, "data_info")),
              helpText(
                "This block converts time series data to a regular data frame,",
                "removing the dygraph visualization."
              )
            )
          )
        )
      )
    },
    dat_val = function(data) {
      # Validate that input is a data.frame
      stopifnot(is.data.frame(data))
    },
    class = c("ts_to_df_block"),
    ...
  )
}