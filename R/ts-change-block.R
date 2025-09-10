#' Time Series Change Calculation Block
#'
#' A transform block that calculates various types of changes in time series data.
#' Supports percentage changes, differences, and year-over-year comparisons.
#'
#' @param method Character string specifying the calculation method:
#'   - "pc": Period-on-period percentage change
#'   - "pcy": Year-on-year percentage change
#'   - "pca": Annualized percentage change
#'   - "diff": First differences (absolute change)
#'   - "diffy": Year-on-year differences
#' @param ... Additional arguments passed to new_ts_transform_block
#'
#' @details
#' This block provides multiple methods for calculating changes in time series:
#' - **pc**: Percentage change from previous period (e.g., month-to-month)
#' - **pcy**: Percentage change from same period last year
#' - **pca**: Annualized percentage change rate
#' - **diff**: Simple difference from previous period
#' - **diffy**: Difference from same period last year
#'
#' @export
new_ts_change_block <- function(method = "pc", ...) {
  
  # Validate method parameter
  method <- match.arg(method, c("pc", "pcy", "pca", "diff", "diffy"))
  
  new_ts_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          
          # Reactive value for method selection
          r_method <- reactiveVal(method)
          
          # Observer for method input
          observeEvent(input$method, {
            r_method(input$method)
          })
          
          # Dynamic description based on method
          output$method_description <- renderUI({
            current_method <- r_method()
            
            desc <- switch(current_method,
              "pc" = "Calculating period-on-period percentage change (e.g., month-to-month)",
              "pcy" = "Calculating year-on-year percentage change (same period last year)",
              "pca" = "Calculating annualized percentage change rate",
              "diff" = "Calculating first differences (absolute change from previous period)",
              "diffy" = "Calculating year-on-year differences (absolute change from same period last year)",
              "Unknown method"
            )
            
            helpText(
              icon("info-circle"),
              desc
            )
          })
          
          list(
            expr = reactive({
              # Build expression based on selected method
              selected_method <- r_method()
              
              # Map method to tsbox function
              func_name <- switch(selected_method,
                "pc" = "ts_pc",
                "pcy" = "ts_pcy",
                "pca" = "ts_pca",
                "diff" = "ts_diff",
                "diffy" = "ts_diffy"
              )
              
              # Create expression
              expr_text <- glue::glue("tsbox::{func_name}(data)")
              parse(text = expr_text)[[1]]
            }),
            state = list(
              method = r_method
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
          .ts-block-container {
            width: 100%;
            margin: 0px;
            padding: 0px;
            padding-bottom: 15px;
          }
          
          .ts-block-form-grid {
            display: grid;
            gap: 15px;
            grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
          }
          
          .ts-block-section,
          .ts-block-section-grid {
            display: contents;
          }
          
          .ts-block-section h4 {
            grid-column: 1 / -1;
            margin-top: 5px;
            margin-bottom: 0px;
            font-size: 1.1rem;
            font-weight: 600;
            color: #333;
          }
          
          .ts-block-input-wrapper {
            width: 100%;
          }
          
          .ts-block-input-wrapper .form-group {
            margin-bottom: 10px;
          }
          
          .ts-block-help-text {
            grid-column: 1 / -1;
            margin-top: 0px;
            padding-top: 0px;
            font-size: 0.875rem;
            color: #666;
          }
          "
        )),
        
        div(
          class = "ts-block-container",
          
          div(
            class = "ts-block-form-grid",
            
            # Calculation Section
            div(
              class = "ts-block-section",
              tags$h4("Calculation"),
              
              div(
                class = "ts-block-section-grid",
                
                div(
                  class = "ts-block-input-wrapper",
                  selectInput(
                    NS(id, "method"),
                    label = "Method",
                    choices = c(
                      "Period-on-period %" = "pc",
                      "Year-on-year %" = "pcy",
                      "Annualized %" = "pca",
                      "First differences" = "diff",
                      "Year-on-year differences" = "diffy"
                    ),
                    selected = method,
                    width = "100%"
                  )
                )
              ),
              
              # Dynamic description
              div(
                class = "ts-block-help-text",
                uiOutput(NS(id, "method_description"))
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
    class = "ts_change_block",
    ...
  )
}