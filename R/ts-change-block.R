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
        div(
          class = "ts-change-container",
          
          # CSS for the block
          tags$style(HTML(
            "
            .ts-change-container {
              padding: 12px;
              background: #f8f9fa;
              border-radius: 6px;
            }
            .method-selector {
              margin-bottom: 10px;
            }
            .method-selector label {
              font-weight: 600;
              color: #495057;
              margin-bottom: 4px;
            }
            "
          )),
          
          # Method selector
          div(
            class = "method-selector",
            selectInput(
              NS(id, "method"),
              label = "Calculation Method",
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
          ),
          
          # Dynamic description
          uiOutput(NS(id, "method_description"))
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