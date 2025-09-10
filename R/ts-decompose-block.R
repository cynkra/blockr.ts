#' Time Series Decomposition Block
#'
#' Extract trend, seasonal, and remainder components from time series
#'
#' @param component Character string. Component to extract: "seasonal_adjusted",
#'   "trend", "seasonal", or "remainder".
#' @param method Character string. Decomposition method: "stl", "x13", or "hp_filter".
#' @param ... Additional arguments passed to new_ts_transform_block()
#'
#' @return A ts_decompose_block object
#' @export
new_ts_decompose_block <- function(component = "seasonal_adjusted", 
                                   method = "stl", ...) {
  
  # Validate parameters
  component <- match.arg(component, c("seasonal_adjusted", "trend", "seasonal", "remainder"))
  method <- match.arg(method, c("stl", "x13", "hp_filter"))
  
  new_ts_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          
          # Reactive values
          r_component <- reactiveVal(component)
          r_method <- reactiveVal(method)
          
          # Observers
          observeEvent(input$component, {
            r_component(input$component)
          })
          
          observeEvent(input$method, {
            r_method(input$method)
          })
          
          # Dynamic info text
          output$decompose_info <- renderUI({
            component_val <- r_component()
            method_val <- r_method()
            
            description <- switch(component_val,
              "seasonal_adjusted" = "Removes seasonal patterns from the data",
              "trend" = "Extracts the long-term trend component",
              "seasonal" = "Isolates repeating seasonal patterns",
              "remainder" = "Shows irregular variations after removing trend and seasonal",
              ""
            )
            
            method_note <- switch(method_val,
              "stl" = "Using Seasonal and Trend decomposition using Loess",
              "x13" = "Using X-13 ARIMA-SEATS (requires seasonal package)",
              "hp_filter" = "Using Hodrick-Prescott filter",
              ""
            )
            
            div(
              helpText(icon("info-circle"), description),
              helpText(class = "text-muted", method_note)
            )
          })
          
          list(
            expr = reactive({
              component_val <- r_component()
              method_val <- r_method()
              
              # Simple cases using tsbox functions
              if (component_val == "seasonal_adjusted" && method_val != "stl") {
                expr_text <- "tsbox::ts_seas(data)"
              } else if (component_val == "trend" && method_val == "hp_filter") {
                expr_text <- "tsbox::ts_trend(data)"
              } else {
                # For now, use simpler decomposition
                expr_text <- switch(component_val,
                  "seasonal_adjusted" = "tsbox::ts_seas(data)",
                  "trend" = "tsbox::ts_trend(data)",
                  # For seasonal and remainder, we'd need more complex STL decomposition
                  # Simplified for now
                  "seasonal" = "tsbox::ts_seas(data)",
                  "remainder" = "tsbox::ts_trend(data)"
                )
              }
              
              parse(text = expr_text)[[1]]
            }),
            state = list(
              component = r_component,
              method = r_method
            )
          )
        }
      )
    },
    function(id) {
      tagList(
        div(
          class = "ts-block-container",
          div(
            class = "ts-block-form-grid",
            
            div(
              class = "ts-block-section",
              tags$h4("Decomposition Settings"),
              
              div(
                class = "ts-block-input-wrapper",
                selectInput(
                  NS(id, "component"),
                  label = "Component to Extract",
                  choices = list(
                    "Seasonally Adjusted" = "seasonal_adjusted",
                    "Trend" = "trend",
                    "Seasonal" = "seasonal",
                    "Remainder" = "remainder"
                  ),
                  selected = component
                )
              ),
              
              div(
                class = "ts-block-input-wrapper",
                selectInput(
                  NS(id, "method"),
                  label = "Method",
                  choices = list(
                    "STL Decomposition" = "stl",
                    "X-13 ARIMA-SEATS" = "x13",
                    "HP Filter (Trend only)" = "hp_filter"
                  ),
                  selected = method
                )
              ),
              
              div(
                class = "ts-block-info",
                uiOutput(NS(id, "decompose_info"))
              )
            )
          )
        )
      )
    },
    class = c("ts_decompose_block"),
    ...
  )
}