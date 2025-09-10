#' Time Series Lag/Lead Block
#'
#' A transform block that shifts time series forward (lag) or backward (lead).
#'
#' @param by Integer specifying the number of periods to shift:
#'   - Positive values create lags (shift forward in time)
#'   - Negative values create leads (shift backward in time)
#'   - Default is 1 (one period lag)
#' @param ... Additional arguments passed to new_ts_transform_block
#'
#' @details
#' This block uses tsbox::ts_lag() to shift time series data by a specified number
#' of periods. Lagging is useful for comparing current values with past values,
#' while leading can be used for forecasting comparisons.
#'
#' @export
new_ts_lag_block <- function(by = 1L, ...) {
  # Ensure by is an integer
  by <- as.integer(by)

  new_ts_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          # Reactive value for lag parameter
          r_by <- reactiveVal(by)

          # Observer for input
          observeEvent(input$by, {
            r_by(as.integer(input$by))
          })

          # Dynamic description based on lag value
          output$lag_description <- renderUI({
            current_by <- r_by()

            if (current_by == 0) {
              helpText(
                icon("info-circle"),
                "No shift applied (by = 0)"
              )
            } else if (current_by > 0) {
              period_text <- if (abs(current_by) == 1) "period" else "periods"
              helpText(
                icon("arrow-right"),
                paste0(
                  "Shifting data forward by ",
                  abs(current_by),
                  " ",
                  period_text,
                  " (lag)"
                )
              )
            } else {
              period_text <- if (abs(current_by) == 1) "period" else "periods"
              helpText(
                icon("arrow-left"),
                paste0(
                  "Shifting data backward by ",
                  abs(current_by),
                  " ",
                  period_text,
                  " (lead)"
                )
              )
            }
          })

          list(
            expr = reactive({
              by_val <- r_by()

              # Create expression using glue
              expr_text <- glue::glue(
                'tsbox::ts_lag(data, by = {by_val}L)'
              )
              parse(text = expr_text)[[1]]
            }),
            state = list(
              by = r_by
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
          
          .ts-block-info-box {
            grid-column: 1 / -1;
            padding: 8px;
            margin-bottom: 10px;
            font-size: 0.9em;
            background-color: #f0f8ff;
            border: 1px solid #b0d4ff;
            border-radius: 4px;
          }
          "
        )),

        div(
          class = "ts-block-container",

          # Info box
          div(
            class = "ts-block-info-box",
            icon("info-circle"),
            tags$strong("Lag/Lead Transformation"),
            tags$br(),
            tags$small(
              "Positive values create lags (shift forward), ",
              "negative values create leads (shift backward)"
            )
          ),

          div(
            class = "ts-block-form-grid",

            # Lag/Lead Section
            div(
              class = "ts-block-section",
              tags$h4("Time Shift"),

              div(
                class = "ts-block-section-grid",

                div(
                  class = "ts-block-input-wrapper",
                  numericInput(
                    NS(id, "by"),
                    label = "Periods to Shift",
                    value = by,
                    step = 1L,
                    width = "100%"
                  )
                )
              )
            ),

            # Dynamic description
            div(
              class = "ts-block-help-text",
              uiOutput(NS(id, "lag_description"))
            )
          )
        )
      )
    },
    dat_val = function(data) {
      # Validate that input is a data.frame
      stopifnot(is.data.frame(data))
    },
    class = "ts_lag_block",
    ...
  )
}
