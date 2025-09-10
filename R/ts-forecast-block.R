#' Time Series Forecasting Block
#'
#' Generate forecasts with confidence intervals
#'
#' @param horizon Integer. Number of periods to forecast (default: 12).
#' @param ... Additional arguments passed to new_ts_transform_block()
#'
#' @return A ts_forecast_block object
#' @export
new_ts_forecast_block <- function(horizon = 12, ...) {
  # Ensure horizon is an integer
  horizon <- as.integer(horizon)

  new_ts_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          # Reactive value for horizon
          r_horizon <- reactiveVal(horizon)

          # Observer for input
          observeEvent(input$horizon, {
            r_horizon(as.integer(input$horizon))
          })

          # Dynamic info text
          output$forecast_info <- renderUI({
            horizon_val <- r_horizon()

            div(
              helpText(
                icon("chart-line"),
                sprintf(
                  "Forecasting %d period%s ahead using exponential smoothing",
                  horizon_val,
                  ifelse(horizon_val == 1, "", "s")
                )
              ),
              helpText(
                class = "text-muted",
                "Confidence bands at 80% and 95% levels will be shown"
              )
            )
          })

          list(
            expr = reactive({
              horizon_val <- r_horizon()

              # Simple forecast using ts_forecast
              expr_text <- glue::glue(
                "tsbox::ts_forecast(data, h = {horizon_val})"
              )
              parse(text = expr_text)[[1]]
            }),
            state = list(
              horizon = r_horizon
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
              tags$h4("Forecast Settings"),

              div(
                class = "ts-block-input-wrapper",
                numericInput(
                  NS(id, "horizon"),
                  label = "Forecast Horizon",
                  value = horizon,
                  min = 1,
                  max = 100,
                  step = 1
                )
              ),

              div(
                class = "ts-block-info",
                uiOutput(NS(id, "forecast_info"))
              )
            )
          )
        )
      )
    },
    class = c("ts_forecast_block"),
    ...
  )
}
