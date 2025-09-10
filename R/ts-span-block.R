#' Time Series Span (Time Range) Block
#'
#' A transform block that filters time series to a specific time range.
#'
#' @param start Character string or Date specifying the start date.
#'   Use NULL to start from the beginning of the series.
#' @param end Character string or Date specifying the end date.
#'   Use NULL to include up to the end of the series.
#' @param ... Additional arguments passed to new_ts_transform_block
#'
#' @details
#' This block uses tsbox::ts_span() to filter time series data to a specific
#' time window. The dates can be specified in various formats that tsbox
#' understands (e.g., "2020-01-01", "2020-01", "2020").
#'
#' @export
new_ts_span_block <- function(start = NULL, end = NULL, ...) {
  new_ts_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          # Reactive values for span parameters
          r_start <- reactiveVal(start)
          r_end <- reactiveVal(end)

          # Detect data time range
          data_range <- reactive({
            # Handle both reactive and non-reactive data
            if (is.reactive(data)) {
              data_val <- data()
            } else if (!is.null(data)) {
              data_val <- data
            } else {
              return(NULL)
            }

            # Check if we have valid data structure
            if (is.null(data_val)) {
              return(NULL)
            }

            # The data comes as a data.frame directly in transform blocks
            df <- if (is.data.frame(data_val)) {
              data_val
            } else if (is.list(data_val) && "data" %in% names(data_val)) {
              data_val$data
            } else {
              return(NULL)
            }

            # Try to get time range from the data
            tryCatch(
              {
                if ("time" %in% names(df)) {
                  list(
                    min = min(df$time, na.rm = TRUE),
                    max = max(df$time, na.rm = TRUE)
                  )
                } else {
                  NULL
                }
              },
              error = function(e) {
                NULL
              }
            )
          })

          # Show current data range info
          output$data_range_info <- renderUI({
            range_info <- data_range()

            if (!is.null(range_info)) {
              tags$div(
                class = "alert alert-info",
                style = "padding: 8px; margin-bottom: 10px; font-size: 0.9em; background-color: #d1ecf1; border: 1px solid #bee5eb;",
                icon("calendar-alt"),
                tags$strong("Data Range: "),
                format(range_info$min, "%Y-%m-%d"),
                " to ",
                format(range_info$max, "%Y-%m-%d")
              )
            } else {
              NULL
            }
          })

          # Render the date range slider dynamically
          output$dateRangeSlider <- renderUI({
            range_info <- data_range()

            if (!is.null(range_info)) {
              # Get the actual data frequency
              # Handle both reactive and non-reactive data
              if (is.reactive(data)) {
                data_val <- data()
              } else if (!is.null(data)) {
                data_val <- data
              } else {
                return(NULL)
              }

              # The data comes as a data.frame directly in transform blocks
              df <- if (is.data.frame(data_val)) {
                data_val
              } else if (is.list(data_val) && "data" %in% names(data_val)) {
                data_val$data
              } else {
                return(NULL)
              }

              # Detect the actual frequency of the time series
              tryCatch(
                {
                  # Convert back to ts object to get frequency
                  ts_obj <- tsbox::ts_ts(df)
                  freq <- frequency(ts_obj)

                  # Set step and format based on actual frequency
                  if (freq == 1) {
                    # Yearly data
                    time_format <- "%Y"
                    step_days <- 365
                  } else if (freq == 4) {
                    # Quarterly data - approximately 91 days
                    time_format <- "%Y Q%q"
                    step_days <- 91
                  } else if (freq == 12) {
                    # Monthly data - use approximate monthly step
                    time_format <- "%Y-%m"
                    step_days <- 30
                  } else if (freq == 52) {
                    # Weekly data
                    time_format <- "%Y-%m-%d"
                    step_days <- 7
                  } else if (freq >= 365 || freq == 260) {
                    # Daily data (365 or 260 for business days)
                    time_format <- "%Y-%m-%d"
                    step_days <- 1
                  } else {
                    # Default to monthly display for unknown frequencies
                    time_format <- "%Y-%m"
                    step_days <- 30
                  }
                },
                error = function(e) {
                  # If frequency detection fails, use a sensible default
                  time_format <- "%Y-%m"
                  step_days <- 30
                }
              )

              # Set initial values
              initial_start <- if (!is.null(isolate(r_start()))) {
                as.Date(isolate(r_start()))
              } else {
                range_info$min
              }

              initial_end <- if (!is.null(isolate(r_end()))) {
                as.Date(isolate(r_end()))
              } else {
                range_info$max
              }

              sliderInput(
                NS(session$ns(NULL), "dateRange"),
                label = "Select Time Range",
                min = range_info$min,
                max = range_info$max,
                value = c(initial_start, initial_end),
                timeFormat = time_format,
                step = step_days,
                width = "100%"
              )
            } else {
              # No data yet - show placeholder
              helpText("Date range will appear when data is loaded")
            }
          })

          # Observer for slider input
          observeEvent(input$dateRange, {
            if (!is.null(input$dateRange)) {
              # Update start and end based on slider
              r_start(format(input$dateRange[1], "%Y-%m-%d"))
              r_end(format(input$dateRange[2], "%Y-%m-%d"))
            }
          })

          # Dynamic description based on settings
          output$span_description <- renderUI({
            current_start <- r_start()
            current_end <- r_end()

            if (is.null(current_start) && is.null(current_end)) {
              helpText(
                icon("info-circle"),
                "No filtering applied - showing full time range"
              )
            } else if (!is.null(current_start) && !is.null(current_end)) {
              helpText(
                icon("filter"),
                paste0(
                  "Filtering data from ",
                  current_start,
                  " to ",
                  current_end
                )
              )
            } else if (!is.null(current_start)) {
              helpText(
                icon("filter"),
                paste0("Filtering data from ", current_start, " onwards")
              )
            } else {
              helpText(
                icon("filter"),
                paste0("Filtering data up to ", current_end)
              )
            }
          })

          list(
            expr = reactive({
              start_val <- r_start()
              end_val <- r_end()

              # Build expression based on NULL values
              if (is.null(start_val) && is.null(end_val)) {
                # No filtering - just return data
                parse(text = "data")[[1]]
              } else if (!is.null(start_val) && !is.null(end_val)) {
                # Both start and end specified
                expr_text <- glue::glue(
                  'tsbox::ts_span(data, start = "{start_val}", end = "{end_val}")'
                )
                parse(text = expr_text)[[1]]
              } else if (!is.null(start_val)) {
                # Only start specified
                expr_text <- glue::glue(
                  'tsbox::ts_span(data, start = "{start_val}")'
                )
                parse(text = expr_text)[[1]]
              } else {
                # Only end specified
                expr_text <- glue::glue(
                  'tsbox::ts_span(data, end = "{end_val}")'
                )
                parse(text = expr_text)[[1]]
              }
            }),
            state = list(
              start = r_start,
              end = r_end
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

          # Show data range info
          uiOutput(NS(id, "data_range_info")),

          div(
            class = "ts-block-form-grid",

            # Time Range Slider
            div(
              class = "ts-block-section",
              tags$h4("Time Range Selection"),

              div(
                style = "width: 100%; padding: 10px 0;",
                uiOutput(NS(id, "dateRangeSlider"))
              )
            ),

            # Dynamic description
            div(
              class = "ts-block-help-text",
              uiOutput(NS(id, "span_description"))
            )
          )
        )
      )
    },
    dat_val = function(data) {
      # Validate that input is a data.frame
      stopifnot(is.data.frame(data))
    },
    class = "ts_span_block",
    ...
  )
}
