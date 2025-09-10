#' Time Series Scaling and Indexing Block
#'
#' Scale, normalize, or create indices from time series data
#'
#' @param method Character string. Transformation method: "normalize" (z-score),
#'   "index" (base 100), or "minmax" (0-1 range).
#' @param base Date string. Base date for indexing (only used when method = "index").
#' @param ... Additional arguments passed to new_ts_transform_block()
#'
#' @return A ts_scale_block object
#' @export
new_ts_scale_block <- function(method = "index", base = NULL, ...) {
  # Validate method parameter
  method <- match.arg(method, c("index", "normalize", "minmax"))

  new_ts_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          # Reactive values
          r_method <- reactiveVal(method)
          r_base <- reactiveVal(base)

          # Observers
          observeEvent(input$method, {
            r_method(input$method)
          })

          observeEvent(input$base_slider, {
            if (!is.null(input$base_slider)) {
              r_base(format(input$base_slider, "%Y-%m-%d"))
            }
          })

          # Dynamic base date selector
          output$base_selector <- renderUI({
            method_val <- r_method()

            if (method_val == "index") {
              # Get data to determine date range
              data_val <- data()

              if (!is.null(data_val)) {
                # Get date range from data
                range_info <- tryCatch(
                  {
                    df <- if (is.data.frame(data_val)) {
                      data_val
                    } else if (
                      is.list(data_val) && "data" %in% names(data_val)
                    ) {
                      data_val$data
                    } else {
                      tsbox::ts_tbl(data_val)
                    }

                    if ("time" %in% names(df)) {
                      list(
                        min = min(as.Date(df$time), na.rm = TRUE),
                        max = max(as.Date(df$time), na.rm = TRUE)
                      )
                    } else {
                      NULL
                    }
                  },
                  error = function(e) NULL
                )

                if (!is.null(range_info)) {
                  # Detect frequency for step size
                  step_days <- tryCatch(
                    {
                      ts_obj <- tsbox::ts_ts(df)
                      freq <- frequency(ts_obj)

                      if (freq == 1) {
                        365 # Yearly
                      } else if (freq == 4) {
                        91 # Quarterly
                      } else if (freq == 12) {
                        30 # Monthly
                      } else if (freq == 52) {
                        7 # Weekly
                      } else if (freq >= 365) {
                        1 # Daily
                      } else {
                        30 # Default to monthly
                      }
                    },
                    error = function(e) 30
                  )

                  # Set initial value
                  initial_base <- if (!is.null(isolate(r_base()))) {
                    as.Date(isolate(r_base()))
                  } else {
                    # Default to middle of the range
                    range_info$min + (range_info$max - range_info$min) / 2
                  }

                  div(
                    class = "ts-block-input-wrapper",
                    tags$label("Base Date for Index"),
                    sliderInput(
                      NS(session$ns(NULL), "base_slider"),
                      label = NULL,
                      min = range_info$min,
                      max = range_info$max,
                      value = initial_base,
                      timeFormat = "%Y-%m-%d",
                      step = step_days,
                      width = "100%"
                    ),
                    helpText(
                      class = "text-muted",
                      "Select the date where the index equals 100"
                    )
                  )
                } else {
                  helpText("Base date slider will appear when data is loaded")
                }
              } else {
                helpText("Base date slider will appear when data is loaded")
              }
            } else {
              NULL
            }
          })

          # Dynamic description
          output$method_description <- renderUI({
            method_val <- r_method()
            base_val <- r_base()

            description <- switch(
              method_val,
              "normalize" = "Standardizes data to mean = 0, SD = 1",
              "index" = if (!is.null(base_val)) {
                paste0("Creates index with ", base_val, " = 100")
              } else {
                "Creates index with selected base date = 100"
              },
              "minmax" = "Scales data to range [0, 1]",
              "Standardizes data to mean = 0, SD = 1" # Default fallback
            )

            helpText(
              icon("info-circle"),
              description
            )
          })

          list(
            expr = reactive({
              method_val <- r_method()
              base_val <- r_base()

              if (method_val == "normalize") {
                expr_text <- "tsbox::ts_scale(data)"
              } else if (method_val == "index") {
                if (is.null(base_val) || base_val == "") {
                  expr_text <- "tsbox::ts_index(data)"
                } else {
                  expr_text <- glue::glue(
                    "tsbox::ts_index(data, base = '{base_val}')"
                  )
                }
              } else if (method_val == "minmax") {
                expr_text <- "
                {
                  data_tbl <- tsbox::ts_tbl(data)
                  data_tbl$value <- (data_tbl$value - min(data_tbl$value, na.rm = TRUE)) / 
                                   (max(data_tbl$value, na.rm = TRUE) - min(data_tbl$value, na.rm = TRUE))
                  data_tbl
                }"
              }

              parse(text = expr_text)[[1]]
            }),
            state = list(
              method = r_method,
              base = r_base
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
            padding-bottom: 15px;
          }
          
          .ts-block-form-grid {
            display: grid;
            gap: 15px;
            grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
          }
          
          .ts-block-section h4 {
            grid-column: 1 / -1;
            font-size: 1.1rem;
            font-weight: 600;
            color: #333;
          }
          
          .ts-block-input-wrapper {
            display: flex;
            flex-direction: column;
            grid-column: 1 / -1;
          }
          
          .ts-block-info {
            grid-column: 1 / -1;
            padding: 10px;
            background-color: #f8f9fa;
            border-radius: 4px;
            margin-top: 5px;
          }
        "
        )),

        div(
          class = "ts-block-container",
          div(
            class = "ts-block-form-grid",

            div(
              class = "ts-block-section",
              tags$h4("Scaling Method"),

              div(
                class = "ts-block-input-wrapper",
                selectInput(
                  NS(id, "method"),
                  label = NULL,
                  choices = list(
                    "Normalize (Z-score)" = "normalize",
                    "Index (Base 100)" = "index",
                    "Min-Max (0-1)" = "minmax"
                  ),
                  selected = method
                )
              ),

              # Dynamic UI for base date selection
              uiOutput(NS(id, "base_selector")),

              div(
                class = "ts-block-info",
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
    class = c("ts_scale_block"),
    ...
  )
}
