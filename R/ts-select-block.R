#' Time Series Selection Block
#'
#' A transform block that selects specific series from multivariate time series data.
#'
#' @param series Character vector of series names to select (default: NULL selects all)
#' @param ... Additional arguments passed to new_ts_transform_block
#'
#' @details
#' This block uses tsbox::ts_pick() to select specific series from multivariate
#' time series data by explicitly selecting series by name.
#' If series is NULL or empty, all available series are selected.
#'
#' @export
new_ts_select_block <- function(series = NULL, ...) {
  new_ts_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          # Get available series from data
          available_series <- reactive({
            # Handle both reactive and non-reactive data
            if (is.reactive(data)) {
              data_val <- data()
            } else if (!is.null(data)) {
              data_val <- data
            } else {
              return(character(0))
            }

            if (is.null(data_val)) {
              return(character(0))
            }

            # The data comes as a data.frame directly in transform blocks
            df <- if (is.data.frame(data_val)) {
              data_val
            } else if (is.list(data_val) && "data" %in% names(data_val)) {
              data_val$data
            } else {
              return(character(0))
            }

            # Check for multivariate series
            if ("id" %in% names(df)) {
              unique(as.character(df$id))
            } else {
              character(0) # Univariate series - no selection needed
            }
          })

          # Initialize reactive value for series selection
          # Start with the constructor parameter or NULL
          r_series <- reactiveVal(series)

          # Update available series in UI
          observe({
            choices <- available_series()
            if (length(choices) > 0) {
              # Clean up series names for display
              display_names <- gsub("^datasets::", "", choices)

              # Determine what should be selected
              current_selection <- isolate(r_series())

              # If no selection or invalid selection, select all
              if (
                is.null(current_selection) ||
                  length(current_selection) == 0 ||
                  !all(current_selection %in% choices)
              ) {
                selected_val <- choices # Select all by default
                r_series(selected_val) # Update reactive value
              } else {
                selected_val <- current_selection
              }

              # Update select input with available series
              updateSelectInput(
                session,
                "series",
                choices = setNames(choices, display_names),
                selected = selected_val
              )
            } else {
              # No series available (univariate data)
              updateSelectInput(
                session,
                "series",
                choices = character(0),
                selected = character(0)
              )
              r_series(NULL)
            }
          })

          # Show warning for univariate data
          output$univariate_warning <- renderUI({
            choices <- available_series()
            if (length(choices) == 0) {
              # Check if we have data at all
              has_data <- if (is.reactive(data)) {
                !is.null(data())
              } else {
                !is.null(data)
              }

              if (has_data) {
                tags$div(
                  class = "alert alert-warning",
                  style = "margin-top: 10px;",
                  icon("info-circle"),
                  "This is univariate time series data. Series selection is only available for multivariate data.",
                  tags$br(),
                  tags$small(
                    "Try with a multivariate dataset like EuStockMarkets or Seatbelts."
                  )
                )
              } else {
                NULL
              }
            } else {
              NULL
            }
          })

          # Observer for series selection - simple and direct
          observeEvent(
            input$series,
            {
              r_series(input$series)
            },
            ignoreNULL = FALSE,
            ignoreInit = TRUE
          )

          # Dynamic description
          output$selection_description <- renderUI({
            selected <- r_series()
            avail <- available_series()

            if (length(avail) == 0) {
              return(NULL)
            }

            if (!is.null(selected) && length(selected) > 0) {
              # Clean display names
              display_selected <- gsub("^datasets::", "", selected)
              helpText(
                icon("info-circle"),
                paste0(
                  "Selecting ",
                  length(selected),
                  " of ",
                  length(avail),
                  " series: ",
                  paste(display_selected, collapse = ", ")
                ),
                paste0(
                  " (Available: ",
                  paste(gsub("^datasets::", "", avail), collapse = ", "),
                  ")"
                )
              )
            } else {
              helpText(
                icon("info-circle"),
                "No series selected"
              )
            }
          })

          list(
            expr = reactive({
              # Get current selection
              series_val <- r_series()
              avail <- available_series()

              # For multivariate data, always use ts_pick
              if (length(avail) > 0) {
                # Use selected series or all available if none selected
                selected_series <- if (
                  !is.null(series_val) && length(series_val) > 0
                ) {
                  series_val
                } else {
                  avail # Select all if nothing selected
                }

                series_str <- paste0(
                  'c(',
                  paste0('"', selected_series, '"', collapse = ", "),
                  ')'
                )
                expr_text <- glue::glue('tsbox::ts_pick(data, {series_str})')
              } else {
                # Univariate data - return as is
                expr_text <- "data"
              }

              parse(text = expr_text)[[1]]
            }),
            state = list(
              series = r_series
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
          
          .ts-block-section {
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

            # Series Selection Section
            div(
              class = "ts-block-section",
              tags$h4("Series Selection"),

              # Series multi-select
              div(
                class = "ts-block-input-wrapper",
                selectInput(
                  NS(id, "series"),
                  label = "Select Series",
                  choices = NULL, # Will be populated dynamically
                  selected = series,
                  multiple = TRUE,
                  width = "100%"
                )
              )
            ),

            # Dynamic description
            div(
              class = "ts-block-help-text",
              uiOutput(NS(id, "selection_description"))
            ),

            # Warning for univariate data
            uiOutput(NS(id, "univariate_warning"))
          )
        )
      )
    },
    dat_val = function(data) {
      # Validate that input is a data.frame
      stopifnot(is.data.frame(data))

      # Warn if data appears to be univariate
      if (!"id" %in% names(data)) {
        warning(
          "Data appears to be univariate (no 'id' column). Selection block works best with multivariate time series."
        )
      }
    },
    class = "ts_select_block",
    ...
  )
}
