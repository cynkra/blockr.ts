#' Time Series Selection Block
#'
#' A transform block that selects specific series from multivariate time series data.
#'
#' @param series Character vector of series names to select (default: NULL selects all)
#' @param multiple Logical, whether to allow multiple series selection (default: TRUE)
#' @param ... Additional arguments passed to new_ts_transform_block
#'
#' @details
#' This block uses tsbox::ts_pick() to select specific series from multivariate
#' time series data by explicitly selecting series by name.
#' If series is NULL or empty, all available series are selected.
#'
#' @export
new_ts_select_block <- function(series = NULL, multiple = TRUE, ...) {
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

          # Initialize reactive value for multiple selection mode
          r_multiple <- reactiveVal(multiple)

          # Update available series in UI
          observe({
            choices <- available_series()
            if (length(choices) > 0) {
              # Clean up series names for display
              display_names <- gsub("^datasets::", "", choices)

              # Determine what should be selected
              current_selection <- isolate(r_series())

              # Determine what should be selected based on mode and current selection
              if (
                is.null(current_selection) ||
                  length(current_selection) == 0 ||
                  !all(current_selection %in% choices)
              ) {
                # No valid selection - behavior depends on mode
                if (r_multiple()) {
                  selected_val <- choices # Select all by default in multi mode
                } else {
                  selected_val <- choices[1] # Select first in single mode
                }
                r_series(selected_val) # Update reactive value
              } else {
                # Valid selection exists
                if (!r_multiple() && length(current_selection) > 1) {
                  # In single mode but multiple selected - keep only first
                  selected_val <- current_selection[1]
                  r_series(selected_val)
                } else {
                  selected_val <- current_selection
                }
              }

              # Update both select inputs with available series
              updateSelectInput(
                session,
                "series_single",
                choices = setNames(choices, display_names),
                selected = if(!is.null(selected_val) && length(selected_val) >= 1) selected_val[1] else NULL
              )

              updateSelectInput(
                session,
                "series_multi",
                choices = setNames(choices, display_names),
                selected = selected_val
              )
            } else {
              # No series available (univariate data)
              updateSelectInput(
                session,
                "series_single",
                choices = character(0),
                selected = character(0)
              )

              updateSelectInput(
                session,
                "series_multi",
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

          # Observer for multiple mode toggle
          observeEvent(
            input$multiple,
            {
              r_multiple(input$multiple)
              current_selection <- r_series()

              # Transfer selection between inputs when switching modes
              if (!is.null(current_selection)) {
                if (input$multiple) {
                  # Switching to multiple: transfer single selection to multi
                  updateSelectInput(
                    session,
                    "series_multi",
                    selected = current_selection
                  )
                } else {
                  # Switching to single: keep only first selection
                  single_selection <- if(length(current_selection) > 0) current_selection[1] else NULL
                  r_series(single_selection)
                  updateSelectInput(
                    session,
                    "series_single",
                    selected = single_selection
                  )
                }
              }
            },
            ignoreInit = TRUE
          )

          # Observer for single series selection
          observeEvent(
            input$series_single,
            {
              if (!is.null(input$series_single)) {
                r_series(input$series_single)
              }
            },
            ignoreNULL = FALSE,
            ignoreInit = TRUE
          )

          # Observer for multiple series selection
          observeEvent(
            input$series_multi,
            {
              r_series(input$series_multi)
            },
            ignoreNULL = FALSE,
            ignoreInit = TRUE
          )

          # Dynamic description - removed as redundant
          output$selection_description <- renderUI({
            return(NULL)
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
              series = r_series,
              multiple = r_multiple
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

              # Single series selector (hidden when multiple=TRUE)
              conditionalPanel(
                condition = "!input.multiple",
                ns = NS(id),
                div(
                  class = "ts-block-input-wrapper",
                  selectInput(
                    NS(id, "series_single"),
                    label = "Select Series",
                    choices = NULL, # Will be populated dynamically
                    selected = if(!is.null(series) && length(series) == 1) series[1] else NULL,
                    multiple = FALSE,
                    width = "100%"
                  )
                )
              ),

              # Multiple series selector (hidden when multiple=FALSE)
              conditionalPanel(
                condition = "input.multiple",
                ns = NS(id),
                div(
                  class = "ts-block-input-wrapper",
                  selectInput(
                    NS(id, "series_multi"),
                    label = "Select Series",
                    choices = NULL, # Will be populated dynamically
                    selected = series,
                    multiple = TRUE,
                    width = "100%"
                  )
                )
              )
            ),

            # Selection mode toggle (moved to right)
            div(
              class = "ts-block-section",
              div(
                class = "ts-block-input-wrapper",
                checkboxInput(
                  NS(id, "multiple"),
                  label = "Allow multiple series selection",
                  value = multiple,
                  width = "100%"
                )
              )
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
