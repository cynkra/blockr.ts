#' Time Series Selection Block
#'
#' A transform block that selects specific series from multivariate time series data.
#' Supports both explicit series selection and pattern-based filtering.
#'
#' @param series Character vector of series names to select (default: NULL)
#' @param pattern Regular expression pattern to match series names (default: NULL)
#' @param ... Additional arguments passed to new_ts_transform_block
#'
#' @details
#' This block uses tsbox::ts_pick() to select specific series from multivariate
#' time series data. You can either:
#' - Explicitly select series by name (e.g., c("DAX", "FTSE"))
#' - Use a regex pattern to match series names (e.g., "^D.*" for all series starting with D)
#' 
#' If both series and pattern are NULL, all series are returned.
#' If both are provided, series takes precedence.
#'
#' @export
new_ts_select_block <- function(series = NULL, pattern = NULL, ...) {
  
  new_ts_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          
          # Get available series from data
          available_series <- reactive({
            if (is.null(data)) return(character(0))
            
            data_val <- if (is.reactive(data)) data() else data
            if (is.null(data_val) || !is.data.frame(data_val$data)) return(character(0))
            
            df <- data_val$data
            if ("id" %in% names(df)) {
              unique(as.character(df$id))
            } else {
              character(0)  # Univariate series
            }
          })
          
          # Initialize reactive values
          r_series <- reactiveVal(series)
          r_pattern <- reactiveVal(pattern)
          r_use_pattern <- reactiveVal(!is.null(pattern))
          
          # Update available series in UI
          observe({
            choices <- available_series()
            if (length(choices) > 0) {
              # Update select input with available series
              updateSelectInput(
                session,
                "series",
                choices = choices,
                selected = if (!is.null(series) && all(series %in% choices)) series else choices[1]
              )
            }
          })
          
          # Observer for method selection (series vs pattern)
          observeEvent(input$method, {
            r_use_pattern(input$method == "pattern")
          })
          
          # Observer for series selection
          observeEvent(input$series, {
            if (!is.null(input$series)) {
              r_series(input$series)
            }
          })
          
          # Observer for pattern input
          observeEvent(input$pattern, {
            r_pattern(input$pattern)
          })
          
          # Dynamic description
          output$selection_description <- renderUI({
            use_pattern <- r_use_pattern()
            
            if (use_pattern) {
              pattern_val <- r_pattern()
              if (!is.null(pattern_val) && pattern_val != "") {
                helpText(
                  icon("info-circle"),
                  paste0("Selecting series matching pattern: ", pattern_val)
                )
              } else {
                helpText(
                  icon("info-circle"),
                  "Enter a regex pattern to match series names"
                )
              }
            } else {
              selected <- r_series()
              if (!is.null(selected) && length(selected) > 0) {
                helpText(
                  icon("info-circle"),
                  paste0("Selecting ", length(selected), " series: ", 
                         paste(selected, collapse = ", "))
                )
              } else {
                helpText(
                  icon("info-circle"),
                  "Select one or more series from the list"
                )
              }
            }
          })
          
          # Show/hide inputs based on method
          observe({
            use_pattern <- r_use_pattern()
            shinyjs::toggle("series_wrapper", condition = !use_pattern)
            shinyjs::toggle("pattern_wrapper", condition = use_pattern)
          })
          
          list(
            expr = reactive({
              use_pattern <- r_use_pattern()
              
              if (use_pattern) {
                # Pattern-based selection
                pattern_val <- r_pattern()
                if (!is.null(pattern_val) && pattern_val != "") {
                  # Use grep to select series matching pattern
                  expr_text <- glue::glue(
                    'tsbox::ts_pick(data, grep("{pattern_val}", unique(data$id), value = TRUE))'
                  )
                } else {
                  # No pattern - return all data
                  expr_text <- "data"
                }
              } else {
                # Explicit series selection
                series_val <- r_series()
                if (!is.null(series_val) && length(series_val) > 0) {
                  # Format series names for R expression
                  series_str <- paste0('c(', paste0('"', series_val, '"', collapse = ", "), ')')
                  expr_text <- glue::glue('tsbox::ts_pick(data, {series_str})')
                } else {
                  # No selection - return all data
                  expr_text <- "data"
                }
              }
              
              parse(text = expr_text)[[1]]
            }),
            state = list(
              series = r_series,
              pattern = r_pattern,
              use_pattern = r_use_pattern
            )
          )
        }
      )
    },
    function(id) {
      tagList(
        # Include shinyjs for toggle functionality
        shinyjs::useShinyjs(),
        
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
            
            # Selection Method Section
            div(
              class = "ts-block-section",
              tags$h4("Series Selection"),
              
              div(
                class = "ts-block-section-grid",
                
                # Method selector
                div(
                  class = "ts-block-input-wrapper",
                  radioButtons(
                    NS(id, "method"),
                    label = "Selection Method",
                    choices = c(
                      "Select by name" = "series",
                      "Select by pattern" = "pattern"
                    ),
                    selected = if (!is.null(pattern)) "pattern" else "series",
                    inline = TRUE
                  )
                ),
                
                # Series multi-select (shown when method = "series")
                div(
                  id = NS(id, "series_wrapper"),
                  class = "ts-block-input-wrapper",
                  selectInput(
                    NS(id, "series"),
                    label = "Select Series",
                    choices = NULL,  # Will be populated dynamically
                    selected = series,
                    multiple = TRUE,
                    width = "100%"
                  )
                ),
                
                # Pattern input (shown when method = "pattern")
                div(
                  id = NS(id, "pattern_wrapper"),
                  class = "ts-block-input-wrapper",
                  style = if (is.null(pattern)) "display: none;" else "",
                  textInput(
                    NS(id, "pattern"),
                    label = "Pattern (regex)",
                    value = pattern,
                    placeholder = "e.g., ^D.* for series starting with D",
                    width = "100%"
                  )
                )
              )
            ),
            
            # Dynamic description
            div(
              class = "ts-block-help-text",
              uiOutput(NS(id, "selection_description"))
            )
          )
        )
      )
    },
    dat_val = function(data) {
      # Validate that input is a data.frame
      stopifnot(is.data.frame(data))
      
      # Warn if data appears to be univariate
      if (!"id" %in% names(data)) {
        warning("Data appears to be univariate (no 'id' column). Selection block works best with multivariate time series.")
      }
    },
    class = "ts_select_block",
    ...
  )
}