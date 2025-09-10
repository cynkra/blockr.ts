#' Time Series Frequency Conversion Block
#'
#' A transform block that converts time series to different frequencies through
#' aggregation or disaggregation (e.g., monthly to quarterly, quarterly to yearly).
#'
#' @param to Character string specifying the target frequency:
#'   - "year": Convert to yearly data
#'   - "quarter": Convert to quarterly data
#'   - "month": Convert to monthly data
#'   - "week": Convert to weekly data
#'   - "day": Convert to daily data
#' @param aggregate Character string specifying the aggregation method:
#'   - "mean": Average values (default)
#'   - "sum": Sum values
#'   - "first": Take first value
#'   - "last": Take last value
#'   - "min": Take minimum value
#'   - "max": Take maximum value
#' @param na.rm Logical whether to remove NA values (default: TRUE)
#' @param ... Additional arguments passed to new_ts_transform_block
#'
#' @details
#' This block uses tsbox::ts_frequency() to convert time series between different
#' temporal granularities. When converting to lower frequencies (e.g., monthly to yearly),
#' data is aggregated using the specified method. When converting to higher frequencies
#' (e.g., yearly to monthly), data is disaggregated using interpolation.
#'
#' @export
new_ts_frequency_block <- function(to = "year", aggregate = "mean", na.rm = TRUE, ...) {
  
  # Validate parameters
  to <- match.arg(to, c("year", "quarter", "month", "week", "day"))
  aggregate <- match.arg(aggregate, c("mean", "sum", "first", "last", "min", "max"))
  
  new_ts_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          
          # Reactive values for parameters
          r_to <- reactiveVal(to)
          r_aggregate <- reactiveVal(aggregate)
          r_na_rm <- reactiveVal(na.rm)
          
          # Detect input data frequency
          input_frequency <- reactive({
            # Handle both reactive and non-reactive data
            if (is.reactive(data)) {
              data_val <- data()
            } else if (!is.null(data)) {
              data_val <- data
            } else {
              return(NULL)
            }
            
            # Check if we have valid data structure
            if (is.null(data_val)) return(NULL)
            
            # The data comes as a data.frame directly in transform blocks
            df <- if (is.data.frame(data_val)) {
              data_val
            } else if (is.list(data_val) && "data" %in% names(data_val)) {
              data_val$data
            } else {
              return(NULL)
            }
            
            # Try to detect frequency from the data
            tryCatch({
              # Convert back to ts object to get frequency
              ts_obj <- tsbox::ts_ts(df)
              freq <- frequency(ts_obj)
              
              # Map numeric frequency to string
              freq_map <- list(
                "365" = "day",
                "260" = "day",
                "52" = "week",
                "12" = "month",
                "4" = "quarter",
                "1" = "year"
              )
              
              # Return mapped frequency or NULL if unknown
              freq_str <- as.character(freq)
              if (freq_str %in% names(freq_map)) {
                freq_map[[freq_str]]
              } else if (freq >= 365) {
                "day"
              } else if (freq >= 52) {
                "week"
              } else if (freq >= 12) {
                "month"
              } else if (freq >= 4) {
                "quarter"
              } else {
                "year"
              }
            }, error = function(e) {
              NULL
            })
          })
          
          # Render the frequency selector dynamically
          output$to_selector <- renderUI({
            current_freq <- input_frequency()
            
            # Default choices if no data yet
            all_choices <- c(
              "Yearly" = "year",
              "Quarterly" = "quarter",
              "Monthly" = "month",
              "Weekly" = "week",
              "Daily" = "day"
            )
            
            if (!is.null(current_freq)) {
              # Define frequency hierarchy (from highest to lowest)
              freq_hierarchy <- c("day", "week", "month", "quarter", "year")
              current_idx <- which(freq_hierarchy == current_freq)
              
              if (length(current_idx) > 0) {
                # Only allow frequencies at or lower than current
                valid_freqs <- freq_hierarchy[current_idx:length(freq_hierarchy)]
                
                # Create choice list with proper labels
                choice_labels <- c(
                  "day" = "Daily",
                  "week" = "Weekly", 
                  "month" = "Monthly",
                  "quarter" = "Quarterly",
                  "year" = "Yearly"
                )
                
                filtered_choices <- setNames(
                  valid_freqs,
                  choice_labels[valid_freqs]
                )
                
                # Determine what should be selected
                current_selection <- isolate(r_to())
                new_selection <- if (current_selection %in% valid_freqs) {
                  current_selection
                } else {
                  # Default to lowest frequency (yearly) if current is invalid
                  valid_freqs[length(valid_freqs)]
                }
                
                selectInput(
                  NS(session$ns(NULL), "to"),
                  label = "Target Frequency",
                  choices = filtered_choices,
                  selected = new_selection,
                  width = "100%"
                )
              } else {
                # No valid frequency detected, show all
                selectInput(
                  NS(session$ns(NULL), "to"),
                  label = "Target Frequency",
                  choices = all_choices,
                  selected = isolate(r_to()),
                  width = "100%"
                )
              }
            } else {
              # No data yet, show all options
              selectInput(
                NS(session$ns(NULL), "to"),
                label = "Target Frequency",
                choices = all_choices,
                selected = isolate(r_to()),
                width = "100%"
              )
            }
          })
          
          # Show current frequency info
          output$current_frequency <- renderUI({
            current_freq <- input_frequency()
            
            if (!is.null(current_freq)) {
              freq_label <- switch(current_freq,
                "day" = "Daily",
                "week" = "Weekly",
                "month" = "Monthly",
                "quarter" = "Quarterly",
                "year" = "Yearly",
                "Unknown"
              )
              
              tags$div(
                class = "alert alert-info",
                style = "padding: 8px; margin-bottom: 10px; font-size: 0.9em; background-color: #d1ecf1; border: 1px solid #bee5eb;",
                icon("info-circle"),
                tags$strong(paste0("Current data frequency: ", freq_label)),
                tags$br(),
                tags$small(style = "color: #666;", "Only lower frequencies available for aggregation")
              )
            } else {
              NULL  # Don't show anything if no frequency detected
            }
          })
          
          # Observers for inputs
          observeEvent(input$to, {
            r_to(input$to)
          })
          
          observeEvent(input$aggregate, {
            r_aggregate(input$aggregate)
          })
          
          observeEvent(input$na_rm, {
            r_na_rm(input$na_rm)
          })
          
          # Dynamic description based on settings
          output$frequency_description <- renderUI({
            current_to <- r_to()
            current_agg <- r_aggregate()
            
            freq_desc <- switch(current_to,
              "year" = "yearly",
              "quarter" = "quarterly",
              "month" = "monthly",
              "week" = "weekly",
              "day" = "daily"
            )
            
            agg_desc <- switch(current_agg,
              "mean" = "averaging",
              "sum" = "summing",
              "first" = "taking first value of",
              "last" = "taking last value of",
              "min" = "taking minimum of",
              "max" = "taking maximum of"
            )
            
            helpText(
              icon("info-circle"),
              paste0("Converting to ", freq_desc, " frequency by ", agg_desc, " each period")
            )
          })
          
          list(
            expr = reactive({
              # Build expression based on selected parameters
              to_val <- r_to()
              aggregate_val <- r_aggregate()
              na_rm_val <- r_na_rm()
              
              # Create expression using glue
              expr_text <- glue::glue(
                'tsbox::ts_frequency(data, to = "{to_val}", aggregate = "{aggregate_val}", na.rm = {na_rm_val})'
              )
              parse(text = expr_text)[[1]]
            }),
            state = list(
              to = r_to,
              aggregate = r_aggregate,
              na.rm = r_na_rm
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
          
          # Show current frequency info at the top
          uiOutput(NS(id, "current_frequency")),
          
          div(
            class = "ts-block-form-grid",
            
            # Frequency Section
            div(
              class = "ts-block-section",
              tags$h4("Frequency Conversion"),
              
              div(
                class = "ts-block-section-grid",
                
                div(
                  class = "ts-block-input-wrapper",
                  # Render select input dynamically based on data frequency
                  uiOutput(NS(id, "to_selector"))
                ),
                
                div(
                  class = "ts-block-input-wrapper",
                  selectInput(
                    NS(id, "aggregate"),
                    label = "Aggregation Method",
                    choices = c(
                      "Mean" = "mean",
                      "Sum" = "sum",
                      "First" = "first",
                      "Last" = "last",
                      "Minimum" = "min",
                      "Maximum" = "max"
                    ),
                    selected = aggregate,
                    width = "100%"
                  )
                )
              )
            ),
            
            # Options Section
            div(
              class = "ts-block-section",
              tags$h4("Options"),
              
              div(
                class = "ts-block-section-grid",
                
                div(
                  class = "ts-block-input-wrapper",
                  checkboxInput(
                    NS(id, "na_rm"),
                    label = "Remove NA values",
                    value = na.rm,
                    width = "100%"
                  )
                )
              )
            ),
            
            # Dynamic description
            div(
              class = "ts-block-help-text",
              uiOutput(NS(id, "frequency_description"))
            )
          )
        )
      )
    },
    dat_val = function(data) {
      # Validate that input is a data.frame
      stopifnot(is.data.frame(data))
    },
    class = "ts_frequency_block",
    ...
  )
}