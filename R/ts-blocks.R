#' Time Series Data Block with Dygraph Display
#'
#' A time series data block that returns time series data but displays it as interactive dygraph widgets.
#' This block provides time series data from various built-in datasets with dygraph visualization.
#'
#' @param dataset Time series dataset to use: "economics", "AirPassengers", or "lynx"
#' @param start_year Starting year for the time series (optional)
#' @param convert_format Convert to specific time series format using tsbox
#' @param ... Additional arguments passed to new_data_block
#'
#' @export
new_ts_block <- function(
  dataset = "economics",
  start_year = NULL,
  convert_format = "tibble",
  ...
) {
  blockr.core::new_data_block(
    function(id) {
      moduleServer(
        id,
        function(input, output, session) {
          # Initialize reactive values with r_ prefix
          r_dataset <- reactiveVal(dataset)
          r_start_year <- reactiveVal(start_year)
          r_convert_format <- reactiveVal(convert_format)

          # Input observers
          observeEvent(input$dataset, {
            r_dataset(input$dataset)
          })

          observeEvent(input$start_year, {
            r_start_year(if(is.null(input$start_year) || input$start_year == "") NULL else as.integer(input$start_year))
          })

          observeEvent(input$convert_format, {
            r_convert_format(input$convert_format)
          })

          list(
            expr = reactive({
              # Build expression using parse/glue pattern (modern blockr style)
              dataset_name <- r_dataset()
              start_val <- r_start_year()
              format_val <- r_convert_format()

              if (is.null(start_val)) {
                expr_text <- glue::glue(
                  "blockr.ts:::get_ts_data('{dataset_name}', convert_format = '{format_val}')"
                )
              } else {
                expr_text <- glue::glue(
                  "blockr.ts:::get_ts_data('{dataset_name}', start_year = {start_val}, convert_format = '{format_val}')"
                )
              }

              parse(text = expr_text)[[1]]
            }),
            state = list(
              dataset = r_dataset,
              start_year = r_start_year,
              convert_format = r_convert_format
            )
          )
        }
      )
    },
    function(id) {
      tagList(
        div(
          class = "ts-block-container",

          # Simple CSS for clean layout
          tags$style(HTML(
            "
            .ts-block-container {
              padding: 15px;
              background: #f8f9fa;
              border-radius: 8px;
              margin-bottom: 15px;
            }
            .ts-section {
              margin-bottom: 15px;
            }
            .ts-section h4 {
              margin-top: 0;
              margin-bottom: 8px;
              color: #495057;
              font-size: 16px;
            }
            .ts-controls {
              display: grid;
              grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
              gap: 10px;
            }
          "
          )),

          # Time Series Selection Section
          div(
            class = "ts-section",
            h4("Time Series Data"),
            div(
              class = "ts-controls",
              selectInput(
                NS(id, "dataset"),
                label = "Dataset",
                choices = c(
                  "US Economic Data" = "economics",
                  "Air Passengers" = "AirPassengers",
                  "Lynx Trappings" = "lynx"
                ),
                selected = dataset,
                width = "100%"
              ),
              numericInput(
                NS(id, "start_year"),
                label = "Start Year (optional)",
                value = start_year,
                min = 1900,
                max = 2030,
                step = 1,
                width = "100%"
              ),
              selectInput(
                NS(id, "convert_format"),
                label = "Output Format",
                choices = c(
                  "Tibble" = "tibble",
                  "Time Series" = "ts",
                  "XTS" = "xts",
                  "Zoo" = "zoo"
                ),
                selected = convert_format,
                width = "100%"
              )
            )
          ),

          # Help text
          helpText(
            "This time series block loads time series data and converts formats using tsbox.",
            "Choose a dataset, optionally filter by start year, and select output format."
          )
        )
      )
    },
    class = "ts_block",
    allow_empty_state = c("start_year"), # start_year is optional
    ...
  )
}

#' Time Series Percentage Change Block
#'
#' A transform block for calculating percentage change rates in time series data.
#' This block wraps tsbox's `ts_pc()` and related functions to compute various
#' percentage change and difference calculations.
#'
#' @param method Method for percentage change calculation:
#'   - "pc": Period-to-period percentage change (ts_pc)
#'   - "pcy": Year-over-year percentage change (ts_pcy) 
#'   - "pca": Annualized percentage change (ts_pca)
#'   - "diff": First differences (ts_diff)
#'   - "diffy": Year-over-year differences (ts_diffy)
#' @param ... Additional arguments passed to new_transform_block
#'
#' @return A transform block for computing percentage changes
#' @export
new_ts_pc_block <- function(
  method = "pc",
  ...
) {
  # Validate method parameter
  method <- match.arg(
    method,
    choices = c("pc", "pcy", "pca", "diff", "diffy")
  )
  
  blockr.core::new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          # Initialize reactive values
          r_method <- reactiveVal(method)
          
          # Input observers
          observeEvent(input$method, {
            r_method(input$method)
          })
          
          list(
            expr = reactive({
              # Build expression using parse/glue pattern
              selected_method <- r_method()
              
              # Map method to corresponding tsbox function
              func_name <- switch(
                selected_method,
                "pc" = "ts_pc",
                "pcy" = "ts_pcy",
                "pca" = "ts_pca", 
                "diff" = "ts_diff",
                "diffy" = "ts_diffy",
                "ts_pc" # fallback
              )
              
              # Build expression that applies the function and converts to ts_tbl
              expr_text <- glue::glue(
                "tsbox::ts_tbl(tsbox::{func_name}(.))"
              )
              
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
          class = "ts-pc-block-container",
          
          # CSS for styling
          tags$style(HTML(
            "
            .ts-pc-block-container {
              padding: 15px;
              background: #f1f3f4;
              border-radius: 8px;
              margin-bottom: 15px;
            }
            .ts-pc-section {
              margin-bottom: 15px;
            }
            .ts-pc-section h4 {
              margin-top: 0;
              margin-bottom: 8px;
              color: #495057;
              font-size: 16px;
            }
            .ts-pc-controls {
              display: grid;
              grid-template-columns: 1fr;
              gap: 10px;
            }
          "
          )),
          
          # Percentage Change Method Selection
          div(
            class = "ts-pc-section",
            h4("Percentage Change Calculation"),
            div(
              class = "ts-pc-controls",
              selectInput(
                NS(id, "method"),
                label = "Calculation Method",
                choices = c(
                  "Period-to-period % change" = "pc",
                  "Year-over-year % change" = "pcy",
                  "Annualized % change" = "pca",
                  "First differences" = "diff",
                  "Year-over-year differences" = "diffy"
                ),
                selected = method,
                width = "100%"
              )
            )
          ),
          
          # Help text
          helpText(
            "Calculate percentage changes and differences in time series data.",
            "Choose the appropriate method based on your analysis needs:", 
            br(),
            "• Period-to-period: Change compared to previous period",
            br(),
            "• Year-over-year: Change compared to same period last year",
            br(), 
            "• Annualized: Annualized rate from period-to-period change",
            br(),
            "• Differences: Absolute differences instead of percentages"
          )
        )
      )
    },
    class = "ts_pc_block",
    ...
  )
}

#' Get Time Series Data (Internal Helper)
#'
#' Internal function to load and process time series data.
#'
#' @param dataset_name Name of the time series dataset
#' @param start_year Optional starting year to filter data
#' @param convert_format Target format for tsbox conversion
#' @keywords internal
get_ts_data <- function(dataset_name, start_year = NULL, convert_format = "tibble") {
  # Get base time series data
  base_data <- switch(
    dataset_name,
    "economics" = ggplot2::economics,
    "AirPassengers" = datasets::AirPassengers,
    "lynx" = datasets::lynx,
    ggplot2::economics # fallback
  )

  # Convert to common time series format first
  if (inherits(base_data, "data.frame")) {
    # For economics data, use date column
    ts_data <- tsbox::ts_tbl(base_data)
  } else {
    # For built-in ts objects
    ts_data <- tsbox::ts_tbl(base_data)
  }

  # Filter by start year if provided
  if (!is.null(start_year)) {
    # Extract year from time column
    time_col <- names(ts_data)[1]  # First column is typically time
    if (inherits(ts_data[[time_col]], "Date")) {
      year_filter <- as.Date(paste0(start_year, "-01-01"))
      ts_data <- ts_data[ts_data[[time_col]] >= year_filter, ]
    } else if (is.numeric(ts_data[[time_col]])) {
      ts_data <- ts_data[ts_data[[time_col]] >= start_year, ]
    }
  }

  # Convert to requested format using tsbox
  result <- switch(
    convert_format,
    "tibble" = tsbox::ts_tbl(ts_data),
    "ts" = tsbox::ts_ts(ts_data),
    "xts" = tsbox::ts_xts(ts_data),
    "zoo" = tsbox::ts_zoo(ts_data),
    tsbox::ts_tbl(ts_data) # fallback to tibble
  )

  # Add metadata as attributes
  attr(result, "source_dataset") <- dataset_name
  attr(result, "start_year") <- start_year
  attr(result, "format") <- convert_format

  result
}

#' Time Series Plot Block
#'
#' A plot block for visualizing time series data using tsbox's ts_ggplot() function.
#' This block creates interactive time series plots with customizable aesthetics
#' and themes, integrating with htmlwidgets for interactivity.
#'
#' @param title Plot title (default: "Time Series Plot")
#' @param color_by Color by series - useful for multiple series (default: TRUE)
#' @param theme ggplot theme: "minimal", "classic", "bw", "grey" (default: "minimal")
#' @param line_size Line thickness (default: 1)
#' @param ... Additional arguments passed to new_plot_block
#'
#' @return A plot block for time series visualization
#' @export
new_ts_plot_block <- function(
  title = "Time Series Plot",
  color_by = TRUE,
  theme = "minimal",
  line_size = 1,
  ...
) {
  # Validate theme parameter
  theme <- match.arg(
    theme,
    choices = c("minimal", "classic", "bw", "grey")
  )
  
  blockr.core::new_plot_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          # Initialize reactive values
          r_title <- reactiveVal(title)
          r_color_by <- reactiveVal(color_by)
          r_theme <- reactiveVal(theme)
          r_line_size <- reactiveVal(line_size)
          
          # Input observers
          observeEvent(input$title, {
            r_title(input$title)
          })
          
          observeEvent(input$color_by, {
            r_color_by(input$color_by)
          })
          
          observeEvent(input$theme, {
            r_theme(input$theme)
          })
          
          observeEvent(input$line_size, {
            r_line_size(input$line_size)
          })
          
          list(
            expr = reactive({
              # Validate data availability
              if (!blockr.core::is_valid(data())) {
                return(quote({
                  p <- ggplot2::ggplot() + ggplot2::geom_blank()
                  p <- p + ggplot2::ggtitle("No data available")
                  p <- p + ggplot2::theme_minimal()
                  plotly::ggplotly(p)
                }))
              }
              
              # Build expression using parse/glue pattern
              plot_title <- r_title()
              use_color <- r_color_by()
              selected_theme <- r_theme()
              size_val <- r_line_size()
              
              # Map theme to corresponding ggplot theme
              theme_func <- switch(
                selected_theme,
                "minimal" = "ggplot2::theme_minimal",
                "classic" = "ggplot2::theme_classic", 
                "bw" = "ggplot2::theme_bw",
                "grey" = "ggplot2::theme_grey",
                "ggplot2::theme_minimal" # fallback
              )
              
              # Build base plot expression using tsbox::ts_ggplot
              if (use_color) {
                base_plot <- "tsbox::ts_ggplot(data, color = 'id')"
              } else {
                base_plot <- "tsbox::ts_ggplot(data)"
              }
              
              # Build complete expression with customizations
              expr_text <- glue::glue(
                "{",
                "  p <- {base_plot}",
                "  p <- p + ggplot2::ggtitle('{plot_title}')",
                "  p <- p + {theme_func}()",
                "  p <- p + ggplot2::geom_line(size = {size_val})", 
                "  plotly::ggplotly(p)",
                "}",
                .sep = "\n"
              )
              
              parse(text = expr_text)[[1]]
            }),
            state = list(
              title = r_title,
              color_by = r_color_by,
              theme = r_theme,
              line_size = r_line_size
            )
          )
        }
      )
    },
    function(id) {
      tagList(
        div(
          class = "ts-plot-block-container",
          
          # CSS for styling
          tags$style(HTML(
            "
            .ts-plot-block-container {
              padding: 15px;
              background: #e8f4f8;
              border-radius: 8px;
              margin-bottom: 15px;
            }
            .ts-plot-section {
              margin-bottom: 15px;
            }
            .ts-plot-section h4 {
              margin-top: 0;
              margin-bottom: 8px;
              color: #495057;
              font-size: 16px;
            }
            .ts-plot-controls {
              display: grid;
              grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
              gap: 10px;
            }
            .ts-plot-single-control {
              display: grid;
              grid-template-columns: 1fr;
              gap: 10px;
            }
          "
          )),
          
          # Title Section
          div(
            class = "ts-plot-section",
            h4("Plot Configuration"),
            div(
              class = "ts-plot-single-control",
              textInput(
                NS(id, "title"),
                label = "Plot Title",
                value = title,
                width = "100%"
              )
            )
          ),
          
          # Aesthetics Section
          div(
            class = "ts-plot-section",
            h4("Aesthetics"),
            div(
              class = "ts-plot-controls",
              checkboxInput(
                NS(id, "color_by"),
                label = "Color by Series",
                value = color_by,
                width = "100%"
              ),
              selectInput(
                NS(id, "theme"),
                label = "Theme",
                choices = c(
                  "Minimal" = "minimal",
                  "Classic" = "classic",
                  "Black & White" = "bw",
                  "Grey" = "grey"
                ),
                selected = theme,
                width = "100%"
              )
            )
          ),
          
          # Line Options Section
          div(
            class = "ts-plot-section",
            h4("Line Options"),
            div(
              class = "ts-plot-single-control",
              sliderInput(
                NS(id, "line_size"),
                label = "Line Thickness",
                min = 0.5,
                max = 3.0,
                value = line_size,
                step = 0.1,
                width = "100%"
              )
            )
          ),
          
          # Help text
          helpText(
            "Create interactive time series plots using tsbox visualization.",
            "The plot will automatically detect time series format and render appropriately.",
            br(),
            "• Color by Series: Useful for multiple time series in same dataset",
            br(),
            "• Interactive: Hover, zoom, and pan enabled via plotly",
            br(),
            "• Input: Accepts any tsboxable format (ts_tbl, ts, xts, etc.)"
          )
        )
      )
    },
    dat_valid = function(data) {
      # Validate that data is tsboxable
      if (!tsbox::ts_boxable(data)) {
        stop("Data must be tsboxable (time series format supported by tsbox)")
      }
      
      # Additional validation for required structure
      if (is.data.frame(data)) {
        # For data frame inputs, should have time and value columns
        if (ncol(data) < 2) {
          stop("Data frame must have at least 2 columns (time and value)")
        }
      }
      
      TRUE
    },
    class = "ts_plot_block",
    allow_empty_state = character(0), # No optional fields
    ...
  )
}

#' S3 Methods for ts_block Custom Display
#'
#' These methods override the default data block display to show dygraphs
#' instead of data tables while still returning the actual time series data.

#' @export
block_output.ts_block <- function(x, result, session) {
  # Convert the data to dygraph format and render as htmlwidget
  dygraphs::renderDygraph({
    # Ensure we have valid data
    if (is.null(result) || !is.data.frame(result) && !is.ts(result)) {
      return(NULL)
    }
    
    # Convert to time series format for dygraph if needed
    if (is.data.frame(result)) {
      # Handle economics data (has date column)
      if ("date" %in% names(result)) {
        # For economics data, create xts from date and numeric columns
        numeric_cols <- result[sapply(result, is.numeric)]
        if (ncol(numeric_cols) > 0) {
          ts_data <- xts::xts(numeric_cols, order.by = result$date)
        } else {
          return(NULL)
        }
      } else {
        # For other data frames, try to convert via tsbox
        ts_data <- try(tsbox::ts_xts(result), silent = TRUE)
        if (inherits(ts_data, "try-error")) {
          return(NULL)
        }
      }
    } else {
      # Already time series, convert to xts for dygraph
      ts_data <- try(tsbox::ts_xts(result), silent = TRUE)
      if (inherits(ts_data, "try-error")) {
        return(NULL)
      }
    }
    
    # Create dygraph
    dygraph <- dygraphs::dygraph(ts_data)
    dygraph <- dygraphs::dyOptions(dygraph, fillGraph = FALSE, drawGrid = TRUE)
    dygraph <- dygraphs::dyHighlight(dygraph, highlightSeriesOpts = list(strokeWidth = 3))
    
    # Add range selector for better interaction
    dygraph <- dygraphs::dyRangeSelector(dygraph)
    
    return(dygraph)
  })
}

#' @export
block_ui.ts_block <- function(id, x, ...) {
  tagList(
    dygraphs::dygraphOutput(NS(id, "result"))
  )
}
