#' Time Series Dataset Selector Block
#'
#' A data block that provides access to all built-in R time series datasets.
#' Supports both univariate and multivariate time series, automatically converting
#' them to tidy data frames using tsbox::ts_tbl().
#'
#' @param dataset Name of the dataset to load (default: "AirPassengers")
#' @param ... Additional arguments passed to new_ts_data_block
#'
#' @export
new_ts_dataset_block <- function(dataset = "AirPassengers", ...) {
  # Helper function to get time series start/end as strings
  get_ts_period <- function(dataset_name) {
    tryCatch(
      {
        ts_obj <- get(dataset_name, envir = as.environment("package:datasets"))
        start_vals <- start(ts_obj)
        end_vals <- end(ts_obj)

        # Format based on frequency
        freq <- frequency(ts_obj)
        if (freq == 1) {
          # Annual data
          list(
            start = as.character(start_vals[1]),
            end = as.character(end_vals[1])
          )
        } else if (freq == 4) {
          # Quarterly
          list(
            start = paste0(start_vals[1], " Q", start_vals[2]),
            end = paste0(end_vals[1], " Q", end_vals[2])
          )
        } else if (freq == 12) {
          # Monthly
          months <- c(
            "Jan",
            "Feb",
            "Mar",
            "Apr",
            "May",
            "Jun",
            "Jul",
            "Aug",
            "Sep",
            "Oct",
            "Nov",
            "Dec"
          )
          list(
            start = paste0(months[start_vals[2]], " ", start_vals[1]),
            end = paste0(months[end_vals[2]], " ", end_vals[1])
          )
        } else {
          # Other frequencies
          list(
            start = paste(start_vals, collapse = "-"),
            end = paste(end_vals, collapse = "-")
          )
        }
      },
      error = function(e) {
        list(start = "", end = "")
      }
    )
  }

  # Define all available time series datasets with metadata
  ts_datasets <- list(
    "AirPassengers" = list(
      desc = "Monthly airline passengers (1949-1960)",
      type = "univariate",
      freq = 12,
      series = 1
    ),
    "BJsales" = list(
      desc = "Sales data time series",
      type = "univariate",
      freq = 1,
      series = 1
    ),
    "EuStockMarkets" = list(
      desc = "European stock indices (DAX, SMI, CAC, FTSE)",
      type = "multivariate",
      freq = 260,
      series = 4
    ),
    "JohnsonJohnson" = list(
      desc = "Quarterly J&J earnings per share",
      type = "univariate",
      freq = 4,
      series = 1
    ),
    "LakeHuron" = list(
      desc = "Lake Huron water level (1875-1972)",
      type = "univariate",
      freq = 1,
      series = 1
    ),
    "Nile" = list(
      desc = "River Nile flow (1871-1970)",
      type = "univariate",
      freq = 1,
      series = 1
    ),
    "Seatbelts" = list(
      desc = "UK road casualties (8 series, 1969-1984)",
      type = "multivariate",
      freq = 12,
      series = 8
    ),
    "UKDriverDeaths" = list(
      desc = "UK driver deaths (1969-1984)",
      type = "univariate",
      freq = 12,
      series = 1
    ),
    "UKgas" = list(
      desc = "UK quarterly gas consumption",
      type = "univariate",
      freq = 4,
      series = 1
    ),
    "USAccDeaths" = list(
      desc = "US accidental deaths (1973-1978)",
      type = "univariate",
      freq = 12,
      series = 1
    ),
    "WWWusage" = list(
      desc = "Internet usage per minute",
      type = "univariate",
      freq = 1,
      series = 1
    ),
    "airmiles" = list(
      desc = "US airline passenger miles (1937-1960)",
      type = "univariate",
      freq = 1,
      series = 1
    ),
    "austres" = list(
      desc = "Australian residents (quarterly)",
      type = "univariate",
      freq = 4,
      series = 1
    ),
    "co2" = list(
      desc = "Mauna Loa CO2 concentration",
      type = "univariate",
      freq = 12,
      series = 1
    ),
    "discoveries" = list(
      desc = "Major scientific discoveries per year",
      type = "univariate",
      freq = 1,
      series = 1
    ),
    "lh" = list(
      desc = "Luteinizing hormone levels",
      type = "univariate",
      freq = 1,
      series = 1
    ),
    "lynx" = list(
      desc = "Canadian lynx trappings (1821-1934)",
      type = "univariate",
      freq = 1,
      series = 1
    ),
    "nhtemp" = list(
      desc = "New Haven temperatures",
      type = "univariate",
      freq = 1,
      series = 1
    ),
    "nottem" = list(
      desc = "Nottingham temperatures (1920-1939)",
      type = "univariate",
      freq = 12,
      series = 1
    ),
    "presidents" = list(
      desc = "US presidential approval ratings",
      type = "univariate",
      freq = 4,
      series = 1
    ),
    "sunspot.month" = list(
      desc = "Monthly sunspot numbers",
      type = "univariate",
      freq = 12,
      series = 1
    ),
    "sunspot.year" = list(
      desc = "Yearly sunspot numbers",
      type = "univariate",
      freq = 1,
      series = 1
    ),
    "sunspots" = list(
      desc = "Monthly sunspot numbers (1749-1983)",
      type = "univariate",
      freq = 12,
      series = 1
    ),
    "treering" = list(
      desc = "Tree ring widths",
      type = "univariate",
      freq = 1,
      series = 1
    )
  )

  new_ts_data_block(
    function(id) {
      moduleServer(
        id,
        function(input, output, session) {
          # Reactive value for selected dataset
          r_dataset <- reactiveVal(dataset)

          # Observer for dataset selection
          observeEvent(input$dataset, {
            r_dataset(input$dataset)
          })

          # Update info display
          output$dataset_info <- renderUI({
            current_dataset <- r_dataset()
            info <- ts_datasets[[current_dataset]]
            period <- get_ts_period(current_dataset)

            freq_label <- switch(
              as.character(info$freq),
              "1" = "Annual",
              "4" = "Quarterly",
              "12" = "Monthly",
              "260" = "Daily",
              paste0("Frequency: ", info$freq)
            )

            tagList(
              tags$div(
                class = "ts-dataset-info-panel",
                tags$h5(
                  info$desc,
                  style = "margin-top: 0; margin-bottom: 10px; color: #333;"
                ),
                tags$div(
                  style = "font-size: 0.9rem;",
                  tags$div(
                    style = "margin-bottom: 5px;",
                    tags$strong("Type: "),
                    tags$span(
                      class = if (info$type == "multivariate") {
                        "badge-multi"
                      } else {
                        "badge-uni"
                      },
                      paste0(info$type, " (", info$series, " series)")
                    )
                  ),
                  tags$div(
                    style = "margin-bottom: 5px;",
                    tags$strong("Frequency: "),
                    tags$span(freq_label)
                  ),
                  tags$div(
                    tags$strong("Period: "),
                    tags$span(paste(period$start, "-", period$end))
                  )
                )
              )
            )
          })

          list(
            expr = reactive({
              # Build expression using tsbox::ts_tbl
              dataset_name <- r_dataset()
              expr_text <- glue::glue("tsbox::ts_tbl(datasets::{dataset_name})")
              parse(text = expr_text)[[1]]
            }),
            state = list(
              dataset = r_dataset
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
            grid-template-columns: 1fr;
          }
          
          @media (min-width: 768px) {
            .ts-block-form-grid {
              grid-template-columns: 1fr 1fr;
            }
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
          
          .ts-dataset-info-panel {
            padding: 12px;
            background: #f8f9fa;
            border: 1px solid #dee2e6;
            border-radius: 4px;
            height: fit-content;
          }
          
          .badge-multi {
            background-color: #17a2b8;
            color: white;
            padding: 2px 8px;
            border-radius: 3px;
            font-size: 0.875em;
          }
          
          .badge-uni {
            background-color: #28a745;
            color: white;
            padding: 2px 8px;
            border-radius: 3px;
            font-size: 0.875em;
          }
          
          /* DataTable specific styling */
          .ts-block-dt-wrapper {
            margin-top: 10px;
          }
          
          .ts-block-dt-wrapper .dataTables_wrapper {
            font-size: 0.9rem;
          }
          
          .ts-block-dt-wrapper table.dataTable thead {
            background-color: #f8f9fa;
          }
          
          .ts-block-dt-wrapper table.dataTable tbody tr.selected {
            background-color: #007bff !important;
            color: white;
          }
          
          .ts-block-dt-wrapper table.dataTable tbody tr:hover {
            background-color: #e9ecef !important;
            cursor: pointer;
          }
          
          .ts-block-dt-wrapper table.dataTable tbody tr.selected:hover {
            background-color: #0056b3 !important;
          }
          "
        )),

        div(
          class = "ts-block-container",

          div(
            class = "ts-block-form-grid",

            # Data Section with two columns
            tags$h4("Time Series Dataset", style = "grid-column: 1 / -1;"),

            # Left column: Dataset selector
            div(
              class = "ts-block-column",
              div(
                class = "ts-block-input-wrapper",
                selectInput(
                  NS(id, "dataset"),
                  label = "Select Dataset",
                  choices = setNames(
                    names(ts_datasets),
                    sapply(names(ts_datasets), function(name) {
                      info <- ts_datasets[[name]]
                      paste0(name, " - ", info$desc)
                    })
                  ),
                  selected = dataset,
                  width = "100%"
                )
              )
            ),

            # Right column: Dataset info
            div(
              class = "ts-block-column",
              uiOutput(NS(id, "dataset_info"))
            ),

            # Help text (spans full width)
            div(
              class = "ts-block-help-text",
              style = "grid-column: 1 / -1;",
              helpText(
                "Access to all 25 built-in R time series datasets. ",
                "Data is converted to tidy format and displayed as an interactive dygraph."
              )
            )
          )
        )
      )
    },
    class = "ts_dataset_block",
    ...
  )
}
