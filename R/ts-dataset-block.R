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
          
          # Reactive for dataset info
          dataset_info <- reactive({
            ts_datasets[[r_dataset()]]
          })
          
          # Update info display
          output$dataset_info <- renderUI({
            info <- dataset_info()
            freq_label <- switch(as.character(info$freq),
              "1" = "Annual",
              "4" = "Quarterly", 
              "12" = "Monthly",
              "260" = "Daily (business days)",
              paste0("Frequency: ", info$freq)
            )
            
            tagList(
              tags$div(
                class = "dataset-info-panel",
                tags$strong("Type: "), 
                tags$span(
                  class = if (info$type == "multivariate") "badge-multi" else "badge-uni",
                  paste0(info$type, " (", info$series, " series)")
                ),
                tags$br(),
                tags$strong("Frequency: "), freq_label
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
        div(
          class = "ts-dataset-container",
          
          # CSS for the block
          tags$style(HTML(
            "
            .ts-dataset-container {
              padding: 15px;
              background: #f8f9fa;
              border-radius: 8px;
              margin-bottom: 15px;
            }
            .dataset-selector {
              margin-bottom: 15px;
            }
            .dataset-info-panel {
              padding: 10px;
              background: white;
              border: 1px solid #dee2e6;
              border-radius: 4px;
              margin-top: 10px;
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
            "
          )),
          
          # Dataset selector
          div(
            class = "dataset-selector",
            h4("Select Time Series Dataset"),
            selectInput(
              NS(id, "dataset"),
              label = NULL,
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
          ),
          
          # Dynamic info display
          uiOutput(NS(id, "dataset_info")),
          
          # Help text
          helpText(
            "This block provides access to all built-in R time series datasets.",
            "Data is automatically converted to tidy format using tsbox and displayed as an interactive dygraph."
          )
        )
      )
    },
    class = "ts_dataset_block",
    ...
  )
}