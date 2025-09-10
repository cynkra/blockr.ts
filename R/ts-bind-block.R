#' Time Series Bind Block
#'
#' Combine multiple time series into a single multivariate time series
#'
#' @param ... Additional arguments passed to new_ts_transform_block()
#'
#' @details
#' This block uses tsbox::ts_c() to combine multiple univariate or multivariate
#' time series into a single multivariate series. Each input series becomes a 
#' separate series in the output, identified by the 'id' column.
#'
#' Unlike other transform blocks, this block accepts multiple data inputs
#' and has no UI controls - it simply combines all incoming data.
#'
#' @return A ts_bind_block object
#' @export
new_ts_bind_block <- function(...) {
  
  new_ts_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          
          # No UI controls needed - just combine the data
          
          list(
            expr = reactive({
              # Expression to combine time series
              # tsbox::ts_c can take multiple arguments
              expr_text <- "
              {
                # Convert to list if needed and ensure tibble format
                if (!is.list(data)) {
                  data <- list(data)
                }
                
                # Convert each element to tibble format and ensure proper IDs
                data_list <- lapply(seq_along(data), function(i) {
                  d <- data[[i]]
                  tbl <- tsbox::ts_tbl(d)
                  
                  # If no id column exists, add one
                  if (!'id' %in% names(tbl)) {
                    tbl$id <- paste0('series', i)
                  }
                  
                  # Clean up existing IDs if they are too long or messy
                  if ('id' %in% names(tbl)) {
                    unique_ids <- unique(tbl$id)
                    if (length(unique_ids) == 1 && nchar(unique_ids[1]) > 50) {
                      # Replace overly long ID with simple name
                      tbl$id <- paste0('series', i)
                    }
                  }
                  
                  tbl
                })
                
                # Combine using ts_c
                if (length(data_list) == 1) {
                  data_list[[1]]
                } else {
                  # Name the list elements to avoid issues
                  names(data_list) <- paste0('series', seq_along(data_list))
                  do.call(tsbox::ts_c, data_list)
                }
              }"
              
              parse(text = expr_text)[[1]]
            }),
            state = list()  # No state needed
          )
        }
      )
    },
    function(id) {
      tagList(
        div(
          class = "ts-block-container",
          div(
            class = "ts-block-info",
            style = "margin: 15px;",
            helpText(
              icon("layer-group"),
              "This block combines all incoming time series into a single multivariate series."
            ),
            helpText(
              class = "text-muted",
              "Connect multiple data sources to combine them. Each series will retain its identity in the 'id' column."
            )
          )
        )
      )
    },
    class = c("ts_bind_block"),
    ...
  )
}