#' Time Series Bind Block
#'
#' Combine multiple time series into a single multivariate time series
#'
#' @param ... Named time series data to combine. Each argument should be a
#'   time series that can be converted to tsbox format.
#'
#' @details
#' This block uses tsbox::ts_c() to combine multiple univariate or multivariate
#' time series into a single multivariate series. Each input series becomes a
#' separate series in the output, identified by the 'id' column.
#'
#' You can pass multiple time series as named arguments:
#' \code{new_ts_bind_block(male = mdeaths, female = fdeaths)}
#'
#' @return A ts_bind_block object
#' @export
new_ts_bind_block <- function(...) {
  # Capture the input data
  dots <- list(...)

  # If data is provided, combine it
  if (length(dots) > 0) {
    # Convert each input to tibble format
    data_list <- lapply(seq_along(dots), function(i) {
      d <- dots[[i]]
      tbl <- tsbox::ts_tbl(d)
      # Use the argument name as the series ID if available
      nm <- names(dots)[i]
      if (!is.null(nm) && nm != "") {
        if (!'id' %in% names(tbl) || length(unique(tbl$id)) == 1) {
          tbl$id <- nm
        }
      } else if (!'id' %in% names(tbl)) {
        tbl$id <- paste0('series', i)
      }
      tbl
    })

    # Combine all series
    combined_data <- if (length(data_list) == 1) {
      data_list[[1]]
    } else {
      do.call(tsbox::ts_c, data_list)
    }

    # Return a data block with the combined data
    new_ts_data_block(
      server = function(id, data) {
        moduleServer(
          id,
          function(input, output, session) {
            list(
              expr = reactive({
                # Simply return the data that was passed to the block
                quote(data)
              }),
              state = list()
            )
          }
        )
      },
      ui = function(id) {
        # Capture combined_data in closure
        local_data <- combined_data
        tagList(
          div(
            class = "ts-block-container",
            div(
              class = "ts-block-info",
              style = "margin: 15px;",
              helpText(
                icon("layer-group"),
                paste0(
                  "Combined ",
                  length(unique(local_data$id)),
                  " time series"
                )
              ),
              helpText(
                class = "text-muted",
                paste("Series:", paste(unique(local_data$id), collapse = ", "))
              )
            )
          )
        )
      },
      class = c("ts_bind_block"),
      data = combined_data
    )
  } else {
    # No initial data - create a transform block that can combine incoming data
    new_ts_transform_block(
      function(id, data) {
        moduleServer(
          id,
          function(input, output, session) {
            list(
              expr = reactive({
                expr_text <- "
                {
                  # Combine incoming data
                  if (is.list(data) && !is.data.frame(data)) {
                    # Multiple inputs - combine them
                    data_list <- lapply(seq_along(data), function(i) {
                      d <- data[[i]]
                      tbl <- tsbox::ts_tbl(d)
                      # Add ID if missing
                      if (!'id' %in% names(tbl)) {
                        tbl$id <- paste0('series', i)
                      }
                      tbl
                    })
                    
                    # Combine using ts_c
                    if (length(data_list) == 1) {
                      data_list[[1]]
                    } else {
                      do.call(tsbox::ts_c, data_list)
                    }
                  } else {
                    # Single input
                    tsbox::ts_tbl(data)
                  }
                }"
                parse(text = expr_text)[[1]]
              }),
              state = list()
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
                "This block combines incoming time series into a single multivariate series."
              ),
              helpText(
                class = "text-muted",
                "Connect multiple data sources to combine them."
              )
            )
          )
        )
      },
      class = c("ts_bind_block")
    )
  }
}
