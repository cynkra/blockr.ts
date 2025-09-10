#' Time Series Data Block Constructor
#'
#' Base constructor for time series data blocks that automatically display results as dygraphs.
#' This adds both ts_data_block and ts_block classes to ensure dygraph rendering.
#'
#' @param server Server function for the block
#' @param ui UI function for the block
#' @param class Additional class names for the block
#' @param ctor The constructor function environment
#' @param ... Additional arguments passed to new_data_block
#'
#' @export
new_ts_data_block <- function(server, ui, class, ctor = sys.parent(), ...) {
  blockr.core::new_data_block(
    server = server,
    ui = ui,
    class = c(class, "ts_data_block", "ts_block"),
    ctor = ctor,
    ...
  )
}

#' Time Series Transform Block Constructor
#'
#' Base constructor for time series transform blocks that automatically display results as dygraphs.
#' This adds both ts_transform_block and ts_block classes to ensure dygraph rendering.
#'
#' @param server Server function for the block
#' @param ui UI function for the block
#' @param class Additional class names for the block
#' @param ctor The constructor function environment
#' @param ... Additional arguments passed to new_transform_block
#'
#' @export
new_ts_transform_block <- function(
  server,
  ui,
  class,
  ctor = sys.parent(),
  ...
) {
  blockr.core::new_transform_block(
    server = server,
    ui = ui,
    class = c(class, "ts_transform_block", "ts_block"),
    ctor = ctor,
    ...
  )
}


#' @export
block_ui.ts_block <- function(id, x, ...) {
  tagList(
    dygraphs::dygraphOutput(NS(id, "result"))
  )
}

#' @export
block_output.ts_block <- function(x, result, session) {
  # This method overrides the default data table display
  # and shows a dygraph instead using tsbox::ts_dygraphs()
  dygraphs::renderDygraph({
    if (is.null(result)) {
      return(NULL)
    }

    # Get number of series to determine how many colors we need
    n_series <- if ("id" %in% names(result)) {
      length(unique(result$id))
    } else {
      1 # univariate series
    }

    # Use tsbox colors palette
    colors_to_use <- tsbox::colors_tsbox()[seq_len(n_series)]

    # Create dygraph without calling ts_dygraphs first to avoid color conflict
    # Convert to xts directly
    ts_data <- tsbox::ts_xts(result)
    dygraph <- dygraphs::dygraph(ts_data)

    # Apply styling with proper colors and thicker lines
    dygraph <- dygraphs::dyOptions(
      dygraph,
      fillGraph = FALSE,
      drawGrid = TRUE,
      colors = colors_to_use,
      strokeWidth = 2.5
    )
    dygraph <- dygraphs::dyRangeSelector(dygraph, height = 20)

    dygraph
  })
}
