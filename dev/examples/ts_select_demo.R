# Load required libraries
library(blockr.core)
library(blockr.ts)
pkgload::load_all()

# Demo workflow for ts-select block with multiple blocks
blockr.core::serve(
  new_board(
    blocks = c(
      # Load multivariate time series data (EuStockMarkets has 4 series)
      data = new_ts_dataset_block(dataset = "EuStockMarkets"),

      # Select specific series from the multivariate data
      selected = new_ts_select_block(
        series = c("DAX", "FTSE"),  # Select only DAX and FTSE indices
        multiple = TRUE              # Allow multiple selection
      ),

      # Apply transformation to selected series
      transformed = new_ts_change_block(method = "pcy")  # Year-over-year percentage change
    ),
    links = c(
      new_link("data", "selected", "data"),
      new_link("selected", "transformed", "data")
    )
  )
)
