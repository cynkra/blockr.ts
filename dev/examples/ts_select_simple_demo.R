# Load required libraries
library(blockr.core)
library(blockr.ts)
pkgload::load_all()

# Simple demo: Just select series without additional transformations
# Using the data argument to pass data directly to a transform block
blockr.core::serve(
  new_ts_select_block(
    series = c("DAX"),
    multiple = FALSE # Single selection mode
  ),
  data = list(data = tsbox::ts_tbl(datasets::EuStockMarkets))
)
