# Simple Time Series Analysis Pipeline Examples for blockr.ts
# 
# Note: blockr.core::serve() works with individual blocks.
# For pipelines, you need to use blockr.ui's DAG board functionality.

library(blockr.core)
library(blockr.ts)
library(blockr.ui)

# Register blocks with blockr
register_ts_blocks()

# ============================================================================
# INDIVIDUAL BLOCKS (work with blockr.core::serve)
# ============================================================================

# Example 1: Single data block
blockr.core::serve(
  new_ts_dataset_block(dataset = "AirPassengers")
)

# Example 2: Transform block with direct data input
blockr.core::serve(
  new_ts_change_block(method = "pcy"),
  data = list(data = tsbox::ts_tbl(datasets::AirPassengers))
)

# Example 3: Decomposition with direct data
blockr.core::serve(
  new_ts_decompose_block(component = "seasonal_adjusted"),
  data = list(data = tsbox::ts_tbl(datasets::AirPassengers))
)

# ============================================================================
# SIMPLE PIPELINES (using DAG board)
# ============================================================================

# Pipeline 1: Basic Time Series Analysis
pipeline1 <- blockr.ui::new_dag_board(
  blocks = c(
    data = new_ts_dataset_block(dataset = "AirPassengers"),
    decompose = new_ts_decompose_block(component = "seasonal_adjusted"),
    change = new_ts_change_block(method = "pcy"),
    forecast = new_ts_forecast_block(horizon = 24)
  ),
  links = c(
    new_link("data", "decompose", "data"),
    new_link("decompose", "change", "data"),
    new_link("change", "forecast", "data")
  )
)

# Serve the pipeline
blockr.ui::serve_dag_board(pipeline1)

# Pipeline 2: Multivariate Analysis
pipeline2 <- blockr.ui::new_dag_board(
  blocks = c(
    data = new_ts_dataset_block(dataset = "EuStockMarkets"),
    select = new_ts_select_block(series = c("DAX", "FTSE")),
    scale = new_ts_scale_block(method = "normalize"),
    change = new_ts_change_block(method = "pc")
  ),
  links = c(
    new_link("data", "select", "data"),
    new_link("select", "scale", "data"),
    new_link("scale", "change", "data")
  )
)

blockr.ui::serve_dag_board(pipeline2)

# Pipeline 3: Frequency Conversion
pipeline3 <- blockr.ui::new_dag_board(
  blocks = c(
    data = new_ts_dataset_block(dataset = "co2"),
    freq = new_ts_frequency_block(to = "year", aggregate = "mean"),
    change = new_ts_change_block(method = "diff"),
    forecast = new_ts_forecast_block(horizon = 5)
  ),
  links = c(
    new_link("data", "freq", "data"),
    new_link("freq", "change", "data"),
    new_link("change", "forecast", "data")
  )
)

blockr.ui::serve_dag_board(pipeline3)

# Pipeline 4: Time Range Analysis
pipeline4 <- blockr.ui::new_dag_board(
  blocks = c(
    data = new_ts_dataset_block(dataset = "Nile"),
    span = new_ts_span_block(start = 1900, end = 1950),
    decompose = new_ts_decompose_block(component = "trend"),
    scale = new_ts_scale_block(method = "index", base = 1900)
  ),
  links = c(
    new_link("data", "span", "data"),
    new_link("span", "decompose", "data"),
    new_link("decompose", "scale", "data")
  )
)

blockr.ui::serve_dag_board(pipeline4)

# Pipeline 5: Lag Analysis
pipeline5 <- blockr.ui::new_dag_board(
  blocks = c(
    data = new_ts_dataset_block(dataset = "lynx"),
    lag = new_ts_lag_block(by = 1),
    change = new_ts_change_block(method = "pc"),
    forecast = new_ts_forecast_block(horizon = 10)
  ),
  links = c(
    new_link("data", "lag", "data"),
    new_link("lag", "change", "data"),
    new_link("change", "forecast", "data")
  )
)

blockr.ui::serve_dag_board(pipeline5)