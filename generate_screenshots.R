# Generate screenshots for all blockr.ts blocks

# Load the package from current directory
devtools::load_all()
library(blockr.ggplot)

# Create output directory
if (!dir.exists("man/figures")) {
  dir.create("man/figures", recursive = TRUE)
}

# Data blocks
message("Generating data block screenshots...")

# AirPassengers block
blockr.ggplot::validate_block_screenshot(
  block = new_ts_airpassenger_block(),
  filename = "ts_airpassenger_block.png",
  output_dir = "man/figures"
)

# Dataset block - with EuStockMarkets (multivariate)
blockr.ggplot::validate_block_screenshot(
  block = new_ts_dataset_block(dataset = "EuStockMarkets"),
  filename = "ts_dataset_block.png",
  output_dir = "man/figures"
)

# Transform blocks
message("Generating transform block screenshots...")

# Change block with multivariate data
blockr.ggplot::validate_block_screenshot(
  block = new_ts_change_block(method = "pcy"),
  data = list(data = tsbox::ts_tbl(tsbox::ts_c(datasets::mdeaths, datasets::fdeaths))),
  filename = "ts_change_block.png",
  output_dir = "man/figures"
)

# Frequency block
blockr.ggplot::validate_block_screenshot(
  block = new_ts_frequency_block(to = "year", aggregate = "mean"),
  data = list(data = tsbox::ts_tbl(datasets::AirPassengers)),
  filename = "ts_frequency_block.png",
  output_dir = "man/figures"
)

# Select block with multivariate data
blockr.ggplot::validate_block_screenshot(
  block = new_ts_select_block(series = c("mdeaths")),
  data = list(data = tsbox::ts_tbl(tsbox::ts_c(datasets::mdeaths, datasets::fdeaths))),
  filename = "ts_select_block.png",
  output_dir = "man/figures"
)

# Lag block
blockr.ggplot::validate_block_screenshot(
  block = new_ts_lag_block(by = 12),
  data = list(data = tsbox::ts_tbl(datasets::AirPassengers)),
  filename = "ts_lag_block.png",
  output_dir = "man/figures"
)

# Span block
blockr.ggplot::validate_block_screenshot(
  block = new_ts_span_block(start = 1950, end = 1955),
  data = list(data = tsbox::ts_tbl(datasets::AirPassengers)),
  filename = "ts_span_block.png",
  output_dir = "man/figures"
)

# New analysis blocks (if they exist)
message("Checking for analysis blocks...")

# Scale block
tryCatch({
  blockr.ggplot::validate_block_screenshot(
    block = new_ts_scale_block(method = "normalize"),
    data = list(data = tsbox::ts_tbl(datasets::AirPassengers)),
    filename = "ts_scale_block.png",
    output_dir = "man/figures"
  )
  message("  - Generated ts_scale_block screenshot")
}, error = function(e) {
  message("  - ts_scale_block not found (not yet implemented)")
})

# Decompose block  
tryCatch({
  blockr.ggplot::validate_block_screenshot(
    block = new_ts_decompose_block(component = "seasonal_adjusted"),
    data = list(data = tsbox::ts_tbl(datasets::AirPassengers)),
    filename = "ts_decompose_block.png",
    output_dir = "man/figures"
  )
  message("  - Generated ts_decompose_block screenshot")
}, error = function(e) {
  message("  - ts_decompose_block not found (not yet implemented)")
})

# Forecast block
tryCatch({
  blockr.ggplot::validate_block_screenshot(
    block = new_ts_forecast_block(horizon = 24),
    data = list(data = tsbox::ts_tbl(datasets::AirPassengers)),
    filename = "ts_forecast_block.png",
    output_dir = "man/figures"
  )
  message("  - Generated ts_forecast_block screenshot")
}, error = function(e) {
  message("  - ts_forecast_block not found (not yet implemented)")
})

# PCA block (multivariate only)
tryCatch({
  blockr.ggplot::validate_block_screenshot(
    block = new_ts_pca_block(n_components = 2),
    data = list(data = tsbox::ts_tbl(datasets::EuStockMarkets)),
    filename = "ts_pca_block.png",
    output_dir = "man/figures"
  )
  message("  - Generated ts_pca_block screenshot")
}, error = function(e) {
  message("  - ts_pca_block not found (not yet implemented)")
})

message("\nScreenshot generation complete!")
message("Generated screenshots in: man/figures/")