#!/usr/bin/env Rscript

# Generate screenshots for all blockr.ts blocks
#
# This script creates screenshots of all blocks for use in pkgdown documentation.
# Screenshots are saved to man/figures/ directory.
#
# To run: source("dev/screenshots/generate_all.R")

# Set NOT_CRAN environment variable BEFORE loading any packages
# This is required for shinytest2 to work in non-interactive mode
Sys.setenv(NOT_CRAN = "true")

# Load package with devtools::load_all() to ensure latest changes are picked up
devtools::load_all(".")

# Source the validation function
source("dev/screenshots/validate-screenshot.R")

cat("Generating screenshots for all blockr.ts blocks...\n")
cat("Output directory: man/figures/\n\n")

# Common screenshot settings
SCREENSHOT_WIDTH <- 1400
SCREENSHOT_HEIGHT <- 700
SCREENSHOT_DELAY <- 3

# Prepare data for transform blocks
airpassengers_data <- list(data = tsbox::ts_tbl(datasets::AirPassengers))
eustocks_data <- list(data = tsbox::ts_tbl(datasets::EuStockMarkets))

# =============================================================================
# 1. DATASET BLOCK - Dataset selector (data block, no input data needed)
# =============================================================================
cat("1/10 - Dataset block\n")
validate_block_screenshot(
  block = new_ts_dataset_block(dataset = "AirPassengers"),
  data = NULL,
  filename = "ts-dataset-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  verbose = FALSE
)

# =============================================================================
# 2. CHANGE BLOCK - Percentage change calculations
# =============================================================================
cat("2/10 - Change block\n")
validate_block_screenshot(
  block = new_ts_change_block(method = "pcy"),
  data = airpassengers_data,
  filename = "ts-change-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  verbose = FALSE
)

# =============================================================================
# 3. FREQUENCY BLOCK - Frequency conversion
# =============================================================================
cat("3/10 - Frequency block\n")
validate_block_screenshot(
  block = new_ts_frequency_block(to = "quarter", aggregate = "mean"),
  data = airpassengers_data,
  filename = "ts-frequency-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY + 1,
  verbose = FALSE
)

# =============================================================================
# 4. SELECT BLOCK - Series selection (needs multivariate data)
# =============================================================================
cat("4/10 - Select block\n")
validate_block_screenshot(
  block = new_ts_select_block(),
  data = eustocks_data,
  filename = "ts-select-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  verbose = FALSE
)

# =============================================================================
# 5. LAG BLOCK - Lag/Lead transformation
# =============================================================================
cat("5/10 - Lag block\n")
validate_block_screenshot(
  block = new_ts_lag_block(by = 12),
  data = airpassengers_data,
  filename = "ts-lag-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  verbose = FALSE
)

# =============================================================================
# 6. SPAN BLOCK - Time range selection
# =============================================================================
cat("6/10 - Span block\n")
validate_block_screenshot(
  block = new_ts_span_block(),
  data = airpassengers_data,
  filename = "ts-span-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY + 1,
  verbose = FALSE
)

# =============================================================================
# 7. SCALE BLOCK - Scaling and indexing
# =============================================================================
cat("7/10 - Scale block\n")
validate_block_screenshot(
  block = new_ts_scale_block(method = "index"),
  data = airpassengers_data,
  filename = "ts-scale-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY,
  verbose = FALSE
)

# =============================================================================
# 8. DECOMPOSE BLOCK - Seasonal decomposition
# =============================================================================
cat("8/10 - Decompose block\n")
validate_block_screenshot(
  block = new_ts_decompose_block(component = "seasonal_adjusted"),
  data = airpassengers_data,
  filename = "ts-decompose-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY + 2,
  verbose = FALSE
)

# =============================================================================
# 9. FORECAST BLOCK - Time series forecasting
# =============================================================================
cat("9/10 - Forecast block\n")
validate_block_screenshot(
  block = new_ts_forecast_block(horizon = 24),
  data = airpassengers_data,
  filename = "ts-forecast-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY + 2,
  verbose = FALSE
)

# =============================================================================
# 10. PCA BLOCK - Principal Component Analysis (needs multivariate data)
# =============================================================================
cat("10/10 - PCA block\n")
validate_block_screenshot(
  block = new_ts_pca_block(n_components = 2),
  data = eustocks_data,
  filename = "ts-pca-block.png",
  output_dir = "man/figures",
  width = SCREENSHOT_WIDTH,
  height = SCREENSHOT_HEIGHT,
  delay = SCREENSHOT_DELAY + 1,
  verbose = FALSE
)

cat("\n✓ All screenshots generated!\n")
cat("Screenshots saved to: man/figures/\n\n")

# =============================================================================
# VALIDATION: List generated screenshots
# =============================================================================

cat("Generated screenshots:\n")
screenshots <- list.files("man/figures", pattern = "^ts-.*\\.png$", full.names = TRUE)
for (s in screenshots) {
  info <- file.info(s)
  cat(sprintf("  %s (%s bytes)\n", basename(s), format(info$size, big.mark = ",")))
}
