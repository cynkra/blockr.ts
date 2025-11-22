#' Generate Screenshots for All blockr.ts Blocks
#'
#' This script generates screenshots for all blockr.ts blocks and saves them
#' to man/figures/ for use in vignettes and documentation.
#'
#' Run this script from the package root directory:
#'   source("dev/screenshots/generate_all.R")

# Setup -----------------------------------------------------------------------

# Set NOT_CRAN environment variable
Sys.setenv(NOT_CRAN = "true")

# Load the package
cat("Loading blockr.ts package...\n")
devtools::load_all(".")

# Source the validation function
source("dev/screenshots/validate-screenshot.R")

# Configuration
OUTPUT_DIR <- "man/figures"
VERBOSE <- FALSE  # Set to TRUE for detailed output from each screenshot
DELAY <- 2  # Seconds to wait for app to load (increase if blocks don't fully render)

# Helper function to generate screenshot
gen_screenshot <- function(block, filename, data = NULL, delay = DELAY, n, total) {
  cat(sprintf("%d/%d - %s\n", n, total, filename))

  # If no data provided, use default NULL for data blocks
  if (is.null(data)) {
    data <- list(data = NULL)
  }

  result <- validate_block_screenshot(
    block = block,
    data = data,
    filename = filename,
    output_dir = OUTPUT_DIR,
    width = 800,
    height = 600,
    delay = delay,
    verbose = VERBOSE
  )

  if (!result$success) {
    cat(sprintf("  WARNING: Failed - %s\n", result$error))
  } else {
    cat(sprintf("  ✓ Saved to %s\n", result$path))
  }

  return(result$success)
}

# Generate Screenshots --------------------------------------------------------

cat("\n=== Generating blockr.ts Screenshots ===\n\n")

total_blocks <- 8
current <- 0
successes <- 0

# 1. AirPassengers Data Block
# Note: Data blocks use blockr.core::serve() without data parameter
current <- current + 1
cat(sprintf("%d/%d - %s\n", current, total_blocks, "ts-airpassenger-block.png"))
result <- tryCatch({
  Sys.setenv(NOT_CRAN = "true")
  temp_dir <- tempfile("blockr_validation_")
  dir.create(temp_dir)

  app_file <- file.path(temp_dir, "app.R")
  writeLines(c(
    "library(shiny)",
    "library(blockr.core)",
    "library(blockr.ts)",
    "blockr.core::serve(new_ts_airpassenger_block())"
  ), app_file)

  app <- shinytest2::AppDriver$new(app_file, wait = TRUE, timeout = 10000)
  Sys.sleep(DELAY)

  output_path <- file.path(OUTPUT_DIR, "ts-airpassenger-block.png")
  app$get_screenshot(output_path, width = 800, height = 600)
  app$stop()

  unlink(temp_dir, recursive = TRUE)
  cat(sprintf("  ✓ Saved to %s\n", output_path))
  successes <- successes + 1
  TRUE
}, error = function(e) {
  cat(sprintf("  WARNING: Failed - %s\n", e$message))
  FALSE
})

# 2. Dataset Selector Block
current <- current + 1
cat(sprintf("%d/%d - %s\n", current, total_blocks, "ts-dataset-block.png"))
result <- tryCatch({
  Sys.setenv(NOT_CRAN = "true")
  temp_dir <- tempfile("blockr_validation_")
  dir.create(temp_dir)

  app_file <- file.path(temp_dir, "app.R")
  writeLines(c(
    "library(shiny)",
    "library(blockr.core)",
    "library(blockr.ts)",
    "blockr.core::serve(new_ts_dataset_block(dataset = 'AirPassengers'))"
  ), app_file)

  app <- shinytest2::AppDriver$new(app_file, wait = TRUE, timeout = 10000)
  Sys.sleep(3)  # Longer delay for DataTable

  output_path <- file.path(OUTPUT_DIR, "ts-dataset-block.png")
  app$get_screenshot(output_path, width = 800, height = 600)
  app$stop()

  unlink(temp_dir, recursive = TRUE)
  cat(sprintf("  ✓ Saved to %s\n", output_path))
  successes <- successes + 1
  TRUE
}, error = function(e) {
  cat(sprintf("  WARNING: Failed - %s\n", e$message))
  FALSE
})

# 3. Percentage Change Block (simple, no UI)
# Note: This block has no UI parameters, may not need screenshot
# Skipping to focus on blocks with visible UI

# 4. Change Block (comprehensive)
current <- current + 1
if (gen_screenshot(
  block = new_ts_change_block(method = "pcy"),
  filename = "ts-change-block.png",
  data = list(data = tsbox::ts_tbl(datasets::AirPassengers)),
  n = current,
  total = total_blocks
)) {
  successes <- successes + 1
}

# 5. Frequency Block
current <- current + 1
if (gen_screenshot(
  block = new_ts_frequency_block(to = "quarter", aggregate = "mean"),
  filename = "ts-frequency-block.png",
  data = list(data = tsbox::ts_tbl(datasets::AirPassengers)),
  delay = 3,  # Longer delay for frequency detection
  n = current,
  total = total_blocks
)) {
  successes <- successes + 1
}

# 6. Select Block (needs multivariate data)
current <- current + 1
if (gen_screenshot(
  block = new_ts_select_block(),
  filename = "ts-select-block.png",
  data = list(data = tsbox::ts_tbl(datasets::EuStockMarkets)),
  n = current,
  total = total_blocks
)) {
  successes <- successes + 1
}

# 7. Lag Block
current <- current + 1
if (gen_screenshot(
  block = new_ts_lag_block(by = 1),
  filename = "ts-lag-block.png",
  data = list(data = tsbox::ts_tbl(datasets::AirPassengers)),
  n = current,
  total = total_blocks
)) {
  successes <- successes + 1
}

# 8. Span Block
current <- current + 1
if (gen_screenshot(
  block = new_ts_span_block(),
  filename = "ts-span-block.png",
  data = list(data = tsbox::ts_tbl(datasets::AirPassengers)),
  delay = 3,  # Longer delay for slider to initialize
  n = current,
  total = total_blocks
)) {
  successes <- successes + 1
}

# Summary ---------------------------------------------------------------------

cat("\n=== Screenshot Generation Complete ===\n")
cat(sprintf("Successfully generated %d/%d screenshots\n", successes, total_blocks))
cat(sprintf("Screenshots saved to: %s/\n", OUTPUT_DIR))

if (successes < total_blocks) {
  cat("\nSome screenshots failed to generate. Review warnings above.\n")
  cat("Common issues:\n")
  cat("  - Block may need more time to load (increase delay parameter)\n")
  cat("  - Block may require specific data format\n")
  cat("  - shinytest2 or chromote may not be properly installed\n")
}

# Validate Registry -----------------------------------------------------------

cat("\n=== Validating Screenshot Registry ===\n")
cat("Run: source('dev/screenshots/validate_registry.R')\n")
