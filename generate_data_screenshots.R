# Generate screenshots for data blocks only

devtools::load_all()
library(blockr.ggplot)

# Create output directory if it doesn't exist
if (!dir.exists("man/figures")) {
  dir.create("man/figures", recursive = TRUE)
}

# AirPassengers block
message("Generating AirPassengers block screenshot...")
result1 <- blockr.ggplot::validate_block_screenshot(
  block = new_ts_airpassenger_block(),
  data = NULL,  # Data blocks don't need data input
  filename = "ts_airpassenger_block.png",
  output_dir = "man/figures"
)
print(result1)

# Dataset block with EuStockMarkets
message("Generating Dataset block screenshot...")
result2 <- blockr.ggplot::validate_block_screenshot(
  block = new_ts_dataset_block(dataset = "EuStockMarkets"),
  data = NULL,  # Data blocks don't need data input
  filename = "ts_dataset_block.png",
  output_dir = "man/figures"
)
print(result2)

message("Data block screenshots complete!")