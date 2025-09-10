library(blockr.ts)
library(blockr.core)

# Create and serve the simplified ts_block
cat("Creating simplified ts_block...\n")
block <- new_ts_block()

cat("Starting blockr server...\n")
cat("The block should display:\n")
cat("1. AirPassengers info in the UI\n")
cat("2. An interactive dygraph visualization (NOT a data table)\n")
cat("3. Blue line chart with range selector at bottom\n")
cat("\n")

blockr.core::serve(block)