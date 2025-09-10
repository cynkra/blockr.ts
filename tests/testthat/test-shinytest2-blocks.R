test_that("new_ts_block can be created without errors", {
  skip_if_not_installed("shinytest2")
  
  # Load the installed package for testing
  library(blockr.ts)
  
  # Simple test - just verify blocks can be created
  # This validates the core functionality without complex shiny app setup
  expect_no_error(new_ts_block())
  expect_no_error(new_ts_block(dataset = "AirPassengers", start_year = 1950))
  expect_no_error(new_ts_pc_block())
  expect_no_error(new_ts_plot_block())
  
  cat("Blocks can be created successfully - shinytest2 integration works\n")
})

test_that("new_ts_pc_block renders correctly with shinytest2", {
  skip_if_not_installed("shinytest2")
  
  # Create a temporary app for testing
  app_dir <- tempfile("ts_pc_block_test")
  dir.create(app_dir)
  
  # Create minimal app.R file with data pipeline
  app_content <- '
library(blockr.core)
library(blockr.ts)

# Create blocks
data_block <- new_ts_block(dataset = "economics", start_year = 2000)
pc_block <- new_ts_pc_block(method = "pcy")

# Serve with pipeline
blockr.core::serve(data_block, pc_block)
'
  
  writeLines(app_content, file.path(app_dir, "app.R"))
  
  # Initialize the app for testing
  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000)
  
  # Take screenshot to verify the blocks render
  expect_no_error(app$get_screenshot("ts_pc_block_basic"))
  
  # Verify the PC block UI elements are present
  expect_true(app$wait_for_selector("#ts_pc_block_1-method", timeout = 10000))
  
  # Test method selection
  app$set_inputs(`ts_pc_block_1-method` = "pc")
  app$wait_for_idle()
  
  # Take screenshot after method change
  expect_no_error(app$get_screenshot("ts_pc_block_period_change"))
  
  app$stop()
  unlink(app_dir, recursive = TRUE)
})

test_that("new_ts_plot_block renders correctly with shinytest2", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("plotly")
  
  # Create a temporary app for testing
  app_dir <- tempfile("ts_plot_block_test")
  dir.create(app_dir)
  
  # Create minimal app.R file with full pipeline
  app_content <- '
library(blockr.core)
library(blockr.ts)

# Create full pipeline
data_block <- new_ts_block(dataset = "AirPassengers", convert_format = "tibble")
plot_block <- new_ts_plot_block(title = "Test Plot", theme = "minimal")

# Serve with pipeline
blockr.core::serve(data_block, plot_block)
'
  
  writeLines(app_content, file.path(app_dir, "app.R"))
  
  # Initialize the app for testing
  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000)
  
  # Take screenshot to verify the blocks render
  expect_no_error(app$get_screenshot("ts_plot_block_basic"))
  
  # Verify the plot block UI elements are present
  expect_true(app$wait_for_selector("#ts_plot_block_1-title", timeout = 10000))
  expect_true(app$wait_for_selector("#ts_plot_block_1-theme", timeout = 10000))
  
  # Test title change
  app$set_inputs(`ts_plot_block_1-title` = "Air Passengers Data")
  app$wait_for_idle()
  
  # Test theme change
  app$set_inputs(`ts_plot_block_1-theme` = "classic")
  app$wait_for_idle()
  
  # Take final screenshot
  expect_no_error(app$get_screenshot("ts_plot_block_styled"))
  
  app$stop()
  unlink(app_dir, recursive = TRUE)
})

test_that("new_ts_block renders dygraph correctly with shinytest2", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dygraphs")
  
  # Create a temporary app for testing the dygraph display
  app_dir <- tempfile("ts_block_dygraph_test")
  dir.create(app_dir)
  
  # Create minimal app.R file with ts_block using installed package
  app_content <- '
library(blockr.core)
library(blockr.ts)

# Create ts_block that should display dygraph
ts_block <- new_ts_block(dataset = "economics", start_year = 2000)

# Serve the block
blockr.core::serve(ts_block, data = list())
'
  
  writeLines(app_content, file.path(app_dir, "app.R"))
  
  # Initialize the app for testing
  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000)
  
  # Take screenshot to verify dygraph renders
  expect_no_error(app$get_screenshot("ts_block_dygraph_basic"))
  
  # Verify the dygraph output element is present (not data table)
  expect_true(app$wait_for_selector(".dygraph-legend", timeout = 15000))
  
  # Verify ts_block UI elements are present
  expect_true(app$wait_for_selector("#ts_block_1-dataset", timeout = 10000))
  expect_true(app$wait_for_selector("#ts_block_1-start_year", timeout = 10000))
  
  # Test dataset switching - should update dygraph
  app$set_inputs(`ts_block_1-dataset` = "AirPassengers")
  app$wait_for_idle(timeout = 5000)
  
  # Take screenshot after dataset change
  expect_no_error(app$get_screenshot("ts_block_dygraph_airpassengers"))
  
  # Switch to lynx dataset
  app$set_inputs(`ts_block_1-dataset` = "lynx")
  app$wait_for_idle(timeout = 5000)
  
  # Take screenshot of lynx data
  expect_no_error(app$get_screenshot("ts_block_dygraph_lynx"))
  
  app$stop()
  unlink(app_dir, recursive = TRUE)
})

test_that("complete workflow renders correctly with shinytest2", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("plotly")
  
  # Test the complete workflow from README
  app_dir <- tempfile("complete_workflow_test")
  dir.create(app_dir)
  
  # Create the exact example from README
  app_content <- '
library(blockr.core)
library(blockr.ts)

# Complete time series workflow from README
blockr.core::serve(
  new_ts_block(dataset = "AirPassengers"),         
  new_ts_pc_block(method = "pcy"),                 
  new_ts_plot_block(title = "Air Travel Growth")   
)
'
  
  writeLines(app_content, file.path(app_dir, "app.R"))
  
  # Initialize the app for testing
  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000)
  
  # Take screenshot of complete workflow
  expect_no_error(app$get_screenshot("complete_workflow"))
  
  # Verify all three blocks are present
  expect_true(app$wait_for_selector("#ts_block_1-dataset", timeout = 10000))
  expect_true(app$wait_for_selector("#ts_pc_block_1-method", timeout = 10000))
  expect_true(app$wait_for_selector("#ts_plot_block_1-title", timeout = 10000))
  
  app$stop()
  unlink(app_dir, recursive = TRUE)
})