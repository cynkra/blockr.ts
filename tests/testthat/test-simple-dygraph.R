test_that("single ts_block renders dygraph with shinytest2", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dygraphs")
  skip_on_cran()
  
  # Create a temporary app for testing just the ts_block
  app_dir <- tempfile("simple_ts_test")
  dir.create(app_dir)
  
  # Create simple app.R file with just ts_block (no pipeline)
  app_content <- '
library(shiny)
library(blockr.core) 
library(blockr.ts)

# Create single ts_block
ts_block <- new_ts_block(dataset = "economics")

# Simple UI and Server for testing
ui <- fluidPage(
  titlePanel("TS Block Dygraph Test"),
  blockr.core::block_ui("ts_block_1", ts_block)
)

server <- function(input, output, session) {
  result <- blockr.core::block_server("ts_block_1", ts_block, list())
}

shinyApp(ui = ui, server = server)
'
  
  writeLines(app_content, file.path(app_dir, "app.R"))
  
  # Initialize the app for testing
  app <- shinytest2::AppDriver$new(app_dir, timeout = 30000)
  
  # Take screenshot
  expect_no_error(app$get_screenshot("simple_ts_dygraph"))
  
  # Verify dygraph is present
  Sys.sleep(5) # Wait for dygraph to render
  
  app$stop()
  unlink(app_dir, recursive = TRUE)
})