
library(shiny)
library(blockr.core)
devtools::load_all()

# Create the block object first
block <- new_ts_dataset_block(dataset = "AirPassengers")

ui <- fluidPage(
  titlePanel("Time Series Dataset Block"),
  block_ui(block, "test")
)

server <- function(input, output, session) {
  block_server(block, "test", list())
}

shinyApp(ui, server)

