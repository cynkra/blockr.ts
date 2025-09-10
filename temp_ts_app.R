
library(shiny)
library(blockr.ts)
library(blockr.core)

devtools::load_all()

block <- new_ts_block()

ui <- fluidPage(
  h2("TS Block with Dygraph"),
  blockr.core:::block_ui(block, "test")
)

server <- function(input, output, session) {
  blockr.core:::block_server(block, "test", list())
}

shinyApp(ui, server)

