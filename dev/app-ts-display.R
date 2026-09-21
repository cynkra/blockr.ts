# The time series display, and the one block that exists because tsbox
# sometimes cannot tell.
#
#   Rscript blockr.ts/dev/app-ts-display.R
#
# Read the tabs left to right; each is a different answer to "is this a time
# series", and only one of them needs a block.
#
#   Orders        customer / order_date / amount. Not a time series in
#                 anybody's data model, and a time series in tsbox's, so it
#                 draws. On a board where you asked for this display, that is
#                 what you want.
#   Stocks        an ordinary long frame. Draws.
#   Pick two      a plain `blockr.dplyr::new_filter_block()` on Stocks. Still
#                 draws, and blockr.dplyr has never heard of this package.
#   Sales wide    month / sales / costs, with month held as the STRING
#                 "2021-01". Refused twice over: two numeric columns, so tsbox
#                 will not guess which is the value, and a time column it
#                 cannot recognise. Renders as a table.
#   Pick cols     `blockr.dplyr::new_select_block()`, month and sales. Still a
#                 table: the time column is still a string.
#   Renamed       `blockr.dplyr::new_rename_block()`, month -> time and
#                 sales -> value. Draws.
#
#                 THIS PACKAGE HAS NO BLOCK FOR THIS AND SHOULD NOT. Two dplyr
#                 blocks are the whole fix, and both are needed: the select
#                 drops the second numeric column, which is what stops tsbox
#                 guessing, and the rename is what makes it parse "2021-01"
#                 into a date. Renaming alone leaves `costs` behind and tsbox
#                 hands the frame back unresolved.
#   Not a series  mtcars, through the app's own preview, untouched.
#
# blockr.ui is loaded on purpose: it sets the HTML table preview that real apps
# use, and the point is that ts_display hands everything non-series to it
# rather than replacing it.
for (p in c("blockr.core", "blockr.ui", "blockr.dag", "blockr.dock",
            "blockr.dplyr", "blockr.ts")) {
  pkgload::load_all(file.path("/workspace", p), quiet = TRUE)
}

options(
  blockr.dock_is_locked = FALSE,
  blockr.tabular_display = blockr.ts::ts_display
)

set.seed(1)
months <- seq(as.Date("2021-01-01"), by = "month", length.out = 60)

orders <- data.frame(
  customer = rep(c("north", "south"), each = 60),
  order_date = rep(months, 2),
  amount = round(runif(120, 100, 900))
)

# Two numeric columns AND a text time column, which is what a CSV of monthly
# figures actually looks like before anybody has touched it.
sales_wide <- data.frame(
  month = format(months, "%Y-%m"),
  sales = round(runif(60, 100, 200)),
  costs = round(runif(60, 50, 90))
)

board <- blockr.dock::new_dock_board(
  blocks = c(
    orders = blockr.core::new_static_block(orders, block_name = "Orders"),
    stocks = blockr.core::new_static_block(
      as.data.frame(tsbox::ts_tbl(datasets::EuStockMarkets)),
      block_name = "Stocks"
    ),
    pick = blockr.dplyr::new_filter_block(
      conditions = list(
        list(type = "values", column = "id",
             values = list("DAX", "SMI"), mode = "include")
      ),
      block_name = "Pick two"
    ),
    wide = blockr.core::new_static_block(
      sales_wide, block_name = "Sales wide"
    ),
    cols = blockr.dplyr::new_select_block(
      columns = list("month", "sales"), block_name = "Pick cols"
    ),
    renamed = blockr.dplyr::new_rename_block(
      renames = list(time = "month", value = "sales"),
      block_name = "Renamed"
    ),
    flat = blockr.core::new_static_block(
      datasets::mtcars, block_name = "Not a series"
    )
  ),
  links = blockr.core::links(
    from = c("stocks", "wide", "cols"),
    to = c("pick", "cols", "renamed")
  ),
  extensions = list(blockr.dag::new_dag_extension())
)

port <- if (exists("blockr_port")) blockr_port() else 3838L
message("http://localhost:", port)
shiny::runApp(serve(board, "ts_display"),
              port = port, host = "0.0.0.0", launch.browser = FALSE)
