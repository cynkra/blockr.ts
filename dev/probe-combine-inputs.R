# Two inputs into Combine, one of each kind:
#
#   "No ids"   a frame with `time, value`  -> gets a name field, placeholder
#              `.arg1`, and that ref is what the expression falls back to.
#   "Two ids"  a frame with an `id` column -> NO field. tsbox keeps the ids
#              and drops the argument name, so the row shows the ids instead.
#
#   Rscript blockr.ts/dev/probe-combine-inputs.R
for (p in c("blockr.core", "blockr.ui", "blockr.dock", "blockr.dplyr",
            "blockr.ts")) {
  pkgload::load_all(file.path("/workspace", p), quiet = TRUE)
}
options(blockr.tabular_display = blockr.ts::ts_display)

ap <- as.data.frame(tsbox::ts_tbl(datasets::AirPassengers))
two <- as.data.frame(tsbox::ts_tbl(tsbox::ts_c(
  mdeaths = datasets::mdeaths, fdeaths = datasets::fdeaths
)))

board <- blockr.core::new_board(
  blocks = blockr.core::blocks(
    a = blockr.core::new_static_block(ap, block_name = "No ids"),
    b = blockr.core::new_static_block(two, block_name = "Two ids"),
    combine = blockr.ts::new_ts_combine_block(method = "c",
                                              block_name = "Combine")
  ),
  links = blockr.core::links(from = c("a", "b"), to = c("combine", "combine"))
)

port <- if (exists("blockr_port")) blockr_port() else 3838L
message("http://localhost:", port)
shiny::runApp(blockr.core::serve(board), port = port, host = "0.0.0.0",
              launch.browser = FALSE)
