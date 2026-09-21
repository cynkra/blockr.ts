# The three blocks.
#
#   Rscript blockr.ts/dev/app-ts-blocks.R
#
# Four tabs, one per registered block plus the many-series case:
#
#   Dataset     new_ts_dataset_block(). Every `ts` object in datasets, which
#               blockr.core's own dataset block cannot offer because it
#               filters its list to data frames.
#   Transform   new_ts_transform_block(). Sixteen unary tsbox verbs behind one
#               tile grid; the arguments under the grid are built from
#               ts_summary() of whatever arrived.
#   Combine     new_ts_combine_block(), variadic, fed two spans of the same
#               series so Append and Chain-link have something to do.
#   Pick        new_ts_pick_block() on the Census MARTS file if it has been
#               cached (168 series), otherwise on the built-in deaths series.
for (p in c("blockr.core", "blockr.ui", "blockr.dag", "blockr.dock",
            "blockr.dplyr", "blockr.ts")) {
  pkgload::load_all(file.path("/workspace", p), quiet = TRUE)
}

options(
  blockr.dock_is_locked = FALSE,
  blockr.tabular_display = blockr.ts::ts_display
)

ap <- tsbox::ts_tbl(datasets::AirPassengers)

# The many-series case. `_scratch/ts-charts/gen-picker.R` writes this cache;
# without it the picker still has something to show, just less of it.
marts_path <- "/workspace/_scratch/ts-charts/marts.rds"
many <- if (file.exists(marts_path)) {
  raw <- readRDS(marts_path)
  err <- c("1" = "CV of sales", "2" = "SE of percent change")
  dt <- c("1" = "Sales", "2" = "Sales, percent change")
  type <- ifelse(raw$dt_idx == 0, err[as.character(raw$et_idx)],
                 dt[as.character(raw$dt_idx)])
  out <- data.frame(
    id = paste0(sub("^[^:]+: ", "", raw$cat_desc), " · ", type, " · ",
                ifelse(raw$is_adj == 1, "SA", "NSA")),
    time = raw$time,
    value = raw$val
  )
  out <- out[!is.na(out$value), ]
  out[order(out$id, out$time), ]
} else {
  as.data.frame(tsbox::ts_tbl(tsbox::ts_c(
    datasets::mdeaths, datasets::fdeaths, datasets::ldeaths,
    datasets::UKDriverDeaths
  )))
}
message("picker source: ", length(unique(many$id)), " series")

board <- blockr.dock::new_dock_board(
  blocks = c(
    dataset = blockr.ts::new_ts_dataset_block(
      dataset = "AirPassengers", block_name = "Dataset"
    ),
    transform = blockr.ts::new_ts_transform_block(
      verb = "pcy", block_name = "Transform"
    ),
    old = blockr.ts::new_ts_transform_block(
      verb = "span", end = "1955-12-01", block_name = "Old vintage"
    ),
    new = blockr.ts::new_ts_transform_block(
      verb = "span", start = "1956-01-01", block_name = "New vintage"
    ),
    combine = blockr.ts::new_ts_combine_block(
      method = "bind", result = "Air passengers", block_name = "Combine"
    ),
    source = blockr.core::new_static_block(many, block_name = "Many series"),
    pick = blockr.ts::new_ts_pick_block(block_name = "Pick")
  ),
  links = blockr.core::links(
    from = c("dataset", "dataset", "dataset", "old", "new", "source"),
    to = c("transform", "old", "new", "combine", "combine", "pick")
  ),
  extensions = list(blockr.dag::new_dag_extension())
)

port <- if (exists("blockr_port")) blockr_port() else 3838L
message("http://localhost:", port)
shiny::runApp(serve(board, "ts_blocks"),
              port = port, host = "0.0.0.0", launch.browser = FALSE)
