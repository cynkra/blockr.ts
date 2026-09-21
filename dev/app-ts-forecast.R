# The two forecast blocks.
#
#   Rscript blockr.ts/dev/app-ts-forecast.R
#
# Five tabs:
#
#   Deaths      the two-series source, so VAR has a system to fit.
#   Forecast    new_ts_forecast_block(). Returns the fitted MODELS, and the
#               preview draws them: history, point path, and the 80 and 95
#               bands the chart pairs out of the `_lower_` / `_upper_` ids.
#               The hamburger swaps to the model output.
#   VAR         the same source fitted jointly, to see the multiselect mean
#               something different.
#   Series      new_ts_forecast_series_block() on the first one. This is the
#               frame, and where the interval naming convention actually
#               lives.
#   Accuracy    the same extractor asking for the one output that is not a
#               time series.
for (p in c("blockr.core", "blockr.ui", "blockr.dag", "blockr.dock",
            "blockr.dplyr", "blockr.ts")) {
  pkgload::load_all(file.path("/workspace", p), quiet = TRUE)
}

options(
  blockr.dock_is_locked = FALSE,
  blockr.tabular_display = blockr.ts::ts_display
)

deaths <- as.data.frame(tsbox::ts_tbl(tsbox::ts_c(
  datasets::mdeaths, datasets::fdeaths
)))

board <- blockr.dock::new_dock_board(
  blocks = c(
    source = blockr.core::new_static_block(deaths, block_name = "Deaths"),
    fc = blockr.ts::new_ts_forecast_block(
      target = "mdeaths", engine = "ets", h = 12L, block_name = "Forecast"
    ),
    var = blockr.ts::new_ts_forecast_block(
      engine = "var", h = 12L, block_name = "VAR"
    ),
    series = blockr.ts::new_ts_forecast_series_block(block_name = "Series"),
    acc = blockr.ts::new_ts_forecast_series_block(
      what = "accuracy", block_name = "Accuracy"
    )
  ),
  links = blockr.core::links(
    from = c("source", "source", "fc", "var"),
    to = c("fc", "var", "series", "acc")
  ),
  extensions = list(blockr.dag::new_dag_extension())
)

port <- if (exists("blockr_port")) blockr_port() else 3838L
message("http://localhost:", port)
shiny::runApp(serve(board, "ts_forecast"),
              port = port, host = "0.0.0.0", launch.browser = FALSE)
