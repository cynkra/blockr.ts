deaths <- function() {
  tsbox::ts_tbl(tsbox::ts_c(m = datasets::mdeaths, f = datasets::fdeaths))
}

fit <- function(...) ts_forecast(deaths(), target = "m", h = 4, ...)

test_that("an interval becomes series, because a frame has no column for one", {
  d <- ts_forecast_frame(fit())
  expect_setequal(
    unique(d$id),
    c("m", "m_lower_80", "m_upper_80", "m_lower_95", "m_upper_95")
  )
  expect_named(d, c("id", "time", "value"))
})

test_that("the bounds are anchored at the last observation when history is kept", {
  f <- fit()
  d <- ts_forecast_frame(f)
  last_obs <- max(d$time[d$id == "m"]) # the end of the forecast
  hist_end <- tsbox::ts_tbl(f[["m"]]$y)
  anchor <- max(hist_end$time)

  lo <- d[d$id == "m_lower_80", ]
  # Anchored: the bound starts AT the last observation rather than one
  # period after it, so the band has no gap before it begins.
  expect_equal(min(lo$time), anchor)
  expect_equal(
    lo$value[which.min(lo$time)],
    hist_end$value[which.max(hist_end$time)]
  )
  expect_true(max(d$time) > last_obs - 1)
})

test_that("dropping the history drops the anchor with it", {
  d <- ts_forecast_frame(fit(), keep_history = FALSE)
  expect_equal(nrow(d[d$id == "m", ]), 4L)
  expect_equal(nrow(d[d$id == "m_lower_80", ]), 4L)
})

test_that("levels can be narrowed to the ones that were fitted", {
  d <- ts_forecast_frame(fit(), levels = 95)
  expect_setequal(unique(d$id), c("m", "m_lower_95", "m_upper_95"))
})

test_that("the other extractions are plain one-series frames", {
  f <- fit()
  for (what in c("fitted", "residuals")) {
    d <- ts_forecast_frame(f, what)
    expect_setequal(unique(d$id), "m")
  }
  comp <- ts_forecast_frame(f, "components")
  expect_true(all(grepl("^m · ", unique(comp$id))))
})

test_that("components are refused by an engine that has none", {
  expect_error(
    ts_forecast_frame(fit(engine = "arima"), "components"),
    "no component paths"
  )
})

test_that("accuracy is one row per series and is not a time series", {
  f <- ts_forecast(deaths(), h = 4)
  a <- ts_forecast_frame(f, "accuracy")
  expect_equal(nrow(a), 2L)
  expect_true(all(c("series", "model", "RMSE") %in% names(a)))
  # Deliberately not a series: there is nothing to draw.
  expect_null(ts_frame(a))
})

test_that("the preview resolves a model through the ts_resolve seam", {
  f <- fit()
  # ts_frame() has no branch for forecasts; the method is what makes this
  # work, so a forecast charts wherever it lands.
  expect_equal(ts_resolve(f), ts_forecast_frame(f))
  expect_equal(nrow(ts_frame(f)), nrow(ts_forecast_frame(f)))
})

test_that("the block writes a call that omits its defaults", {
  expect_equal(
    fc_call(character(), "ets", 12L, c(80, 95), character(), "lag", numeric()),
    quote(blockr.ts::ts_forecast(data))
  )
  expect_equal(
    fc_call("m", "arima", 24L, c(80, 95), "f", "hold", 6),
    quote(blockr.ts::ts_forecast(data, target = "m", engine = "arima",
                                 h = 24L, xreg = "f", xreg_mode = "hold",
                                 lag = 6))
  )
})

test_that("the extractor writes a call that omits its defaults", {
  expect_equal(
    fc_series_call("forecast", numeric(), TRUE),
    quote(blockr.ts::ts_forecast_frame(data))
  )
  expect_equal(
    fc_series_call("accuracy", 95, FALSE),
    quote(blockr.ts::ts_forecast_frame(data, what = "accuracy", levels = 95,
                                       keep_history = FALSE))
  )
})

test_that("the extractor refuses a frame and says what it wants", {
  expect_error(fc_validate_model(deaths()), "fitted models, not a frame")
  expect_true(fc_validate_model(fit()))
})

test_that("ets gets the tidiers broom does not ship", {
  m <- fit()[["m"]]$model
  t <- tidy.ets(m)
  g <- glance.ets(m)
  expect_named(t, c("term", "estimate"))
  expect_true("alpha" %in% t$term)
  # No standard error column: ets() carries no covariance matrix, and an
  # absent column says that where a column of NAs would not.
  expect_false("std.error" %in% names(t))
  expect_named(g, c("sigma", "logLik", "AIC", "AICc", "BIC", "nobs"))
  expect_equal(g$nobs, length(m$x))
})
