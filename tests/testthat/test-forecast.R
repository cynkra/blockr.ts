deaths <- function() {
  tsbox::ts_tbl(tsbox::ts_c(m = datasets::mdeaths, f = datasets::fdeaths))
}

test_that("the result is always a named list, whatever the target count", {
  one <- ts_forecast(deaths(), target = "m", h = 4)
  two <- ts_forecast(deaths(), h = 4)

  expect_s3_class(one, "ts_forecast")
  expect_s3_class(two, "ts_forecast")
  # The point of there being no `_multi` twin: one target and two give the
  # same shape, so a board downstream does not break when the multiselect
  # is touched.
  expect_type(one, "list")
  expect_named(one, "m")
  expect_named(two, c("m", "f"))
})

test_that("an empty target means every series", {
  expect_named(ts_forecast(deaths(), h = 4), c("m", "f"))
})

test_that("each engine fits and reports what it chose", {
  for (eng in c("ets", "arima", "theta")) {
    f <- ts_forecast(deaths(), target = "m", engine = eng, h = 4)
    expect_s3_class(f, "ts_forecast")
    expect_true(nzchar(fc_method(f[[1]])), info = eng)
  }
})

test_that("VAR fits the targets as one system and refuses a single one", {
  skip_if_not_installed("vars")
  f <- ts_forecast(deaths(), engine = "var", h = 4)
  expect_named(f, c("m", "f"))
  # One model, two entries pointing at it.
  expect_identical(f[[1]]$model, f[[2]]$model)
  expect_error(
    ts_forecast(deaths(), target = "m", engine = "var"),
    "at least two targets"
  )
})

test_that("an engine that takes no regressors says so", {
  expect_error(
    ts_forecast(deaths(), target = "m", xreg = "f", engine = "ets"),
    "takes no regressors"
  )
})

test_that("a lag shorter than the horizon cannot cover it", {
  expect_error(
    ts_forecast(deaths(), target = "m", xreg = "f", engine = "arima",
                h = 6, lag = 3),
    "do not cover the horizon"
  )
})

test_that("regressors reach the horizon by lag and by hold", {
  for (mode in c("lag", "hold")) {
    f <- ts_forecast(deaths(), target = "m", xreg = "f", engine = "arima",
                     h = 6, xreg_mode = mode)
    expect_s3_class(f, "ts_forecast")
  }
})

test_that("a target that is not in the data is named", {
  expect_error(ts_forecast(deaths(), target = "nope"), "None of the targets")
})
