test_that("ts_lag_block - default lag of 1 period", {

  block <- new_ts_lag_block()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::AirPassengers)
      expected <- tsbox::ts_lag(original, by = 1L)

      # Test structure
      expect_true(is.data.frame(result))
      expect_true("time" %in% names(result))
      expect_true("value" %in% names(result))

      # Test dimensions
      expect_equal(nrow(result), nrow(expected))

      # Vectorized comparison
      expect_equal(result$time, expected$time)
      expect_equal(result$value, expected$value, tolerance = 0.001)
    },
    args = list(
      x = block,
      data = list(data = function() tsbox::ts_tbl(datasets::AirPassengers))
    )
  )
})

test_that("ts_lag_block - lag by 3 periods", {

  block <- new_ts_lag_block(by = 3L)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::AirPassengers)
      expected <- tsbox::ts_lag(original, by = 3L)

      # Test dimensions
      expect_equal(nrow(result), nrow(expected))

      # Vectorized comparison
      expect_equal(result$time, expected$time)
      expect_equal(result$value, expected$value, tolerance = 0.001)
    },
    args = list(
      x = block,
      data = list(data = function() tsbox::ts_tbl(datasets::AirPassengers))
    )
  )
})

test_that("ts_lag_block - lead by 1 period (negative lag)", {

  block <- new_ts_lag_block(by = -1L)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::AirPassengers)
      expected <- tsbox::ts_lag(original, by = -1L)

      # Test dimensions
      expect_equal(nrow(result), nrow(expected))

      # Vectorized comparison
      expect_equal(result$time, expected$time)
      expect_equal(result$value, expected$value, tolerance = 0.001)
    },
    args = list(
      x = block,
      data = list(data = function() tsbox::ts_tbl(datasets::AirPassengers))
    )
  )
})

test_that("ts_lag_block - lead by 3 periods (negative lag)", {

  block <- new_ts_lag_block(by = -3L)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::AirPassengers)
      expected <- tsbox::ts_lag(original, by = -3L)

      # Test dimensions
      expect_equal(nrow(result), nrow(expected))

      # Vectorized comparison
      expect_equal(result$time, expected$time)
      expect_equal(result$value, expected$value, tolerance = 0.001)
    },
    args = list(
      x = block,
      data = list(data = function() tsbox::ts_tbl(datasets::AirPassengers))
    )
  )
})

test_that("ts_lag_block - zero lag (no shift)", {

  block <- new_ts_lag_block(by = 0L)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::AirPassengers)
      expected <- tsbox::ts_lag(original, by = 0L)

      # Test dimensions - should be same as original
      expect_equal(nrow(result), nrow(expected))
      expect_equal(nrow(result), nrow(original))

      # Vectorized comparison
      expect_equal(result$time, expected$time)
      expect_equal(result$value, expected$value, tolerance = 0.001)
    },
    args = list(
      x = block,
      data = list(data = function() tsbox::ts_tbl(datasets::AirPassengers))
    )
  )
})

test_that("ts_lag_block - works with multivariate data", {

  block <- new_ts_lag_block(by = 2L)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::EuStockMarkets)
      expected <- tsbox::ts_lag(original, by = 2L)

      # Test structure - should have id column for multivariate
      expect_true("id" %in% names(result))
      expect_equal(length(unique(result$id)), 4)

      # Test dimensions
      expect_equal(nrow(result), nrow(expected))

      # Vectorized comparison - tests all series
      expect_equal(result$time, expected$time)
      expect_equal(result$id, expected$id)
      expect_equal(result$value, expected$value, tolerance = 0.001)
    },
    args = list(
      x = block,
      data = list(data = function() tsbox::ts_tbl(datasets::EuStockMarkets))
    )
  )
})

test_that("ts_lag_block - handles short time series", {

  block <- new_ts_lag_block(by = 1L)

  # Create short time series
  short_data <- data.frame(
    time = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    value = c(100, 110, 121)
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      expected <- tsbox::ts_lag(short_data, by = 1L)

      # Test dimensions
      expect_equal(nrow(result), nrow(expected))

      # Vectorized comparison
      expect_equal(result$time, expected$time)
      expect_equal(result$value, expected$value, tolerance = 0.001)
    },
    args = list(
      x = block,
      data = list(data = function() short_data)
    )
  )
})

# UI Input Tests - test that setInputs changes state and expression
test_that("ts_lag_block - by input updates state", {
  test_data <- reactive(tsbox::ts_tbl(datasets::AirPassengers))
  block <- new_ts_lag_block(by = 1L)

  testServer(
    block$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))
      expect_true(is.list(result$state))

      # Set initial input
      session$setInputs(by = 1)
      session$flushReact()

      # Check state
      expect_equal(result$state$by(), 1L)

      # Change to 3
      session$setInputs(by = 3)
      session$flushReact()
      expect_equal(result$state$by(), 3L)

      # Change to negative (lead)
      session$setInputs(by = -2)
      session$flushReact()
      expect_equal(result$state$by(), -2L)

      # Change to 0
      session$setInputs(by = 0)
      session$flushReact()
      expect_equal(result$state$by(), 0L)
    }
  )
})

test_that("ts_lag_block - by input updates expression", {
  test_data <- reactive(tsbox::ts_tbl(datasets::AirPassengers))
  block <- new_ts_lag_block(by = 1L)

  testServer(
    block$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned

      # Set by = 1
      session$setInputs(by = 1)
      session$flushReact()

      # Check expression
      expr_result <- result$expr()
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("by = 1L", expr_text)))

      # Change to by = 5
      session$setInputs(by = 5)
      session$flushReact()

      # Expression should update
      expr_result <- result$expr()
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("by = 5L", expr_text)))

      # Change to negative
      session$setInputs(by = -3)
      session$flushReact()

      expr_result <- result$expr()
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("by = -3L", expr_text)))
    }
  )
})
