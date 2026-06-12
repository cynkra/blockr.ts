test_that("ts_scale_block - normalize method (z-score)", {

  block <- new_ts_scale_block(method = "normalize")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::AirPassengers)
      expected <- tsbox::ts_scale(original)

      # Test structure
      expect_true(is.data.frame(result))
      expect_true("time" %in% names(result))
      expect_true("value" %in% names(result))

      # Test dimensions
      expect_equal(nrow(result), nrow(expected))

      # Vectorized comparison
      expect_equal(result$time, expected$time)
      expect_equal(result$value, expected$value, tolerance = 0.001)

      # Verify normalization properties
      expect_equal(mean(result$value, na.rm = TRUE), 0, tolerance = 0.001)
      expect_equal(sd(result$value, na.rm = TRUE), 1, tolerance = 0.001)
    },
    args = list(
      x = block,
      data = list(data = function() tsbox::ts_tbl(datasets::AirPassengers))
    )
  )
})

test_that("ts_scale_block - index method without base", {

  block <- new_ts_scale_block(method = "index", base = NULL)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::AirPassengers)
      expected <- tsbox::ts_index(original)

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

test_that("ts_scale_block - index method with base date", {

  block <- new_ts_scale_block(method = "index", base = "1955-01-01")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::AirPassengers)
      expected <- tsbox::ts_index(original, base = "1955-01-01")

      # Test structure
      expect_true(is.data.frame(result))

      # Test dimensions
      expect_equal(nrow(result), nrow(expected))

      # Vectorized comparison
      expect_equal(result$time, expected$time)
      expect_equal(result$value, expected$value, tolerance = 0.001)

      # Verify index property - value at base date should be 1 (tsbox default)
      base_row <- result[result$time == as.Date("1955-01-01"), ]
      expect_equal(base_row$value, 1, tolerance = 0.001)
    },
    args = list(
      x = block,
      data = list(data = function() tsbox::ts_tbl(datasets::AirPassengers))
    )
  )
})

test_that("ts_scale_block - minmax method (0-1 scaling)", {

  block <- new_ts_scale_block(method = "minmax")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result (manual minmax calculation)
      original <- tsbox::ts_tbl(datasets::AirPassengers)
      min_val <- min(original$value, na.rm = TRUE)
      max_val <- max(original$value, na.rm = TRUE)
      expected_values <- (original$value - min_val) / (max_val - min_val)

      # Test structure
      expect_true(is.data.frame(result))
      expect_true("time" %in% names(result))
      expect_true("value" %in% names(result))

      # Test dimensions
      expect_equal(nrow(result), nrow(original))

      # Vectorized comparison
      expect_equal(result$time, original$time)
      expect_equal(result$value, expected_values, tolerance = 0.001)

      # Verify minmax properties
      expect_equal(min(result$value, na.rm = TRUE), 0, tolerance = 0.001)
      expect_equal(max(result$value, na.rm = TRUE), 1, tolerance = 0.001)
    },
    args = list(
      x = block,
      data = list(data = function() tsbox::ts_tbl(datasets::AirPassengers))
    )
  )
})

test_that("ts_scale_block - normalize works with multivariate data", {

  block <- new_ts_scale_block(method = "normalize")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::EuStockMarkets)
      expected <- tsbox::ts_scale(original)

      # Test structure - should have id column for multivariate
      expect_true(is.data.frame(result))
      expect_true("time" %in% names(result))
      expect_true("value" %in% names(result))
      expect_true("id" %in% names(result))

      # Should have 4 series
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

test_that("ts_scale_block - index works with multivariate data", {

  block <- new_ts_scale_block(method = "index", base = "1995-01-01")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::EuStockMarkets)
      expected <- tsbox::ts_index(original, base = "1995-01-01")

      # Test structure - should have id column for multivariate
      expect_true("id" %in% names(result))
      expect_equal(length(unique(result$id)), 4)

      # Test dimensions
      expect_equal(nrow(result), nrow(expected))

      # Vectorized comparison
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

test_that("ts_scale_block - different methods produce different results", {

  # Get expected results for comparison
  original <- tsbox::ts_tbl(datasets::AirPassengers)
  expected_normalize <- tsbox::ts_scale(original)
  expected_index <- tsbox::ts_index(original)

  # Verify normalize and index produce different values
  expect_false(isTRUE(all.equal(expected_normalize$value[10], expected_index$value[10])))

  # Test normalize method
  block_normalize <- new_ts_scale_block(method = "normalize")
  testServer(
    blockr.core:::get_s3_method("block_server", block_normalize),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(result$value, expected_normalize$value, tolerance = 0.001)
    },
    args = list(
      x = block_normalize,
      data = list(data = function() tsbox::ts_tbl(datasets::AirPassengers))
    )
  )

  # Test index method
  block_index <- new_ts_scale_block(method = "index")
  testServer(
    blockr.core:::get_s3_method("block_server", block_index),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(result$value, expected_index$value, tolerance = 0.001)
    },
    args = list(
      x = block_index,
      data = list(data = function() tsbox::ts_tbl(datasets::AirPassengers))
    )
  )
})

test_that("ts_scale_block - handles short time series", {

  block <- new_ts_scale_block(method = "normalize")

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
      expected <- tsbox::ts_scale(short_data)

      # Test dimensions
      expect_equal(nrow(result), 3)
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
test_that("ts_scale_block - method input updates state", {
  test_data <- reactive(tsbox::ts_tbl(datasets::AirPassengers))
  block <- new_ts_scale_block(method = "normalize")

  testServer(
    block$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))
      expect_true(is.list(result$state))

      # Set initial input
      session$setInputs(method = "normalize")
      session$flushReact()

      # Check state
      expect_equal(result$state$method(), "normalize")

      # Change to index
      session$setInputs(method = "index")
      session$flushReact()
      expect_equal(result$state$method(), "index")

      # Change to minmax
      session$setInputs(method = "minmax")
      session$flushReact()
      expect_equal(result$state$method(), "minmax")
    }
  )
})

test_that("ts_scale_block - method input updates expression", {
  test_data <- reactive(tsbox::ts_tbl(datasets::AirPassengers))
  block <- new_ts_scale_block(method = "normalize")

  testServer(
    block$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned

      # Set normalize method
      session$setInputs(method = "normalize")
      session$flushReact()

      # Check expression contains ts_scale
      expr_result <- result$expr()
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("ts_scale", expr_text)))

      # Change to index
      session$setInputs(method = "index")
      session$flushReact()

      # Expression should now contain ts_index
      expr_result <- result$expr()
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("ts_index", expr_text)))

      # Change to minmax - uses custom calculation
      session$setInputs(method = "minmax")
      session$flushReact()

      expr_result <- result$expr()
      expr_text <- deparse(expr_result)
      # Minmax uses min/max calculation
      expect_true(any(grepl("min", expr_text)) || any(grepl("max", expr_text)))
    }
  )
})
