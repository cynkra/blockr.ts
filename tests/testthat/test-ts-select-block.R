test_that("ts_select_block - select single series from multivariate data", {

  block <- new_ts_select_block(series = c("DAX"), multiple = FALSE)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::EuStockMarkets)
      expected <- tsbox::ts_pick(original, "DAX")

      # Test structure
      expect_true(is.data.frame(result))
      expect_true("time" %in% names(result))
      expect_true("value" %in% names(result))
      expect_true("id" %in% names(result))

      # Should have only 1 series
      expect_equal(length(unique(result$id)), 1)
      expect_equal(unique(result$id), "DAX")

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

test_that("ts_select_block - select multiple series", {

  block <- new_ts_select_block(series = c("DAX", "FTSE"), multiple = TRUE)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::EuStockMarkets)
      expected <- tsbox::ts_pick(original, c("DAX", "FTSE"))

      # Should have 2 series
      expect_equal(length(unique(result$id)), 2)
      expect_setequal(unique(result$id), c("DAX", "FTSE"))

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

test_that("ts_select_block - select all series when series=NULL", {

  block <- new_ts_select_block(series = NULL, multiple = TRUE)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result - should have all 4 series
      original <- tsbox::ts_tbl(datasets::EuStockMarkets)

      # Should have all 4 series
      expect_equal(length(unique(result$id)), 4)
      expect_setequal(unique(result$id), c("DAX", "SMI", "CAC", "FTSE"))

      # Test dimensions - should be same as original
      expect_equal(nrow(result), nrow(original))

      # Vectorized comparison
      expect_equal(result$time, original$time)
      expect_equal(result$id, original$id)
      expect_equal(result$value, original$value, tolerance = 0.001)
    },
    args = list(
      x = block,
      data = list(data = function() tsbox::ts_tbl(datasets::EuStockMarkets))
    )
  )
})

# UI Input Tests - test that setInputs changes state
test_that("ts_select_block - series input updates state (single mode)", {
  test_data <- reactive(tsbox::ts_tbl(datasets::EuStockMarkets))
  block <- new_ts_select_block(series = "DAX", multiple = FALSE)

  testServer(
    block$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))
      expect_true(is.list(result$state))

      # Set series_single input (single mode uses this input)
      session$setInputs(series_single = "FTSE")
      session$flushReact()

      # Check state updated
      expect_equal(result$state$series(), "FTSE")

      # Change to another series
      session$setInputs(series_single = "SMI")
      session$flushReact()
      expect_equal(result$state$series(), "SMI")
    }
  )
})

test_that("ts_select_block - series input updates state (multiple mode)", {
  test_data <- reactive(tsbox::ts_tbl(datasets::EuStockMarkets))
  block <- new_ts_select_block(series = c("DAX"), multiple = TRUE)

  testServer(
    block$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned

      # Set series_multi input (multiple mode uses this input)
      session$setInputs(series_multi = c("DAX", "FTSE"))
      session$flushReact()

      # Check state updated with multiple series
      series_val <- result$state$series()
      expect_true(length(series_val) >= 1)
      expect_true(all(c("DAX", "FTSE") %in% series_val) || "DAX" %in% series_val)
    }
  )
})

test_that("ts_select_block - multiple mode toggle updates state", {
  test_data <- reactive(tsbox::ts_tbl(datasets::EuStockMarkets))
  block <- new_ts_select_block(series = c("DAX"), multiple = TRUE)

  testServer(
    block$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned

      # Check initial multiple mode state
      expect_true(result$state$multiple())

      # Toggle to single mode
      session$setInputs(multiple = FALSE)
      session$flushReact()
      expect_false(result$state$multiple())

      # Toggle back to multiple mode
      session$setInputs(multiple = TRUE)
      session$flushReact()
      expect_true(result$state$multiple())
    }
  )
})
