test_that("ts_change_block - pc method computes correct percentage changes", {

  block <- new_ts_change_block(method = "pc")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::AirPassengers)
      expected <- tsbox::ts_pc(original)

      # Test structure
      expect_true(is.data.frame(result))
      expect_true("time" %in% names(result))
      expect_true("value" %in% names(result))

      # Test dimensions
      expect_equal(nrow(result), nrow(expected))

      # Vectorized comparison - tests ALL values at once
      expect_equal(result$time, expected$time)
      expect_equal(result$value, expected$value, tolerance = 0.001)
    },
    args = list(
      x = block,
      data = list(data = function() tsbox::ts_tbl(datasets::AirPassengers))
    )
  )
})

test_that("ts_change_block - pcy method computes year-over-year changes", {

  block <- new_ts_change_block(method = "pcy")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::AirPassengers)
      expected <- tsbox::ts_pcy(original)

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

test_that("ts_change_block - diff method computes differences", {

  block <- new_ts_change_block(method = "diff")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::AirPassengers)
      expected <- tsbox::ts_diff(original)

      # Test structure
      expect_true(is.data.frame(result))

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

test_that("ts_change_block - diffy method computes year-over-year differences", {

  block <- new_ts_change_block(method = "diffy")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::AirPassengers)
      expected <- tsbox::ts_diffy(original)

      # Test structure
      expect_true(is.data.frame(result))

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

test_that("ts_change_block - pca method computes annualized changes", {

  block <- new_ts_change_block(method = "pca")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::AirPassengers)
      expected <- tsbox::ts_pca(original)

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

test_that("ts_change_block - works with multivariate data", {

  block <- new_ts_change_block(method = "pc")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::EuStockMarkets)
      expected <- tsbox::ts_pc(original)

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

test_that("ts_change_block - handles short time series", {

  block <- new_ts_change_block(method = "pc")

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
      expected <- tsbox::ts_pc(short_data)

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

test_that("ts_change_block - different methods produce different results", {

  # Get expected results for comparison
  original <- tsbox::ts_tbl(datasets::AirPassengers)
  expected_pc <- tsbox::ts_pc(original)
  expected_diff <- tsbox::ts_diff(original)

  # Verify pc and diff produce different values
  expect_false(isTRUE(all.equal(expected_pc$value[2], expected_diff$value[2])))

  # Test pc method
  block_pc <- new_ts_change_block(method = "pc")
  testServer(
    blockr.core:::get_s3_method("block_server", block_pc),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(result$value, expected_pc$value, tolerance = 0.001)
    },
    args = list(
      x = block_pc,
      data = list(data = function() tsbox::ts_tbl(datasets::AirPassengers))
    )
  )

  # Test diff method
  block_diff <- new_ts_change_block(method = "diff")
  testServer(
    blockr.core:::get_s3_method("block_server", block_diff),
    {
      session$flushReact()
      result <- session$returned$result()
      expect_equal(result$value, expected_diff$value, tolerance = 0.001)
    },
    args = list(
      x = block_diff,
      data = list(data = function() tsbox::ts_tbl(datasets::AirPassengers))
    )
  )
})

# UI Input Tests - test that setInputs changes state and expression
test_that("ts_change_block - method input updates state", {
  test_data <- reactive(tsbox::ts_tbl(datasets::AirPassengers))
  block <- new_ts_change_block(method = "pc")

  testServer(
    block$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))
      expect_true(is.list(result$state))

      # Set initial input
      session$setInputs(method = "pc")
      session$flushReact()

      # Check state has method
      expect_equal(result$state$method(), "pc")

      # Change method input
      session$setInputs(method = "diff")
      session$flushReact()

      # State should update
      expect_equal(result$state$method(), "diff")

      # Change to pcy
      session$setInputs(method = "pcy")
      session$flushReact()
      expect_equal(result$state$method(), "pcy")
    }
  )
})

test_that("ts_change_block - method input updates expression", {
  test_data <- reactive(tsbox::ts_tbl(datasets::AirPassengers))
  block <- new_ts_change_block(method = "pc")

  testServer(
    block$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned

      # Set pc method
      session$setInputs(method = "pc")
      session$flushReact()

      # Check expression contains ts_pc
      expr_result <- result$expr()
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("ts_pc", expr_text)))

      # Change to diff method
      session$setInputs(method = "diff")
      session$flushReact()

      # Expression should now contain ts_diff
      expr_result <- result$expr()
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("ts_diff", expr_text)))

      # Change to pcy method
      session$setInputs(method = "pcy")
      session$flushReact()

      # Expression should now contain ts_pcy
      expr_result <- result$expr()
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("ts_pcy", expr_text)))
    }
  )
})

test_that("ts_change_block - all methods produce correct expressions", {
  test_data <- reactive(tsbox::ts_tbl(datasets::AirPassengers))
  block <- new_ts_change_block(method = "pc")

  testServer(
    block$expr_server,
    args = list(data = test_data),
    {
      session$flushReact()

      result <- session$returned

      # Test all methods
      methods_to_funcs <- list(
        pc = "ts_pc",
        pcy = "ts_pcy",
        pca = "ts_pca",
        diff = "ts_diff",
        diffy = "ts_diffy"
      )

      for (m in names(methods_to_funcs)) {
        session$setInputs(method = m)
        session$flushReact()

        expr_result <- result$expr()
        expr_text <- deparse(expr_result)
        expected_func <- methods_to_funcs[[m]]

        expect_true(
          any(grepl(expected_func, expr_text)),
          info = paste("Method:", m, "should contain", expected_func)
        )
      }
    }
  )
})
