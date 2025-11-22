test_that("ts_span_block - filter with both start and end", {

  block <- new_ts_span_block(start = "1950-01-01", end = "1955-12-31")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::AirPassengers)
      expected <- tsbox::ts_span(original, start = "1950-01-01", end = "1955-12-31")

      # Test structure
      expect_true(is.data.frame(result))
      expect_true("time" %in% names(result))
      expect_true("value" %in% names(result))

      # Test dimensions
      expect_equal(nrow(result), nrow(expected))
      expect_true(nrow(result) < nrow(original))

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

test_that("ts_span_block - filter with start only", {

  block <- new_ts_span_block(start = "1955-01-01", end = NULL)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::AirPassengers)
      expected <- tsbox::ts_span(original, start = "1955-01-01")

      # Test structure
      expect_true(is.data.frame(result))
      expect_true("time" %in% names(result))
      expect_true("value" %in% names(result))

      # Test dimensions
      expect_equal(nrow(result), nrow(expected))
      expect_true(nrow(result) < nrow(original))

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

test_that("ts_span_block - filter with end only", {

  block <- new_ts_span_block(start = NULL, end = "1955-12-31")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::AirPassengers)
      expected <- tsbox::ts_span(original, end = "1955-12-31")

      # Test structure
      expect_true(is.data.frame(result))
      expect_true("time" %in% names(result))
      expect_true("value" %in% names(result))

      # Test dimensions
      expect_equal(nrow(result), nrow(expected))
      expect_true(nrow(result) < nrow(original))

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

test_that("ts_span_block - no filter (NULL start and end)", {

  block <- new_ts_span_block(start = NULL, end = NULL)

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected - should return original data unchanged
      original <- tsbox::ts_tbl(datasets::AirPassengers)

      # Test structure
      expect_true(is.data.frame(result))
      expect_true("time" %in% names(result))
      expect_true("value" %in% names(result))

      # Test dimensions - should be same as original
      expect_equal(nrow(result), nrow(original))

      # Vectorized comparison - should be identical to original
      expect_equal(result$time, original$time)
      expect_equal(result$value, original$value, tolerance = 0.001)
    },
    args = list(
      x = block,
      data = list(data = function() tsbox::ts_tbl(datasets::AirPassengers))
    )
  )
})

test_that("ts_span_block - works with multivariate data", {

  block <- new_ts_span_block(start = "1995-01-01", end = "1997-12-31")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::EuStockMarkets)
      expected <- tsbox::ts_span(original, start = "1995-01-01", end = "1997-12-31")

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

test_that("ts_span_block - narrow time range", {

  block <- new_ts_span_block(start = "1952-06-01", end = "1952-12-31")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::AirPassengers)
      expected <- tsbox::ts_span(original, start = "1952-06-01", end = "1952-12-31")

      # Test dimensions
      expect_equal(nrow(result), nrow(expected))
      expect_true(nrow(result) < 12) # Less than a year

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
