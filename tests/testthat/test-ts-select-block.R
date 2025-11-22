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
