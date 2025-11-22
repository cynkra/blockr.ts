test_that("ts_to_df_block - converts to long format (default)", {

  block <- new_ts_to_df_block(format = "long")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::AirPassengers)
      expected <- tsbox::ts_tbl(original) # Long format

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

test_that("ts_to_df_block - converts to wide format", {

  block <- new_ts_to_df_block(format = "wide")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::EuStockMarkets)
      expected <- tsbox::ts_wide(original)

      # Test structure - wide format should have time column + series columns
      expect_true(is.data.frame(result))
      expect_true("time" %in% names(result))

      # Should have columns for each series (DAX, SMI, CAC, FTSE)
      expect_true("DAX" %in% names(result))
      expect_true("SMI" %in% names(result))
      expect_true("CAC" %in% names(result))
      expect_true("FTSE" %in% names(result))

      # Test dimensions - wide format has fewer rows (one per time point)
      expect_equal(nrow(result), nrow(expected))

      # Vectorized comparison of time column
      expect_equal(result$time, expected$time)
    },
    args = list(
      x = block,
      data = list(data = function() tsbox::ts_tbl(datasets::EuStockMarkets))
    )
  )
})

test_that("ts_to_df_block - long format works with univariate data", {

  block <- new_ts_to_df_block(format = "long")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result
      original <- tsbox::ts_tbl(datasets::AirPassengers)
      expected <- tsbox::ts_tbl(original)

      # Test structure
      expect_true(is.data.frame(result))
      expect_true("time" %in% names(result))
      expect_true("value" %in% names(result))

      # Should NOT have id column for univariate series
      expect_false("id" %in% names(result))

      # Test dimensions
      expect_equal(nrow(result), nrow(expected))
      expect_equal(nrow(result), 144) # AirPassengers has 144 observations

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

test_that("ts_to_df_block - long format works with multivariate data", {

  block <- new_ts_to_df_block(format = "long")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result
      original <- tsbox::ts_tbl(datasets::EuStockMarkets)
      expected <- tsbox::ts_tbl(original)

      # Test structure - should have id column for multivariate
      expect_true(is.data.frame(result))
      expect_true("time" %in% names(result))
      expect_true("value" %in% names(result))
      expect_true("id" %in% names(result))

      # Should have 4 series
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

test_that("ts_to_df_block - wide format with univariate data", {

  block <- new_ts_to_df_block(format = "wide")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result
      original <- tsbox::ts_tbl(datasets::AirPassengers)
      expected <- tsbox::ts_wide(original)

      # Test structure
      expect_true(is.data.frame(result))
      expect_true("time" %in% names(result))

      # For univariate data, wide format should have time + value columns
      expect_equal(ncol(result), ncol(expected))

      # Test dimensions
      expect_equal(nrow(result), nrow(expected))
      expect_equal(nrow(result), 144)

      # Vectorized comparison
      expect_equal(result$time, expected$time)
    },
    args = list(
      x = block,
      data = list(data = function() tsbox::ts_tbl(datasets::AirPassengers))
    )
  )
})

test_that("ts_to_df_block - wide format preserves column names", {

  block <- new_ts_to_df_block(format = "wide")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get original data
      original <- tsbox::ts_tbl(datasets::EuStockMarkets)

      # Check that all series are present as columns
      series_names <- unique(original$id)
      for (series in series_names) {
        expect_true(series %in% names(result),
                    info = paste("Series", series, "should be a column"))
      }

      # Verify we have the correct columns
      expected_cols <- c("time", series_names)
      expect_setequal(names(result), expected_cols)
    },
    args = list(
      x = block,
      data = list(data = function() tsbox::ts_tbl(datasets::EuStockMarkets))
    )
  )
})

test_that("ts_to_df_block - handles quarterly data", {

  block <- new_ts_to_df_block(format = "long")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result
      original <- tsbox::ts_tbl(datasets::JohnsonJohnson)
      expected <- tsbox::ts_tbl(original)

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
      data = list(data = function() tsbox::ts_tbl(datasets::JohnsonJohnson))
    )
  )
})

test_that("ts_to_df_block - handles annual data", {

  block <- new_ts_to_df_block(format = "long")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result
      original <- tsbox::ts_tbl(datasets::LakeHuron)
      expected <- tsbox::ts_tbl(original)

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
      data = list(data = function() tsbox::ts_tbl(datasets::LakeHuron))
    )
  )
})
