test_that("ts_from_df_block - converts wide data frame to long format", {

  block <- new_ts_from_df_block()

  # Create sample wide-format data
  df_wide <- data.frame(
    time = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    sales = c(100, 110, 120),
    revenue = c(1000, 1100, 1200)
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      expected <- tsbox::ts_long(df_wide)

      # Test structure - should be in long format
      expect_true(is.data.frame(result))
      expect_true("time" %in% names(result))
      expect_true("value" %in% names(result))
      expect_true("id" %in% names(result))

      # Should have 2 series (sales and revenue)
      expect_equal(length(unique(result$id)), 2)

      # Should have 6 rows (3 time points × 2 series)
      expect_equal(nrow(result), 6)

      # Vectorized comparison
      expect_equal(result$time, expected$time)
      expect_equal(result$id, expected$id)
      expect_equal(result$value, expected$value, tolerance = 0.001)
    },
    args = list(
      x = block,
      data = list(data = function() df_wide)
    )
  )
})

test_that("ts_from_df_block - handles single series", {

  block <- new_ts_from_df_block()

  # Create sample data with single numeric column
  df_single <- data.frame(
    time = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    value = c(100, 110, 120)
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      expected <- tsbox::ts_long(df_single)

      # Test structure
      expect_true(is.data.frame(result))
      expect_true("time" %in% names(result))
      expect_true("value" %in% names(result))

      # Should have 3 rows
      expect_equal(nrow(result), 3)

      # Vectorized comparison
      expect_equal(result$time, expected$time)
      expect_equal(result$value, expected$value, tolerance = 0.001)
    },
    args = list(
      x = block,
      data = list(data = function() df_single)
    )
  )
})

test_that("ts_from_df_block - handles multiple series", {

  block <- new_ts_from_df_block()

  # Create sample data with many series
  df_multi <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
    series1 = rnorm(12, 100, 10),
    series2 = rnorm(12, 200, 20),
    series3 = rnorm(12, 300, 30),
    series4 = rnorm(12, 400, 40)
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      expected <- tsbox::ts_long(df_multi)

      # Test structure
      expect_true(is.data.frame(result))
      expect_true("time" %in% names(result))
      expect_true("value" %in% names(result))
      expect_true("id" %in% names(result))

      # Should have 4 series
      expect_equal(length(unique(result$id)), 4)

      # Should have 48 rows (12 time points × 4 series)
      expect_equal(nrow(result), 48)

      # Vectorized comparison
      expect_equal(result$time, expected$time)
      expect_equal(result$id, expected$id)
      expect_equal(result$value, expected$value, tolerance = 0.001)
    },
    args = list(
      x = block,
      data = list(data = function() df_multi)
    )
  )
})

test_that("ts_from_df_block - works with POSIXct time column", {

  block <- new_ts_from_df_block()

  # Create data with POSIXct time
  df_posix <- data.frame(
    timestamp = as.POSIXct(c("2020-01-01 00:00:00", "2020-01-01 01:00:00", "2020-01-01 02:00:00")),
    value = c(10, 20, 30)
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      expected <- tsbox::ts_long(df_posix)

      # Test structure
      expect_true(is.data.frame(result))
      expect_true("time" %in% names(result))
      expect_true("value" %in% names(result))

      # Test dimensions
      expect_equal(nrow(result), nrow(expected))

      # Vectorized comparison
      expect_equal(result$value, expected$value, tolerance = 0.001)
    },
    args = list(
      x = block,
      data = list(data = function() df_posix)
    )
  )
})

test_that("ts_from_df_block - preserves series names", {

  block <- new_ts_from_df_block()

  # Create data with specific column names
  df_named <- data.frame(
    date = as.Date(c("2020-01-01", "2020-02-01")),
    apples = c(50, 55),
    oranges = c(30, 35),
    bananas = c(20, 25)
  )

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Check that series names are preserved
      series_names <- unique(result$id)
      expect_setequal(series_names, c("apples", "oranges", "bananas"))

      # Verify correct number of series
      expect_equal(length(series_names), 3)
    },
    args = list(
      x = block,
      data = list(data = function() df_named)
    )
  )
})
