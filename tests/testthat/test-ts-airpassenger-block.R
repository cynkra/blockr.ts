test_that("ts_airpassenger_block - loads AirPassengers data", {

  block <- new_ts_airpassenger_block()

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected data
      expected <- tsbox::ts_tbl(datasets::AirPassengers)

      # Test structure
      expect_true(is.data.frame(result))
      expect_true("time" %in% names(result))
      expect_true("value" %in% names(result))

      # Test dimensions - AirPassengers is 1949-1960 monthly
      expect_equal(nrow(result), 144)
      expect_equal(nrow(result), nrow(expected))

      # Vectorized comparison
      expect_equal(result$time, expected$time)
      expect_equal(result$value, expected$value)
    },
    args = list(x = block)
  )
})
