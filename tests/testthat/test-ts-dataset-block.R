test_that("ts_dataset_block - default AirPassengers dataset", {

  block <- new_ts_dataset_block()

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

test_that("ts_dataset_block - select different dataset", {

  block <- new_ts_dataset_block(dataset = "Nile")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected data
      expected <- tsbox::ts_tbl(datasets::Nile)

      # Test structure
      expect_true(is.data.frame(result))
      expect_true("time" %in% names(result))
      expect_true("value" %in% names(result))

      # Test dimensions
      expect_equal(nrow(result), nrow(expected))

      # Vectorized comparison
      expect_equal(result$time, expected$time)
      expect_equal(result$value, expected$value)
    },
    args = list(x = block)
  )
})

test_that("ts_dataset_block - multivariate dataset (EuStockMarkets)", {

  block <- new_ts_dataset_block(dataset = "EuStockMarkets")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected data
      expected <- tsbox::ts_tbl(datasets::EuStockMarkets)

      # Test structure - multivariate should have id column
      expect_true(is.data.frame(result))
      expect_true("time" %in% names(result))
      expect_true("value" %in% names(result))
      expect_true("id" %in% names(result))

      # Should have 4 series (DAX, SMI, CAC, FTSE)
      expect_equal(length(unique(result$id)), 4)

      # Test dimensions
      expect_equal(nrow(result), nrow(expected))

      # Vectorized comparison
      expect_equal(result$time, expected$time)
      expect_equal(result$id, expected$id)
      expect_equal(result$value, expected$value)
    },
    args = list(x = block)
  )
})

test_that("ts_dataset_block - quarterly data (JohnsonJohnson)", {

  block <- new_ts_dataset_block(dataset = "JohnsonJohnson")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected data
      expected <- tsbox::ts_tbl(datasets::JohnsonJohnson)

      # Test structure
      expect_true(is.data.frame(result))
      expect_true("time" %in% names(result))
      expect_true("value" %in% names(result))

      # Test dimensions
      expect_equal(nrow(result), nrow(expected))

      # Vectorized comparison
      expect_equal(result$time, expected$time)
      expect_equal(result$value, expected$value)
    },
    args = list(x = block)
  )
})

test_that("ts_dataset_block - annual data (LakeHuron)", {

  block <- new_ts_dataset_block(dataset = "LakeHuron")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected data
      expected <- tsbox::ts_tbl(datasets::LakeHuron)

      # Test structure
      expect_true(is.data.frame(result))
      expect_true("time" %in% names(result))
      expect_true("value" %in% names(result))

      # Test dimensions
      expect_equal(nrow(result), nrow(expected))

      # Vectorized comparison
      expect_equal(result$time, expected$time)
      expect_equal(result$value, expected$value)
    },
    args = list(x = block)
  )
})

test_that("ts_dataset_block - multiple univariate datasets", {

  # Test several univariate datasets
  test_datasets <- c("co2", "lynx", "sunspots")

  for (ds in test_datasets) {
    block <- new_ts_dataset_block(dataset = ds)

    testServer(
      blockr.core:::get_s3_method("block_server", block),
      {
        session$flushReact()
        result <- session$returned$result()

        # Get expected data
        expected <- tsbox::ts_tbl(get(ds, envir = as.environment("package:datasets")))

        # Test dimensions
        expect_equal(nrow(result), nrow(expected))

        # Vectorized comparison
        expect_equal(result$time, expected$time)
        expect_equal(result$value, expected$value)
      },
      args = list(x = block)
    )
  }
})
