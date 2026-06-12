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

# UI Input Tests - test that setInputs changes state
test_that("ts_dataset_block - dataset input updates state", {
  block <- new_ts_dataset_block(dataset = "AirPassengers")

  testServer(
    block$expr_server,
    args = list(),
    {
      session$flushReact()

      result <- session$returned
      expect_true(is.reactive(result$expr))
      expect_true(is.list(result$state))

      # Check initial dataset state
      expect_equal(result$state$dataset(), "AirPassengers")

      # Change dataset input
      session$setInputs(dataset = "Nile")
      session$flushReact()

      # Check state updated
      expect_equal(result$state$dataset(), "Nile")

      # Change to another dataset
      session$setInputs(dataset = "co2")
      session$flushReact()
      expect_equal(result$state$dataset(), "co2")
    }
  )
})

test_that("ts_dataset_block - dataset input updates expression", {
  block <- new_ts_dataset_block(dataset = "AirPassengers")

  testServer(
    block$expr_server,
    args = list(),
    {
      session$flushReact()

      result <- session$returned

      # Check initial expression
      expr_result <- result$expr()
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("AirPassengers", expr_text)))

      # Change dataset
      session$setInputs(dataset = "EuStockMarkets")
      session$flushReact()

      # Expression should now use EuStockMarkets
      expr_result <- result$expr()
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("EuStockMarkets", expr_text)))
    }
  )
})
