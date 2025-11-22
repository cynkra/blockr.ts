# Testing Guide for blockr.ts

## Core Testing Philosophy

**The Golden Rule: Test the DATA, not the expression.**

When testing blockr.ts blocks, we validate the **actual transformed data** that users see in their dygraphs, not just the expression structure. This approach catches real bugs and ensures transformations work correctly.

## Why Test Data Results?

### The Wrong Approach (Expression Testing)
```r
# ❌ BAD: Only tests that expression contains the right function name
test_that("ts_change_block - pc method", {
  blk <- new_ts_change_block(method = "pc")

  shiny::testServer(
    blk$expr_server,  # ❌ Testing expression generation
    args = list(data = test_data),
    {
      expr_result <- result$expr()
      expr_text <- deparse(expr_result)
      expect_true(any(grepl("ts_pc", expr_text)))  # ❌ Boring! Doesn't test if it works!
    }
  )
})
```

**Problems with expression testing:**
- Doesn't verify the transformation actually works
- Doesn't catch calculation errors
- Doesn't validate output data structure
- Provides false confidence - test passes even if data is wrong

### The Right Approach (Data Result Testing)
```r
# ✅ GOOD: Tests actual transformed data
test_that("ts_change_block - pc method computes correct percentage changes", {
  block <- new_ts_change_block(method = "pc")

  testServer(
    blockr.core:::get_s3_method("block_server", block),  # ✅ Test block_server
    {
      session$flushReact()

      # ✅ Get ACTUAL transformed data
      result <- session$returned$result()

      # ✅ Compare against tsbox function (source of truth)
      original <- tsbox::ts_tbl(datasets::AirPassengers)
      expected <- tsbox::ts_pc(original)

      # ✅ Verify every value matches
      expect_equal(nrow(result), nrow(expected))
      expect_equal(result$time, expected$time)

      for (i in seq_len(nrow(result))) {
        if (is.na(expected$value[i])) {
          expect_true(is.na(result$value[i]))
        } else {
          expect_equal(result$value[i], expected$value[i], tolerance = 0.001)
        }
      }
    },
    args = list(
      x = block,
      data = list(data = function() tsbox::ts_tbl(datasets::AirPassengers))
    )
  )
})
```

**Benefits of data result testing:**
- Validates the transformation produces correct output
- Catches calculation errors immediately
- Verifies data structure (columns, rows, types)
- Compares against tsbox functions (the source of truth)
- Tests what users actually see in their dygraphs

## The Two Testing Patterns

### Pattern 1: Testing `block_server` (Transform Blocks)

Use this for **transform blocks** that modify incoming data:

```r
test_that("ts_frequency_block - converts monthly to yearly", {
  block <- new_ts_frequency_block(to = "year", aggregate = "mean")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::AirPassengers)
      expected <- tsbox::ts_frequency(original, to = "year", aggregate = "mean")

      # Compare actual vs expected
      expect_equal(nrow(result), nrow(expected))
      # ... compare values
    },
    args = list(
      x = block,
      data = list(data = function() tsbox::ts_tbl(datasets::AirPassengers))
    )
  )
})
```

### Pattern 2: Testing Data Blocks

Use this for **data blocks** that load data:

```r
test_that("ts_dataset_block - loads AirPassengers correctly", {
  block <- new_ts_dataset_block(dataset = "AirPassengers")

  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      result <- session$returned$result()

      # Compare against expected dataset
      expected <- tsbox::ts_tbl(datasets::AirPassengers)

      expect_equal(nrow(result), 144)  # 1949-1960, monthly
      expect_equal(nrow(result), nrow(expected))
      expect_equal(result$time, expected$time)
      expect_equal(result$value, expected$value)
    },
    args = list(x = block)
  )
})
```

## Key Testing Principles

### 1. Compare Against tsbox Functions

**Always compare block output against the corresponding tsbox function:**

```r
# Block uses tsbox::ts_pc internally
result <- session$returned$result()
expected <- tsbox::ts_pc(original_data)

# Compare them
expect_equal(result$value, expected$value, tolerance = 0.001)
```

This ensures the block is using tsbox correctly and produces identical results.

### 2. Use Vectorized Comparisons

**Test ALL values at once using vectorized comparisons:**

```r
# ✅ GOOD: Vectorized comparison tests all values at once
expect_equal(result$value, expected$value, tolerance = 0.001)

# ❌ BAD: Loop through every value individually (silly!)
for (i in seq_len(nrow(result))) {
  if (is.na(expected$value[i])) {
    expect_true(is.na(result$value[i]))
  } else {
    expect_equal(result$value[i], expected$value[i], tolerance = 0.001)
  }
}
# This creates 2,614 expectations for 8 tests - way too many!

# ❌ ALSO BAD: Only test first few values
expect_equal(result$value[2], expected_value, tolerance = 0.01)
```

**Why vectorized is better:**
- `expect_equal(vec1, vec2)` tests **all values** - no loop needed
- Much faster (42 tests vs 2,614 tests for the same validation)
- Cleaner, more readable code
- Follows R best practices
- R reports which values differ if the test fails

### 3. Test Data Structure

**Verify the output has the correct structure:**

```r
# Verify it's a data frame
expect_true(is.data.frame(result))

# Verify required columns exist
expect_true("time" %in% names(result))
expect_true("value" %in% names(result))

# For multivariate data
expect_true("id" %in% names(result))
expect_equal(length(unique(result$id)), 4)  # 4 series

# Verify row counts
expect_equal(nrow(result), nrow(expected))
```

### 4. Handle NA Values Correctly

**NA values need special handling:**

```r
# ✅ GOOD: Check if expected is NA first
if (is.na(expected$value[i])) {
  expect_true(is.na(result$value[i]))
} else {
  expect_equal(result$value[i], expected$value[i], tolerance = 0.001)
}

# ❌ BAD: This fails when comparing NA values
expect_equal(result$value[i], expected$value[i])
```

### 5. Test Edge Cases

**Test with unusual data:**

```r
# Short time series
short_data <- data.frame(
  time = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
  value = c(100, 110, 121)
)

# Single observation
single_data <- data.frame(
  time = as.Date("2020-01-01"),
  value = 100
)

# Data with NA values
data_with_na <- data.frame(
  time = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
  value = c(100, NA, 121)
)

# Multivariate data
multivariate_data <- tsbox::ts_tbl(datasets::EuStockMarkets)
```

### 6. Test All Parameters

**Test every parameter option:**

```r
# Test all methods for ts_change_block
test_that("ts_change_block - pc method", { ... })
test_that("ts_change_block - pcy method", { ... })
test_that("ts_change_block - pca method", { ... })
test_that("ts_change_block - diff method", { ... })
test_that("ts_change_block - diffy method", { ... })

# Test all aggregation methods for ts_frequency_block
test_that("ts_frequency_block - mean aggregation", { ... })
test_that("ts_frequency_block - sum aggregation", { ... })
test_that("ts_frequency_block - first aggregation", { ... })
test_that("ts_frequency_block - last aggregation", { ... })
```

## Complete Test Template

Here's a complete template for testing a transform block:

```r
test_that("BLOCK_NAME - DESCRIPTION", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("blockr.core")
  skip_if_not_installed("tsbox")

  # 1. Create the block with specific parameters
  block <- new_BLOCK_NAME(param1 = value1, param2 = value2)

  # 2. Test with block_server
  testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      # 3. Flush reactive context
      session$flushReact()

      # 4. Get actual result from block
      result <- session$returned$result()

      # 5. Verify data structure
      expect_true(is.data.frame(result))
      expect_true("time" %in% names(result))
      expect_true("value" %in% names(result))

      # 6. Get expected result using tsbox
      original <- tsbox::ts_tbl(datasets::AirPassengers)
      expected <- tsbox::FUNCTION_NAME(original, param1 = value1, param2 = value2)

      # 7. Compare structure
      expect_equal(nrow(result), nrow(expected))
      expect_equal(result$time, expected$time)

      # 8. Compare every value
      for (i in seq_len(nrow(result))) {
        if (is.na(expected$value[i])) {
          expect_true(is.na(result$value[i]))
        } else {
          expect_equal(result$value[i], expected$value[i], tolerance = 0.001)
        }
      }
    },
    # 9. Provide input data
    args = list(
      x = block,
      data = list(data = function() tsbox::ts_tbl(datasets::AirPassengers))
    )
  )
})
```

## Anti-Patterns to Avoid

### ❌ Don't test expressions
```r
# BAD
expr_result <- result$expr()
expect_true(inherits(expr_result, "call"))
expect_true(any(grepl("ts_pc", deparse(expr_result))))
```

### ❌ Don't manually calculate expected values
```r
# BAD
expected_pc <- (value[2] - value[1]) / value[1] * 100
expect_equal(result$value[2], expected_pc)
```

### ❌ Don't test only a few values
```r
# BAD
expect_equal(result$value[2], expected$value[2])
# What about values 3, 4, 5, ..., 144?
```

### ❌ Don't skip NA handling
```r
# BAD
expect_equal(result$value[i], expected$value[i])  # Fails for NA
```

## Best Practices Summary

1. **Test actual data results**, not expressions
2. **Compare against tsbox functions** (the source of truth)
3. **Test every value** in the output
4. **Verify data structure** (columns, rows, types)
5. **Handle NA values** correctly
6. **Test all parameters** and methods
7. **Test edge cases** (short series, NA values, multivariate data)
8. **Use tolerance** for floating point comparisons (0.001)

## Example: Complete Test Suite

See `tests/testthat/test-ts-change-block.R` for a complete example of proper data result testing. This file demonstrates:

- Testing all 5 transformation methods
- Comparing against tsbox functions
- Testing every value in the output
- Handling NA values correctly
- Testing multivariate data
- Testing edge cases
- **Result: 2,614 passing tests validating actual data transformations**

## Resources

- **blockr.dplyr tests**: Similar approach for dplyr blocks
- **tsbox documentation**: <https://docs.ropensci.org/tsbox/>
- **testServer documentation**: <https://shiny.rstudio.com/articles/testing-overview.html>

---

Remember: **The goal is to ensure blocks produce correct data transformations, not just correct expressions.**
