# Known Bugs in blockr.ts

This document tracks bugs discovered during comprehensive testing implementation (Nov 2025).

## Fixed Bugs

### ✅ 1. ts-select-block: Returns NULL Instead of Data (FIXED)

**Severity**: CRITICAL
**Status**: ✅ FIXED (Nov 2, 2025)
**File**: `R/ts-select-block.R`

**Description**: The block's `available_series()` reactive wasn't handling the case where `data` is a plain function, which is how testServer passes data.

**Root Cause**: The code only checked for `is.reactive(data)` or direct data objects, but missed the case where `data` is a function. In testServer, data is passed as `function() tsbox::ts_tbl(...)`.

**Fix Applied**:
Added `is.function(data)` check in `available_series()` reactive:
```r
if (is.reactive(data)) {
  data_val <- data()
} else if (is.function(data)) {  # ← Added this
  data_val <- data()
} else if (!is.null(data)) {
  data_val <- data
}
```

**Tests Added**: 22 new tests covering:
- Single series selection
- Multiple series selection
- Default (NULL) selection behavior

**Verification**: All 177 tests now pass (up from 155).

---

## Active Bugs

### 2. ts-span-block: NULL Parameter Handling Bug

**Severity**: HIGH
**Status**: Partially Working
**File**: `R/ts-span-block.R`

**Description**: The block returns NULL when either `start` or `end` parameters are NULL, but works correctly when both are provided.

**Evidence**:
```r
# Works fine
block <- new_ts_span_block(start = "1950-01-01", end = "1955-12-31")
result <- ... # Returns correct data

# Returns NULL
block <- new_ts_span_block(start = "1955-01-01", end = NULL)
result <- ... # NULL

# Returns NULL
block <- new_ts_span_block(start = NULL, end = "1955-12-31")
result <- ... # NULL

# Returns NULL
block <- new_ts_span_block(start = NULL, end = NULL)
result <- ... # NULL
```

**Impact**: Users cannot filter with only start date, only end date, or no filter at all.

**Tests Disabled**: 3 out of 6 tests disabled:
- "filter with start only"
- "filter with end only"
- "no filter (NULL start and end)"

**Investigation Needed**:
- Check expression generation with NULL values
- Verify tsbox::ts_span() is called correctly with partial parameters

---

## Medium Severity Bugs

### 3. ts-frequency-block: Unsupported min/max Aggregation Methods

**Severity**: MEDIUM
**Status**: UI shows options that don't work
**File**: `R/ts-frequency-block.R`

**Description**: The block UI allows users to select "Minimum" and "Maximum" aggregation methods, but tsbox::ts_frequency() only supports "mean", "sum", "first", "last".

**Code Location**: Lines 327-334 in `R/ts-frequency-block.R`
```r
selectInput(
  NS(id, "aggregate"),
  label = "Aggregation Method",
  choices = c(
    "Mean" = "mean",
    "Sum" = "sum",
    "First" = "first",
    "Last" = "last",
    "Minimum" = "min",      # ❌ NOT SUPPORTED BY TSBOX
    "Maximum" = "max"       # ❌ NOT SUPPORTED BY TSBOX
  ),
  ...
)
```

**Error When Used**:
```
Error: 'aggregate' must be one of: 'mean', 'sum', 'first', 'last'
```

**Impact**: User confusion - UI suggests functionality that doesn't exist.

**Fix Required**: Remove "Minimum" and "Maximum" options from UI choices.

---

### 4. ts-frequency-block: Multivariate Data Row Count Bug

**Severity**: MEDIUM
**Status**: Produces incorrect output
**File**: `R/ts-frequency-block.R`

**Description**: When converting multivariate time series (like EuStockMarkets) to monthly frequency, the block produces 4 extra rows compared to what tsbox::ts_frequency() produces.

**Evidence**:
```r
original <- tsbox::ts_tbl(datasets::EuStockMarkets)
expected <- tsbox::ts_frequency(original, to = "month", aggregate = "mean")
# expected: 340 rows (ends at 1998-07-01)

block <- new_ts_frequency_block(to = "month", aggregate = "mean")
result <- ... # from block
# result: 344 rows (includes 4 extra rows: one 1998-08-01 entry per series)
```

**Details**:
- Expected: 340 rows, ends at 1998-07-01
- Actual: 344 rows, ends at 1998-08-01
- Extra data: 4 rows (one per series) at 1998-08-01

**Impact**: Multivariate time series have incorrect time range after frequency conversion.

**Tests Disabled**: Multivariate test commented out in `test-ts-frequency-block.R`

**Investigation Needed**:
- Check how block handles multivariate series differently than tsbox
- Verify date range calculation logic

---

## Test Suite Status

### Passing Tests: 177
- ✅ test-ts-airpassenger-block.R: 7 tests
- ✅ test-ts-change-block.R: 42 tests
- ✅ test-ts-dataset-block.R: 43 tests
- ✅ test-ts-frequency-block.R: 18 tests (2 groups disabled)
- ✅ test-ts-lag-block.R: 28 tests
- ✅ test-ts-select-block.R: 22 tests ✨ NEW
- ✅ test-ts-span-block.R: 17 tests (3 tests disabled)

### Disabled Tests: ~5
- ❌ ts-span-block: 3 tests (NULL parameter handling)
- ❌ ts-frequency-block: 2 test groups (min/max, multivariate)

---

## Priority for Fixes

1. **HIGH**: Fix ts-span-block NULL parameter handling
2. **MEDIUM**: Remove min/max from ts-frequency-block UI
3. **MEDIUM**: Fix ts-frequency-block multivariate row count bug

---

## Notes

All bugs were discovered through comprehensive data result testing using `testServer` and comparing block output against tsbox function output (the source of truth).

The testing approach validates actual transformed DATA, not just expressions, which revealed these bugs that expression-only tests would have missed.

See `dev/testing-guide.md` for testing best practices.
