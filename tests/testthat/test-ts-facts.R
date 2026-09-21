test_that("facts come off ts_summary, and non-series never error", {
  f <- ts_facts(suppressMessages(tsbox::ts_tbl(datasets::AirPassengers)))
  expect_true(f$ok)
  expect_identical(f$freq, 12)
  expect_identical(f$unit, "1 month")
  expect_length(f$periods, 144L)

  expect_false(ts_facts(datasets::mtcars)$ok)
  expect_false(ts_facts(NULL)$ok)
  expect_false(ts_facts("nonsense")$ok)
})

test_that("only coarser frequencies are offered", {
  expect_identical(unname(ts_coarser(12)), c("quarter", "year"))
  expect_identical(unname(ts_coarser(4)), "year")
  expect_length(ts_coarser(1), 0L)
})

test_that("one year is as many periods as the data has", {
  expect_true("12" %in% ts_lag_choices(12))
  expect_true("4" %in% ts_lag_choices(4))
  expect_identical(unname(ts_lag_choices(1)), c("-1", "1"))
})

test_that("a stored value that is no longer on offer falls back", {
  expect_identical(ts_one_of("b", c("a", "b")), "b")
  expect_identical(ts_one_of("z", c("a", "b")), "a")
  expect_identical(ts_one_of("z", c("a", "b"), fallback = ""), "")
  # A board saved against another series can leave no choices at all.
  expect_null(ts_one_of("z", character()))
})
