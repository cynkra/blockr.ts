test_that("ts_name puts the id first and keeps the rows", {
  x <- suppressMessages(tsbox::ts_tbl(datasets::AirPassengers))
  r <- ts_name(x, "Air passengers")
  expect_identical(names(r)[[1L]], "id")
  expect_identical(unique(r$id), "Air passengers")
  expect_identical(nrow(r), nrow(x))
})

test_that("naming a multi-series frame is refused, not silently collapsed", {
  # Collapsing them would leave several values per period, which tsbox then
  # refuses to read, so the block would just stop drawing.
  m <- suppressMessages(
    tsbox::ts_tbl(tsbox::ts_c(datasets::mdeaths, datasets::fdeaths))
  )
  expect_error(ts_name(m, "All"), "names ONE series")
})

test_that("a named single series is still drawable", {
  x <- suppressMessages(tsbox::ts_tbl(datasets::AirPassengers))
  expect_false(is.null(ts_frame(ts_name(x, "Air passengers"))))
})
