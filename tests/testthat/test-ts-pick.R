many <- function() {
  # Named, because ts_c() would otherwise name the series by deparsing its
  # arguments and call this one "datasets::mdeaths".
  suppressMessages(tsbox::ts_tbl(tsbox::ts_c(
    mdeaths = datasets::mdeaths,
    fdeaths = datasets::fdeaths,
    ldeaths = datasets::ldeaths
  )))
}

test_that("one row per series, with what ts_summary knows", {
  rows <- ts_pick_rows(many())
  expect_length(rows, 3L)
  expect_identical(rows[[1L]]$id, "mdeaths")
  expect_identical(rows[[1L]]$obs, 72L)
  expect_identical(rows[[1L]]$start, "1974-01")
  expect_true(nzchar(rows[[1L]]$spark))
})

test_that("a single unnamed series has nothing to pick", {
  expect_length(
    ts_pick_rows(suppressMessages(tsbox::ts_tbl(datasets::AirPassengers))),
    0L
  )
  expect_length(ts_pick_rows(datasets::mtcars), 0L)
})

test_that("a sparkline is one path, scaled to its own range", {
  p <- ts_sparkline(c(1, 2, 3))
  expect_match(p, "^M")
  expect_false(grepl("NA", p))
  # A flat series has no range to scale to and must not divide by zero.
  expect_false(grepl("NaN", ts_sparkline(rep(5, 10))))
  expect_identical(ts_sparkline(1), "")
  expect_identical(ts_sparkline(c(NA, NA)), "")
})

test_that("the unnamed tile group renders its tiles", {
  # `groups[[""]]` is NULL, so indexing by name drops a whole group in
  # silence.
  g <- stats::setNames(list(c("a", "b")), "")
  html <- paste(as.character(
    ts_tile_grid("x", g, c(a = "A", b = "B"), "a")
  ), collapse = "")
  # "blockr-ts-tiles" and "blockr-ts-tiles__grid" both start with the tile
  # class, so count the buttons instead.
  expect_length(gregexpr("<button", html, fixed = TRUE)[[1L]], 2L)
})
