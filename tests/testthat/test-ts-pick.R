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

test_that("the constructor's selection survives startup", {
  # `input$picked` is NULL when the block is created. An observer that does
  # not ignore its init fires on that NULL and overwrites `series` with
  # nothing before a single click, and the block then passes every series
  # through while the list shows the right rows ticked.
  blk <- new_ts_pick_block(series = c("a", "b"))

  d <- suppressMessages(tsbox::ts_tbl(tsbox::ts_c(
    a = datasets::mdeaths, b = datasets::fdeaths, c = datasets::ldeaths
  )))

  shiny::testServer(
    blk$expr_server,
    args = list(data = shiny::reactive(d)),
    {
      session$flushReact()
      expect_identical(session$getReturned()$state$series(), c("a", "b"))
      e <- session$getReturned()$expr()
      expect_identical(e[[1L]], quote(tsbox::ts_pick))
      expect_identical(as.character(e[[3L]]), "a")
    }
  )
})

test_that("a frame with several id columns is still pickable", {
  # ts_pick() matches against ONE id column. An API that returns a column per
  # dimension has no single id, and the composite label the list shows is one
  # this package pastes for display, so handing it to ts_pick() fails with
  # "values missing in data" after a list that looked like it worked.
  e <- ts_pick_expr("a · x", c("dim1", "dim2"))
  d <- data.frame(
    dim1 = rep(c("a", "b"), each = 4L),
    dim2 = "x",
    time = rep(seq(as.Date("2020-01-01"), by = "month", length.out = 4L), 2L),
    value = 1:8
  )
  r <- eval(e, list(data = d))
  expect_identical(unique(r$dim1), "a")
  expect_identical(nrow(r), 4L)
})

test_that("one id column still gets ts_pick(), which keeps the order", {
  expect_identical(
    ts_pick_expr(c("b", "a"), "id"),
    quote(tsbox::ts_pick(data, "b", "a"))
  )
})
