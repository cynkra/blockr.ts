# The block emits `.(data)` placeholders (`expr_type = "bquoted"`), which
# blockr.core replaces with the upstream block's own name.
as_bound <- function(e, nm = "data") {
  do.call(bquote, list(e, stats::setNames(list(as.name(nm)), "data")))
}

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

test_that("a pinned dimension identifies nothing and is dropped", {
  # An API that takes its query as dimensions returns a column per dimension
  # whether the query pinned it or not, so two thirds of a label like
  # `real \u00b7 manu \u00b7 na` is the question rather than the answer. The
  # only way out used to be a select block upstream whose whole job was
  # deleting those columns.
  d <- data.frame(
    type = "real",
    dim = rep(c("a", "b"), each = 4L),
    adj = "na",
    time = rep(seq(as.Date("2020-01-01"), by = "month", length.out = 4L), 2L),
    value = 1:8
  )
  lay <- ts_layout(d)
  expect_identical(lay$ids, c("type", "dim", "adj"))
  expect_identical(lay$varying, "dim")

  rows <- ts_pick_rows(d)
  expect_identical(vapply(rows, `[[`, character(1L), "id"), c("a", "b"))

  # The expression filters AND drops the pinned columns, which is the work
  # the select block was doing by hand. `ts_pick()` could not: it reads the
  # frame's columns, not our labels, and fails with "values missing in data".
  r <- eval(as_bound(ts_pick_expr("a", lay)), list(data = d))
  expect_identical(names(r), c("dim", "time", "value"))
  expect_identical(unique(r$dim), "a")
  expect_identical(nrow(r), 4L)
})

test_that("one id column still gets ts_pick(), which keeps the order", {
  lay <- list(ids = "id", varying = "id", time = "time", value = "value")
  expect_identical(
    as_bound(ts_pick_expr(c("b", "a"), lay)),
    quote(tsbox::ts_pick(data, "b", "a"))
  )
})

test_that("a single series keeps its name when every column is pinned", {
  d <- data.frame(
    type = "real", dim = "gdp",
    time = seq(as.Date("2020-01-01"), by = "month", length.out = 4L),
    value = 1:4
  )
  # All constant is the single-series case: over-described beats unnamed.
  expect_identical(ts_layout(d)$varying, "dim")
  expect_identical(ts_pick_rows(d)[[1L]]$id, "gdp")
})
