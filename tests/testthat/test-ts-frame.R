ap <- function() {
  suppressMessages(tsbox::ts_span(tsbox::ts_tbl(datasets::AirPassengers),
                                  end = "1950-12-01"))
}

test_that("every column order tsbox reads is a column order we draw", {
  # ts_tbl() ONLY reorders when it converts from something else. A frame it
  # can already read comes back exactly as it went in, so a guard that reads
  # the last two columns rejects three of these five.
  x <- ap()
  shapes <- list(
    "time,value" = x,
    "value,time" = x[c("value", "time")],
    "time,value,id" = transform(x, id = "A"),
    "id,time,value" = transform(x, id = "A")[c("id", "time", "value")],
    "time,id,value" = transform(x, id = "A")[c("time", "id", "value")]
  )
  for (nm in names(shapes)) {
    expect_false(is.null(ts_frame(shapes[[nm]])), info = nm)
  }
})

test_that("a frame tsbox passed through unresolved is refused", {
  # `ts_tbl()` succeeding is not the same as it having found a series: handed
  # `time, value, costs` it returns all three columns unchanged.
  expect_null(ts_frame(transform(ap(), costs = 1)))
  expect_null(ts_frame(datasets::mtcars))
  expect_null(ts_frame(datasets::iris))
  expect_null(ts_frame(NULL))
})

test_that("ts_resolve is the seam, and NULL means 'not a series'", {
  expect_s3_class(ts_resolve(datasets::AirPassengers), "data.frame")
  expect_null(ts_resolve(datasets::mtcars))

  # A package with a model class adds a method rather than a branch.
  on.exit(rm("ts_resolve.ts_fake", envir = globalenv()), add = TRUE)
  assign("ts_resolve.ts_fake",
         function(x) suppressMessages(tsbox::ts_tbl(datasets::AirPassengers)),
         envir = globalenv())
  fake <- structure(list(), class = "ts_fake")
  expect_false(is.null(ts_frame(fake)))
})

test_that("several id columns become one label, pinned ones excluded", {
  x <- ap()
  d <- rbind(
    transform(x, region = "north", channel = "web"),
    transform(x, region = "south", channel = "web")
  )
  f <- ts_frame(d[c("region", "channel", "time", "value")])
  # `channel` is the same everywhere, so it says nothing about which series
  # this is; `region` is what distinguishes them.
  expect_setequal(unique(f$id), c("north", "south"))

  d2 <- rbind(
    transform(x, region = "north", channel = "web"),
    transform(x, region = "north", channel = "shop")
  )
  f2 <- ts_frame(d2[c("region", "channel", "time", "value")])
  expect_setequal(unique(f2$id), c("web", "shop"))
})
