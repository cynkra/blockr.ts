# The blocks emit `.(data)` placeholders (`expr_type = "bquoted"`), which
# blockr.core replaces with the upstream block's own name. Tests read the
# expression the way core evaluates it.
as_bound <- function(e, nm = "data") {
  do.call(bquote, list(e, stats::setNames(list(as.name(nm)), "data")))
}

test_that("every verb writes a call with the data as first argument", {
  for (v in ts_verb_keys()) {
    e <- as_bound(ts_verb_expr(v, list()))
    expect_true(is.call(e), info = v)
    expect_identical(e[[2L]], as.name("data"), info = v)
    expect_identical(typeof(e), "language", info = v)
  }
})

test_that("unset arguments are left out, in all three of their shapes", {
  # This is the one that took the app down: `!nzchar(character(0))` is
  # `logical(0)`, and `if` on it errors instead of skipping.
  unset <- list(base = character(), start = "", end = NA_character_,
                by = NULL, to = character())
  for (v in ts_verb_keys()) {
    e <- as_bound(ts_verb_expr(v, unset))
    expect_identical(length(as.list(e)), 2L, info = v)
  }
})

test_that("arguments left at the tsbox default are left out", {
  expect_identical(as_bound(ts_verb_expr("lag", list(by = 1))),
                   quote(tsbox::ts_lag(data)))
  expect_identical(as_bound(ts_verb_expr("lag", list(by = 12))),
                   quote(tsbox::ts_lag(data, by = 12)))
  expect_identical(
    as_bound(ts_verb_expr("scale", list(center = TRUE, scale = TRUE))),
    quote(tsbox::ts_scale(data))
  )
  expect_identical(
    as_bound(ts_verb_expr("scale", list(center = TRUE, scale = FALSE))),
    quote(tsbox::ts_scale(data, scale = FALSE))
  )
  expect_identical(
    as_bound(ts_verb_expr("frequency", list(to = "year", aggregate = "mean"))),
    quote(tsbox::ts_frequency(data, to = "year"))
  )
})

test_that("every verb is in exactly one group, and every group key exists", {
  keys <- ts_verb_keys()
  grouped <- unlist(ts_verb_groups(), use.names = FALSE)
  expect_setequal(keys, grouped)
  expect_length(grouped, length(keys))
})

test_that("every verb and method has an icon", {
  keys <- c(ts_verb_keys(), names(ts_methods()))
  expect_true(all(keys %in% names(ts_icon_paths())))
})
