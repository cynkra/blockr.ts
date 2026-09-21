test_that("only ts_c() gets argument names", {
  # ts_bind() ignores them and ts_chain() ERRORS on them, so a named chain
  # call would take a working board down.
  expect_identical(
    ts_combine_expr("c", c(".arg1", ".arg2"), c("Male", "Female"), ""),
    quote(tsbox::ts_c(Male = .(.arg1), Female = .(.arg2)))
  )
  expect_identical(
    ts_combine_expr("bind", c(".arg1", ".arg2"), c("Old", "New"), ""),
    quote(tsbox::ts_bind(.(.arg1), .(.arg2)))
  )
  expect_identical(
    ts_combine_expr("chain", c(".arg1", ".arg2"), c("Old", "New"), ""),
    quote(tsbox::ts_chain(.(.arg1), .(.arg2)))
  )
})

test_that("a named result is wrapped in ts_name(), an unnamed one is not", {
  expect_identical(
    ts_combine_expr("bind", c(".arg1", ".arg2"), c("", ""), "Air"),
    quote(blockr.ts::ts_name(tsbox::ts_bind(.(.arg1), .(.arg2)), "Air"))
  )
  expect_identical(
    ts_combine_expr("c", c(".arg1", ".arg2"), c("a", "b"), "ignored"),
    quote(tsbox::ts_c(a = .(.arg1), b = .(.arg2)))
  )
})

test_that("the operators are self-qualified and need exactly two inputs", {
  e <- ts_combine_expr("divide", c(".arg1", ".arg2"), c("a", "b"), "")
  expect_identical(e, quote(tsbox::`%ts/%`(.(.arg1), .(.arg2))))
  expect_identical(e[[1L]][[2L]], as.name("tsbox"))

  one <- ts_combine_expr("divide", ".arg1", "a", "")
  expect_true(is.call(one))
  expect_identical(one[[1L]], as.name("stop"))
})

test_that("dot refs are VALUES; the names are the link names", {
  refs <- ts_dot_refs(stats::setNames(list(1, 2), c("", "")))
  expect_identical(unname(refs), c(".arg1", ".arg2"))
  expect_identical(names(refs), c("", ""))

  named <- ts_dot_refs(stats::setNames(list(1, 2), c("sales", "")))
  expect_identical(unname(named), c("sales", ".arg1"))
})

test_that("no inputs yields a call, not an error", {
  expect_true(is.call(ts_combine_expr("c", character(), character(), "")))
})

test_that("a cleared slot name does not fall back to the deparse", {
  # `ts_c(x, b = y)` names the first series by deparsing it, which is the
  # whole failure this block exists to stop.
  e <- ts_combine_expr("c", c(".arg1", ".arg2"), c("", "Female"), "")
  expect_identical(names(as.list(e))[-1L], c("series1", "Female"))
})

test_that("an id that is not a syntactic R name still works as one", {
  # tsbox ids are labels, not names: anything can be in one. The same class
  # of bug bit the forecast block, where VAR equations go through
  # make.names() and a non-syntactic id then looks up as NULL.
  for (id in c("Retail `trade`", "Sales · NSA", "if", "2024", "a\nb")) {
    e <- ts_combine_expr("c", c(".arg1", ".arg2"), c(id, "b"), "")
    expect_identical(names(as.list(e))[[2L]], id)
  }
})
