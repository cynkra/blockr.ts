.onLoad <- function(libname, pkgname) {

  # broom is a Suggests: `tidy.ets()` and `glance.ets()` are useful to anyone
  # who has it, and this package has to load for anyone who does not. See
  # `s3_register()`.
  s3_register("broom::tidy", "ets")
  s3_register("broom::glance", "ets")

  tryCatch(
    register_ts_blocks(),
    error = function(e) {
      warning("blockr.ts: block registration failed: ", conditionMessage(e))
    }
  )
  invisible(NULL)
}
