.onLoad <- function(libname, pkgname) {
  tryCatch(
    register_ts_blocks(),
    error = function(e) {
      warning("blockr.ts: block registration failed: ", conditionMessage(e))
    }
  )
  invisible(NULL)
}
