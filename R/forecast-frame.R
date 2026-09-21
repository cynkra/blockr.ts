#' Turn fitted models back into a frame
#'
#' THIS IS WHERE THE INTERVAL CONVENTION LIVES, and it lives here rather than
#' on the forecast block because a block that hands out a model has the real
#' forecast object and needs no naming trick to find its bounds. It is only
#' once somebody wants the forecast as DATA that an interval has to survive as
#' something, and `(id, time, value)` has no column for it.
#'
#' So it becomes more series: `sales`, `sales_lower_80`, `sales_upper_80`.
#' The chart pairs them back into a band. Unpaired they are ordinary lines, a
#' `dplyr::filter()` downstream cannot silently destroy them, and a board that
#' never heard of forecasting draws five lines rather than crashing.
#'
#' Rejected: an attribute on the frame. It survives nothing.
#'
#' @param x A `ts_forecast`.
#' @param what `"forecast"`, `"fitted"`, `"residuals"`, `"components"` or
#'   `"accuracy"`.
#' @param levels Which intervals to include. `NULL` means the ones that were
#'   fitted.
#' @param keep_history Whether the observed series comes along.
#'
#' @return A long frame with `id`, `time` and `value`, except for
#'   `"accuracy"`, which is one row per series.
#' @export
ts_forecast_frame <- function(x, what = c("forecast", "fitted", "residuals",
                                          "components", "accuracy"),
                              levels = NULL, keep_history = TRUE) {

  stopifnot(inherits(x, "ts_forecast"))
  what <- match.arg(what)

  if (what == "accuracy") {
    return(fc_accuracy(x))
  }

  parts <- lapply(names(x), function(k) {
    fc_one_frame(x[[k]], k, what, levels, keep_history)
  })
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out[order(out$id, out$time), , drop = FALSE]
}

#' @noRd
fc_one_frame <- function(fit, key, what, levels, keep_history) {

  if (what == "fitted") {
    return(fc_rows(key, stats::fitted(fit$fc)))
  }
  if (what == "residuals") {
    return(fc_rows(key, stats::residuals(fit$fc)))
  }
  if (what == "components") {
    return(fc_components(fit, key))
  }

  have <- fit$fc$level
  lv <- if (is.null(levels)) have else intersect(as.numeric(levels), have)

  base <- if (isTRUE(keep_history)) {
    fc_rows(key, stats::ts(c(as.numeric(fit$y), as.numeric(fit$fc$mean)),
                           start = stats::start(fit$y),
                           frequency = stats::frequency(fit$y)))
  } else {
    fc_rows(key, fit$fc$mean)
  }

  bounds <- lapply(lv, function(l) {
    j <- match(l, have)
    # ANCHORED AT THE LAST OBSERVATION when the history is kept. Without it
    # the band starts one period after the line it belongs to, and the gap
    # reads as a break in the data rather than as the moment the forecast
    # begins. The interval is genuinely zero-width there, so the anchor
    # states a fact rather than smoothing one over.
    anchor <- isTRUE(keep_history)
    rbind(
      fc_rows(paste0(key, "_lower_", l), fit$fc$lower[, j], fit$y, anchor),
      fc_rows(paste0(key, "_upper_", l), fit$fc$upper[, j], fit$y, anchor)
    )
  })

  do.call(rbind, c(list(base), bounds))
}

#' Rows for one series
#'
#' @param anchor Whether to prepend the last observed point of `y`, so a
#'   bound path starts where the line it brackets is.
#' @noRd
fc_rows <- function(id, v, y = NULL, anchor = FALSE) {

  z <- if (stats::is.ts(v)) {
    v
  } else {
    stats::ts(as.numeric(v), start = stats::start(y), frequency = stats::frequency(y))
  }

  if (isTRUE(anchor) && !is.null(y)) {
    z <- stats::ts(c(as.numeric(utils::tail(y, 1L)), as.numeric(z)),
                   start = stats::tsp(z)[1L] - 1 / stats::frequency(z),
                   frequency = stats::frequency(z))
  }

  d <- suppressMessages(tsbox::ts_tbl(z))
  out <- data.frame(id = id, time = as.Date(d$time),
                    value = as.numeric(d$value), stringsAsFactors = FALSE)
  out[!is.na(out$value), , drop = FALSE]
}

#' The state paths, where the engine has them
#'
#' @noRd
fc_components <- function(fit, key) {
  st <- fit$model$states
  if (is.null(st)) {
    stop("The ", fit$engine, " engine has no component paths. ETS does.",
         call. = FALSE)
  }
  nm <- colnames(st)
  keep <- nm[nm %in% c("l", "b", "s1")]
  lab <- c(l = "level", b = "trend", s1 = "season")
  do.call(rbind, lapply(keep, function(cl) {
    fc_rows(paste0(key, " · ", lab[[cl]]), st[, cl])
  }))
}

#' One row per series, worst first
#'
#' Sorted by MAPE because the reason to look at this table is to find the
#' series that need a hand, which is the same reason `seas_triage()` sorts.
#'
#' @noRd
fc_accuracy <- function(x) {
  rows <- lapply(names(x), function(k) {
    a <- tryCatch(forecast::accuracy(x[[k]]$fc), error = function(e) NULL)
    if (is.null(a)) {
      return(data.frame(series = k, model = fc_method(x[[k]]),
                        ME = NA_real_, RMSE = NA_real_, MAE = NA_real_,
                        MPE = NA_real_, MAPE = NA_real_, MASE = NA_real_,
                        stringsAsFactors = FALSE))
    }
    a <- a[1L, , drop = FALSE]
    cols <- intersect(c("ME", "RMSE", "MAE", "MPE", "MAPE", "MASE"), colnames(a))
    cbind(
      data.frame(series = k, model = fc_method(x[[k]]), stringsAsFactors = FALSE),
      as.data.frame(a[, cols, drop = FALSE])
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  if ("MAPE" %in% names(out)) out <- out[order(-out$MAPE), , drop = FALSE]
  out
}

#' What the engine picked, in its own words
#'
#' @noRd
fc_method <- function(fit) {
  m <- fit$fc$method
  if (!is.null(m) && nzchar(m)) {
    return(as.character(m))
  }
  if (!is.null(fit$model$method)) as.character(fit$model$method) else fit$engine
}

#' Let the preview draw a model
#'
#' The seam blockr.ts already has: `ts_resolve()` is where a package says how
#' its own object turns into something tsbox reads, so the chart needs no
#' branch for forecasts and neither does `ts_frame()`.
#'
#' A forecast resolves to its own extractor's default output, history and all
#' the fitted levels, which is exactly what the chart should draw. The bounds
#' come through as `_lower_80` / `_upper_80` series and the chart pairs them
#' into bands.
#'
#' @param x A `ts_forecast`.
#' @return A long frame.
#' @export
ts_resolve.ts_forecast <- function(x) {
  ts_forecast_frame(x)
}

#' @param ... Ignored.
#' @rdname ts_forecast
#' @export
print.ts_forecast <- function(x, ...) {
  cat("<ts_forecast>", attr(x, "engine"), "· h =", attr(x, "h"),
      "· levels", paste(attr(x, "levels"), collapse = "/"), "\n")
  for (k in names(x)) {
    cat("  ", k, ": ", fc_method(x[[k]]), "\n", sep = "")
  }
  invisible(x)
}
