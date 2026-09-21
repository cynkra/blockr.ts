#' Fit forecasting models to a long frame
#'
#' THE BLOCK RETURNS THE MODEL. A frame in, the fitted models out, the same
#' contract the seas block has: the preview draws the model and
#' `new_ts_forecast_series_block()` turns it back into a frame. The reason is
#' that a forecast is not one series, it is a point path, two or more interval
#' paths, the fitted values, the residuals and whatever the engine chose, and
#' collapsing that to a frame at the block boundary throws away everything the
#' second question needs.
#'
#' ALWAYS A NAMED LIST, length one when one target is picked. A multiselect
#' that changes the output type is a multiselect that breaks the board when
#' you touch it, which is why there is no `_multi` twin of this block.
#'
#' @param data A frame tsbox can read.
#' @param target Series to forecast. Empty means all of them.
#' @param engine One of `names(fc_engines())`.
#' @param h Horizon, in periods.
#' @param levels Interval levels, in per cent.
#' @param xreg Series to use as regressors, for the engines that take them.
#' @param xreg_mode How the regressors reach the horizon. `"lag"` shifts them
#'   by `lag` periods, so the values the forecast needs are already observed.
#'   `"hold"` carries the last observation forward, which UNDERSTATES the
#'   interval: it hands the model a guess dressed as an observation.
#' @param lag Shift for `xreg_mode = "lag"`. `NULL` means `h`, the only shift
#'   for which nothing has to be invented.
#'
#' @return An object of class `ts_forecast`: a named list of fits, one per
#'   target, each `list(engine, model, fc, y)`.
#' @export
ts_forecast <- function(data, target = character(), engine = "ets", h = 12L,
                        levels = c(80, 95), xreg = character(),
                        xreg_mode = c("lag", "hold"), lag = NULL) {

  engine <- match.arg(engine, names(fc_engines()))
  xreg_mode <- match.arg(xreg_mode)
  h <- as.integer(h)
  levels <- sort(unique(as.numeric(levels)))
  stopifnot(h >= 1L, length(levels) >= 1L, all(levels > 0 & levels < 100))

  d <- fc_long(data)
  ids <- unique(d$id)

  target <- as.character(target)
  target <- if (!length(target)) ids else intersect(target, ids)
  if (!length(target)) {
    stop("None of the targets are in the data. Available: ",
         paste(ids, collapse = ", "), call. = FALSE)
  }

  xreg <- setdiff(intersect(as.character(xreg), ids), target)
  spec <- fc_engines()[[engine]]
  if (length(xreg) && !spec$xreg) {
    stop("The ", spec$label, " engine takes no regressors. Drop them, or ",
         "pick an engine that does: ",
         paste(fc_engine_labels(xreg = TRUE), collapse = ", "), call. = FALSE)
  }

  out <- if (spec$joint) {
    fc_fit_joint(d, target, engine, h, levels)
  } else {
    stats::setNames(lapply(target, function(k) {
      fc_fit_one(d, k, engine, h, levels, xreg, xreg_mode, lag %||% h)
    }), target)
  }

  structure(
    out,
    engine = engine,
    h = h,
    levels = levels,
    xreg = xreg,
    class = c("ts_forecast", "list")
  )
}

#' The engines, and what each of them can be asked for
#'
#' ONE BLOCK, NOT ONE PER ENGINE. Horizon, target, levels and regressors are
#' the same question whatever fits the model, so a block per engine would be
#' four copies of one file. Only `fit` differs, and it is a few lines.
#'
#' `joint` is the real distinction and it is not cosmetic: every engine here
#' except VAR treats the target set as a loop, and VAR treats it as one
#' system. That is why the target control is a multiselect for all of them and
#' means two different things.
#'
#' @noRd
fc_engines <- function() {
  list(
    ets = list(label = "ETS", joint = FALSE, xreg = FALSE,
               hint = "Exponential smoothing, state chosen by AICc"),
    arima = list(label = "ARIMA", joint = FALSE, xreg = TRUE,
                 hint = "auto.arima, with optional regressors"),
    theta = list(label = "Theta", joint = FALSE, xreg = FALSE,
                 hint = "Univariate and hard to beat"),
    var = list(label = "VAR", joint = TRUE, xreg = FALSE,
               hint = "Joint: the targets are one system")
  )
}

#' @noRd
fc_engine_labels <- function(xreg = NULL, joint = NULL) {
  e <- fc_engines()
  if (!is.null(xreg)) e <- e[vapply(e, function(s) s$xreg == xreg, logical(1))]
  if (!is.null(joint)) e <- e[vapply(e, function(s) s$joint == joint, logical(1))]
  vapply(e, `[[`, character(1), "label")
}

#' One series, one model
#'
#' @noRd
fc_fit_one <- function(d, key, engine, h, levels, xreg, xreg_mode, lag) {

  y <- fc_ts(d, key)

  x <- NULL
  nx <- NULL
  if (length(xreg)) {
    z <- fc_xreg(d, xreg, y, h, xreg_mode, lag)
    x <- z$past
    nx <- z$future
  }

  fit <- switch(
    engine,
    ets = {
      m <- forecast::ets(y)
      list(model = m, fc = forecast::forecast(m, h = h, level = levels))
    },
    arima = {
      m <- forecast::auto.arima(y, xreg = x)
      list(model = m,
           fc = forecast::forecast(m, h = h, xreg = nx, level = levels))
    },
    theta = {
      f <- forecast::thetaf(y, h = h, level = levels)
      list(model = f$model, fc = f)
    }
  )

  c(list(engine = engine, y = y, xreg = xreg), fit)
}

#' Every target at once, as one system
#'
#' `vars::predict()` carries ONE interval per call, so a second level is a
#' second call rather than a second column. Cheap: the fit is already done.
#'
#' @noRd
fc_fit_joint <- function(d, target, engine, h, levels) {

  if (length(target) < 2L) {
    stop("VAR needs at least two targets. With one series it is an AR model, ",
         "so pick ARIMA instead.", call. = FALSE)
  }

  ys <- lapply(target, function(k) fc_ts(d, k))
  fr <- unique(vapply(ys, stats::frequency, numeric(1)))
  if (length(fr) > 1L) {
    stop("VAR needs every target at the same frequency; got ",
         paste(fr, collapse = " and "), ".", call. = FALSE)
  }

  Y <- stats::na.omit(do.call(cbind, ys))
  colnames(Y) <- target

  p <- vars::VARselect(Y, lag.max = min(12L, floor(nrow(Y) / 4)))$selection[["AIC(n)"]]
  m <- vars::VAR(Y, p = max(1L, as.integer(p)), type = "const")

  # One predict per level. The point path is identical across them, so the
  # first one supplies it and the rest only contribute their bounds.
  preds <- lapply(levels, function(l) stats::predict(m, n.ahead = h, ci = l / 100))

  stats::setNames(lapply(seq_along(target), function(i) {
    k <- target[[i]]
    y <- ys[[i]]
    first <- preds[[1L]]$fcst[[k]]
    mean <- fc_as_ts(first[, "fcst"], y)
    lower <- lapply(preds, function(p) fc_as_ts(p$fcst[[k]][, "lower"], y))
    upper <- lapply(preds, function(p) fc_as_ts(p$fcst[[k]][, "upper"], y))
    list(engine = "var", y = y, model = m, xreg = character(),
         fc = structure(
           list(mean = mean,
                lower = do.call(cbind, lower),
                upper = do.call(cbind, upper),
                level = levels,
                x = y,
                fitted = fc_var_fitted(m, k, y),
                residuals = fc_as_ts(stats::residuals(m)[, k], y, at = "fit"),
                method = paste0("VAR(", m$p, ")")),
           class = "forecast"))
  }), target)
}

#' @noRd
fc_var_fitted <- function(m, k, y) {
  f <- stats::fitted(m)[, k]
  # A VAR loses its first `p` observations to the lags, so the fitted path
  # starts later than the series does. Pad rather than shift: a fitted value
  # drawn at the wrong date is worse than a gap.
  stats::ts(c(rep(NA_real_, length(y) - length(f)), f),
            start = stats::start(y), frequency = stats::frequency(y))
}

#' A single series out of the long frame, as a `ts`
#'
#' @noRd
fc_ts <- function(d, key) {
  s <- d[d$id == key, c("time", "value"), drop = FALSE]
  if (!nrow(s)) {
    stop("No rows for series '", key, "'.", call. = FALSE)
  }
  suppressMessages(tsbox::ts_ts(s[order(s$time), , drop = FALSE]))
}

#' Put a bare numeric vector back on the calendar
#'
#' @param at `"fc"` for the periods after `y` ends, `"fit"` for the ones it
#'   covers.
#' @noRd
fc_as_ts <- function(v, y, at = c("fc", "fit")) {
  at <- match.arg(at)
  f <- stats::frequency(y)
  if (at == "fc") {
    stats::ts(as.numeric(v), start = fc_next(y), frequency = f)
  } else {
    stats::ts(c(rep(NA_real_, length(y) - length(v)), as.numeric(v)),
              start = stats::start(y), frequency = f)
  }
}

#' @noRd
fc_next <- function(y) {
  f <- stats::frequency(y)
  stats::tsp(y)[2L] + 1 / f
}

#' Regressors, and the horizon they have to cover
#'
#' A fitted regression cannot predict without values of its regressors over
#' the horizon, and this is the whole design question. `"lag"` is the only
#' mode that invents nothing: shifted by `h`, the values the forecast needs
#' were observed `h` periods ago. `"hold"` carries the last observation
#' forward and the resulting interval treats that guess as an observation,
#' which is why it is not the default.
#'
#' @noRd
fc_xreg <- function(d, xreg, y, h, mode, lag) {

  cols <- lapply(xreg, function(k) fc_ts(d, k))
  names(cols) <- xreg

  bad <- xreg[vapply(cols, function(z) stats::frequency(z) != stats::frequency(y),
                     logical(1))]
  if (length(bad)) {
    stop("Regressor(s) ", paste(bad, collapse = ", "), " are not at the same ",
         "frequency as the target.", call. = FALSE)
  }

  if (mode == "lag") {
    cols <- lapply(cols, function(z) stats::lag(z, -lag))
  }

  # ONE REGRESSOR STILL HAS TO BE A MATRIX. `cbind()` on a single `ts` hands
  # back a vector, `colnames<-` then fails, and `forecast()` would not know
  # what to match the future values against by name.
  m <- do.call(cbind, cols)
  if (is.null(dim(m))) {
    m <- stats::ts(matrix(as.numeric(m), ncol = 1L),
                   start = stats::start(m), frequency = stats::frequency(m))
  }
  colnames(m) <- xreg

  win <- stats::window(m, start = stats::start(y), end = stats::end(y),
                       extend = TRUE)
  fut <- stats::window(m, start = fc_next(y),
                       end = stats::tsp(y)[2L] + h / stats::frequency(y),
                       extend = TRUE)

  if (mode == "hold") {
    # THE VALUE CARRIED FORWARD COMES FROM THE SERIES, NOT FROM THE WINDOW.
    # `fut` is the span past the end of the target, so for a regressor that
    # ends when the target does, every cell in it is already NA and there is
    # nothing there to carry.
    held <- vapply(seq_len(ncol(m)), function(j) {
      v <- stats::na.omit(as.numeric(m[, j]))
      if (length(v)) v[[length(v)]] else NA_real_
    }, numeric(1))
    filled <- vapply(seq_len(ncol(fut)), function(j) {
      v <- as.numeric(fut[, j])
      ifelse(is.na(v), held[[j]], v)
    }, numeric(nrow(fut)))
    fut <- stats::ts(
      matrix(filled, ncol = length(xreg), dimnames = list(NULL, xreg)),
      start = fc_next(y), frequency = stats::frequency(y)
    )
  }

  if (anyNA(fut)) {
    stop("The regressors do not cover the horizon. ",
         if (mode == "lag") {
           paste0("With mode 'lag' the shift has to be at least the horizon (",
                  h, "), and it is ", lag, ".")
         } else {
           "There is nothing to carry forward: a regressor is empty."
         }, call. = FALSE)
  }

  list(past = as.matrix(win), future = as.matrix(fut))
}

#' The long frame, normalised
#'
#' @noRd
fc_long <- function(x) {
  d <- suppressMessages(tsbox::ts_tbl(x))
  nm <- names(d)
  is_time <- function(z) inherits(z, c("Date", "POSIXct", "POSIXt"))
  tcol <- utils::tail(nm[vapply(d, is_time, logical(1))], 1L)
  vcol <- utils::tail(nm[vapply(d, is.numeric, logical(1))], 1L)
  if (!length(tcol) || !length(vcol)) {
    stop("This is not something tsbox reads as a time series.", call. = FALSE)
  }
  idcols <- setdiff(nm, c(tcol, vcol))
  id <- if (length(idcols)) {
    do.call(paste, c(unname(lapply(d[idcols], as.character)), sep = " · "))
  } else {
    "value"
  }
  data.frame(id = id, time = as.Date(d[[tcol]]),
             value = as.numeric(d[[vcol]]), stringsAsFactors = FALSE)
}
