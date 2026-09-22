#' What the data says about itself
#'
#' Every control in these blocks is built from `tsbox::ts_summary()` rather
#' than from a fixed list: the base-period select is bounded by the series,
#' the frequency select offers only frequencies coarser than the one the data
#' has, and "one year" in the lag select is 12 or 4 because the data said so.
#'
#' The old blocks each re-derived a piece of this by hand, which is most of
#' why the span block alone was 319 lines.
#'
#' @param data A block input.
#' @return A list with `ok`, and when `ok`, `freq`, `unit`, `periods`,
#'   `start`, `end` and `ids`. Never errors.
#' @noRd
ts_facts <- function(data) {
  none <- list(ok = FALSE)
  if (is.null(data) || inherits(data, "tbl_lazy")) {
    return(none)
  }
  d <- tryCatch(
    suppressMessages(tsbox::ts_tbl(data)),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (is.null(d) || !nrow(d)) {
    return(none)
  }
  s <- tryCatch(suppressMessages(tsbox::ts_summary(d)), error = function(e) NULL)
  if (is.null(s) || !nrow(s)) {
    return(none)
  }

  # ts_summary() reports one row per series. The controls need one answer, so
  # take the finest frequency present: a control that offers quarterly to a
  # frame holding a monthly and a quarterly series is right about the monthly
  # one and harmless about the other.
  freq <- suppressWarnings(max(s$freq, na.rm = TRUE))
  if (!is.finite(freq)) freq <- NA_real_

  times <- sort(unique(d[[if ("time" %in% names(d)) "time" else
                          names(d)[[length(names(d)) - 1L]]]]))

  list(
    ok = TRUE,
    freq = freq,
    unit = as.character(s$diff[[1L]]),
    periods = times,
    start = min(s$start),
    end = max(s$end),
    ids = if ("id" %in% names(d)) unique(as.character(d$id)) else character()
  )
}

#' The frequencies a series can be aggregated to
#'
#' Only coarser ones. Offering "monthly" to a monthly series is an option that
#' does nothing, and offering "daily" is one that fails.
#'
#' @param freq Observations per year.
#' @noRd
ts_coarser <- function(freq) {
  steps <- c(Daily = 365, Weekly = 52, Monthly = 12, Quarterly = 4,
             Annual = 1)
  to <- c(Daily = "day", Weekly = "week", Monthly = "month",
          Quarterly = "quarter", Annual = "year")
  if (is.na(freq)) return(to)
  to[steps < freq]
}

#' Period labels for a select, at the resolution the data has
#' @noRd
ts_period_labels <- function(times, freq) {
  fmt <- if (isTRUE(freq <= 1)) "%Y" else if (isTRUE(freq <= 4)) "%Y Q%q" else
    if (isTRUE(freq <= 12)) "%Y-%m" else "%Y-%m-%d"
  lab <- if (identical(fmt, "%Y Q%q")) {
    paste0(format(times, "%Y"), " Q", (as.integer(format(times, "%m")) - 1L) %/% 3L + 1L)
  } else {
    format(times, fmt)
  }
  stats::setNames(as.character(times), lab)
}

#' What the data says about one slot, in one line
#'
#' The ids, the frequency and the span, which together answer "which input is
#' this" without the block having to see the board. `no id column` is the
#' load-bearing case: it is the only slot the combine block may name.
#'
#' @param f A [ts_facts()] list.
#' @param ids Whether to lead with the ids. A row that shows the ids as its
#'   name says them once, not twice.
#' @return A single string.
#' @noRd
ts_facts_line <- function(f, ids = TRUE) {

  if (!isTRUE(f$ok)) {
    return("no series")
  }

  ids <- if (!isTRUE(ids)) "" else ts_ids_label(f)

  span <- if (is.null(f$start) || is.na(f$start)) {
    NULL
  } else {
    yrs <- format(c(f$start, f$end), "%Y")
    if (identical(yrs[[1L]], yrs[[2L]])) yrs[[1L]] else
      paste0(yrs[[1L]], "–", yrs[[2L]])
  }

  parts <- c(ids, ts_freq_label(f$freq), span)

  paste(parts[nzchar(parts)], collapse = " · ")
}

#' The ids a frame carries, or the fact that it carries none
#'
#' Two are listed and the rest counted: three ids at 11.5px is already wider
#' than the name beside it.
#' @noRd
ts_ids_label <- function(f) {

  if (!isTRUE(f$ok) || !length(f$ids)) {
    return("no id column")
  }

  if (length(f$ids) > 2L) {
    paste0(paste(f$ids[1:2], collapse = ", "), " +", length(f$ids) - 2L)
  } else {
    paste(f$ids, collapse = ", ")
  }
}

#' The word for a frequency
#' @noRd
ts_freq_label <- function(freq) {
  if (is.null(freq) || !is.finite(freq)) {
    return("")
  }
  words <- c("365" = "daily", "52" = "weekly", "12" = "monthly",
             "4" = "quarterly", "2" = "half-yearly", "1" = "annual")
  # `[[` on a name that is not there ERRORS; `[` gives NA, which is the
  # branch this needs.
  hit <- unname(words[as.character(freq)])
  if (is.na(hit)) paste0(signif(freq, 3), "/year") else hit
}
