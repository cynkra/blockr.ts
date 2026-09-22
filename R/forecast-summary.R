#' The model behind the chart
#'
#' What the hamburger shows when the result is a `ts_forecast`. For every
#' other result that side of the switch is the app's own table preview; a
#' model is not a frame, so rendering one as a table would print a list.
#'
#' @details
#' ONE TARGET GETS THE MODEL, SEVERAL GET THE TRIAGE. With one series the
#' useful thing is the fit itself: what was chosen, its parameters, how well
#' it reproduces the history. With twenty the useful thing is which two need a
#' hand, which is a sorted table and not twenty tables. Same call
#' `seas_triage()` makes in blockr.seasonal, for the same reason.
#'
#' The numbers come from broom where broom has a method and from `coef()`
#' where it does not, see `fc_tidy()`. ETS is the gap this package fills
#' itself, in `tidy.ets()`.
#'
#' @param x A `ts_forecast`.
#' @return A `shiny.tag`.
#' @noRd
fc_model_view <- function(x) {
  htmltools::tagList(
    fc_summary_css(),
    htmltools::div(
      class = "blockr-ts-model",
      if (length(x) == 1L) fc_model_one(x[[1L]], names(x)[[1L]])
      else fc_model_many(x)
    )
  )
}

#' @noRd
fc_model_one <- function(fit, key) {

  g <- fc_glance(fit)
  t <- fc_tidy(fit)
  a <- tryCatch(forecast::accuracy(fit$fc), error = function(e) NULL)

  htmltools::tagList(
    htmltools::div(
      class = "blockr-ts-model__head",
      htmltools::tags$b(fc_method(fit)),
      htmltools::span(paste0(" \u00b7 ", length(fit$y), " observations"))
    ),
    if (!is.null(g)) fc_stat_strip(g),
    if (!is.null(t) && nrow(t)) {
      htmltools::tagList(
        htmltools::div(class = "blockr-ts-model__h", "Parameters"),
        fc_table(fc_round(t))
      )
    },
    if (!is.null(a)) {
      htmltools::tagList(
        htmltools::div(class = "blockr-ts-model__h", "Training accuracy"),
        fc_table(fc_round(as.data.frame(a[1L, , drop = FALSE])))
      )
    }
  )
}

#' @noRd
fc_model_many <- function(x) {
  htmltools::tagList(
    htmltools::div(
      class = "blockr-ts-model__head",
      htmltools::tags$b(paste(length(x), "models")),
      htmltools::span(paste0(" \u00b7 ", attr(x, "engine"),
                             " \u00b7 h = ", attr(x, "h")))
    ),
    htmltools::div(class = "blockr-ts-model__h", "Worst first, by MAPE"),
    fc_table(fc_round(fc_accuracy(x)))
  )
}

#' The fit statistics, as a strip rather than a one-row table
#'
#' Five numbers with five headings read as a strip and not as data; a table
#' with one row invites you to look for the second.
#'
#' @noRd
fc_stat_strip <- function(g) {
  htmltools::div(
    class = "blockr-ts-model__strip",
    lapply(names(g), function(k) {
      htmltools::div(
        htmltools::div(class = "blockr-ts-model__dt", k),
        htmltools::div(class = "blockr-ts-model__dd", fc_fmt(g[[k]]))
      )
    })
  )
}

#' @noRd
fc_table <- function(d) {
  htmltools::tags$table(
    class = "blockr-ts-model__tbl",
    htmltools::tags$thead(
      htmltools::tags$tr(lapply(names(d), function(k) {
        htmltools::tags$th(
          class = if (is.numeric(d[[k]])) "num",
          k
        )
      }))
    ),
    htmltools::tags$tbody(
      lapply(seq_len(nrow(d)), function(i) {
        htmltools::tags$tr(lapply(names(d), function(k) {
          htmltools::tags$td(
            class = if (is.numeric(d[[k]])) "num" else "mono",
            fc_fmt(d[[i, k]])
          )
        }))
      })
    )
  )
}

#' @noRd
fc_round <- function(d) {
  num <- vapply(d, is.numeric, logical(1))
  d[num] <- lapply(d[num], function(v) signif(v, 4L))
  d
}

#' @noRd
fc_fmt <- function(v) {
  if (is.numeric(v)) format(v, big.mark = "", trim = TRUE) else as.character(v)
}

#' @noRd
fc_summary_css <- function() {
  htmltools::tags$style(htmltools::HTML("
    .blockr-ts-model { font-size: 13px; }
    .blockr-ts-model__head { margin-bottom: 10px; color: #3c3c43; }
    .blockr-ts-model__head span { color: #67676c; }
    .blockr-ts-model__h {
      font-size: 11px;
      font-weight: 500;
      color: #67676c;
      text-transform: uppercase;
      letter-spacing: .05em;
      margin: 16px 0 6px;
    }
    .blockr-ts-model__strip {
      display: flex;
      flex-wrap: wrap;
      gap: 0 22px;
      margin-bottom: 4px;
    }
    .blockr-ts-model__dt { font-size: 11px; color: #67676c; }
    .blockr-ts-model__dd { font-variant-numeric: tabular-nums; font-weight: 500; }
    .blockr-ts-model__tbl {
      width: 100%;
      border-collapse: collapse;
      font-variant-numeric: tabular-nums;
    }
    .blockr-ts-model__tbl th {
      text-align: left;
      font-weight: 500;
      font-size: 11px;
      color: #67676c;
      text-transform: uppercase;
      letter-spacing: .04em;
      padding: 4px 8px 4px 0;
      border-bottom: 1px solid rgba(128, 128, 135, .28);
    }
    .blockr-ts-model__tbl td {
      padding: 3px 8px 3px 0;
      border-bottom: 1px solid rgba(128, 128, 135, .12);
    }
    .blockr-ts-model__tbl .num { text-align: right; }
    .blockr-ts-model__tbl .mono {
      font-family: 'SF Mono', 'Fira Code', Consolas, Monaco, monospace;
      font-size: 12px;
    }
  "))
}

#' How a fitted model reads
#'
#' The other side of the chart/table switch, for a result that is a model
#' rather than a frame. [ts_resolve()] says how a class DRAWS; this says how
#' it READS, and a class that answers both behaves on the time series display
#' wherever it lands.
#'
#' The default returns `NULL`, which means "not a model, use the app's table
#' preview". That is what keeps this additive: every existing result is
#' unaffected.
#'
#' @param x A block result.
#' @return A `shiny.tag` or `tagList` for a model, `NULL` for anything else.
#' @export
#' @examples
#' ts_model_view(datasets::AirPassengers)
ts_model_view <- function(x) {
  UseMethod("ts_model_view")
}

#' @export
ts_model_view.default <- function(x) {
  NULL
}

#' @export
ts_model_view.ts_forecast <- function(x) {
  fc_model_view(x)
}
