#' Transform a time series
#'
#' One block for all sixteen unary tsbox verbs. Pick the function from the
#' tile grid in the gear; the arguments under it are the ones that function
#' takes, and they are built from the data.
#'
#' @details
#' ONE BLOCK, NOT SIXTEEN. Every verb here is `f(data, a scalar or two)` and
#' leaves the ids untouched, so sixteen constructors would be sixteen copies
#' of one file. Picking a different function never rewires the board either: a
#' percentage change you decide should have been year-on-year is one click,
#' where two blocks would be a delete and two reconnections. Same call as
#' "histogram is a chart type, not a histogram block".
#'
#' THE CONTROLS COME FROM `tsbox::ts_summary()`, which is the part worth the
#' rewrite. The base-period list is bounded by the series, the frequency list
#' offers only frequencies coarser than the one the data has, and "one year"
#' in the lag list is 12 or 4 because the data said so. The block this
#' replaces re-derived a piece of that by hand in every constructor, which is
#' why the span block alone was 319 lines.
#'
#' Arguments left at their tsbox default are left out of the expression, so
#' what you read back is the choices you made.
#'
#' @param verb One of the keys of the function grid: `"pc"`, `"pcy"`,
#'   `"pca"`, `"diff"`, `"diffy"`, `"compound"`, `"index"`, `"scale"`,
#'   `"span"`, `"lag"`, `"frequency"`, `"regular"`, `"first_of_period"`,
#'   `"trend"`, `"na_omit"` or `"na_interpolation"`.
#' @param base Base period for `"index"`, as a date string.
#' @param start,end Bounds for `"span"`, as date strings.
#' @param by Shift for `"lag"`, in periods.
#' @param to,aggregate Target frequency and aggregation for `"frequency"`.
#' @param center,scale Passed to [tsbox::ts_scale()].
#' @param denominator Passed to [tsbox::ts_compound()].
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @examples
#' new_ts_transform_block(verb = "pcy")
#' new_ts_transform_block(verb = "index", base = "1954-01-01")
#'
#' @export
new_ts_transform_block <- function(verb = "pcy", base = character(),
                                   start = character(), end = character(),
                                   by = 1, to = character(),
                                   aggregate = "mean", center = TRUE,
                                   scale = TRUE, denominator = 100, ...) {

  verb <- match.arg(verb, ts_verb_keys())

  blockr.core::new_transform_block(
    function(id, data) {
      shiny::moduleServer(
        id,
        function(input, output, session) {

          r_verb <- shiny::reactiveVal(verb)
          r_base <- shiny::reactiveVal(as.character(base))
          r_start <- shiny::reactiveVal(as.character(start))
          r_end <- shiny::reactiveVal(as.character(end))
          r_by <- shiny::reactiveVal(as.numeric(by))
          r_to <- shiny::reactiveVal(as.character(to))
          r_aggregate <- shiny::reactiveVal(as.character(aggregate))
          r_center <- shiny::reactiveVal(isTRUE(center))
          r_scale <- shiny::reactiveVal(isTRUE(scale))
          r_denominator <- shiny::reactiveVal(as.numeric(denominator))

          facts <- shiny::reactive(ts_facts(data()))

          shiny::observeEvent(input$verb_pick, {
            if (input$verb_pick %in% ts_verb_keys()) r_verb(input$verb_pick)
          })

          # Rendered ONCE. The grid's only reactive content is which tile
          # carries `is-active`, and the tile moves that itself on click, so
          # re-rendering sixteen tiles to shift one class is a round trip
          # that buys nothing and blanks the grid mid-click.
          output$tiles <- shiny::renderUI({
            labels <- vapply(ts_verbs(), `[[`, character(1L), "label")
            ts_tile_grid(session$ns("verb_pick"), ts_verb_groups(), labels,
                         shiny::isolate(r_verb()))
          })

          # The argument row is rebuilt when the verb changes or the data
          # does, because both decide what the controls are allowed to offer.
          # THE VALUES ARE SEEDS, NOT DEPENDENCIES. Each one is written by
          # the observeEvent on the input it seeds, so reading them live
          # means every keystroke rebuilds the row and destroys the control
          # being typed in. What legitimately changes the shape of the row
          # is the verb and the data.
          output$args <- shiny::renderUI({
            vals <- shiny::isolate(list(
              base = r_base(), start = r_start(), end = r_end(),
              by = r_by(), to = r_to(), aggregate = r_aggregate(),
              center = r_center(), scale = r_scale(),
              denominator = r_denominator()
            ))
            ts_verb_fields(session$ns, r_verb(), facts(), vals)
          })

          # Each control writes its own reactiveVal. `ignoreInit` is wrong
          # here: renderUI creates the input with the restored value and
          # Shiny reports it once, which is the value we already hold, so the
          # write is a no-op rather than a loop.
          shiny::observeEvent(input$base, r_base(input$base))
          shiny::observeEvent(input$start, r_start(input$start))
          shiny::observeEvent(input$end, r_end(input$end))
          shiny::observeEvent(input$by, r_by(input$by))
          shiny::observeEvent(input$to, r_to(input$to))
          shiny::observeEvent(input$aggregate, r_aggregate(input$aggregate))
          shiny::observeEvent(input$center, r_center(input$center))
          shiny::observeEvent(input$scale, r_scale(input$scale))
          shiny::observeEvent(input$denominator, r_denominator(input$denominator))

          list(
            expr = shiny::reactive(
              ts_verb_expr(r_verb(), list(
                base = r_base(), start = r_start(), end = r_end(),
                by = r_by(), to = r_to(), aggregate = r_aggregate(),
                center = r_center(), scale = r_scale(),
                denominator = r_denominator()
              ))
            ),
            state = list(
              verb = r_verb, base = r_base, start = r_start, end = r_end,
              by = r_by, to = r_to, aggregate = r_aggregate,
              center = r_center, scale = r_scale,
              denominator = r_denominator
            )
          )
        }
      )
    },
    function(id) {
      ts_gear(
        shiny::NS(id),
        shiny::uiOutput(shiny::NS(id, "tiles")),
        shiny::uiOutput(shiny::NS(id, "args")),
        open = TRUE
      )
    },
    dat_val = function(data) ts_validate_series(data),
    # See `ts_verb_expr()`: the expression carries `.(data)`, which core
    # replaces with the upstream block's name rather than binding it.
    expr_type = "bquoted",
    # Four of the ten state values are legitimately empty: a span with no
    # bounds is the whole series, an index with no base is tsbox's own
    # default. Leaving them off this list does not error, it WEDGES: the
    # block stops producing a result and everything downstream freezes with
    # "waiting for its inputs to be set".
    allow_empty_state = c("base", "start", "end", "to"),
    class = "ts_transform_block",
    ...
  )
}

#' The controls one verb needs, given what the data is
#'
#' @param ns Namespace function.
#' @param verb Verb key.
#' @param facts Result of [ts_facts()].
#' @param state Current argument values.
#' @noRd
ts_verb_fields <- function(ns, verb, facts, state) {
  spec <- ts_verbs()[[verb]]
  if (!length(spec$args)) {
    return(NULL)
  }
  if (!isTRUE(facts$ok)) {
    return(NULL)
  }

  periods <- ts_period_labels(facts$periods, facts$freq)

  fields <- switch(
    verb,
    index = list(
      ts_select(ns("base"), "Base period", periods,
                selected = ts_one_of(state$base, periods))
    ),
    span = list(
      ts_select(ns("start"), "Start", c("First" = "", periods),
                selected = ts_one_of(state$start, periods, "")),
      ts_select(ns("end"), "End", c("Last" = "", periods),
                selected = ts_one_of(state$end, periods, ""))
    ),
    lag = list(
      ts_select(ns("by"), "Shift by", ts_lag_choices(facts$freq),
                selected = as.character(state$by))
    ),
    frequency = list(
      ts_select(ns("to"), "To", ts_coarser(facts$freq),
                selected = ts_one_of(state$to, ts_coarser(facts$freq))),
      ts_select(ns("aggregate"), "Aggregate by",
                c(Mean = "mean", Sum = "sum", First = "first", Last = "last"),
                selected = state$aggregate)
    ),
    scale = list(
      ts_check(ns("center"), "Subtract the mean", state$center),
      ts_check(ns("scale"), "Divide by the standard deviation", state$scale)
    ),
    compound = list(
      ts_select(ns("denominator"), "Rates are in",
                c("Percent (100)" = "100", "Fractions (1)" = "1"),
                selected = as.character(state$denominator))
    ),
    list()
  )

  do.call(ts_fields, unname(fields))
}

#' Lag choices that mean something at this frequency
#' @noRd
ts_lag_choices <- function(freq) {
  out <- c("1 period" = "1", "-1 period (lead)" = "-1")
  if (!is.na(freq) && freq > 1) {
    out <- c(
      out,
      stats::setNames(as.character(freq),
                      paste0(freq, " periods (one year)")),
      stats::setNames(as.character(-freq),
                      paste0(-freq, " periods (one year ahead)"))
    )
  }
  out[order(as.numeric(out))]
}

#' Keep a stored value only if it is still on offer
#'
#' A board saved against a monthly series and reopened against a quarterly
#' one holds a base period that no longer exists. Falling back to the first
#' choice is right; passing the stale one to `selectInput()` silently selects
#' nothing.
#' @noRd
ts_one_of <- function(value, choices, fallback = NULL) {
  value <- as.character(value)
  if (length(value) && nzchar(value) && value %in% as.character(choices)) {
    return(value)
  }
  if (!is.null(fallback)) {
    return(fallback)
  }
  if (!length(choices)) NULL else unname(choices)[[1L]]
}

#' Refuse data tsbox cannot read, and say what to do about it
#' @noRd
ts_validate_series <- function(data) {
  if (isTRUE(ts_facts(data)$ok)) {
    return(invisible(TRUE))
  }
  stop(
    "This is not a time series tsbox can read. It needs one time column and ",
    "one value column, at most one value per period per series. Use a ",
    "blockr.dplyr select to drop any extra numeric column and a rename to ",
    "`time` and `value`.",
    call. = FALSE
  )
}
