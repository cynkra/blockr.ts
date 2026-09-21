#' The sixteen unary tsbox verbs, as one table
#'
#' Every one of these takes a series and gives back a series with the ids
#' untouched, and every one is `f(data, a scalar or two)`. That is the whole
#' reason they are one block rather than sixteen: the shape is identical, so
#' sixteen constructors would be sixteen copies of the same file.
#'
#' The icons show what the verb does to a series rather than standing for its
#' name. Sixteen name-glyphs cannot stay apart at 16px; sixteen glyphs showing
#' a line being indexed, cropped, thinned or bridged can.
#'
#' @name ts-verbs
#' @keywords internal
NULL

#' @noRd
ts_verb_groups <- function() {
  list(
    Change = c("pc", "pcy", "pca", "diff", "diffy", "compound"),
    Level = c("index", "scale"),
    `Time axis` = c("span", "lag", "frequency", "regular", "first_of_period"),
    Smoothing = "trend",
    Gaps = c("na_omit", "na_interpolation")
  )
}

#' @noRd
ts_verbs <- function() {
  list(
    pc = list(label = "Period %", fn = "ts_pc"),
    pcy = list(label = "Year %", fn = "ts_pcy"),
    pca = list(label = "Annualised", fn = "ts_pca"),
    diff = list(label = "Difference", fn = "ts_diff"),
    diffy = list(label = "Year diff", fn = "ts_diffy"),
    compound = list(label = "Compound", fn = "ts_compound",
                    args = "denominator"),
    index = list(label = "Index", fn = "ts_index", args = "base"),
    scale = list(label = "Scale", fn = "ts_scale",
                 args = c("center", "scale")),
    span = list(label = "Span", fn = "ts_span", args = c("start", "end")),
    lag = list(label = "Lag", fn = "ts_lag", args = "by"),
    frequency = list(label = "Frequency", fn = "ts_frequency",
                     args = c("to", "aggregate")),
    regular = list(label = "Regular", fn = "ts_regular"),
    first_of_period = list(label = "Period start", fn = "ts_first_of_period"),
    trend = list(label = "Trend", fn = "ts_trend"),
    na_omit = list(label = "Drop gaps", fn = "ts_na_omit"),
    na_interpolation = list(label = "Interpolate",
                            fn = "ts_na_interpolation")
  )
}

#' @noRd
ts_verb_keys <- function() names(ts_verbs())

#' The expression one verb writes
#'
#' Built as a call, never as text run through `parse()`. Arguments left at
#' their tsbox default are left out, so the expression a user reads is the
#' choices they made and nothing else.
#'
#' @param verb A key of [ts_verbs()].
#' @param state Named list of the block's argument state.
#' @param data Symbol for the input.
#' Is this argument value something to write into the call?
#' @noRd
ts_is_set <- function(v) {
  if (is.null(v) || !length(v) || is.na(v[[1L]])) {
    return(FALSE)
  }
  if (is.character(v)) nzchar(v[[1L]]) else TRUE
}

#' @noRd
ts_verb_expr <- function(verb, state, data = as.name("data")) {
  spec <- ts_verbs()[[verb]]
  args <- list()

  for (a in spec$args) {
    v <- state[[a]]
    # Unset comes in three shapes and all of them mean "leave the argument
    # out": NULL, `character(0)` from a constructor default, and `""` from a
    # select whose first option is the empty one. `!nzchar(character(0))` is
    # `logical(0)`, which is what made `if` complain rather than skip.
    if (!ts_is_set(v)) next
    args[[a]] <- switch(
      a,
      base = as.character(v),
      start = as.character(v),
      end = as.character(v),
      by = as.numeric(v),
      to = as.character(v),
      aggregate = as.character(v),
      denominator = as.numeric(v),
      center = isTRUE(v),
      scale = isTRUE(v),
      v
    )
  }

  # A boolean left at its default adds nothing to the expression.
  if (identical(verb, "scale")) {
    if (isTRUE(args$center)) args$center <- NULL
    if (isTRUE(args$scale)) args$scale <- NULL
  }
  if (identical(verb, "compound") && identical(args$denominator, 100)) {
    args$denominator <- NULL
  }
  if (identical(verb, "lag") && identical(args$by, 1)) {
    args$by <- NULL
  }
  if (identical(verb, "frequency") && identical(args$aggregate, "mean")) {
    args$aggregate <- NULL
  }

  as.call(c(
    list(call("::", as.name("tsbox"), as.name(spec$fn)), data),
    args
  ))
}
