#' Give a single series a name
#'
#' tsbox has no call that names one series. `ts_c()` builds the `id` column by
#' deparsing its arguments, so it needs two of them to have anything to build
#' from: `ts_c(\`A\` = x)` on a single series drops the name and hands back
#' `time` and `value`. Anything that reduces to one series, `ts_bind()`,
#' `ts_chain()` and the arithmetic operators, therefore comes back anonymous,
#' and tsbox calls it `value`.
#'
#' @details
#' Returns a data frame rather than the class it was given, because an `id`
#' column is something only the data frame classes have. A `ts` object cannot
#' carry a series name at all.
#'
#' @param x Anything tsbox can read.
#' @param name The series name.
#' @return A tibble with `id` first.
#'
#' @examples
#' ts_name(tsbox::ts_pc(datasets::AirPassengers), "Monthly change")
#'
#' @export
ts_name <- function(x, name) {
  stopifnot(is.character(name), length(name) == 1L, nzchar(name))
  d <- suppressMessages(tsbox::ts_tbl(x))
  nm <- names(d)
  # Naming a frame that already holds several series would collapse them into
  # one id and leave several values per period, which tsbox then refuses to
  # read. Better to say so here than to hand back a frame that stops drawing.
  if ("id" %in% nm && length(unique(d$id)) > 1L) {
    stop("ts_name() names ONE series; this frame holds ",
         length(unique(d$id)), ". Use tsbox::ts_c() with named arguments.",
         call. = FALSE)
  }
  d$id <- name
  # Id columns go first. tsbox reads by name so it would cope either way, but
  # a frame whose columns are `time, value, id` is one the position-based
  # guards in this package and elsewhere will get wrong.
  d[c("id", setdiff(nm, "id"))]
}
