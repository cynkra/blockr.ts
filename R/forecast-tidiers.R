#' broom methods for an ETS model
#'
#' broom covers `Arima` and seasonal 1.11.0 registers `tidy.seas`,
#' `glance.seas` and `augment.seas`, so `ets` is the one engine here with no
#' maintained tidier anywhere. `sweep` has `sw_tidy` and has not moved in
#' years. These are the twenty lines that close the gap, so the hamburger
#' shows the same two tables whatever fitted the model.
#'
#' `ets()` reports no standard errors: the parameters are chosen by numerical
#' likelihood maximisation and the object carries no covariance matrix. The
#' column is absent rather than filled with `NA`, so nothing downstream
#' mistakes silence for a number.
#'
#' @param x An `ets` object.
#' @param ... Ignored, for compatibility with the generic.
#' @return A data frame: `term` and `estimate` for `tidy()`, one row of fit
#'   statistics for `glance()`.
#' @name ets-tidiers
NULL

#' @rdname ets-tidiers
#' @export
tidy.ets <- function(x, ...) {
  p <- x$par
  data.frame(
    term = names(p),
    estimate = as.numeric(p),
    stringsAsFactors = FALSE
  )
}

#' @rdname ets-tidiers
#' @export
glance.ets <- function(x, ...) {
  data.frame(
    sigma = sqrt(x$sigma2),
    logLik = as.numeric(x$loglik),
    AIC = as.numeric(x$aic),
    AICc = as.numeric(x$aicc),
    BIC = as.numeric(x$bic),
    nobs = length(x$x),
    stringsAsFactors = FALSE
  )
}

#' Whatever the engine has, as two small frames
#'
#' Prefers broom, because broom's method is the one that stays right when the
#' model class changes under us. Falls back to `coef()` so a model class
#' nobody has tidied still shows something rather than an error in a preview.
#'
#' @param fit One element of a `ts_forecast`.
#' @noRd
fc_tidy <- function(fit) {
  m <- fit$model
  out <- tryCatch(broom::tidy(m), error = function(e) NULL)
  if (!is.null(out) && nrow(out)) {
    return(as.data.frame(out))
  }
  cf <- tryCatch(stats::coef(m), error = function(e) NULL)
  if (is.null(cf) || !length(cf)) {
    return(NULL)
  }
  data.frame(term = names(cf), estimate = as.numeric(cf),
             stringsAsFactors = FALSE)
}

#' @noRd
fc_glance <- function(fit) {
  out <- tryCatch(broom::glance(fit$model), error = function(e) NULL)
  if (!is.null(out) && nrow(out)) {
    return(as.data.frame(out))
  }
  NULL
}

#' Register an S3 method for a generic that may not be installed
#'
#' broom is a Suggests: the methods above are useful to anyone who has it and
#' this package must load for anyone who does not. The standard snippet, from
#' vctrs, rather than a `@export` that would make broom a hard dependency.
#'
#' @noRd
s3_register <- function(generic, class, method = NULL) {
  stopifnot(is.character(generic), length(generic) == 1L)
  stopifnot(is.character(class), length(class) == 1L)

  pieces <- strsplit(generic, "::")[[1L]]
  stopifnot(length(pieces) == 2L)
  package <- pieces[[1L]]
  generic <- pieces[[2L]]

  caller <- parent.frame()

  get_method <- function(env) {
    if (is.null(method)) get(paste0(generic, ".", class), envir = env) else method
  }

  register <- function(...) {
    envir <- asNamespace(package)
    method_fn <- get_method(caller)
    registerS3method(generic, class, method_fn, envir = envir)
  }

  if (isNamespaceLoaded(package)) {
    register()
  }

  setHook(packageEvent(package, "onLoad"), function(...) register())
  invisible()
}
