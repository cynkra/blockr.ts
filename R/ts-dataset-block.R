#' A built-in time series dataset
#'
#' Every `ts` and `mts` object in an installed package, as a long frame.
#'
#' @details
#' This block exists for one reason: `blockr.core::new_dataset_block()`
#' filters its list to objects that inherit from `data.frame`, so it never
#' offers `AirPassengers`. Thirty-one of them ship with R, forty-one series
#' once the multivariate ones are counted out.
#'
#' The result is `tsbox::ts_tbl()`, not the `ts` object, because a frame is
#' what every other block on a board can work on.
#'
#' @param dataset Name of the object.
#' @param package Package to look in.
#' @param ... Forwarded to [blockr.core::new_data_block()].
#'
#' @examples
#' new_ts_dataset_block(dataset = "AirPassengers")
#'
#' @export
new_ts_dataset_block <- function(dataset = "AirPassengers",
                                 package = "datasets", ...) {

  blockr.core::new_data_block(
    function(id) {
      shiny::moduleServer(
        id,
        function(input, output, session) {

          r_package <- shiny::reactiveVal(package)
          r_dataset <- shiny::reactiveVal(dataset)

          choices <- shiny::reactive(ts_datasets(r_package()))

          shiny::observeEvent(input$package, {
            r_package(input$package)
            opts <- ts_datasets(input$package)
            if (length(opts) && !r_dataset() %in% opts) r_dataset(opts[[1L]])
          })
          shiny::observeEvent(input$dataset, r_dataset(input$dataset))

          output$controls <- shiny::renderUI({
            ts_fields(
              ts_select(session$ns("package"), "Package",
                        ts_packages_with_series(), selected = r_package()),
              ts_select(session$ns("dataset"), "Dataset", choices(),
                        selected = ts_one_of(r_dataset(), choices()))
            )
          })

          list(
            expr = shiny::reactive(
              bquote(
                tsbox::ts_tbl(.(obj)),
                list(obj = call("::", as.name(r_package()),
                                as.name(r_dataset())))
              )
            ),
            state = list(dataset = r_dataset, package = r_package)
          )
        }
      )
    },
    function(id) {
      ts_gear(shiny::NS(id), shiny::uiOutput(shiny::NS(id, "controls")),
              open = TRUE)
    },
    class = "ts_dataset_block",
    ...
  )
}

#' The `ts` objects in a package
#' @noRd
ts_datasets <- function(package) {
  objs <- tryCatch(
    utils::data(package = package)$results[, "Item"],
    error = function(e) character()
  )
  objs <- unique(sub(" .*", "", objs))
  if (!length(objs)) {
    return(character())
  }
  # Load each one out of THAT package. `getAnywhere()` searches everything on
  # the path, so asking it about a package that ships no data still finds
  # `AirPassengers`, and the block then writes `thatpackage::AirPassengers`.
  env <- new.env(parent = emptyenv())
  keep <- vapply(objs, function(x) {
    ok <- tryCatch({
      utils::data(list = x, package = package, envir = env)
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    ok && exists(x, envir = env, inherits = FALSE) &&
      inherits(get(x, envir = env, inherits = FALSE), "ts")
  }, logical(1L))
  sort(objs[keep])
}

#' Packages that actually hold a `ts` object
#'
#' Offering a package with none in it is an option that can only fail: picking
#' it leaves the dataset select with nothing to show and the expression
#' pointing at an object that is not there.
#' @noRd
ts_packages_with_series <- function() {
  have <- c("datasets", "tsbox", "astsa", "fpp2")
  have <- have[vapply(have, requireNamespace, logical(1L), quietly = TRUE)]
  unname(have[vapply(have, function(p) length(ts_datasets(p)) > 0L,
                     logical(1L))])
}
