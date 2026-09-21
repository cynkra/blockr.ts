#' Combine time series
#'
#' Two or more series in, one block. Side by side, appended in time,
#' chain-linked, or the four arithmetic operators, all of which align on time
#' and id before they compute.
#'
#' @details
#' THIS IS THE BLOCK dplyr CANNOT REPLACE, and the reason is the `id` column.
#'
#' `tsbox::ts_c()` is not `dplyr::bind_rows()`. It builds the `id` column by
#' **deparsing its own arguments**, and inside blockr the arguments are called
#' `.arg1` and `.arg2`, so the obvious implementation produces series named
#' `.arg1`. Where the argument is a nested call, tsbox names the series after
#' the entire source text. Meanwhile `bind_rows()` on two single-series frames
#' returns twice the rows with no id column and two values per date, and
#' `ts_tbl()` then errors with "series contains duplicated values in time
#' column", which under this package's convention means the block silently
#' stops drawing.
#'
#' So: **no deparse ever decides an id.** Either the frame carries one, or
#' this block supplies one. Each input slot gets a name, defaulted from the
#' series already in that input and editable, and the expression is written
#' with those names.
#'
#' The four methods that reduce to a single series have the opposite problem:
#' the result has no `id` at all and tsbox calls it `value`. They get a
#' "Name the result" field and the expression is wrapped in [ts_name()].
#'
#' The arithmetic operators take exactly two inputs and error usefully when
#' the id structures differ, so there is nothing for this block to check.
#'
#' @param method One of `"c"` (side by side), `"bind"` (append in time),
#'   `"chain"` (chain-link), `"divide"`, `"subtract"`, `"multiply"` or
#'   `"add"`.
#' @param names Series names, one per input slot.
#' @param result Name for the result, for the methods that produce one series.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @examples
#' new_ts_combine_block(method = "chain", result = "Chained index")
#'
#' @export
new_ts_combine_block <- function(method = "c", names = character(),
                                 result = "", ...) {

  method <- match.arg(method, base::names(ts_methods()))

  blockr.core::new_transform_block(
    function(id, ...args) {
      shiny::moduleServer(
        id,
        function(input, output, session) {

          r_method <- shiny::reactiveVal(method)
          r_names <- shiny::reactiveVal(as.character(names))
          r_result <- shiny::reactiveVal(as.character(result))

          refs <- shiny::reactive(ts_dot_refs(...args))

          # A slot's default name is the series already inside it. The block
          # cannot see what the upstream block is called -- blocks get data,
          # not board state -- so the data is the only honest default.
          slot_names <- shiny::reactive({
            held <- r_names()
            vapply(seq_along(refs()), function(i) {
              if (length(held) >= i && nzchar(held[[i]])) return(held[[i]])
              ids <- ts_facts(ts_arg_value(...args, i))$ids
              if (length(ids) == 1L) ids else paste0("series", i)
            }, character(1L))
          })

          shiny::observeEvent(input$method_pick, {
            if (input$method_pick %in% base::names(ts_methods())) {
              r_method(input$method_pick)
            }
          })

          output$tiles <- shiny::renderUI({
            labels <- vapply(ts_methods(), `[[`, character(1L), "label")
            ts_tile_grid(
              session$ns("method_pick"),
              stats::setNames(list(base::names(ts_methods())), ""),
              labels, r_method()
            )
          })

          output$slots <- shiny::renderUI({
            spec <- ts_methods()[[r_method()]]
            nms <- slot_names()
            n <- length(refs())
            htmltools::tagList(
              ts_hint(spec$hint),
              if (isTRUE(spec$binary) && n != 2L) ts_hint(paste0(
                "This one takes exactly two inputs; ", n,
                if (n == 1L) " is connected." else " are connected."
              )),
              if (isTRUE(spec$named)) htmltools::div(
                class = "blockr-ts-slots",
                lapply(seq_len(n), function(i) {
                  htmltools::div(
                    class = "blockr-ts-slot",
                    htmltools::div(class = "blockr-ts-slot__ref",
                                   unname(refs())[[i]]),
                    ts_text(session$ns(paste0("name_", i)), "Series name",
                            nms[[i]])
                  )
                })
              ),
              if (spec$one) ts_fields(
                ts_text(session$ns("result"), "Name the result", r_result(),
                        placeholder = "value")
              ),
              if (spec$one) ts_hint(paste0(
                "Without a name the result is called “value”: ",
                "tsbox has no call that names a single series, so this is ",
                "ts_name(), a helper of ours."
              ))
            )
          })

          # ONE observer for all the slots, not one created per slot. Dynamic
          # observers inside renderUI stack up: every re-render adds another,
          # and they all fire.
          shiny::observe({
            n <- length(refs())
            vals <- vapply(seq_len(n), function(i) {
              v <- input[[paste0("name_", i)]]
              if (is.null(v)) NA_character_ else as.character(v)
            }, character(1L))
            if (all(is.na(vals))) return()
            keep <- slot_names()
            keep[!is.na(vals)] <- vals[!is.na(vals)]
            if (!identical(keep, r_names())) r_names(keep)
          })

          shiny::observeEvent(input$result, r_result(input$result))

          list(
            expr = shiny::reactive(
              # The VALUES are the references (`.arg1`); the NAMES are the
              # link names, which are empty for a positional link and blow up
              # `as.name()`.
              ts_combine_expr(r_method(), unname(refs()), slot_names(),
                              r_result())
            ),
            state = list(
              method = r_method, names = r_names, result = r_result
            )
          )
        }
      )
    },
    function(id) {
      ts_gear(
        shiny::NS(id),
        shiny::uiOutput(shiny::NS(id, "tiles")),
        shiny::uiOutput(shiny::NS(id, "slots")),
        open = TRUE
      )
    },
    expr_type = "bquoted",
    # Both are empty until someone types: the slot names default from the
    # data and the result name is optional. Unlisted empties wedge the block.
    allow_empty_state = c("names", "result"),
    class = "ts_combine_block",
    ...
  )
}

#' The seven ways to combine series
#' @noRd
ts_methods <- function() {
  list(
    c = list(label = "Side by side", fn = "tsbox::ts_c", one = FALSE,
             named = TRUE,
             hint = "N series in, N series out, one id each."),
    bind = list(label = "Append", fn = "tsbox::ts_bind", one = TRUE,
                hint = "One series out. The later input wins where they overlap."),
    chain = list(label = "Chain-link", fn = "tsbox::ts_chain", one = TRUE,
                 hint = "One series out, re-based onto the first at the overlap."),
    divide = list(label = "Ratio", op = "%ts/%", one = TRUE, binary = TRUE,
                  hint = "Aligned on time and id first, so the spans and the frequencies need not match."),
    subtract = list(label = "Difference", op = "%ts-%", one = TRUE,
                    binary = TRUE, hint = "Aligned the same way."),
    multiply = list(label = "Product", op = "%ts*%", one = TRUE,
                    binary = TRUE, hint = "Aligned the same way."),
    add = list(label = "Sum", op = "%ts+%", one = TRUE, binary = TRUE,
               hint = "Aligned the same way.")
  )
}

#' The expression one method writes
#' @noRd
ts_combine_expr <- function(method, refs, names, result) {
  spec <- ts_methods()[[method]]
  args <- lapply(refs, function(r) call(".", as.name(r)))

  if (isTRUE(spec$binary)) {
    if (length(args) != 2L) {
      return(quote(stop("This method needs exactly two inputs.")))
    }
    # Self-qualified, like every other expression a block writes. An
    # operator can be namespaced by calling it: `tsbox::`%ts/%`(a, b)`.
    out <- as.call(list(
      call("::", as.name("tsbox"), as.name(spec$op)),
      args[[1L]], args[[2L]]
    ))
  } else {
    # ONLY ts_c() TAKES NAMES. ts_bind() ignores them, and ts_chain() ERRORS
    # on them ("replacement has 0 rows"), so a named chain call would take a
    # working board down. The other methods reduce to one series anyway, and
    # that one is named by ts_name() below.
    if (isTRUE(spec$named)) args <- stats::setNames(args, names)
    out <- as.call(c(list(str2lang(spec$fn)), args))
  }

  if (isTRUE(spec$one) && length(result) && nzchar(result)) {
    out <- bquote(blockr.ts::ts_name(.(out), .(result)))
  }
  out
}

#' Slot references, `.arg1` and friends
#'
#' `blockr.core:::dot_arg_refs()` is not exported, and this is three lines.
#' Unnamed slots become `.arg1`, `.arg2`; a named link keeps its name.
#' @noRd
ts_dot_refs <- function(args) {
  nms <- base::names(args)
  if (is.null(nms)) nms <- character(length(args))
  unnamed <- !nzchar(nms)
  # Values are the references, names are the link names. Positional links
  # have no name, so reading the names of this vector gives `""`, which is
  # what `as.name()` refuses.
  stats::setNames(
    replace(nms, unnamed, paste0(".arg", seq_len(sum(unnamed)))),
    nms
  )
}

#' The value in one variadic slot, or NULL
#' @noRd
ts_arg_value <- function(args, i) {
  tryCatch(args[[i]](), error = function(e) NULL)
}
