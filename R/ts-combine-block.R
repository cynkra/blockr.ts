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
#' **A slot whose frame already carries an `id` column needs no name**, and
#' will not take one: `ts_c(P = x, Q = y)` with a two-id `x` returns `a | b |
#' Q`, so the ids win and `P` is dropped silently. That holds for one id as
#' well as for many, which is why the row appears only where
#' `length(ts_facts(x)$ids) == 0`.
#'
#' The six methods that reduce to a single series get no naming UI at all.
#' Their result has no `id` and tsbox calls it `value`; naming it here would
#' be a rename dressed as a combine, and tsbox cannot name a single series
#' anyway (see [ts_name()], which is what such a rename needs).
#'
#' The arithmetic operators take exactly two inputs and error usefully when
#' the id structures differ, so there is nothing for this block to check.
#'
#' @param method One of `"c"` (side by side), `"bind"` (append in time),
#'   `"chain"` (chain-link), `"divide"`, `"subtract"`, `"multiply"` or
#'   `"add"`.
#' @param names Series names, one per input slot. Interim: these belong on
#'   the link (`link$input`), and the whole argument goes away when a link can
#'   be renamed in the UI.
#' @param result LEGACY. Was the name for the methods that produce one series.
#'   Accepted so old boards restore, ignored otherwise: use [ts_name()] or a
#'   downstream rename instead.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @examples
#' new_ts_combine_block(method = "chain")
#'
#' @export
new_ts_combine_block <- function(method = "c", names = character(),
                                 result = NULL, ...) {

  method <- match.arg(method, base::names(ts_methods()))

  # LEGACY, mapped in one place: the block no longer names its result, but
  # core requires one state entry per formal and old boards saved this one.
  # Quiet on purpose -- no deprecation warning until there is a policy.
  result <- if (is.null(result)) "" else as.character(result)

  blockr.core::new_transform_block(
    function(id, ...args) {
      shiny::moduleServer(
        id,
        function(input, output, session) {

          r_method <- shiny::reactiveVal(method)
          r_names <- shiny::reactiveVal(as.character(names))
          # Held, never read: see the LEGACY note on the formal.
          r_result <- shiny::reactiveVal(result)

          refs <- shiny::reactive(ts_dot_refs(...args))

          facts <- shiny::reactive(
            lapply(seq_along(refs()), function(i) {
              ts_facts(ts_arg_value(...args, i))
            })
          )

          # THE ID COLUMNS HAVE TO MATCH, AND tsbox COMPARES THEM BY NAME.
          # Two frames that are both perfectly good single series refuse to
          # combine when they disagree about that: one carrying `id` out of a
          # model extractor beside one carrying `seas_adj` out of the file it
          # was read from, or -- the commoner case -- one that has an id
          # column beside one that has none at all. Both fail with "[id]
          # columns are not identical", which is true and unhelpful: neither
          # frame is wrong, and neither is the user's to fix. A rename block
          # to fix it is the plumbing this block exists to avoid.
          #
          # Every slot with at most ONE id column can be brought to the same
          # shape. A frame that has one gets it renamed; a frame that has
          # none gets the slot's name as its id, which is what the name was
          # for. With several id columns per frame there is no way to tell
          # which lines up with which without guessing, and tsbox's own error
          # says more than a wrong guess would.
          id_cols <- shiny::reactive(
            lapply(seq_along(refs()), function(i) {
              ts_layout(ts_arg_value(...args, i))$ids
            })
          )

          normalise <- shiny::reactive({
            ids <- id_cols()
            if (length(ids) < 2L || !all(lengths(ids) <= 1L)) {
              return(logical())
            }
            # Two shapes already agree and are left alone. Every slot has
            # an id column under the same name, which tsbox is happy with.
            # Or NO slot has one, which is the case the argument names were
            # written for: `ts_c(a = x, b = y)` names both.
            n <- lengths(ids)
            if (all(n == 0L)) {
              return(logical())
            }
            if (all(n == 1L) && length(unique(unlist(ids))) == 1L) {
              return(logical())
            }
            n == 1L
          })

          # THE FRAME WINS. tsbox keeps an existing `id` column and drops the
          # argument name, so a slot that already holds named series has
          # nothing to name and must not offer a field that does nothing.
          nameable <- shiny::reactive(
            vapply(facts(), function(f) !length(f$ids), logical(1L))
          )

          # A slot's default name is what the slot is already called, which
          # is `.arg1` until a link is named. The block cannot see what the
          # upstream block is called -- blocks get data, not board state --
          # and once `link$input` carries a name, the ref IS that name and
          # this default becomes the right answer with nothing typed.
          slot_names <- shiny::reactive({
            held <- r_names()
            rf <- unname(refs())
            vapply(seq_along(rf), function(i) {
              if (length(held) >= i && nzchar(held[[i]])) held[[i]] else rf[[i]]
            }, character(1L))
          })

          # What the expression may name: the others go in positionally, and
          # an unnamed slot is only safe BECAUSE it has ids -- an unnamed
          # id-less frame is the deparse this block exists to stop.
          expr_names <- shiny::reactive(
            replace(slot_names(), !nameable(), NA_character_)
          )

          shiny::observeEvent(input$method_pick, {
            if (input$method_pick %in% base::names(ts_methods())) {
              r_method(input$method_pick)
            }
          })

          # Once, like the transform block's grid: the tile moves its own
          # highlight.
          output$tiles <- shiny::renderUI({
            labels <- vapply(ts_methods(), `[[`, character(1L), "label")
            ts_tile_grid(
              session$ns("method_pick"),
              stats::setNames(list(base::names(ts_methods())), ""),
              labels, shiny::isolate(r_method())
            )
          })

          # Same rule as the argument row: the method and the number of
          # connected inputs change the SHAPE of this panel, the names are
          # seeds written by the observer below. Reading the names live
          # rebuilds the text field you are typing in.
          output$slots <- shiny::renderUI({
            spec <- ts_methods()[[r_method()]]
            if (!isTRUE(spec$named)) {
              return(NULL)
            }
            can <- nameable()
            rf <- unname(refs())
            held <- shiny::isolate(r_names())
            ts_inputs_rail(
              session$ns,
              # A slot that names itself shows those names; the field is for
              # the other kind, and it stays EMPTY until someone types, with
              # the ref as its placeholder.
              vapply(seq_along(can), function(i) {
                if (!can[[i]]) return(ts_ids_label(facts()[[i]]))
                if (length(held) >= i) held[[i]] else ""
              }, character(1L)),
              rf,
              can,
              vapply(seq_along(can), function(i) {
                ts_facts_line(facts()[[i]], ids = can[[i]])
              }, character(1L))
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

          list(
            expr = shiny::reactive(
              # The VALUES are the references (`.arg1`); the NAMES are the
              # link names, which are empty for a positional link and blow up
              # `as.name()`.
              ts_combine_expr(r_method(), unname(refs()), expr_names(),
                              normalise())
            ),
            state = list(method = r_method, names = r_names,
                         result = r_result)
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
    # Empty until someone types: the slot names default from the refs, and
    # the legacy result name is empty on every board written from now on.
    # Unlisted empties wedge the block.
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
             named = TRUE),
    bind = list(label = "Append", fn = "tsbox::ts_bind", one = TRUE),
    chain = list(label = "Chain-link", fn = "tsbox::ts_chain", one = TRUE),
    divide = list(label = "Ratio", op = "%ts/%", one = TRUE, binary = TRUE),
    subtract = list(label = "Difference", op = "%ts-%", one = TRUE,
                    binary = TRUE),
    multiply = list(label = "Product", op = "%ts*%", one = TRUE,
                    binary = TRUE),
    add = list(label = "Sum", op = "%ts+%", one = TRUE, binary = TRUE)
  )
}

#' The expression one method writes
#'
#' `names` carries one entry per slot: a string to name that argument with,
#' or `NA` for a slot the frame already names, which goes in positionally.
#' @noRd
ts_combine_expr <- function(method, refs, names, normalise = logical()) {
  spec <- ts_methods()[[method]]
  args <- lapply(refs, function(r) call(".", as.name(r)))

  # See `id_cols()` in the server. `normalise` is one entry per slot, TRUE
  # where the frame has an id column to rename and FALSE where it has none
  # and needs one made; empty means the slots already agree.
  if (length(normalise)) {
    has_id <- rep_len(normalise, length(args))
    nms <- rep_len(as.character(names), length(args))
    nms[!nzchar(nms) | is.na(nms)] <- as.character(refs)[!nzchar(nms) | is.na(nms)]
    args <- Map(
      function(a, id, nm) {
        if (id) {
          bquote(stats::setNames(.(a), c("id", "time", "value")))
        } else {
          bquote(blockr.ts::ts_name(.(a), .(nm)))
        }
      },
      args, has_id, nms
    )
    # Every slot now carries its own id, so an argument name would only be
    # dropped. They go in positionally.
    names <- rep(NA_character_, length(args))
  }

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
    # working board down. The other methods reduce to one series, which tsbox
    # calls `value`; naming that is a rename, see ts_name().
    # AN EMPTY NAME REINTRODUCES THE DEPARSE. `ts_c(x, b = y)` names the
    # first series by deparsing it, so a slot whose text field was cleared
    # falls back to its ref rather than to nothing. `NA` is the other case
    # entirely: the frame carries ids, tsbox would drop the name anyway, and
    # the argument goes in positionally.
    if (isTRUE(spec$named)) {
      nms <- rep_len(as.character(names), length(args))
      blank <- !nzchar(nms) & !is.na(nms)
      nms[blank] <- as.character(refs)[blank]
      nms[is.na(nms)] <- ""
      args <- stats::setNames(args, nms)
    }
    out <- as.call(c(list(str2lang(spec$fn)), args))
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
#'
#' NOT `args[[i]]()`. A `reactives` store holds each slot as an ACTIVE
#' BINDING, so `[[` already calls the reactive and hands back the data;
#' calling the result tries to call a data frame, which throws, and a
#' tryCatch around it turns the whole thing into a silent NULL. That is what
#' made every slot look like "no series" here.
#' @noRd
ts_arg_value <- function(args, i) {
  tryCatch(args[[i]], error = function(e) NULL)
}
