#' Shared chrome for the time series blocks
#'
#' An R-driven block renders Shiny's own markup, so looking native is a
#' matter of styling that markup rather than of reaching for the JS design
#' system components: `selectize = FALSE` and CSS, never Selectize, whose
#' `<select>` no stylesheet of ours can reach.
#'
#' The gear header, the gear button and the in-flow section are the shared
#' blockr classes, whose CSS comes from blockr.dplyr.
#'
#' @name ts-block-ui
#' @keywords internal
NULL

#' @noRd
ts_blocks_dep <- function() {
  htmltools::tagList(
    blockr.dplyr::blockr_blocks_css_dep(),
    # The picker is a delta on the canonical table preview rather than a
    # table of its own. blockr.ui is a Suggests, so this is conditional and
    # `ts-blocks.css` carries enough of a floor to stand without it.
    if (requireNamespace("blockr.ui", quietly = TRUE)) {
      getExportedValue("blockr.ui", "table_preview_dep")()
    },
    htmltools::htmlDependency(
      name = "blockr-ts-blocks",
      version = ts_asset_version(),
      src = ts_asset("css"),
      stylesheet = "ts-blocks.css"
    )
  )
}

#' A gear button and the in-flow section it opens
#'
#' Toggled inline rather than through a Shiny input: the band's open state is
#' not block state, it should not survive a save, and it should not cost a
#' server round trip.
#'
#' @param ns Namespace function.
#' @param ... Section contents.
#' @param open Whether the section starts open.
#' @noRd
ts_gear <- function(ns, ..., open = FALSE) {
  sec <- ns("gear_section")
  btn <- ns("gear_btn")
  htmltools::tagList(
    ts_blocks_dep(),
    htmltools::div(
      class = "blockr-gear-header blockr-ts-header",
      htmltools::tags$button(
        id = btn,
        type = "button",
        class = paste("blockr-gear-btn", if (open) "blockr-gear-active"),
        title = "Block settings",
        htmltools::HTML(ts_gear_svg())
      )
    ),
    htmltools::div(
      id = sec,
      class = "blockr-gear-section blockr-ts-section",
      style = if (open) NULL else "display: none;",
      onclick = NULL,
      ...
    ),
    htmltools::tags$script(htmltools::HTML(sprintf(
      "(function(){var b=document.getElementById('%s');if(!b||b._ts)return;b._ts=1;b.addEventListener('click',function(){var s=document.getElementById('%s');var open=s.style.display!=='none';s.style.display=open?'none':'block';b.classList.toggle('blockr-gear-active',!open);});})();",
      btn, sec
    )))
  )
}

#' The canonical blockr gear
#'
#' The filled Bootstrap cog, copied from `Blockr.icons.gear` in
#' `blockr.dplyr/inst/js/blockr-core.js`. Not a hand-drawn stroked one: a
#' second gear glyph in the same dashboard reads as a different control.
#' @noRd
ts_gear_svg <- function() {
  paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" ',
    'fill="currentColor" viewBox="0 0 16 16" aria-hidden="true">',
    '<path d="M9.405 1.05c-.413-1.4-2.397-1.4-2.81 0l-.1.34a1.464 ',
    '1.464 0 0 1-2.105.872l-.31-.17c-1.283-.698-2.686.705-1.987 1.987l.169',
    '.311c.446.82.023 1.841-.872 2.105l-.34.1c-1.4.413-1.4 2.397 0 2.81l.34',
    '.1a1.464 1.464 0 0 1 .872 2.105l-.17.31c-.698 1.283.705 2.686 1.987 ',
    '1.987l.311-.169a1.464 1.464 0 0 1 2.105.872l.1.34c.413 1.4 2.397 1.4 ',
    '2.81 0l.1-.34a1.464 1.464 0 0 1 2.105-.872l.31.17c1.283.698 2.686-.705 ',
    '1.987-1.987l-.169-.311a1.464 1.464 0 0 1 .872-2.105l.34-.1c1.4-.413 ',
    '1.4-2.397 0-2.81l-.34-.1a1.464 1.464 0 0 1-.872-2.105l.17-.31c.698-',
    '1.283-.705-2.686-1.987-1.987l-.311.169a1.464 1.464 0 0 1-2.105-.872zM8 ',
    '10.93a2.929 2.929 0 1 1 0-5.86 2.929 2.929 0 0 1 0 5.858z"/></svg>'
  )
}

#' The function tile grid
#'
#' Clicking a tile sets one Shiny input rather than firing one of sixteen
#' action buttons: sixteen ids and sixteen observers to carry one choice is
#' sixteen times the bookkeeping for the same event.
#'
#' @param input_id Namespaced id the pick is written to.
#' @param groups Named list of group -> keys.
#' @param labels Named character vector of key -> label.
#' @param current Selected key.
#' @noRd
ts_tile_grid <- function(input_id, groups, labels, current) {
  heads <- names(groups)
  # BY INDEX, NOT BY NAME. A single unheaded group is named `""`, and
  # `groups[[""]]` is NULL, so the grid comes out empty with no error.
  htmltools::div(
    class = "blockr-ts-tiles",
    lapply(seq_along(groups), function(i) {
      htmltools::tagList(
        if (length(heads) && nzchar(heads[[i]])) {
          htmltools::div(class = "blockr-ts-tiles__head", heads[[i]])
        },
        htmltools::div(
          class = "blockr-ts-tiles__grid",
          lapply(groups[[i]], function(k) {
            htmltools::tags$button(
              type = "button",
              class = paste("blockr-ts-tile",
                            if (identical(k, current)) "is-active"),
              title = labels[[k]],
              # The highlight moves here, not on the server: see the
              # `isolate()` on the renderUI that draws this grid.
              onclick = sprintf(
                paste0(
                  "var g=this.closest('.blockr-ts-tiles');",
                  "if(g){g.querySelectorAll('.blockr-ts-tile').forEach(",
                  "function(t){t.classList.remove('is-active')});}",
                  "this.classList.add('is-active');",
                  "Shiny.setInputValue('%s', '%s', {priority: 'event'});"
                ),
                input_id, k
              ),
              htmltools::HTML(ts_icon(k)),
              htmltools::tags$span(labels[[k]])
            )
          })
        )
      )
    })
  )
}

#' A labelled control row
#' @noRd
ts_fields <- function(...) {
  htmltools::div(class = "blockr-ts-fields", ...)
}

#' @noRd
ts_select <- function(id, label, choices, selected = NULL, width = NULL) {
  htmltools::div(
    class = "blockr-ts-field",
    style = if (!is.null(width)) paste0("flex-basis:", width),
    shiny::selectInput(
      id, label, choices = choices, selected = selected,
      selectize = FALSE, width = "100%"
    )
  )
}

#' @noRd
ts_check <- function(id, label, value) {
  htmltools::div(
    class = "blockr-ts-field blockr-ts-field--check",
    shiny::checkboxInput(id, label, value = value, width = "100%")
  )
}

#' @noRd
ts_text <- function(id, label, value, placeholder = NULL) {
  htmltools::div(
    class = "blockr-ts-field",
    shiny::textInput(id, label, value = value, width = "100%",
                     placeholder = placeholder)
  )
}

#' @noRd
ts_hint <- function(...) {
  htmltools::div(class = "blockr-ts-hint", ...)
}

#' The inputs rail
#'
#' One row per variadic slot, below the method tiles because the method is
#' what decides whether the rows exist at all. A row is a name and, in grey
#' on the right, what the data says about that slot.
#'
#' Only a slot whose frame has no `id` column gets a field: tsbox keeps an
#' existing id and drops the argument name, so a field there would be a
#' control that does nothing. Those rows show the ids instead.
#'
#' The field is a plain `textInput` that draws no border until you touch it,
#' which is the design-system input with its chrome held back rather than a
#' click-to-edit widget of our own. An untouched slot holds NO VALUE and
#' shows its ref as the placeholder: the ref is what the expression falls
#' back to, so the field says what will happen rather than pre-filling a
#' string the user then has to delete. It also keeps the panel from having to
#' re-render to restyle itself while someone is typing in it.
#'
#' @param ns Namespace function.
#' @param values One value per slot: the name typed so far, or the ids for a
#'   slot that names itself.
#' @param refs One ref per slot, shown as the placeholder.
#' @param nameable Which slots may be named.
#' @param facts One line per slot, from [ts_facts_line()].
#' @noRd
ts_inputs_rail <- function(ns, values, refs, nameable, facts) {

  if (!length(values)) {
    return(NULL)
  }

  htmltools::div(
    class = "blockr-ts-inputs",
    htmltools::div(class = "blockr-ts-inputs__head", "Inputs"),
    lapply(seq_along(values), function(i) {
      htmltools::div(
        class = "blockr-ts-inputs__row",
        if (isTRUE(nameable[[i]])) {
          htmltools::div(
            class = "blockr-ts-inputs__name",
            shiny::textInput(
              ns(paste0("name_", i)), NULL, value = values[[i]],
              width = "100%", placeholder = refs[[i]]
            )
          )
        } else {
          htmltools::div(class = "blockr-ts-inputs__given", values[[i]])
        },
        htmltools::div(class = "blockr-ts-inputs__fact", facts[[i]])
      )
    })
  )
}
