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
              onclick = sprintf(
                "Shiny.setInputValue('%s', '%s', {priority: 'event'});",
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
