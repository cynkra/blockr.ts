#' Chart or table, per block
#'
#' A two-state switch above the preview. Chart is the default; table gives the
#' block back whatever preview the app normally uses, because it is the
#' fallback display that renders it, not us.
#'
#' It lives outside the output container. In table mode the container holds
#' the fallback's markup, which has nowhere to put a control of ours, and a
#' switch you can use to leave a view but not to come back is not a switch.
#'
#' It is hidden until the server says there is something to draw, so a block
#' whose result is not a time series looks exactly as it always did.
#'
#' @name ts-view-toggle
#' @keywords internal
NULL

#' @param id The output's namespaced id, as handed to `tabular_ui()`.
#' @return A `shiny.tag`.
#' @noRd
ts_view_toggle <- function(id) {
  htmltools::tagList(
    ts_view_toggle_css(),
    htmltools::div(
      class = "blockr-ts-view",
      id = paste0(id, "_view_toggle"),
      `data-input` = paste0(id, "_view"),
      htmltools::div(
        class = "blockr-ts-view__track",
        htmltools::tags$button(
          type = "button", class = "blockr-ts-view__btn is-active",
          `data-view` = "chart", title = "Show as a chart",
          htmltools::HTML(ICON_CHART)
        ),
        htmltools::tags$button(
          type = "button", class = "blockr-ts-view__btn",
          `data-view` = "table", title = "Show as a table",
          htmltools::HTML(ICON_TABLE)
        )
      )
    ),
    htmltools::tags$script(htmltools::HTML(
      "(function(){ var f = function(){ if (window.BlockrTsView) { window.BlockrTsView.mount(); } else { setTimeout(f, 50); } }; f(); })();"
    ))
  )
}

#' Take the switch away from a block that can no longer draw
#'
#' The reveal is the chart's job, see `ts_chart_html()`. This is only the
#' other direction: a block that used to draw and now cannot, where the
#' element is already on the page.
#'
#' @param session The block's module session.
#' @noRd
ts_view_hide <- function(session) {
  session$sendCustomMessage(
    "blockr-ts-view",
    list(id = session$ns("result_view_toggle"), offer = FALSE)
  )
}

# Two 14px glyphs, inline so the package needs no icon set.
ICON_CHART <- paste0(
  '<svg viewBox="0 0 16 16" width="14" height="14" fill="none" ',
  'stroke="currentColor" stroke-width="1.5" stroke-linecap="round" ',
  'stroke-linejoin="round"><path d="M1.5 12.5 5.5 7 8.5 9.5 14.5 3"/></svg>'
)

ICON_TABLE <- paste0(
  '<svg viewBox="0 0 16 16" width="14" height="14" fill="none" ',
  'stroke="currentColor" stroke-width="1.5"><rect x="1.5" y="2.5" ',
  'width="13" height="11" rx="1"/><path d="M1.5 6.5h13M6 6.5v7"/></svg>'
)

#' @noRd
ts_view_toggle_css <- function() {
  htmltools::tags$style(htmltools::HTML("
    /* A SEGMENTED CONTROL, NOT TWO LOOSE GLYPHS. The first version was two
       14px icons in #9a9aa2 with no container, right-aligned above the chart,
       and it was invisible in practice: nothing said it was a control and
       nothing said which side was on. A border, a track and a raised active
       segment are what make a two-state switch read as one. */
    .blockr-ts-view {
      display: none;
      justify-content: flex-end;
      margin: 0 0 6px 0;
    }
    .blockr-ts-view.is-offered { display: flex; }

    .blockr-ts-view__track {
      display: inline-flex;
      gap: 2px;
      padding: 2px;
      border: 1px solid rgba(128,128,135,.28);
      border-radius: 7px;
      background: rgba(128,128,135,.08);
    }

    .blockr-ts-view__btn {
      display: inline-flex;
      align-items: center;
      justify-content: center;
      border: 0;
      background: transparent;
      color: #67676c;
      width: 30px;
      height: 22px;
      padding: 0;
      border-radius: 5px;
      line-height: 0;
      cursor: pointer;
      transition: background .12s, color .12s;
    }
    .blockr-ts-view__btn:hover { color: #3c3c43; }
    .blockr-ts-view__btn.is-active {
      color: #3c3c43;
      background: #fff;
      box-shadow: 0 1px 2px rgba(0,0,0,.10);
    }
  "))
}
