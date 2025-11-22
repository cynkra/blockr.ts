#' CSS Utilities for blockr.ts Blocks
#'
#' Centralized CSS functions for consistent styling across all time series blocks.
#' These utilities provide responsive grid layouts and styling that match the
#' modern blockr design patterns used in blockr.dplyr and blockr.ggplot.
#'
#' @name css-utilities
#' @keywords internal
NULL

#' Responsive Grid Layout CSS for TS Blocks
#'
#' Generates the core CSS for responsive grid-based layouts used by all
#' blockr.ts blocks. This replaces inline CSS that was previously duplicated
#' across multiple block files.
#'
#' The grid automatically adapts from 1 column (narrow screens) to multiple
#' columns (wide screens) using CSS Grid with `auto-fit` and `minmax()`.
#'
#' @return HTML style tag containing responsive grid CSS
#' @noRd
#'
#' @examples
#' \dontrun{
#' ui <- function(id) {
#'   tagList(
#'     ts_responsive_css(),
#'     div(class = "ts-block-container", ...)
#'   )
#' }
#' }
ts_responsive_css <- function() {
  tags$style(HTML("
    .ts-block-container {
      width: 100%;
      margin: 0px;
      padding: 0px;
      padding-bottom: 15px;
    }

    .ts-block-form-grid {
      display: grid;
      gap: 15px;
      grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
    }

    .ts-block-section,
    .ts-block-section-grid {
      display: contents;
    }

    .ts-block-section h4 {
      grid-column: 1 / -1;
      margin-top: 5px;
      margin-bottom: 0px;
      font-size: 1.1rem;
      font-weight: 600;
      color: #333;
    }

    .ts-block-input-wrapper {
      width: 100%;
    }

    .ts-block-input-wrapper .form-group {
      margin-bottom: 10px;
    }

    .ts-block-input-wrapper label {
      font-weight: 500;
      margin-bottom: 0.3rem;
      display: block;
    }

    .ts-block-help-text {
      grid-column: 1 / -1;
      margin-top: 0px;
      padding-top: 0px;
      font-size: 0.875rem;
      color: #666;
    }

    .ts-block-help-text p {
      margin-top: 5px;
      margin-bottom: 5px;
    }
  "))
}

#' Force Single-Column Layout
#'
#' Forces a specific block to use single-column layout regardless of screen width.
#' Useful for blocks with many inputs or complex UI that works better in vertical
#' arrangement.
#'
#' @param block_name Name of the block (e.g., "span", "change", "dataset")
#' @return HTML style tag with single-column override
#' @noRd
#'
#' @examples
#' \dontrun{
#' ui <- function(id) {
#'   tagList(
#'     ts_responsive_css(),
#'     ts_single_column("dataset"),
#'     div(class = "ts-block-container", ...)
#'   )
#' }
#' }
ts_single_column <- function(block_name) {
  tags$style(HTML(sprintf("
    .ts-%s-block-container .ts-block-form-grid {
      grid-template-columns: 1fr !important;
    }
  ", block_name)))
}
