#' Glyphs for the function tiles
#'
#' EACH ONE DEPICTS THE OPERATION, NOT THE NAME. A geom icon can draw the
#' thing it is called after; "annualised percentage change" cannot be drawn,
#' and sixteen attempts at drawing names end up as sixteen indistinguishable
#' smudges at 16px. What is drawable is what the verb does to a line: a change
#' oscillates around a baseline, an index rises from a marked base point, a
#' span puts crop brackets on it, a frequency thins its ticks, a gap verb
#' opens or bridges a hole.
#'
#' Inline SVG so the package needs no icon set, `currentColor` so the tile's
#' active state colours them, half opacity in CSS so the label stays the
#' thing you read first.
#'
#' @param key A verb or method key.
#' @return An SVG string.
#' @noRd
ts_icon <- function(key) {
  body <- ts_icon_paths()[[key]]
  if (is.null(body)) body <- '<circle cx="8" cy="8" r="5"/>'
  paste0(
    '<svg viewBox="0 0 16 16" width="16" height="16" fill="none" ',
    'stroke="currentColor" stroke-width="1.3" stroke-linecap="round" ',
    'stroke-linejoin="round" aria-hidden="true">', body, '</svg>'
  )
}

#' @noRd
ts_icon_paths <- function() {
  base <- '<path d="M1 8h14" opacity=".35"/>'
  list(
    # -- change: something oscillating around a zero line
    pc = paste0(base, '<path d="M2 10.5 5 6 8 9.5 11 5.5 14 9"/>'),
    pcy = paste0(base, '<path d="M2 11.5C5 3.5 8 12.5 14 5"/>'),
    pca = paste0(base, '<path d="M2 10.5 6 8 9.5 9.5 13 4"/>',
                 '<path d="M10.2 4h2.9v2.9"/>'),
    diff = paste0(base, '<path d="M3 8V4.5M6.2 8v4M9.4 8V3.5M12.6 8v3.2"/>'),
    diffy = paste0(base, '<path d="M4 8V4M8 8v4.2M12 8V5"/>'),
    compound = paste0('<path d="M1.5 12.5 3.5 10 5.5 12"/>',
                      '<path d="M5.5 12C9 12 10 4 14.5 2.5"/>'),

    # -- level: a marked starting point, or a re-centred line
    index = paste0('<path d="M2 11.5h12" opacity=".35"/>',
                   '<path d="M3 11.5 7 8.5 10 9.5 14 3"/>',
                   '<circle cx="3" cy="11.5" r="1.5" fill="currentColor" ',
                   'stroke="none"/>'),
    scale = paste0('<path d="M4.5 11.5 8 7 11 9 14.5 4"/>',
                   '<path d="M2 3.5v9M.8 4.7 2 3.5l1.2 1.2',
                   'M.8 11.3 2 12.5l1.2-1.2"/>'),

    # -- time axis: the line held still and the axis worked on
    span = paste0('<path d="M1.5 11.5 5 6.5 9 8.5 14.5 3.5" opacity=".3"/>',
                  '<path d="M5 2v12M11 2v12"/>'),
    lag = paste0('<path d="M1 11.5 4.5 6 7.5 8 10.5 4" opacity=".35"/>',
                 '<path d="M4.5 11.5 8 6 11 8 14 4"/>'),
    frequency = paste0('<path d="M1 12.5h14" opacity=".35"/>',
                       '<path d="M2 12.5V8.5M3.6 12.5v-3M5.2 12.5v-4.5',
                       'M6.8 12.5v-3.2M8.4 12.5v-4"/>',
                       '<path d="M11.4 12.5v-6M14 12.5v-5"/>'),
    regular = paste0('<path d="M1 12.5h14" opacity=".35"/>',
                     '<path d="M3 12.5v-5M6 12.5v-7M13 12.5v-6"/>',
                     '<path d="M9.5 12.5v-4" stroke-dasharray="1.4 1.4"/>'),
    first_of_period = paste0('<path d="M4 2v12M10.5 2v12" opacity=".3"/>',
                             '<path d="M7 7.5H4.4M13.5 7.5h-2.6"/>',
                             '<path d="M5.4 6.4 4.3 7.5l1.1 1.1',
                             'M11.9 6.4l-1.1 1.1 1.1 1.1"/>'),

    # -- smoothing: the input behind, the smooth line in front
    trend = paste0('<path d="M1.5 11.5 3.5 7 5.5 10.5 7.5 6 9.5 9.5 11.5 5',
                   ' 14.5 8" opacity=".35"/>',
                   '<path d="M1.5 10.5C6 10.5 9 6.5 14.5 6"/>'),

    # -- gaps: a hole, and a hole bridged
    na_omit = '<path d="M1.5 11 5 6.5"/><path d="M9.5 8.5 14.5 4"/>',
    na_interpolation = paste0('<path d="M1.5 11 5 6.5"/>',
                              '<path d="M5 6.5 9.5 8.5" ',
                              'stroke-dasharray="1.4 1.4"/>',
                              '<path d="M9.5 8.5 14.5 4"/>'),

    # -- combining
    c = paste0('<path d="M1.5 5.5 5.5 3.5 9.5 6.5 14.5 2.5"/>',
               '<path d="M1.5 12.5 5.5 10.5 9.5 13.5 14.5 9.5"/>'),
    bind = paste0('<path d="M1.5 11 6.5 7.5"/><path d="M9.5 7.5 14.5 4"/>',
                  '<path d="M8 2.5v11" opacity=".35" ',
                  'stroke-dasharray="1.6 1.6"/>'),
    chain = paste0('<path d="M1.5 12 8 8.5"/><path d="M8 8.5 14.5 4"/>',
                   '<circle cx="8" cy="8.5" r="1.6" fill="currentColor" ',
                   'stroke="none"/>'),
    divide = paste0('<path d="M3 13 13 3"/>',
                    '<circle cx="4.6" cy="4.6" r="1.3" fill="currentColor" ',
                    'stroke="none"/><circle cx="11.4" cy="11.4" r="1.3" ',
                    'fill="currentColor" stroke="none"/>'),
    subtract = paste0('<path d="M1.5 5 5.5 3 9.5 6 14.5 2" opacity=".35"/>',
                      '<path d="M4 11.5h8"/>'),
    multiply = paste0('<path d="M3.5 3.5 12.5 12.5M12.5 3.5 3.5 12.5"/>'),
    add = paste0('<path d="M8 3v10M3 8h10"/>')
  )
}
