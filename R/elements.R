#' Theme elements for ggplot2 to Visualize College Basketball Elements
#'
#' These functions enable the use of images in non-data parts of ggplot2 layers, specifically for
#' visualizing elements related to college basketball.
#'
#' @section Functions:
#' - `element_cbb_teams()`: Replaces text with team logos in ggplot2 themes.
#' - `element_cbb_conferences()`: Replaces text with conference logos in ggplot2 themes.
#' - `element_cbb_headshots()`: Replaces ESPN player IDs with player headshots in ggplot2 themes.
#'
#' @inheritParams ggpath::element_path
#' @inheritDotParams ggpath::element_path
#'
#' @param alpha The transparency level [0-1].
#' @param color (Optional) Colorization for the images; use "b/w" for black and white.
#' @param hjust,vjust Horizontal and vertical adjustment [0-1].
#' @param size Size of the output grob in cm.
#' @param logo_type Type of logo to use ('normal' or 'wordmark' // 'normal' or
#'   'dark'), applicable for `element_cbb_conferences` and `element_cbb_teams`.
#' @param player_id_column Column name for player IDs, applicable for `element_cbb_headshots`.
#' @name element_cbb
#' @aliases NULL
#' @return An S3 object of class `element` appropriate for use in ggplot2 theme settings.
#'
NULL

#' @rdname element_cbb
#' @export
element_cbb_teams <- function(alpha = NULL, colour = NA, hjust = NULL, vjust = NULL,
                              color = NULL, size = 0.5, logo_type = 'normal') {
  if (!is.null(color))  colour <- color
  structure(
    list(alpha = alpha, colour = colour, hjust = hjust, vjust = vjust, size = size,
         logo_type = logo_type),
    class = c("element_cbb_teams", "element_text", "element")
  )
}

#' @rdname element_cbb
#' @export
element_cbb_headshots <- function(player_id_column, alpha = NULL, colour = NA, hjust = NULL, vjust = NULL,
                                  color = NULL, size = 1.5) {
  if (!is.null(color))  colour <- color
  structure(
    list(alpha = alpha, colour = colour, hjust = hjust, vjust = vjust, size = size),
    class = c("element_cbb_headshots", "element_text", "element")
  )
}

#' @rdname element_cbb
#' @export
element_cbb_conferences <- function(alpha = NULL, colour = NA, hjust = NULL, vjust = NULL,
                                    color = NULL, size = 0.5, logo_type = 'normal') {
  if (!is.null(color))  colour <- color
  structure(
    list(alpha = alpha, colour = colour, hjust = hjust, vjust = vjust, size = size,
         logo_type = logo_type),
    class = c("element_cbb_conferences", "element_text", "element")
  )
}

#' @rdname element_cbb
#' @export
element_grob.element_cbb_teams <- function(element, label = "", x = NULL, y = NULL,
                                           alpha = NULL, colour = NULL,
                                           hjust = 0.5, vjust = 0.5,
                                           size = NULL, ...) {

  if (is.null(label)) return(ggplot2::zeroGrob())

  logo_type <- element$logo_type

  label <- logo_from_team(team = label, logo_type = logo_type)

  class(element) <- c("element_path", "element_text", "element")

  ggplot2::element_grob(
    element = element,
    label = label,
    x = x,
    y = y,
    alpha = alpha,
    colour = colour,
    hjust = hjust,
    vjust = vjust,
    size = size,
    ...
  )
}

#' @rdname element_cbb
#' @export
element_grob.element_cbb_headshots <- function(element, label = "", x = NULL, y = NULL,
                                               alpha = NULL, colour = NULL,
                                               hjust = 0.5, vjust = 0.5,
                                               size = NULL, ...) {

  label <- glue::glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/{label}.png')

  class(element) <- c("element_path", "element_text", "element")

  ggplot2::element_grob(
    element = element,
    label = label,
    x = x,
    y = y,
    alpha = alpha,
    colour = colour,
    hjust = hjust,
    vjust = vjust,
    size = size,
    ...
  )
}

#' @rdname element_cbb
#' @export
element_grob.element_cbb_conferences <- function(element, label = "", x = NULL, y = NULL,
                                                 alpha = NULL, colour = NULL,
                                                 hjust = 0.5, vjust = 0.5,
                                                 size = NULL, ...) {
  if (is.null(label)) return(ggplot2::zeroGrob())

  logo_type <- element$logo_type

  label <- logo_from_conference(conference = label, logo_type = logo_type)

  class(element) <- c("element_path", "element_text", "element")

  ggplot2::element_grob(
    element = element,
    label = label,
    x = x,
    y = y,
    alpha = alpha,
    colour = colour,
    hjust = hjust,
    vjust = vjust,
    size = size,
    ...
  )
}

