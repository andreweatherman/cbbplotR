#' Theme elements for ggplot2
#'
#' These functions enable images in non-data parts of ggplot2 layers.
#'
#'   - `element_cbb_logo()`: draws CBB team logos instead of their names.
#'   - `element_cbb_conferences()`: draws CBB conference logos instead of their names.
#'   - `element_cbb_headshot()`: draws CBB player headshots instead of their ESPN player IDs.
#'   - `element_path()`: draws images from valid image URLs instead of the URL.
#'
#' @inheritParams ggpath::element_path
#' @inheritDotParams ggpath::element_path
#'
#' @param alpha The transparency level [0-1].
#' @param color The image will be colorized with this color. Use the special character `"b/w"` to set it to black and white.
#' @param hjust,vjust The horizontal and vertical adjustment respectively [0-1].
#' @param size The output grob size in `cm`.
#' @return An S3 object of class `element`.

#' @export
#' @rdname element
element_cbb_teams <- function(alpha = NULL, colour = NA, hjust = NULL, vjust = NULL,
                             color = NULL, size = 0.5) {
  if (!is.null(color))  colour <- color
  structure(
    list(alpha = alpha, colour = colour, hjust = hjust, vjust = vjust, size = size),
    class = c("element_cbb_teams", "element_text", "element")
  )
}

#' @export
#' @rdname element
element_cbb_headshots <- function(player_id_column, alpha = NULL, colour = NA, hjust = NULL, vjust = NULL,
                                 color = NULL, size = 1.5) {
  if (!is.null(color))  colour <- color
  structure(
    list(alpha = alpha, colour = colour, hjust = hjust, vjust = vjust, size = size),
    class = c("element_cbb_headshots", "element_text", "element")
  )
}

#' @export
#' @rdname element
element_cbb_conferences <- function(alpha = NULL, colour = NA, hjust = NULL, vjust = NULL,
                             color = NULL, size = 0.5) {
  if (!is.null(color))  colour <- color
  structure(
    list(alpha = alpha, colour = colour, hjust = hjust, vjust = vjust, size = size),
    class = c("element_cbb_conferences", "element_text", "element")
  )
}

#' @export
#' @rdname element
element_path <- function(...) ggpath::element_path(...)


# This S3 method is just a wrapper of the ggpath theme element method.
# It translates the labels (which are team abbreviations) to local paths
# and passes those paths to ggpath
#' @export
element_grob.element_cbb_teams <- function(element, label = "", x = NULL, y = NULL,
                                          alpha = NULL, colour = NULL,
                                          hjust = 0.5, vjust = 0.5,
                                          size = NULL, ...) {

  if (is.null(label)) return(ggplot2::zeroGrob())

  label <- logo_from_team(team = label)

  # We want ggpath to do the actual work, so we change the class here to make
  # ggplot2 call the S3 method of ggpath
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

#' @export
element_grob.element_cbb_headshots <- function(element, label = "", x = NULL, y = NULL,
                                              alpha = NULL, colour = NULL,
                                              hjust = 0.5, vjust = 0.5,
                                              size = NULL, ...) {

  label <- glue::glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/{label}.png')

  # We want ggpath to do the actual work, so we change the class here to make
  # ggplot2 call the S3 method of ggpath
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

#' @export
element_grob.element_cbb_conferences <- function(element, label = "", x = NULL, y = NULL,
                                          alpha = NULL, colour = NULL,
                                          hjust = 0.5, vjust = 0.5,
                                          size = NULL, ...) {

  if (is.null(label)) return(ggplot2::zeroGrob())

  label <- logo_from_conference(conference = label)

  # We want ggpath to do the actual work, so we change the class here to make
  # ggplot2 call the S3 method of ggpath
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

