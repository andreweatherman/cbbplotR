#' ggplot2 Layer for Visualizing College Basketball Player Headshots
#'
#' This geom is used to plot college basketball player headshots instead of points in
#' a ggplot. It requires x, y aesthetics as well as a valid player ID from ESPN. Use `width` to adjust
#' the size of headshots.
#'
#' @inheritParams ggplot2::geom_point
#' @section `geom_cbb_headshots()` understands the following aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item{**x**}{ - The x-coordinate.}
#'   \item{**y**}{ - The y-coordinate.}
#'   \item{**player_id**}{ - A valid player ID from ESPN.}
#'   \item{`alpha = NULL`}{ - The transparency level [0-1].}
#'   \item{`color = NULL`}{ - The image will be colorized with this color. Use the special character `"b/w"` to set it to black and white. }
#'   \item{`angle = 0`}{ - The angle of the image [0-360].}
#'   \item{`hjust = 0.5`}{ - The horizontal adjustment relative to the given x coordinate [0-1].}
#'   \item{`vjust = 0.5`}{ - The vertical adjustment relative to the given y coordinate [0-1].}
#'   \item{`width = 1.0`}{ - The width of the image. }
#'   \item{`height = 1.0`}{ - The height of the image. }
#' }
#' @param ... Other arguments passed on to [ggplot2::layer()].
#'
#' @return A ggplot2 layer ([ggplot2::layer()]) that can be added to a plot
#'   created with [ggplot2::ggplot()].
#'
#'  @examples
#' \donttest{
#' library(cbbplotR)
#' library(ggplot2)
#'
#' df <- data.frame(
#'   player_id = c('4431674', '4600663', '4684793'),
#'   val = c(100, 80, 60),
#'   val2 = c(50, 60, 70)
#' )
#'
#' ggplot(data = df, aes(val, val2)) +
#'   geom_cbb_headshots(aes(player_id = player_id), width = 0.1)
#' }
#'
#' @export
geom_cbb_headshots <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           na.rm = FALSE,
                           show.legend = FALSE,
                           inherit.aes = TRUE, highlight_players = NULL,
                           highlight_method = "alpha", highlight_alpha = 0.5) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCBBheads,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      highlight_players = highlight_players,
      highlight_method = highlight_method,
      highlight_alpha = highlight_alpha,
      ...
    )
  )
}

#' @rdname cbbplotR-package
#' @export
GeomCBBheads <- ggplot2::ggproto(
  "GeomCBBheads", ggplot2::Geom,
  required_aes = c("x", "y", "player_id"),
  default_aes = ggplot2::aes(
    alpha = NULL, colour = NULL, angle = 0, hjust = 0.5,
    vjust = 0.5, width = 0.08, height = 1.0
  ),
  draw_panel = function(data, panel_params, coord,
                        highlight_players = NULL, highlight_method = "alpha",
                        highlight_alpha = 0.5, na.rm = FALSE) {

    # Apply highlighting logic
    if (!is.null(highlight_players)) {
      if (highlight_method == "alpha") {
        data$alpha <- ifelse(data$player_id %in% highlight_players, 1, highlight_alpha)
      } else if (highlight_method == "color") {
        data$colour <- ifelse(data$player_id %in% highlight_players, as.character(data$colour), 'b/w')
      } else if (highlight_method == "both") {
        data$alpha <- ifelse(data$player_id %in% highlight_players, 1, 0.6)
        data$colour <- ifelse(data$player_id %in% highlight_players, as.character(data$colour), 'b/w')
      }
    }

    data$path <- glue::glue('https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/{data$player_id}.png')

    ggpath::GeomFromPath$draw_panel(
      data = data,
      panel_params = panel_params,
      coord = coord,
      na.rm = na.rm
    )
  },
  draw_key = function(...) grid::nullGrob()
)
