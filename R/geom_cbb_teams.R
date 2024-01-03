#' ggplot2 Layer for Visualizing College Basketball Team Logos
#'
#' This geom is used to plot college basketball team logos instead of points in
#' a ggplot. It requires x, y aesthetics as well as a valid CBB team
#' name from `cbbdata`. Use `width` to adjust the size of logos.
#'
#' @inheritParams ggplot2::geom_point
#' @section Aesthetics:
#' `geom_cbb_teams()` understands the following aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item{**x**}{ - The x-coordinate.}
#'   \item{**y**}{ - The y-coordinate.}
#'   \item{**team**}{ - A valid team name from `cbbdata`.}
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
#'   team = c('Duke', 'Arizona', 'North Carolina'),
#'   val = c(100, 80, 60),
#'   val2 = c(50, 60, 70)
#' )
#'
#' ggplot(data = df, aes(val, val2)) +
#'   geom_cbb_teams(aes(team = team), width = 0.1)
#' }
#'
#' @export
geom_cbb_teams <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           na.rm = FALSE,
                           show.legend = FALSE,
                           inherit.aes = TRUE,
                           highlight_teams = NULL, highlight_method = "alpha",
                           highlight_alpha = 0.5) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCBBteam,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      highlight_teams = highlight_teams,
      highlight_method = highlight_method,
      highlight_alpha = highlight_alpha,
      ...
    )
  )
}


#' @rdname cbbplotR-package
#' @export
GeomCBBteam <- ggplot2::ggproto(
  "GeomCBBteam", ggplot2::Geom,
  required_aes = c("x", "y", "team"),
  default_aes = ggplot2::aes(
    alpha = NULL, colour = NULL, angle = 0, hjust = 0.5,
    vjust = 0.5, width = 0.035, height = 1.0
  ),
  draw_panel = function(data, panel_params, coord,
                        highlight_teams = NULL, highlight_method = "alpha",
                        highlight_alpha = 0.5, na.rm = FALSE) {

    # Apply highlighting logic
    if (!is.null(highlight_teams)) {
      if (highlight_method == "alpha") {
        data$alpha <- ifelse(data$team %in% highlight_teams, 1, highlight_alpha)
      } else if (highlight_method == "color") {
          data$colour <- ifelse(data$team %in% highlight_teams, as.character(data$colour), 'b/w')
      }
        else if (highlight_method == "both") {
          data$alpha <- ifelse(data$team %in% highlight_teams, 1, 0.6)
          data$colour <- ifelse(data$team %in% highlight_teams, as.character(data$colour), 'b/w')
        }
    }

    data$path <- logo_from_team(team = data$team)

    # Call to draw the logos
    ggpath::GeomFromPath$draw_panel(
      data = data,
      panel_params = panel_params,
      coord = coord,
      na.rm = na.rm
    )
  },
  draw_key = function(...) grid::nullGrob()
)



