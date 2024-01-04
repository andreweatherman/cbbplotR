#' Visualize College Basketball Conference Logos in ggplot2
#'
#' @description
#' `geom_cbb_conferences` creates a ggplot2 layer that plots college basketball conference logos
#' in place of points. The layer requires `x` and `y` aesthetics and a valid conference name.
#' The `width` parameter can be used to adjust the size of the logos. Logos can be depicted as
#' either `normal` or `wordmark` by setting the `logo_type` parameter.
#'
#' @section Aesthetics:
#' This geom requires the following aesthetics:
#' \itemize{
#'   \item{**x**}{ - The x-coordinate for the logo's position.}
#'   \item{**y**}{ - The y-coordinate for the logo's position.}
#'   \item{**conference**}{ - The name of the conference, which should be a valid name from `cbbdata`.}
#' }
#' @param mapping Set of aesthetic mappings created by `ggplot2::aes()` or `ggplot2::aes_()`.
#'        If specified and `inherit.aes = TRUE` (the default), it is combined with the default
#'        mapping at the top level of the plot. You must supply `mapping` if there is no plot
#'        mapping.
#' @param data The data to be displayed in this layer. If `NULL`, the default, the data is
#'        inherited from the plot data as specified in the call to `ggplot()`.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a
#'        position adjustment function.
#' @param ... Other arguments passed on to `layer()`.
#' @param na.rm If `FALSE`, the default, missing values are removed with a warning.
#'        If `TRUE`, missing values are silently removed.
#' @param show.legend Logical. Should this layer be included in the legends?
#'        `NA`, the default, includes if any aesthetics are mapped. `FALSE` never includes,
#'        and `TRUE` always includes.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics, rather than combining
#'        with them. This is most useful for helper functions that define both data and
#'        aesthetics and shouldn't inherit behaviour from the default plot specification,
#'        e.g. `borders()`.
#' @param logo_type Character specifying the type of logos to plot (`"normal"` or `"wordmark"`).
#' @param highlight_conferences Vector of conference names to highlight.
#' @param highlight_method Method of highlighting (`"alpha"`, `"color"`, or `"both"`).
#' @param highlight_alpha Alpha value for highlighted conferences.
#'
#' @return A ggplot2 layer that can be added to a plot created with `ggplot()`.
#'
#' @examples
#' library(ggplot2)
#'
#' # Example data frame
#' df <- data.frame(
#'   x = 1:3,
#'   y = c(100, 80, 60),
#'   conference = c('ACC', 'B10', 'B12')
#' )
#'
#' # Create a ggplot and add the custom geom for conference logos
#' ggplot(df, aes(x, y)) +
#'   geom_cbb_conferences(aes(conference = conference), width = 0.1)
#'
#' @export
geom_cbb_conferences <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           na.rm = FALSE,
                           show.legend = FALSE,
                           inherit.aes = TRUE, logo_type = "normal",
                           highlight_conferences = NULL, highlight_method = "alpha",
                           highlight_alpha = 0.5) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCBBconference,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      highlight_conferences = highlight_conferences,
      highlight_method = highlight_method,
      highlight_alpha = highlight_alpha,
      logo_type = logo_type,
      ...
    )
  )
}


#' @rdname cbbplotR-package
#' @export
GeomCBBconference <- ggplot2::ggproto(
  "GeomCBBconference", ggplot2::Geom,
  required_aes = c("x", "y", "conference"),
  default_aes = ggplot2::aes(
    alpha = NULL, colour = NULL, angle = 0, hjust = 0.5,
    vjust = 0.5, width = 0.035, height = 1.0
  ),
  draw_panel = function(data, panel_params, coord, logo_type = "normal",
                        highlight_conferences = NULL, highlight_method = "alpha",
                        highlight_alpha = 0.5, na.rm = FALSE) {

    # apply highlighting logic
    if (!is.null(highlight_conferences)) {
      if (highlight_method == "alpha") {
        data$alpha <- ifelse(data$conference %in% highlight_conferences, 1, highlight_alpha)
      } else if (highlight_method == "color") {
        data$colour <- ifelse(data$conference %in% highlight_conferences, as.character(data$colour), 'b/w')
      }
      else if (highlight_method == "both") {
        data$alpha <- ifelse(data$conference %in% highlight_conferences, 1, 0.6)
        data$colour <- ifelse(data$conference %in% highlight_conferences, as.character(data$colour), 'b/w')
      }
    }

    data$path <- logo_from_conference(conference = data$conference, logo_type = logo_type)

    # resize some conference logos that are typically smaller than others
    conf_to_resize <- c('A10', 'ACC', 'BE', 'BSth')
    conf_to_downsize <- c('Amer', 'P12', 'SEC', 'MWC')

    cbbplotR:::.conf_name_matches()
    conf_matches <- readRDS(cbbplotR:::conf_matches_path())
    needed_to_resize <- names(conf_matches[data$conference][conf_matches[data$conference] %in% conf_to_resize])
    needed_to_downsize <- names(conf_matches[data$conference][conf_matches[data$conference] %in% conf_to_downsize])

    # define base width // need this (?) so we can change width vals. if certain conferences are given
    if (!"width" %in% names(data)) {
      data$width <- rep(0.035, nrow(data))
    }

    # i played around with diff. sizes and found this rescale to be the best
    if(length(needed_to_resize) > 0) {
      data$width <- ifelse(data$conference %in% needed_to_resize, data$width + 0.02, data$width)
    }

    # i played around with diff. sizes and found this rescale to be the best
    if(length(conf_to_downsize) > 0) {
      data$width <- ifelse(data$conference %in% conf_to_downsize, data$width - 0.02, data$width)
    }

    ggpath::GeomFromPath$draw_panel(
      data = data,
      panel_params = panel_params,
      coord = coord,
      na.rm = na.rm
    )
  },
  draw_key = function(...) grid::nullGrob()
)



