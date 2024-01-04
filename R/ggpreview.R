#' Preview ggplot in Specified Dimensions
#'
#' This function previews a ggplot in its actual dimensions in order to see how
#' it will look when saved. It is also significantly faster than the default
#' preview in RStudio for ggplots created using cbbplotR. This is copied
#' directly from `nflplotR`.
#'
#' @inheritParams ggplot2::ggsave
#' @param asp The aspect ratio of the plot calculated as `width / height`. If
#'   this is a numeric value (and not `NULL`) the `height` of the plot will be
#'   recalculated to `height = width / asp`.
#' @return No return value, called for side effects.
#' @export

ggpreview <- function(plot = ggplot2::last_plot(),
                      width = NA,
                      height = NA,
                      asp = NULL,
                      dpi = 300,
                      device = "png",
                      units = c("in", "cm", "mm", "px"),
                      scale = 1,
                      limitsize = TRUE,
                      bg = NULL,
                      ...){
  rlang::check_installed("rstudioapi", reason = "to preview a ggplot file")
  file <- tempfile()
  if (is.numeric(asp)) height <- width / asp
  ggplot2::ggsave(
    file,
    plot = plot,
    device = device,
    scale = scale,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    limitsize = limitsize,
    bg = bg,
    ...
  )
  rstudioapi::viewer(file)
}
