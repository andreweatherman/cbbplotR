#' Add a team logo in-line with plot title with `ggplot2`
#' @param team The team whose logo will be plotted. Must match team names from
#'   `cbbdata`.
#' @param size The size of the plotted logo
#' @param hjust The horizontal position of the logo
#' @param vjust The vertical position of the logo
#' @param position Something...
#'
#' @examples
#' \donttest {
#' library(cbbplotR)
#' library(ggplot2)
#'
#' ggplot(data = mtcars, aes(mpg, hp)) +
#'   geom_point() +
#'   ggplot_cbb_logo_title('Duke', size = 1) +
#'   labs(title = 'Random data to show functionality',
#'        subtitle = 'Test')
#' }
#' @export

ggplot_cbb_logo_title <- function(value,
                                  type = c('team', 'conference', 'headshot'),
                                  size = 5,
                                  hjust = 1,
                                  vjust = 1,
                                  position = c(1, 1)) {

  fx_to_use <- if(type == 'team') {
    get('element_cbb_teams', envir = asNamespace('cbbplotR'))
  } else if(type == 'conference') {
    get('element_cbb_conferences', envir = asNamespace('cbbplotR'))
  } else if(type == 'headshot') {
    get('element_cbb_headshots', envir = asNamespace('cbbplotR'))
  }

  list(
    labs(tag = value),
    theme(
      plot.tag = fx_to_use(size = size, hjust = hjust, vjust = vjust),
      plot.tag.position = position
    )
  )

}
