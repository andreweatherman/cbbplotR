#' Add a Team Logo and Title/Subtitle to a `gt` Table.
#'
#' This requires wrapping the function around `html` and passing
#'   through to `tab_header` in `gt`. See function example.
#'
#' @param title The `gt` table title
#' @param subtitle The `gt` table subtitle
#' @param team A valid team name from `cbbdata`.
#' @param logo_link If you want to plot a different image, pass a valid URL link to it.
#' @param title_font_size The size of the title font (in px).
#' @param title_font_weight The weight of the title (normal or bold).
#' @param title_lineheight The line-height of the title.
#' @param subtitle_font_size The size of the subtitle font (in px).
#' @param subtitle_font_weight The weight of the subtitle (normal or bold).
#' @param subtitle_lineheight The line-height of the subtitle.
#' @param logo_height The height of the logo (in px).
#'
#' @examples
#' \donttest {
#' library(gt)
#' library(dplyr)
#' library(cbbplotR)
#'
#' gt(head(mtcars)) %>%
#' tab_header(title = html(gt_cbb_logo_title('Test title', 'Test subtitle', 'Duke')))
#'
#' }
#' @export
gt_cbb_logo_title <- function(title,
                              subtitle,
                              value = NULL,
                              logo_link = NULL,
                              type = c('team', 'conference', 'headshot'),
                              title_font_size = 24,
                              title_font_weight = 'bold',
                              title_lineheight = 0.5,
                              subtitle_font_size = 16,
                              subtitle_font_weight = 'normal',
                              subtitle_lineheight = 0.5,
                              logo_height = 65) {

  # logo link is an optional parameter in case a user wants to plot a different logo (e.g. retro)
  link <- if(!is.null(logo_link)) {
    logo_link
  } else if(type == 'team') {
    cbbplotR:::cbb_logo_links[value]
  } else if (type == 'conference') {
    cbbplotR:::conf_logo_links[value]
  } else if (type == 'headshot') {
    paste0('https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/', value, '.png')
  }

  title_header <- glue::glue(
    "<div style='display: flex; justify-content: space-between; align-items: center;'>
     <div style='flex-grow: 1;'>
       <span style='font-weight: {title_font_weight}; font-size: {title_font_size}px; line-height: {title_lineheight};'>{title}</span><br>
       <span style='font-size: {subtitle_font_size}px; font-weight: {subtitle_font_weight}; line-height: {subtitle_lineheight};'>{subtitle}</span>
     </div>
     <div>
       <img src='{link}' style='height: {logo_height}px; width: auto; vertical-align: middle;'>
     </div>
   </div>"
  )

  return(title_header)

}


