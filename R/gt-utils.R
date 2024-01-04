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
#'
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
    cbbplotR::cbb_logo_links[value]
  } else if (type == 'conference') {
    cbbplotR::conf_logo_links[value]
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



#' Color Rows in gt Table Based on Win/Loss Results
#'
#' This function styles rows in a `gt` table based on win/loss
#' results. It allows customization of colors and text for win and loss rows.
#' Users can specify the column with result data, customize colors, and choose
#' between traditional win/loss or binary result types.
#'
#' @param gt_object A gt table object.
#' @param result_column The name of the column in `data` that contains the
#'   result indicators. Default is 'result'.
#' @param win_color The background color for rows indicating a win. Default is
#'   '#5DA271' (green).
#' @param loss_color The background color for rows indicating a loss. Default is
#'   '#C84630' (red).
#' @param wins_text_color The text color for rows indicating a win. Default is
#'   'white'.
#' @param loss_text_color The text color for rows indicating a loss. Default is
#'   'white'.
#' @param result_type The type of result indicators used in the `result_column`.
#'   Options are 'wl' (default, for 'W' and 'L') or 'binary' (for 1 and 0).
#'
#' @return A `gt` table with styled rows based on the win/loss results.
#'
#' @export
gt_color_results <- function(gt_object,
                             result_column = 'result',
                             win_color = '#5DA271',
                             loss_color = '#C84630',
                             wins_text_color = 'white',
                             loss_text_color = 'white',
                             result_type = 'wl') {

  # adjust the result matching based on result_type
  if(result_type == 'binary') {
    win_condition <- 1
    loss_condition <- 0
  } else {
    win_condition <- 'W'
    loss_condition <- 'L'
  }

  # extract data
  data <- gt_object[['_data']]

  # apply styles
  gt_object %>%
    gt::tab_style(
      locations = gt::cells_body(rows = which(data[[result_column]] == win_condition)),
      style = list(gt::cell_fill(color = win_color), gt::cell_text(color = wins_text_color))
    ) %>%
    gt::tab_style(
      locations = gt::cells_body(rows = which(data[[result_column]] == loss_condition)),
      style = list(gt::cell_fill(color = loss_color), gt::cell_text(color = loss_text_color))
    )

}


#' Set Font Family for All Parts of a gt Table
#'
#' This function sets the font family for all customizable parts of a `gt` table
#' including title, table body, labels, label spanners, and more. This is a convenience
#' function if you want to quickly apply a font to your table.
#'
#' @param gt_table A gt table object.
#' @param font_family The font family to apply to the entire table.
#' @param from_goog_font Should the font be pulled from Google Fonts (TRUE;
#'   default) or from your local machine (FALSE)
#'
#' @return A `gt` table with the specified font family applied to all customizable parts.
#'
#' @export
gt_set_font <- function(gt_table,
                        font_family,
                        from_google_font = TRUE) {

  style_list <- ifelse(from_google_font == TRUE,
                       list(cell_text(font = gt::google_font(font_family))),
                       list(cell_text(font = font_family)))

  gt_table %>%
    gt::tab_style(
      style = style_list,
      locations = list(
        gt::cells_title(),
        gt::cells_stubhead(),
        gt::cells_column_spanners(spanners = gt::everything()),
        gt::cells_column_labels(columns = gt::everything()),
        gt::cells_row_groups(groups = gt::everything()),
        gt::cells_stub(rows = gt::everything()),
        gt::cells_body(columns = gt::everything()),
        #gt::cells_summary(columns = gt::everything(), rows = gt::everything(), groups = gt::everything()),
        #gt::cells_grand_summary(columns = gt::everything(), rows = gt::everything()),
        #gt::cells_stub_summary(rows = gt::everything(), groups = gt::everything()),
        #gt::cells_stub_grand_summary(rows = gt::everything()),
        gt::cells_footnotes(),
        gt::cells_source_notes()
      )
    )
}



#' Bold rows in a GT table with optional filtering and highlighting
#'
#' `gt_bold_rows` applies bold styling to rows of a GT table. You can specify rows to bold through a
#' filter statement or by default, all rows are bolded. There is also an option to change the text
#' color and add a background highlight color to the bolded rows.
#'
#' @param gt_object A GT table object.
#' @param row A vector of row indices to bold, default is all rows using `gt::everything()`.
#' @param text_color The color to be used for the text, default is 'black'.
#' @param highlight_color The color to be used for the row background highlight, default is 'white'.
#'        If `NULL`, no background color will be applied.
#' @param filter_statement A character string that contains a filter expression which is evaluated
#'        to determine which rows to bold. It should be a valid R expression as a string,
#'        for example, `"column_name > 5"`. Default is `NULL`, which means no filter is applied.
#'
#' @return A GT table object with the specified rows styled in bold text, and optionally with
#'         changed text color and background highlight.
#'
#' @export
gt_bold_rows <- function(gt_object,
                        row = gt::everything(),
                        text_color = 'black',
                        highlight_color = NULL,
                        filter_statement = NULL) {

  # extract data
  data <- gt_object[['_data']]

  # get rows that correspond to filter statement if one is given
  rows_to_change <- if(!is.null(filter_statement)) {
    which(eval(parse(text = filter_statement), envir = data))
  } else {
    1:nrow(data)
  }

  highlight_color <- ifelse(is.null(highlight_color), 'white', highlight_color)

  # apply styles
  gt_object %>%
    gt::tab_style(
      locations = gt::cells_body(rows = rows_to_change),
      style = list(gt::cell_fill(color = highlight_color), gt::cell_text(color = text_color,
                                                                   weight = 'bold'))
    )

}


#' Render in Logo-Team HTML for Tables
#'
#' Appends a column with HTML that will render in team logos and names
#' for table plotting.
#'
#' I really hate putting team logos and names in separate columns,
#' and this helper function will add conference logos or wordmarks and create
#' the HTML code to render in logos and teams in the same column for tables.
#' If you are using `gt` to plot, you must use `fmt_markdown({logo_column})` to
#' render in the HTML. Set `logo_column` to your `team` variable if you wish
#' to overwrite your `team` column with the logo HTML (recommended but not default).
#'
#' @returns Returns data with an appended HTML column.
#' @param data Pass through your plotting data
#' @param team_column Indicate which column contains your `team` variable
#' @param logo_column Indicates the name of the HTML column
#' @param logo_type Indicate whether you want to plot logos ('logo', default) or
#' wordmarks ('wordmark')
#' @param logo_height The height of the logo or wordmark in the HTML string
#' @param include_name Should the conference name be included in the column?
#'   Defaults to FALSE as wordmarks are distinctive.
#' @import dplyr
#' @importFrom glue glue
#' @importFrom rlang ensym as_string
#' @importFrom magrittr %>%
#'
#' @export
gt_cbb_teams <- function(data, team_column, logo_column = 'team_logo',
                         logo_type = 'logo', logo_height = 25, include_name = TRUE) {

  # load the team matches
  cbbplotR:::.team_name_matches()
  team_matches <- readRDS(cbbplotR:::team_matches_path())
  matching_dict <- if(logo_type == 'logo') cbb_logo_links else cbb_wordmark_links

  team_column_sym <- rlang::ensym(team_column)

  data <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      converted_team = team_matches[[as.character(.data[[team_column_sym]])]],
      logo = matching_dict[converted_team]
    ) %>%
    dplyr::mutate(
      {{logo_column}} := glue("<img src='{logo}' style='height: {logo_height}px; width: auto; vertical-align: middle;'> {ifelse(include_name, converted_team, '')}")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(logo, converted_team))

  return(data)
}


#' Render in Conference Wordmarks in `gt` Tables
#'
#' Appends a column with HTML that will render in conference wordmarks and
#' (optionally) names for table plotting.
#'
#' I really hate putting conference logos and names in separate columns,
#' and this helper function will add conference wordmarks and create
#' the HTML code to render in wordmarks and columns in the same column for tables.
#' If you are using `gt` to plot, you must use `fmt_markdown({logo_column})` to
#' render in the HTML. Set `logo_column` to your `conf` variable if you wish
#' to overwrite your `conf` column with the logo HTML (recommended but not default).
#'
#' @returns Returns data with an appended HTML column.
#' @param data Pass through your plotting data
#' @param conf_column Indicate which column contains your `team` variable
#' @param logo_column Indicates the name of the HTML column
#' @param logo_height The height of the logo or wordmark in the HTML string
#' @param include_name Should the conference name be included in the column?
#'   Defaults to FALSE as wordmarks are distinctive.
#' @import dplyr
#' @importFrom glue glue
#' @importFrom rlang ensym as_string
#' @importFrom magrittr %>%
#'
#' @export
gt_cbb_conferences <- function(data, conf_column, logo_column = 'conf_logo',
                               logo_height = 20, include_name = FALSE) {

  # load conference matches
  cbbplotR:::.conf_name_matches()
  conf_matches <- readRDS(cbbplotR:::conf_matches_path())

  conf_column_sym <- rlang::ensym(conf_column)

  data <- data %>%
    rowwise() %>%
    mutate(
      converted_conf = conf_matches[[as.character(.data[[conf_column_sym]])]],
      logo = conf_wordmark_links[converted_conf]
    ) %>%
    mutate(
      {{logo_column}} := glue("<img src='{logo}' style='height: {logo_height}px; width: auto; vertical-align: middle;'> {ifelse(include_name, converted_conf, '')}")
    ) %>%
    ungroup() %>%
    select(-c(logo, converted_conf))

  return(data)
}



#' The Athletic `gt` Table Theme
#'
#' A theme for styling `gt` tables similar to The Athletic.
#'
#' @returns Returns data with an appended HTML column.
#' @param gt_object An existing gt table object of class `gt_tbl`
#' @param ... Optional additional arguments to `gt::table_options()`
#' @import gt
#' @importFrom magrittr %>%
#'
#' @export
gt_theme_athletic <- function(gt_object, ...) {

  stopifnot(`'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` = "gt_tbl" %in%
              class(gt_object))

  table_id <- subset(gt_object[['_options']], parameter == 'table_id')$value[[1]]

  if (is.na(table_id)) {
    table_id <- gt::random_id()
    opt_position <- which("table_id" %in% gt_object[["_options"]][["parameter"]])[[1]]
    gt_object[["_options"]][["value"]][[opt_position]] <- table_id
  }

  table <- gt_object %>%
    gt::opt_table_font(
      font = list(
        gt::google_font('Spline Sans Mono'),
        gt::default_fonts()
      ),
      weight = 500
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_labels(
        columns = gt::everything()
      ),
      style = gt::cell_text(
        font = gt::google_font('Work Sans'),
        weight = 650,
        size = px(12),
        transform = 'uppercase'
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_title('title'),
      style = gt::cell_text(
        font = gt::google_font('Work Sans'),
        weight = 650
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_title('subtitle'),
      style = gt::cell_text(
        font = gt::google_font('Work Sans'),
        weight = 500
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = 'left', weight = px(0.5), color = 'black'),
      locations = gt::cells_body(
        columns = c(-names(gt_object[['_data']])[1])
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = "top", color = 'black', weight = px(1.5), style = 'dotted'),
      locations = gt::cells_body(
        rows = gt::everything()
      )
    ) %>%
    gt::cols_align(
      align = 'center',
      columns = gt::everything()
    ) %>%
    gt::tab_options(
      table.font.size = 12,
      column_labels.border.bottom.width = 2,
      column_labels.border.bottom.color = 'black',
      column_labels.border.top.color = 'white',
      row_group.border.bottom.color = 'white',
      table.border.top.style = 'none',
      table.border.bottom.style = 'none',
      heading.border.bottom.style = 'none',
      heading.align = 'left',
      heading.title.font.size = px(26),
      source_notes.border.lr.style = 'none',
      source_notes.font.size = 10,
      ...
    ) %>%
    gt::opt_css(
      paste0("#", table_id, " tbody tr:last-child {border-bottom: 2px solid #ffffff00;}"),
      add = TRUE
    )

  return(table)

}
