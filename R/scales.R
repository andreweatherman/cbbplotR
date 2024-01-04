#' Color and Fill Scales for College Basketball Teams and Conferences
#'
#' These functions provide color and fill scales for ggplot2 based on college basketball team and conference colors.
#'
#' @section Functions:
#' - `scale_color_cbb_teams()`: Provides color scales for college basketball teams.
#' - `scale_fill_cbb_teams()`: Provides fill scales for college basketball teams.
#' - `scale_color_cbb_conferences()`: Provides color scales for college basketball conferences.
#' - `scale_fill_cbb_conferences()`: Provides fill scales for college basketball conferences.
#'
#' @inheritParams ggplot2::scale_x_discrete
#' @param type The type of color scale, either `"primary"` or `"secondary"`.
#' @param values A named list of colors to be used if overriding the default team or conference colors.
#' @param aesthetics The ggplot2 aesthetic to apply the scale to, defaults to `"colour"` or `"fill"`.
#' @param breaks A set of breaks for the scale, defaults to `ggplot2::waiver()`.
#' @param na.value The color to use for missing values.
#' @param guide A guide for the scale, such as a legend.
#' @param alpha Transparency level [0-1], applied to all colors if not `NA`.
#' @name scale_cbb
#' @aliases NULL
#' @return A ggplot2 color or fill scale.
#'
NULL

#' @rdname scale_cbb
#' @export
scale_color_cbb_teams <- function(type = c("primary", "secondary"),
                            values = NULL,
                            ...,
                            aesthetics = "colour",
                            breaks = ggplot2::waiver(),
                            na.value = "grey50",
                            guide = NULL,
                            alpha = NA) {

  type <- rlang::arg_match0(type, c("primary", "secondary"))

  if(is.null(values)){
    values <- switch(type,
                     "primary"   = cbb_primary_colors,
                     "secondary" = cbb_secondary_colors
    )
  }

  if(!is.na(alpha)) values <- scales::alpha(values, alpha = alpha)

  ggplot2::scale_color_manual(
    ...,
    values = values,
    aesthetics = aesthetics,
    breaks = breaks,
    na.value = na.value,
    guide = guide
  )
}

#' @rdname scale_cbb
#' @export
scale_colour_cbb_teams <- scale_color_cbb_teams

#' @rdname scale_cbb
#' @export
scale_fill_cbb_teams <- function(type = c("primary", "secondary"),
                           values = NULL,
                           ...,
                           aesthetics = "fill",
                           breaks = ggplot2::waiver(),
                           na.value = "grey50",
                           guide = NULL,
                           alpha = NA) {

  type <- rlang::arg_match0(type, c("primary", "secondary"))

  if(is.null(values)){
    values <- switch(type,
                     "primary"   = cbb_primary_colors,
                     "secondary" = cbb_secondary_colors
    )
  }

  if(!is.na(alpha)) values <- scales::alpha(values, alpha = alpha)

  ggplot2::scale_fill_manual(
    ...,
    values = values,
    aesthetics = aesthetics,
    breaks = breaks,
    na.value = na.value,
    guide = guide
  )
}


### ==== CONFERENCE

#' @rdname scale_cbb
#' @export
scale_color_cbb_conferences <- function(type = c("primary", "secondary"),
                            values = NULL,
                            ...,
                            aesthetics = "colour",
                            breaks = ggplot2::waiver(),
                            na.value = "grey50",
                            guide = NULL,
                            alpha = NA) {

  type <- rlang::arg_match0(type, c("primary", "secondary"))

  if(is.null(values)){
    values <- switch(type,
                     "primary"   = conf_primary_colors,
                     "secondary" = conf_secondary_colors
    )
  }

  if(!is.na(alpha)) values <- scales::alpha(values, alpha = alpha)

  ggplot2::scale_color_manual(
    ...,
    values = values,
    aesthetics = aesthetics,
    breaks = breaks,
    na.value = na.value,
    guide = guide
  )
}

#' @rdname scale_cbb
#' @export
scale_colour_cbb_conferences <- scale_color_cbb_conferences

#' @rdname scale_cbb
#' @export
scale_fill_cbb_conferences <- function(type = c("primary", "secondary"),
                           values = NULL,
                           ...,
                           aesthetics = "fill",
                           breaks = ggplot2::waiver(),
                           na.value = "grey50",
                           guide = NULL,
                           alpha = NA) {

  type <- rlang::arg_match0(type, c("primary", "secondary"))

  if(is.null(values)){
    values <- switch(type,
                     "primary"   = conf_primary_colors,
                     "secondary" = conf_secondary_colors
    )
  }

  if(!is.na(alpha)) values <- scales::alpha(values, alpha = alpha)

  ggplot2::scale_fill_manual(
    ...,
    values = values,
    aesthetics = aesthetics,
    breaks = breaks,
    na.value = na.value,
    guide = guide
  )
}
