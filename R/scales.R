#' Color and fill for college basketball teams
#'
#' These functions map college basketball team names to their team colors in color and fill aesthetics
#' @inheritParams ggplot2::scale_x_discrete
#' @param size The logo size in pixels. It is applied as height for an x-scale
#'   and as width for an y-scale.
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
