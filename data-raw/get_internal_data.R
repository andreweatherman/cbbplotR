## cbb_teams <- cbbdata::cbd_teams() %>% select(team = common_team, logo)

# purrr::walk2(.x = dat$team,
#              .y = dat$logo,
#              \(team, logo) download.file(logo, glue::glue('inst/CBB/{team}.png'), mode = 'wb'))
#
#
# ## get team colors
cbb_primary_colors <- cbbdata::cbd_teams() %>% pull('color') %>%
rlang::set_names(cbbdata::cbd_teams()$common_team)
#
cbb_secondary_colors <- cbbdata::cbd_teams() %>% pull('alt_color') %>%
rlang::set_names(cbbdata::cbd_teams()$common_team)
#
cbb_logo_links <- cbbdata::cbd_teams() %>% pull('logo') %>%
rlang::set_names(cbbdata::cbd_teams()$common_team)
#
#
# dat <- read_csv('/Users/andrewweatherman/conf_logo.csv')
#
# purrr::walk2(.x = dat$conf,
#              .y = dat$logo,
#              \(conf, logo) download.file(logo, glue::glue('inst/CONF/{conf}.png'), mode = 'wb'))
#
# conf_primary_colors <- dat %>% pull('primary') %>%
#   rlang::set_names(dat$conf)
#
# conf_secondary_colors <- dat %>% pull('secondary') %>%
#   rlang::set_names(dat$conf)
#
# conf_logo_links <- dat %>% pull('logo') %>%
#   rlang::set_names(dat$conf)
#
# use_data(
#   cbb_primary_colors, cbb_secondary_colors, conf_primary_colors, conf_secondary_colors,
#   cbb_logo_links, conf_logo_links
#   internal = FALSE, overwrite = TRUE
# )
