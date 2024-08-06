## cbb_teams <- cbbdata::cbd_teams()

 purrr::walk2(.x = cbb_teams$common_team,
              .y = cbb_teams$espn_dark_logo,
              \(team, logo) download.file(logo, glue::glue('inst/CBB/dark/{team}.png'), mode = 'wb'))
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

cbb_dark_logo_links <- cbbdata::cbd_teams() %>% pull('espn_dark_logo') %>%
rlang::set_names(cbbdata::cbd_teams()$common_team)

cbb_wordmark_links <- cbbdata::cbd_teams() %>% mutate(wordmark = ifelse(is.na(wordmark), logo, wordmark)) %>%
  pull('wordmark') %>% rlang::set_names(cbbdata::cbd_teams()$common_team)

cbb_espn_ids <- cbbdata::cbd_teams() %>% pull('espn_id') %>%
  rlang::set_names(cbbdata::cbd_teams()$common_team)

abbr_dict <- cbd_teams() %>% pull('espn_abbreviation') %>% set_names(cbd_teams() %>% pull('common_team'))

#
dat <- read_csv('/Users/andrewweatherman/conf_logo.csv')
#
 purrr::walk2(.x = dat$conf,
              .y = dat$wordmark,
              \(conf, logo) download.file(logo, glue::glue('inst/CONF/wordmark/{conf}.png'), mode = 'wb'))

 conf_primary_colors <- dat %>% pull('primary') %>%
   rlang::set_names(dat$conf)
#
 conf_secondary_colors <- dat %>% pull('secondary') %>%
   rlang::set_names(dat$conf)
#
 conf_logo_links <- dat %>% pull('logo') %>%
   rlang::set_names(dat$conf)

 conf_wordmark_links <- dat %>% pull('wordmark') %>%
   rlang::set_names(dat$conf)
#
 usethis::use_data(
   cbb_primary_colors, cbb_secondary_colors, conf_primary_colors, conf_secondary_colors,
   cbb_logo_links, cbb_dark_logo_links, cbb_wordmark_links, cbb_espn_ids,
   conf_logo_links, conf_wordmark_links,
   internal = FALSE, overwrite = TRUE
 )
