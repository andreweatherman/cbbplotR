#' Clear cbbplotR Cache
#'
#' This function clears the memoised cache of all functions memoised by `cbbplotR`.
#'
#' @export
#' @return Invisibly `NULL`
#' @examples
#' cbbplotR_clear_cache()
cbbplotR_clear_cache <- function(){
  to_delete <- c(team_matches_path(), conf_matches_path())
  file.remove(to_delete[file.exists(to_delete)])
  invisible(TRUE)
}

# build team matching dictionary (this runs once per package load)
# has the benefit of being updated on the cbbdata API so as to not
# require a package update to access new names

team_matches_path <- function() file.path(tools::R_user_dir("cbbplotR", "cache"), "team_matches.rds")
conf_matches_path <- function() file.path(tools::R_user_dir("cbbplotR", "cache"), "conf_matches.rds")

.team_name_matches <- function() {

  if(!file.exists(team_matches_path())) {
    saveRDS(cbbplotR:::build_team_name_matches(), team_matches_path())
  }

  else {
    readRDS(team_matches_path())
  }

}

.conf_name_matches <- function() {

  if(!file.exists(conf_matches_path())) {
    saveRDS(cbbplotR:::build_conf_name_matches(), conf_matches_path())
  }

  else {
    readRDS(conf_matches_path())
  }

}

build_team_name_matches <- function() {

  data <- cbbplotR:::parquet_from_url('https://www.cbbdata.com/api/data/teams?')

  data <- dplyr::select(data, -c(arena_lon, 
        arena_lat, espn_logo, espn_dark_logo, logo, wordmark, 
        color, alt_color, primary_arena, capacity, arena_elevation, 
        arena_elevation_rank))
  
  data <- tidyr::pivot_longer(data, -common_team)

  cbb_matches <- dplyr::pull(data, 'common_team')
  cbb_matches <- rlang::set_names(cbb_matches, data$value)

  return(cbb_matches)

}

build_conf_name_matches <- function() {

  data <- cbbplotR:::parquet_from_url('https://www.cbbdata.com/api/data/conf?')

  data <- tidyr::pivot_longer(data, -main_name)

  conf_matches <- dplyr::pull(data, 'main_name')
  conf_matches <- rlang::set_names(conf_matches, data$value)

  return(conf_matches)

}
