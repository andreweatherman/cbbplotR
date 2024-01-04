#' Get ESPN Player IDs for a team
#'
#' This grab player IDs from ESPN to be used as headshot column
#'
#' @param team Team name
#'
#' @examples
#' \donttest {
#' cbbplotR::get_espn_players('Duke')
#' }
#' @export
get_espn_players <- function(team) {

  team <- cbbplotR:::match_team(team)
  espn_id <- cbb_espn_ids[team]

  data <- jsonlite::fromJSON(paste0('http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/teams/', espn_id, '/roster')) %>%
    purrr::pluck('athletes') %>%
    dplyr::as_tibble() %>%
    dplyr::select(id, displayName)

  return(data)

}

