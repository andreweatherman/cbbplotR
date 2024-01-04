.onLoad <- function(libname,pkgname){

  backports::import(pkgname, "R_user_dir", force = TRUE)

  is_online <- !is.null(curl::nslookup("github.com", error = FALSE))
  keep_matches <- isTRUE(getOption("cbbplotR.keep_matches", FALSE))

  if(!is_online && !keep_games) rlang::warn("GitHub.com seems offline, and `options(cbbplotR.keep_matches)` is not set to TRUE. Deleting the matches cache may not be available without an internet connection.")

  if(!is_online && keep_games) rlang::warn("GitHub.com seems offline, and `options(cbbplotR.keep_matches)` is set to TRUE. To get updates, clear the games cache with `cbbplotR::cbbplotR_clear_cache()`")

  # create package cache directory if it doesn't exist
  if (!dir.exists(R_user_dir("cbbplotR", "cache"))){
    dir.create(R_user_dir("cbbplotR", "cache"), recursive = TRUE, showWarnings = FALSE)
  } else if ((file.exists(cbbplotR:::team_matches_path()) | file.exists(cbbplotR:::conf_matches_path()))
             && !keep_matches) {
    suppressWarnings({
      file.remove(cbbplotR:::team_matches_path())
      file.remove(cbbplotR:::conf_matches_path())
    })

  }
}
