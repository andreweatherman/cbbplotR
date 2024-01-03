# internal helper that outputs local path to logo files
logo_from_team <- function(team, type){
  # get name matches from cache
  cbbplotR:::.team_name_matches()
  team_matches <- readRDS(cbbplotR:::team_matches_path())
  # run it through the matches
  team <- team_matches[team]

  img_vctr <- paste0("CBB", "/", team, ".png")
  # This used to call the following system.file line
  # but it drops non matches which results in errors
  # system.file(img_vctr, package = "nbaplotR")

  # Now we use some code from system.file but keep the non matches
  packagePath <- find.package("cbbplotR", quiet = TRUE)
  img_files <- file.path(packagePath, img_vctr)
  present <- file.exists(img_files)
  img_files[!present] <- img_vctr[!present]

  img_files
}

logo_from_conference <- function(conference, type){
  # get name matches from cache
  cbbplotR:::.conf_name_matches()
  conf_matches <- readRDS(cbbplotR:::conf_matches_path())
  # run it through the matches
  conference <- conf_matches[conference]
  img_vctr <- paste0("CONF", "/", conference, ".png")
  # This used to call the following system.file line
  # but it drops non matches which results in errors
  # system.file(img_vctr, package = "nbaplotR")

  # Now we use some code from system.file but keep the non matches
  packagePath <- find.package("cbbplotR", quiet = TRUE)
  img_files <- file.path(packagePath, img_vctr)
  present <- file.exists(img_files)
  img_files[!present] <- img_vctr[!present]

  img_files
}

parquet_from_url <- function(url) {
  rlang::check_installed("arrow")

  load <- try(curl::curl_fetch_memory(url), silent = TRUE)

  if (inherits(load, "try-error")) {
    cli::cli_warn("Failed to retrieve data from {.url {url}}")
    return(data.frame())
  }

  content <- try(arrow::read_parquet(load$content), silent = TRUE)

  if (inherits(content, "try-error")) {
    cli::cli_warn("Failed to parse file with {.fun arrow::read_parquet()} from {.url {url}}")
    return(data.frame())
  }

  content <- data.frame(content)
  return(content)
}

