get_game_details_api <- function(gm_id, verbose = T) {
  if (verbose) {
    message("Getting game {gm_id} details (API)" |> glue::glue())
  }

  .validate_gm_id_format(gm_id)

  pbp_json <- get_game_raw_pbp_json_api(gm_id, verbose)
  shifts_json <- get_game_raw_shifts_json_api(gm_id, verbose)
  info_json <- get_game_raw_info_json_api(gm_id, verbose)

  list(
    meta = pbp_json |> extract_game_metadata_from_raw_pbp_json_api(verbose),
    rosters = pbp_json |> extract_game_rosters_from_raw_pbp_json_api(verbose),
    scratches = info_json |> extract_scratches_from_raw_info_json_api(pbp_json, verbose),
    coaches = info_json |> extract_coaches_from_raw_info_json_api(pbp_json, verbose),
    referees = info_json |> extract_referees_from_raw_info_json_api(pbp_json, verbose),
    linesmen = info_json |> extract_linesmen_from_raw_info_json_api(pbp_json, verbose),
    shifts = shifts_json |> extract_game_shifts_from_raw_shifts_json_api(verbose),
    pbp = pbp_json |> extract_pbp_from_raw_pbp_json_api(verbose)
  )
}

get_game_details_html <- function(gm_id, verbose = T) {
  if (verbose) {
    message("Getting game {gm_id} details (HTML)" |> glue::glue())
  }

  .validate_gm_id_format(gm_id)

  gm_id <- gm_id |> as.character()

  roster_html <- get_game_rosters_raw_html(gm_id, verbose)
  home_shifts_html <- get_game_shifts_raw_html(gm_id, "home", verbose)
  away_shifts_html <- get_game_shifts_raw_html(gm_id, "away", verbose)
  pbp_html <- get_game_pbp_raw_html(gm_id, verbose)

  list(
    meta = roster_html |> extract_game_metadata_from_raw_html(gm_id, verbose),
    rosters = roster_html |> extract_rosters_from_raw_roster_html(gm_id, verbose),
    scratches = roster_html |> extract_scratches_from_raw_roster_html(gm_id, verbose),
    coaches = roster_html |> extract_coaches_from_raw_roster_html(gm_id, verbose),
    referees = roster_html |> extract_referees_from_raw_roster_html(gm_id, verbose),
    linesmen = roster_html |> extract_linesmen_from_raw_roster_html(gm_id, verbose),
    shifts =
      dplyr::bind_rows(
        extract_shifts_from_raw_shifts_html(home_shifts_html, gm_id, "home", verbose),
        extract_shifts_from_raw_shifts_html(away_shifts_html, gm_id, "away", verbose)
      ),
    pbp = pbp_html |> extract_pbp_from_raw_pbp_html(gm_id, verbose)
  )
}

get_game_details_all_sources <- function(gm_id, verbose = T) {
  if (verbose) {
    message("Getting game {gm_id} details" |> glue::glue())
  }

  .validate_gm_id_format(gm_id)

  gm_id <- gm_id |> as.character()

  list(
    api_results = get_game_details_api(gm_id, verbose),
    html_results = get_game_details_html(gm_id, verbose)
  )
}

get_game_details_all_sources_clean <- function(gm_id, verbose = T) {
  if (verbose) {
    message("Getting game {gm_id} details (Clean)" |> glue::glue())
  }

  .validate_gm_id_format(gm_id)

  gm_id <- gm_id |> as.character()

  scrape_results <- get_game_details_all_sources(gm_id, verbose)

  clean_game_details_all_sources(scrape_results, verbose)
}

