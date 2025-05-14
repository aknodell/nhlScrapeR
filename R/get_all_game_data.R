get_game_details_all_sources <- function(gm_id) {
  message("getting game {gm_id} details" |> glue::glue())
  gm_id <- gm_id |> as.character()

  list(
    api_results = get_game_details_api(gm_id),
    html_results = get_game_details_html(gm_id)
  )
}

get_game_details_all_sources_clean <- function(gm_id) {
  message("getting game {gm_id} details" |> glue::glue())
  gm_id <- gm_id |> as.character()

  list(
    api_results = get_game_details_api(gm_id),
    html_results = get_game_details_html(gm_id)
  ) |>
    clean_game_details_all_sources()
}

