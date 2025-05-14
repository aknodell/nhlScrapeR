.extract_details_from_html_pbp <- function(html_pbp, scrape_results) {
  message("cleaning play-by-play (html)")
  teams <-
    c(
      scrape_results$api_results$meta$away_team,
      scrape_results$api_results$meta$home_team
    )

  sweater_id_lookup <-
    scrape_results$api_results$rosters |>
    dplyr::select(team, sweater_number, api_id) |>
    tidyr::unite("sweater_number", team, sweater_number, sep = "#")

  html_pbp |>
    dplyr::mutate(
      event_team_html =
        ifelse(
          event_type %in%
            c(
              "BLOCK", "MISS", "SHOT", "GOAL",
              "HIT", "GIVE", "TAKE", "FAC", "PENL", "DELPEN", "CHL"
            ),
          stringr::str_sub(event_description, end = 3),
          NA_character_
        ),
      event_team_zone_html =
        event_description |>
        stringr::str_to_upper() |>
        stringr::str_extract("(OFF|DEF|NEU)\\.? ZONE") |>
        stringr::str_sub(end = 1),
      event_player_1_sweater_html =
        dplyr::case_when(
          event_type == "FAC" ~
            event_description |>
            stringr::str_extract(stringr::str_c(event_team_html, '\\s*#\\d+')),
          event_type %in% c("MISS", "BLOCK", "PENL", "HIT") ~
            event_description |>
            stringr::str_extract('[A-Z\\.]{3}\\s*#\\d+'),
          event_type %in% c("SHOT", "GOAL", "GIVE", "TAKE") ~
            stringr::str_c(
              event_team_html |>
                stringr::str_c(
                  event_description |>
                    stringr::str_extract('#\\d+')
                )
            ),
          T ~ NA_character_
        ) |>
        stringr::str_squish(),
      event_player_2_sweater_html =
        dplyr::case_when(
          event_type %in% c("FAC", "PENL", "HIT", "BLOCK") ~
            event_description |>
            stringr::str_remove(event_player_1_sweater_html) |>
            stringr::str_extract('[A-Z\\.]{3}\\s?#\\d+'),
          event_type == "GOAL" ~
            stringr::str_c(
              event_team_html,
              event_description |>
                stringr::str_remove(
                  event_player_1_sweater_html |>
                    stringr::str_extract("#\\d+")
                ) |>
                stringr::str_extract('#\\d+')
            ),
          T ~ NA_character_
        ) |>
        stringr::str_squish(),
      event_player_3_sweater_html =
        dplyr::case_when(
          event_type == "GOAL" ~
            stringr::str_c(
              event_team_html,
              event_description |>
                stringr::str_remove(
                  event_player_1_sweater_html |>
                    stringr::str_extract("#\\d+")
                ) |>
                stringr::str_remove(
                  event_player_2_sweater_html |>
                    stringr::str_extract("#\\d+")
                ) |>
                stringr::str_extract('#\\d+')
            ),
          T ~ NA_character_
        ) |>
        stringr::str_squish(),
      dplyr::across(
        c(event_player_1_sweater_html, event_player_2_sweater_html, event_player_3_sweater_html),
        .fns = (\(x) stringr::str_remove_all(x, "\\s"))
      ),
      event_detail_1_html =
        purrr::map2_chr(
          event_type,
          event_description,
          function(t, d) {
            if (t %in% c("BLOCK", "MISS", "SHOT", "GOAL")) {
              if (d |> stringr::str_to_upper() |> stringr::str_detect("OWN GOAL")) {
                "OWN GOAL"
              } else {
                ret <-
                  d |>
                  stringr::str_split(",") |>
                  purrr::flatten_chr() |>
                  purrr::pluck(
                    ifelse(
                      d |> stringr::str_to_upper() |> stringr::str_detect("PENALTY SHOT"),
                      3,
                      2
                    )
                  ) |>
                  stringr::str_to_upper() |>
                  stringr::str_squish()

                if (stringr::str_detect(ret, "(OFF|DEF|NEU)\\.? ZONE")) {
                  NA_character_
                } else {
                  ret
                }
              }
            } else if (t == "PENL") {
              d |>
                stringr::str_extract("^.*\\(") |>
                stringr::str_remove("^.*[A-Z]{2}") |>
                stringr::str_remove("\\(") |>
                stringr::str_to_upper() |>
                stringr::str_remove_all("[^A-Z\\s-]") |>
                stringr::str_squish()
            } else {
              NA_character_
            }
          }
        ),
      event_detail_2_html =
        purrr::pmap_chr(
          list(
            t = event_type,
            d = event_description,
            detail = event_detail_1_html
          ),
          function(t, d, detail) {
            if (is.na(detail)) {
              NA_character_
            } else if (t %in% c("MISS", "SHOT", "GOAL", "BLOCK")) {
              ret <-
                d |>
                stringr::str_to_upper() |>
                stringr::str_split(",") |>
                purrr::flatten_chr() |>
                purrr::pluck(
                  ifelse(
                    d |> stringr::str_to_upper() |> stringr::str_detect("PENALTY SHOT|OWN GOAL"),
                    2,
                    3
                  ),
                  .default = NA_character_
                ) |>
                stringr::str_to_upper() |>
                stringr::str_squish()

              if (
                any(
                  stringr::str_detect(ret, "((OFF|DEF|NEU)\\.? ZONE)|(PENALTY SHOT)"),
                  ret == "",
                  is.null(ret),
                  length(ret) == 0,
                  is.na(ret)
                )
              ) {
                NA_character_
              } else {
                ret
              }
            } else if (t == "PENL") {
              d |>
                stringr::str_to_upper() |>
                stringr::str_extract("\\(\\d+\\s+MIN\\)") |>
                stringr::str_remove_all("[\\(\\)]") |>
                stringr::str_squish()
            } else {
              NA_character_
            }
          }
        ),
      event_detail_3_html =
        purrr::map2_chr(
          event_type,
          event_description,
          function(t, d) {
            if (t %in% c("MISS", "SHOT", "GOAL")) {
              splits <-
                d |>
                stringr::str_to_upper() |>
                stringr::str_count("(\\d+\\s*FT\\.?)|(\\(\\d+\\))|(FAILED ATTEMPT)")

              ret <-
                d |>
                stringr::str_to_upper() |>
                stringr::str_split("(\\d+\\s*FT\\.?)|(\\(\\d+\\))|(FAILED ATTEMPT)") |>
                purrr::flatten_chr() |>
                purrr::pluck(
                  splits + 1,
                  .default = NA_character_
                ) |>
                stringr::str_squish()

              if (
                any(
                  ret == "",
                  is.null(ret),
                  length(ret) == 0,
                  is.na(ret)
                )
              ) {
                NA_character_
              } else {
                ret
              }
            } else if (t == "BLOCK") {
              ret <-
                d |>
                stringr::str_to_upper() |>
                stringr::str_split("(OFF|DEF|NEU)\\.?\\s*ZONE") |>
                purrr::flatten_chr() |>
                purrr::pluck(
                  2,
                  .default = NA_character_
                ) |>
                stringr::str_squish()

              if (
                any(
                  ret == "",
                  is.null(ret),
                  length(ret) == 0,
                  is.na(ret)
                )
              ) {
                NA_character_
              } else {
                ret
              }
            } else {
              NA_character_
            }
          }
        ),
      next_faceoff_team = ifelse(event_type == "FAC", event_team_html, NA_character_),
      next_faceoff_zone = ifelse(event_type == "FAC", event_team_zone_html, NA_character_)
    ) |>
    tidyr::fill(c(next_faceoff_team, next_faceoff_zone), .direction = "up") |>
    dplyr::left_join(
      sweater_id_lookup |>
        dplyr::rename(event_player_1_html = api_id),
      by = dplyr::join_by(event_player_1_sweater_html == sweater_number)
    ) |>
    dplyr::left_join(
      sweater_id_lookup |>
        dplyr::rename(event_player_2_html = api_id),
      by = dplyr::join_by(event_player_2_sweater_html == sweater_number)
    ) |>
    dplyr::left_join(
      sweater_id_lookup |>
        dplyr::rename(event_player_3_html = api_id),
      by = dplyr::join_by(event_player_3_sweater_html == sweater_number)
    ) |>
    dplyr::mutate(
      event_team_html =
        ifelse(
          event_type == "STOP",
          purrr::pmap_chr(
            list(
              description = event_description,
              next_team = next_faceoff_team,
              next_zone = next_faceoff_zone
            ),
            function(description, next_team, next_zone) {
              if (
                description |>
                stringr::str_to_upper() |>
                stringr::str_detect(
                  "(ICING)|(GOALIE STOPPED)|(PUCK FROZEN - GOALIE)|(OFF-?SIDE)"
                )
              ) {
                if (next_zone == "D") {
                  next_team
                } else if (next_zone == "O") {
                  teams |>
                    purrr::discard(magrittr::equals, e2 = next_team)
                } else {
                  NA_character_
                }
              } else if (
                description |>
                stringr::str_to_upper() |>
                stringr::str_detect("TIMEOUT\\s*-?\\s*(HOME|AWAY)")
              ) {
                if (description |> stringr::str_to_upper() |> stringr::str_detect("HOME")) {
                  scrape_results$api_results$meta$home_team
                } else if (description |> stringr::str_to_upper() |> stringr::str_detect("AWAY")) {
                  scrape_results$api_results$meta$away_team
                } else {
                  NA_character_
                }
              } else {
                NA_character_
              }
            }
          ),
          event_team_html
        ),
      event_detail_3_html =
        ifelse(
          event_type == "STOP" &
            stringr::str_detect(event_description |> stringr::str_to_upper(), "OFF-?SIDE") &
            !is.na(event_team_html) &
            next_faceoff_zone != "N",
          "INTENTIONAL",
          event_detail_3_html
        ),
      event_distance_html =
        ifelse(
          event_type %in% c("MISS", "SHOT", "GOAL"),
          event_description |>
            stringr::str_to_upper() |>
            stringr::str_extract("\\d+\\s*FT\\.?") |>
            stringr::str_extract("\\d+") |>
            as.integer(),
          NA_integer_
        ) |>
        as.double()
    ) |>
    dplyr::select(
      -c(
        event_player_1_sweater_html:event_player_3_sweater_html,
        next_faceoff_team,
        next_faceoff_zone
      )
    )
}

.extract_details_from_api_pbp <- function(api_pbp, scrape_results) {
  message("cleaning play-by-play (api)")
  home_def_side_lookup <-
    api_pbp |>
    dplyr::filter(
      (event_team == scrape_results$api_results$meta$home_team_id &
         zone_code == "D") |
        (event_team == scrape_results$api_results$meta$away_team_id &
           zone_code == "O")
    ) |>
    dplyr::group_by(game_period) |>
    dplyr::summarise(coords_x = median(coords_x)) |>
    dplyr::mutate(
      home_team_def_zone_imp =
        dplyr::case_when(
          sign(coords_x) == -1 ~ "left",
          sign(coords_x) == 1 ~ "right",
          T ~ NA_character_
        )
    ) |>
    dplyr::select(-coords_x)

  home_team_abb <-
    tibble::tibble(
      event_team =
        c(
          scrape_results$api_results$meta$away_team_id,
          scrape_results$api_results$meta$home_team_id
        ),
      event_team_api =
        c(
          scrape_results$api_results$meta$away_team,
          scrape_results$api_results$meta$home_team
        )
    )

  api_pbp |>
    dplyr::left_join(
      home_team_abb,
      by = dplyr::join_by(event_team)
    ) |>
    dplyr::left_join(
      home_def_side_lookup,
      by = dplyr::join_by(game_period)
    ) |>
    dplyr::mutate(
      home_team_def_zone =
        purrr::map2_chr(
          home_team_def_zone,
          home_team_def_zone_imp,
          function(x, y) {
            tidyr::replace_na(x, y)
          }
        ),
      next_faceoff_x = ifelse(event_type == "faceoff", coords_x, NA_integer_),
      next_faceoff_team = ifelse(event_type == "faceoff", event_team_api, NA_character_),
      next_faceoff_zone = ifelse(event_type == "faceoff", zone_code, NA_character_)
    ) |>
    tidyr::fill(c(next_faceoff_x, next_faceoff_team, next_faceoff_zone), .direction = "up") |>
    dplyr::mutate(
      event_team_api =
        purrr::pmap_chr(
          list(
            t = event_type,
            team = event_team_api,
            description = event_reason_1,
            def_side = home_team_def_zone,
            next_x = next_faceoff_x,
            next_team = next_faceoff_team,
            next_zone = next_faceoff_zone
          ),
          function(t, team, description, def_side, next_x, next_team, next_zone) {
            if (t == "stoppage") {
              if (
                description |>
                stringr::str_to_lower() |>
                stringr::str_detect("(icing)|(goalie-stopped-after-sog)|(puck-frozen)")
              ) {
                if (next_zone == "D") {
                  next_team
                } else if (next_zone == "O") {
                  home_team_abb$event_team_api |>
                    purrr::discard(magrittr::equals, e2 = next_team)
                } else {
                  NA_character_
                }
              } else if (description |> stringr::str_to_lower() |> stringr::str_detect("offside")) {
                if (next_zone == "D") {
                  next_team
                } else if (next_zone == "O") {
                  home_team_abb$event_team_api |>
                    purrr::discard(magrittr::equals, e2 = next_team)
                } else {
                  if (tidyr::replace_na(def_side == "right", F)) {
                    if (next_x > 0) {
                      scrape_results$api_results$meta$away_team
                    } else if (next_x < 0) {
                      scrape_results$api_results$meta$home_team
                    } else {
                      NA_character_
                    }
                  } else if (tidyr::replace_na(def_side == "left", F)) {
                    if (next_x > 0) {
                      scrape_results$api_results$meta$home_team
                    } else if (next_x < 0) {
                      scrape_results$api_results$meta$away_team
                    } else {
                      NA_character_
                    }
                  } else {
                    NA_character_
                  }
                }
              } else {
                NA_character_
              }
            } else {
              team
            }
          }
        ),
      event_reason_3 =
        ifelse(
          event_type == "stoppage" &
            stringr::str_detect(event_reason_1 |> stringr::str_to_lower(), "offside") &
            !is.na(event_team_api) &
            next_faceoff_zone != "N",
          "intentional",
          NA_character_
        )
    ) |>
    dplyr::select(-c(home_team_def_zone_imp, next_faceoff_x, next_faceoff_team, next_faceoff_zone))
}

.join_api_pbp_to_html_pbp <- function(html_pbp, api_pbp) {
  message("joining html and api play-by-plays")
  starting_rows <- nrow(html_pbp)

  ret <-
    html_pbp |>
    dplyr::group_by(game_seconds, event_type) |>
    dplyr::mutate(event_type_index = cumsum(!is.na(event_type))) |>
    dplyr::full_join(
      api_pbp |>
        dplyr::mutate(
          event_type =
            event_type |>
            stringr::str_replace_all(
              c(
                "blocked-shot" = "BLOCK",
                "delayed-penalty" = "DELPEN",
                "faceoff" = "FAC",
                "game-end" = "GEND",
                "giveaway" = "GIVE",
                "shot-on-goal" = "SHOT",
                "goal" = "GOAL",
                "hit" = "HIT",
                "missed-shot" = "MISS",
                "failed-shot-attempt" = "MISS",
                "penalty" = "PENL",
                "period-end" = "PEND",
                "period-start" = "PSTR",
                "stoppage" = "STOP",
                "takeaway" = "TAKE",
                "shootout-complete" = "SOC"
              )
            )
        ) |>
        dplyr::group_by(game_seconds, event_type) |>
        dplyr::mutate(event_type_index = cumsum(!is.na(event_type))),
      by = dplyr::join_by(game_id, game_period, game_seconds, event_type, event_type_index)
    ) |>
    dplyr::ungroup() |>
    tibble::rowid_to_column() |>
    dplyr::mutate(
      event_team = ifelse(is.na(event_team_api), event_team_html, event_team_api),
      event_team_zone = ifelse(is.na(zone_code), event_team_zone_html, zone_code),
      event_player_1 = ifelse(is.na(event_player_1), event_player_1_html, event_player_1),
      event_player_2 = ifelse(is.na(event_player_2), event_player_2_html, event_player_2),
      event_player_3 = ifelse(is.na(event_player_3), event_player_3_html, event_player_3),
      event_detail_1 =
        dplyr::case_when(
          event_type %in% c("BLOCK", "MISS", "SHOT", "GOAL", "PENL") ~
            ifelse(is.na(event_type_detail), event_detail_1_html, event_type_detail),
          event_type == "STOP" ~
            event_reason_1,
          T ~ NA_character_
        ) |>
        stringr::str_to_title(),
      event_detail_2 =
        dplyr::case_when(
          event_type %in% c("MISS", "SHOT", "GOAL") ~
            ifelse(is.na(event_reason_1), event_detail_2_html, event_reason_1),
          event_type == "PENL" ~ penalty_class,
          event_type == "STOP" ~
            event_reason_2,
          T ~ NA_character_
        ) |>
        stringr::str_to_title(),
      event_detail_3 =
        dplyr::case_when(
          event_type %in% c("BLOCK", "MISS", "SHOT", "GOAL") ~
            ifelse(is.na(event_reason_2), event_detail_3_html, event_reason_2),
          event_type == "PENL" ~
            ifelse(
              is.na(penalty_in_minutes),
              stringr::str_extract("\\d+", event_detail_2_html),
              as.character(penalty_in_minutes)
            ),
          event_type == "STOP" ~
            event_reason_3,
          T ~ NA_character_
        ) |>
        stringr::str_to_title()
    ) |>
    dplyr::select(
      game_id, game_period, game_seconds, event_id, event_type, event_team,
      event_team_strength = event_strength, game_strength_state, event_team_zone,
      event_description, event_player_1, event_player_2, event_player_3,
      event_detail_1, event_detail_2, event_detail_3, coords_x, coords_y,
      event_distance_pbp = event_distance_html, home_team_def_zone,
      tidyselect::starts_with("away_on"),
      tidyselect::starts_with("home_on")
    )

  if (nrow(ret) > starting_rows) {
    warning("API contains plays not in HTML play-by-play")
  }

  ret
}

.split_on_ice_ids <- function(df) {
  df |>
    dplyr::mutate(
      away_on =
        away |>
        purrr::map(
          function(on) {
            tibble::tibble(
              value =
                on |>
                stringr::str_split(", ") |>
                purrr::flatten_chr() |>
                as.integer()
            ) |>
              tibble::rowid_to_column() |>
              dplyr::mutate(
                name =
                  "away_on_{rowid}" |>
                  glue::glue()
              ) |>
              dplyr::select(-rowid) |>
              tidyr::pivot_wider(names_from = name, values_from = value)
          }
        ),
      home_on =
        home |>
        purrr::map(
          function(on) {
            tibble::tibble(
              value =
                on |>
                stringr::str_split(", ") |>
                purrr::flatten_chr() |>
                as.integer()
            ) |>
              tibble::rowid_to_column() |>
              dplyr::mutate(
                name =
                  "home_on_{rowid}" |>
                  glue::glue()
              ) |>
              dplyr::select(-rowid) |>
              tidyr::pivot_wider(names_from = name, values_from = value)
          }
        )
    ) |>
    tidyr::unnest(c(away_on, home_on)) |>
    dplyr::select(-c(away, home))
}

.get_shift_events <- function(scrape_results) {
  message("getting shift events")
  scrape_results$html_results$shifts |>
    dplyr::left_join(
      scrape_results$api_results$rosters |>
        dplyr::select(venue, sweater_number, api_id, position),
      by = dplyr::join_by(venue, sweater_number)
    ) |>
    tidyr::pivot_longer(
      c(shift_start, shift_end),
      names_to = "event_type",
      values_to = "game_seconds"
    ) |>
    dplyr::arrange(game_period, game_seconds, venue, event_type, api_id) |>
    (\(df) {
      away_on <- c()
      away_goalie <- NA_integer_
      home_on <- c()
      home_goalie <- NA_integer_

      for (r in seq(nrow(df))) {
        if (df[[r, "event_type"]] == "shift_start") {
          if (df[[r, "venue"]] == "away") {
            if (df[[r, "position"]] == "G") {
              away_goalie <- df[[r, "api_id"]]
            } else {
              away_on <- sort(c(away_on, df[[r, "api_id"]]))
            }
          } else {
            if (df[[r, "position"]] == "G") {
              home_goalie <- df[[r, "api_id"]]
            } else {
              home_on <- sort(c(home_on, df[[r, "api_id"]]))
            }
          }
        } else {
          if (df[[r, "venue"]] == "away") {
            if (df[[r, "position"]] == "G") {
              away_goalie <- NA_integer_
            } else {
              away_on <- purrr::discard(away_on, magrittr::equals, df[[r, "api_id"]])
            }
          } else {
            if (df[[r, "position"]] == "G") {
              home_goalie <- NA_integer_
            } else {
              home_on <- purrr::discard(home_on, magrittr::equals, df[[r, "api_id"]])
            }
          }
        }

        df[[r, "away"]] <- stringr::str_c(away_on, collapse = ", ")
        df[[r, "away_skaters_on"]] <- length(away_on)
        df[[r, "away_goalie"]] <- away_goalie
        df[[r, "home"]] <- stringr::str_c(home_on, collapse = ", ")
        df[[r, "home_skaters_on"]] <- length(home_on)
        df[[r, "home_goalie"]] <- home_goalie
      }

      df
    })() |>
    dplyr::group_by(
      game_id, game_period, game_seconds, event_type = "CHANGE", venue
    ) |>
    dplyr::summarise(
      away = dplyr::last(away),
      away_skaters_on = dplyr::last(away_skaters_on),
      away_goalie = dplyr::last(away_goalie),
      home = dplyr::last(home),
      home_skaters_on = dplyr::last(home_skaters_on),
      home_goalie = dplyr::last(home_goalie),
      .groups = "drop"
    ) |>
    tibble::rowid_to_column(var = "shift_id") |>
    .split_on_ice_ids() |>
    dplyr::arrange(shift_id) |>
    dplyr::mutate(
      event_team_html =
        ifelse(
          venue == "home",
          scrape_results$api_results$meta$home_team,
          scrape_results$api_results$meta$away_team
        )
    )
}

.replace_sweater_numbers_with_player_ids <- function(html_pbp, scrape_results) {
  message("matching on-ice sweater numbers to api ids")
  html_pbp |>
    dplyr::select(
      event_id, game_period, game_seconds,
      tidyselect::starts_with("home_on"),
      tidyselect::starts_with("away_on")
    ) |>
    tidyr::pivot_longer(
      c(tidyselect::starts_with("home_on"), tidyselect::starts_with("away_on")),
      names_to = "venue",
      values_to = "sweater_number"
    ) |>
    dplyr::mutate(venue = venue |> stringr::str_sub(end = 4)) |>
    dplyr::left_join(
      scrape_results$api_results$rosters |>
        dplyr::select(venue, sweater_number, api_id, position),
      by = dplyr::join_by(venue, sweater_number)
    ) |>
    dplyr::mutate(
      away_goalie = ifelse(stringr::str_detect(venue, "away") & position == "G", api_id, NA_integer_),
      home_goalie = ifelse(stringr::str_detect(venue, "home") & position == "G", api_id, NA_integer_)
    ) |>
    dplyr::group_by(event_id) |>
    tidyr::fill(c(away_goalie, home_goalie), .direction = "downup") |>
    dplyr::ungroup() |>
    dplyr::filter(
      !(
        tidyr::replace_na(api_id, -2) == tidyr::replace_na(away_goalie, -1) |
          tidyr::replace_na(api_id, -2) == tidyr::replace_na(home_goalie, -1)
      )
    ) |>
    tidyr::pivot_wider(
      id_cols = c(event_id, game_period, game_seconds, away_goalie, home_goalie),
      names_from = venue,
      values_from = api_id,
      values_fn = \(x) stringr::str_c(sort(x), collapse = ", ")
    ) |>
    .split_on_ice_ids()
}

.get_event_ids_with_shifts <- function(html_pbp, scrape_results, shift_events) {
  html_pbp |>
    .replace_sweater_numbers_with_player_ids(scrape_results) |>
    dplyr::left_join(
      shift_events |>
        dplyr::mutate(end = dplyr::lead(game_seconds)) |>
        dplyr::filter(!is.na(end)) |>
        dplyr::mutate(
          game_seconds =
            purrr::map2(
              game_seconds, end,
              function(s, e) {
                tibble::tibble(
                  game_seconds = seq(s, e)
                )
              }
            )
        ) |>
        tidyr::unnest(game_seconds) |>
        dplyr::select(-c(game_id, event_type, venue, end))
    )
}

.add_shifts_to_html_pbp <- function(html_pbp, scrape_results) {
  message("adding shifts to play-by-play")
  shift_events <- .get_shift_events(scrape_results)

  event_ids_with_shifts <- .get_event_ids_with_shifts(html_pbp, scrape_results, shift_events)

  html_pbp |>
    dplyr::select(
      -c(
        tidyselect::starts_with("away_on"),
        tidyselect::starts_with("home_on")
      )
    ) |>
    dplyr::left_join(
      event_ids_with_shifts |>
        dplyr::select(-game_seconds)
    ) |>
    dplyr::group_by(game_period) |>
    tidyr::fill(shift_id, .direction = "downup") |>
    dplyr::ungroup() |>
    dplyr::bind_rows(
      shift_events |>
        dplyr::mutate(event_id = 0) |>
        dplyr::group_by(game_period, venue) |>
        dplyr::filter(!(event_type == "CHANGE" & shift_id == max(shift_id))) |>
        dplyr::ungroup()
    ) |>
    dplyr::arrange(
      shift_id,
      event_id,
      venue
    ) |>
    dplyr::group_by(game_period) |>
    tidyr::fill(home_team_def_zone, .direction = "downup") |>
    dplyr::ungroup() |>
    dplyr::group_by(shift_id) |>
    dplyr::mutate(
      dplyr::across(
        .cols =
          c(
            tidyselect::starts_with("away_on"),
            away_skaters_on,
            away_goalie,
            tidyselect::starts_with("home_on"),
            home_skaters_on,
            home_goalie
          ),
        .fns = (\(x) dplyr::first(x))
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      scrape_results$api_results$meta |>
        dplyr::select(game_id, away_team, home_team),
      by = dplyr::join_by(game_id)
    ) |>
    dplyr::mutate(
      event_length = tidyr::replace_na(dplyr::lead(game_seconds) - game_seconds, 0),
      home_skater_strength_state =
        dplyr::case_when(
          stringr::str_to_lower(event_description) |>
            stringr::str_detect("penalty shot") ~
            "Penalty Shot",
          game_period == 5 & scrape_results$api_results$meta$session != 3 ~
            "Shootout",
          T ~
            "{
              ifelse(is.na(home_goalie), 'E', '')
            }{
              tidyr::replace_na(home_skaters_on, 0)
            }v{
              tidyr::replace_na(away_skaters_on, 0)
            }{
              ifelse(is.na(away_goalie), 'E', '')
            }" |>
            glue::glue()
        ),
      empty_net_team =
        dplyr::case_when(
          is.na(away_goalie) & is.na(home_goalie) ~ "Both",
          is.na(away_goalie) ~ away_team,
          is.na(home_goalie) ~ home_team,
          T ~ NA_character_
        ),
      event_team = ifelse(event_type == "CHANGE", event_team_html, event_team),
      shift_id = dplyr::dense_rank(shift_id)
    ) |>
    # dplyr::select(-c(venue, event_id, event_team_html)) |>
    dplyr::select(-c(event_id)) |>
    tibble::rowid_to_column(var = "event_id") |>
    dplyr::select(
      game_id, game_period, game_seconds, shift_id, event_id, event_type,
      event_length, event_team, event_team_strength, home_skater_strength_state,
      empty_net_team, event_team_zone, event_description, event_player_1,
      event_player_2, event_player_3, event_detail_1, event_detail_2,
      event_detail_3, coords_x, coords_y, event_distance_pbp, home_team_def_zone,
      away_team, tidyselect::starts_with("away_on"), away_skaters_on, away_goalie,
      home_team, tidyselect::starts_with("home_on"), home_skaters_on, home_goalie
    )
}

.clean_metadata <- function(scrape_results) {
  message("cleaning metadata")

  scrape_results$api_results$meta |>
    dplyr::full_join(
      scrape_results$html_results$meta,
      by = dplyr::join_by(game_id)
    )
}

.clean_game_rosters <- function(scrape_results) {
  message("cleaning game rosters")

  scrape_results$api_results$rosters |>
    dplyr::full_join(
      scrape_results$html_results$rosters |>
        dplyr::select(-team),
      by = dplyr::join_by(game_id, venue, sweater_number, position_category, position)
    )
}

.clean_scratches <- function(scrape_results) {
  message("cleaning scratches")

  scrape_results$html_results$scratches |>
    dplyr::select(-team) |>
    dplyr::left_join(
      tibble::tibble(
        venue = c("away", "home"),
        team =
          c(
            scrape_results$api_results$meta$away_team,
            scrape_results$api_results$meta$home_team
          )
      ),
      by = dplyr::join_by(venue)
    )
}

.clean_coaches <- function(scrape_results) {
  message("cleaning coaches")

  scrape_results$html_results$coaches |>
    dplyr::select(-team) |>
    dplyr::left_join(
      tibble::tibble(
        venue = c("away", "home"),
        team =
          c(
            scrape_results$api_results$meta$away_team,
            scrape_results$api_results$meta$home_team
          )
      ),
      by = dplyr::join_by(venue)
    )
}

.clean_officials <- function(scrape_results) {
  message("cleaning officials")

  dplyr::bind_rows(
    scrape_results$html_results$referees |>
      dplyr::mutate(role = "R"),
    scrape_results$html_results$linesmen |>
      dplyr::mutate(role = "L")
  )
}

.clean_shifts <- function(scrape_results, clean_pbp) {
  message("cleaning shifts")

  faceoffs <-
    clean_pbp |>
    dplyr::filter(event_type == "FAC") |>
    dplyr::select(game_period, game_seconds, event_id, event_team_strength, event_team, event_team_zone, home_team, away_team) |>
    dplyr::group_by(game_period, game_seconds) |>
    dplyr::filter(event_id == min(event_id)) |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(c(home_team, away_team), values_to = "team", names_to = "venue") |>
    dplyr::mutate(
      venue = venue |> stringr::str_sub(end = 4),
      zone =
        ifelse(
          team == event_team,
          event_team_zone,
          dplyr::case_when(
            event_team_zone == "O" ~ "D",
            event_team_zone == "D" ~ "O",
            T ~ event_team_zone
          )
        ) |>
        stringr::str_c("ZS")
    )

  scrape_results$html_results$shifts |>
    dplyr::left_join(
      tibble::tibble(
        venue = c("away", "home"),
        team = c(
          scrape_results$api_results$meta$away_team,
          scrape_results$api_results$meta$home_team
        )
      ),
      by = dplyr::join_by(venue)
    ) |>
    dplyr::left_join(
      scrape_results$api_results$rosters |>
        dplyr::select(team, sweater_number, api_id),
      by = dplyr::join_by(team, sweater_number)
    ) |>
    dplyr::left_join(
      faceoffs |>
        dplyr::transmute(
          game_period,
          shift_start = game_seconds,
          team,
          shift_start_zone = zone
        ),
      by = dplyr::join_by(team, game_period, shift_start)
    ) |>
    dplyr::left_join(
      faceoffs |>
        dplyr::transmute(
          game_period,
          shift_end = game_seconds,
          team,
          shift_end_zone = zone
        ),
      by = dplyr::join_by(team, game_period, shift_end)
    ) |>
    dplyr::mutate(
      shift_start_zone = tidyr::replace_na(shift_start_zone, "OTF"),
      shift_end_zone = tidyr::replace_na(shift_end_zone, "OTF")
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      game_id,
      api_id,
      team,
      game_period,
      shift_start,
      shift_start_zone,
      shift_end,
      shift_end_zone
    )
}

.clean_pbp <- function(scrape_results) {
  message("cleaning play-by-play")

  clean_html_pbp <- .extract_details_from_html_pbp(scrape_results$html_results$pbp, scrape_results)

  clean_api_pbp <- .extract_details_from_api_pbp(scrape_results$api_results$pbp, scrape_results)

  combined_pbp <- .join_api_pbp_to_html_pbp(clean_html_pbp, clean_api_pbp)

  .add_shifts_to_html_pbp(combined_pbp, scrape_results) |>
    dplyr::select(
      tidyselect::where(
        fn = (\(x) {!all(is.na(x))})
      )
    )
}

clean_game_details_all_sources <- function(scrape_results) {
  clean_meta <- .clean_metadata(scrape_results)
  clean_rosters <- .clean_game_rosters(scrape_results)
  clean_scratches <- .clean_scratches(scrape_results)
  clean_coaches <- .clean_coaches(scrape_results)
  clean_officials <- .clean_officials(scrape_results)
  clean_pbp <- .clean_pbp(scrape_results)
  clean_shifts <- .clean_shifts(scrape_results, clean_pbp)

  list(
    game_metadata_clean = clean_meta,
    game_rosters_clean = clean_rosters,
    scratches_clean = clean_scratches,
    coaches_clean = clean_coaches,
    officials_clean = clean_officials,
    shifts_clean = clean_shifts,
    pbp_clean = clean_pbp
  )
}
