get_game_raw_pbp_json_api <- function(gm_id, verbose = T) {
  if (verbose) {
    message("Getting play-by-play JSON (API)")
  }
  .validate_gm_id_format(gm_id)

  resp <-
    "https://api-web.nhle.com/v1/gamecenter/{gm_id}/play-by-play" |>
    glue::glue() |>
    httr::GET() |>
    httr::content(type = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON()

  if (!is.na(resp |> purrr::pluck("status", .default = NA))) {
    stop("Game ID {gm_id} not found" |> glue::glue())
  } else {
    resp
  }
}

get_game_raw_shifts_json_api <- function(gm_id, verbose = T) {
  if (verbose) {
    message("Getting shift JSON (API)")
  }

  .validate_gm_id_format(gm_id)

  resp <-
    "https://api.nhle.com/stats/rest/en/shiftcharts?cayenneExp=gameId={gm_id}" |>
    glue::glue() |>
    httr::GET() |>
    httr::content(type = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON()

  if (!is.na(resp |> purrr::pluck("status", .default = NA))) {
    stop("Game ID {gm_id} not found" |> glue::glue())
  } else {
    resp
  }
}

get_game_raw_info_json_api <- function(gm_id, verbose = T) {
  if (verbose) {
    message("Getting info JSON (API)")
  }

  .validate_gm_id_format(gm_id)

  resp <-
    "https://api-web.nhle.com/v1/gamecenter/{gm_id}/right-rail" |>
    glue::glue() |>
    httr::GET() |>
    httr::content(type = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON()

  if (!is.na(resp |> purrr::pluck("status", .default = NA))) {
    stop("Game ID {gm_id} not found" |> glue::glue())
  } else {
    resp
  }
}

get_raw_all_teams_json_api <- function() {
  "https://api.nhle.com/stats/rest/en/team" |>
    httr::GET() |>
    httr::content(type = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON() |>
    purrr::pluck("data")
}

get_raw_team_season_schedule_json_api <- function(tm, season) {
  "https://api-web.nhle.com/v1/club-schedule-season/{tm}/{season}" |>
    glue::glue() |>
    httr::GET() |>
    httr::content(type = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON()
}

extract_all_teams_from_raw_all_teams_json <- function(all_teams_json) {
  all_teams_json |>
    tibble::tibble() |>
    dplyr::rename(team_id = id)
}

extract_team_season_schedule_from_raw_season_schedule_api <- function(season_schedule_json) {
  season_schedule_json |>
    purrr::pluck("games") |>
    tibble::as_tibble()
}


get_full_season_schedule_api <- function(season) {
  get_raw_all_teams_json_api() |>
    extract_all_teams_from_raw_all_teams_json() |>
    dplyr::mutate(
      schedule =
      purrr::map(
        triCode,
        function(t) {
          get_raw_team_season_schedule_json_api(tm = t, season = season) |>
            extract_team_season_schedule_from_raw_season_schedule_api()
        }
      )
  ) |>
    # dplyr::select(schedule) |>
    tidyr::unnest(schedule) |>
    # dplyr::rename(game_id = id) |>
    # tidyr::unnest(venue) |>
    # dplyr::rename() |>
    # tidyr::unnest(awayTeam, .names_repair = "unique") |>
    # dplyr::rename(
    #   away_team = abbrev,
    #   away_team_id = id,
    #   away_team_place_name = placeName
    # ) |>
    # tidyr::unnest(homeTeam, .names_repair = "unique") |>
    # dplyr::rename(
    #   home_team = abbrev,
    #   home_team_id = id,
    #   home_team_place_name = placeName$default
    # ) |>
    dplyr::transmute(
      game_id = id,
      season,
      game_date = gameDate |> lubridate::as_date(),
      start_time_utc = startTimeUTC |> lubridate::as_datetime(),
      session = gameType,
      venue_name = venue$default,
      neutral_site = neutralSite,
      home_team = homeTeam$abbrev,
      home_team_id = homeTeam$id,
      away_team = awayTeam$abbrev,
      away_team_id = awayTeam$id,
      home_team_place_name = homeTeam$placeName$default
    ) |>
    dplyr::distinct()
}

extract_game_shifts_from_raw_shifts_json_api <- function(shifts_json, verbose = T) {
  if (verbose) {
    message("Extracting shifts (API)")
  }

  data <-
    shifts_json |>
    purrr::pluck("data") |>
    tibble::tibble()
  if (nrow(data) > 0) {
    data |>
      dplyr::filter(typeCode == 517) |>
      dplyr::transmute(
        game_id = gameId,
        game_period = period,
        event_player_1 = playerId,
        shift_start =
          purrr::map2_int(
            startTime,
            game_period,
            function(t, p) {
              t |>
                stringr::str_split(":") |>
                purrr::flatten_chr() |>
                as.integer() |>
                magrittr::multiply_by(c(60, 1)) |>
                sum() |>
                magrittr::add((p - 1) * 1200)
            }
          ),
        shift_end =
          purrr::map2_int(
            endTime,
            game_period,
            function(t, p) {
              t |>
                stringr::str_split(":") |>
                purrr::flatten_chr() |>
                as.integer() |>
                magrittr::multiply_by(c(60, 1)) |>
                sum() |>
                magrittr::add((p - 1) * 1200)
            }
          )
      ) |>
      tryCatch(
        error = function(e) {
          message(e$message)
          tibble::tibble()
        }
      )
  } else {
    tibble::tibble()
  }
}

extract_game_metadata_from_raw_pbp_json_api <- function(pbp_json, verbose = T) {
  if (verbose) {
    message("Extracting metadata (API)")
  }

  tibble::tibble(
    game_id = pbp_json$id,
    season = pbp_json$season,
    game_date = pbp_json$gameDate |> lubridate::as_date(),
    start_time_utc = pbp_json$startTimeUTC |> lubridate::as_datetime(),
    session = pbp_json$gameType,
    venue_name = pbp_json$venue$default,
    venue_place_name = pbp_json$venueLocation$default,
    home_team = pbp_json$homeTeam$abbrev,
    home_team_id = pbp_json$homeTeam$id,
    away_team = pbp_json$awayTeam$abbrev,
    away_team_id = pbp_json$awayTeam$id,
    home_team_place_name = pbp_json$homeTeam$placeName$default
  )
}

extract_game_rosters_from_raw_pbp_json_api <- function(pbp_json, verbose = T) {
  if (verbose) {
    message("Extracting rosters (API)")
  }

  pbp_json$rosterSpots |>
    tibble::tibble() |>
    dplyr::transmute(
      game_id = pbp_json$id,
      api_id = playerId,
      sweater_number = sweaterNumber,
      position_category = ifelse(positionCode |> stringr::str_detect("[CLR]"), "F", positionCode),
      position = positionCode,
      team_id = teamId
    ) |>
    dplyr::left_join(
      tibble::tibble(
        team_id = c(pbp_json$homeTeam$id, pbp_json$awayTeam$id),
        venue = c("home", "away"),
        team = c(pbp_json$homeTeam$abbrev, pbp_json$awayTeam$abbrev)
      ),
      by = dplyr::join_by(team_id)
    ) |>
    dplyr::select(
      game_id,
      api_id,
      venue,
      team,
      sweater_number,
      position_category,
      position
    )
}

extract_scratches_from_raw_info_json_api <- function(info_json, pbp_json, verbose = T) {
  if (verbose) {
    message("Extracting scratches (API)")
  }

  dplyr::bind_rows(
    info_json$gameInfo$awayTeam$scratches |>
      tibble::tibble() |>
      dplyr::mutate(
        venue = "away",
        team = pbp_json$awayTeam$abbrev
      ),
    info_json$gameInfo$homeTeam$scratches |>
      tibble::tibble() |>
      dplyr::mutate(
        venue = "home",
        team = pbp_json$homeTeam$abbrev
      )
  ) |>
    dplyr::transmute(
      game_id = pbp_json$id,
      api_id = id,
      name = stringr::str_c(firstName$default, lastName$default, sep = " "),
      venue,
      team
    )
}

extract_coaches_from_raw_info_json_api <- function(info_json, pbp_json, verbose = T) {
  if (verbose) {
    message("Extracting coaches (API)")
  }

  dplyr::bind_rows(
    tibble::tibble(
      game_id = pbp_json$id,
      name = info_json$gameInfo$awayTeam$headCoach$default,
      venue = "away",
      team = pbp_json$awayTeam$abbrev
    ),
    tibble::tibble(
      game_id = pbp_json$id,
      name = info_json$gameInfo$homeTeam$headCoach$default,
      venue = "home",
      team = pbp_json$homeTeam$abbrev
    )
  )
}

extract_referees_from_raw_info_json_api <- function(info_json, pbp_json, verbose = T) {
  if (verbose) {
    message("Extracting referees (API)")
  }

  tibble::tibble(
    game_id = pbp_json$id,
    name = info_json$gameInfo$referees$default
  )
}

extract_linesmen_from_raw_info_json_api <- function(info_json, pbp_json, verbose = T) {
  if (verbose) {
    message("Extracting linesmen (API)")
  }

  tibble::tibble(
    game_id = pbp_json$id,
    name = info_json$gameInfo$linesmen$default
  )
}

extract_pbp_from_raw_pbp_json_api <- function(pbp_json, verbose = T) {
  if (verbose) {
    message("Extracting play-by-play (API)")
  }

  if (length(pbp_json$plays) > 0) {
    pbp_json$plays |>
      dplyr::select(-typeCode) |>
      tidyr::unnest(cols = c(periodDescriptor, details)) |>
      dplyr::arrange(sortOrder) |>
      dplyr::transmute(
        game_id = pbp_json$id,
        game_period = number,
        sort_order = sortOrder,
        game_seconds =
          purrr::map2_int(
            timeInPeriod,
            game_period,
            function(t, p) {
              t |>
                stringr::str_split(":") |>
                purrr::flatten_chr() |>
                as.integer() |>
                magrittr::multiply_by(c(60, 1)) |>
                sum() |>
                magrittr::add(
                  ifelse(
                    pbp_json$gameType != 3 & p >= 5,
                    3900,
                    (p - 1) * 1200
                  )
                )
            }
          ),
        game_strength_state = situationCode,
        home_team_def_zone = homeTeamDefendingSide |> tryCatch(error = function(e) NA_character_),
        event_team = eventOwnerTeamId,
        event_type = typeDescKey,
        event_type_detail =
          dplyr::case_when(
            !is.na(shotType) ~ shotType,
            !is.na(descKey) ~ descKey,
            T ~ NA_character_
          ) |>
          tryCatch(error = function(e) NA_integer_),
        event_player_1 =
          dplyr::case_when(
            !is.na(winningPlayerId) ~ winningPlayerId,
            !is.na(playerId) ~ playerId,
            !is.na(hittingPlayerId) ~ hittingPlayerId,
            !is.na(shootingPlayerId) ~ shootingPlayerId,
            !is.na(committedByPlayerId) ~ committedByPlayerId,
            !is.na(scoringPlayerId) ~ scoringPlayerId,
            T ~ NA_integer_
          ) |>
          tryCatch(error = function(e) NA_integer_),
        event_player_2 =
          dplyr::case_when(
            !is.na(losingPlayerId) ~ losingPlayerId,
            !is.na(hitteePlayerId) ~ hitteePlayerId,
            !is.na(blockingPlayerId) ~ blockingPlayerId,
            !is.na(drawnByPlayerId) ~ drawnByPlayerId,
            !is.na(assist1PlayerId) ~ assist1PlayerId,
            T ~ NA_integer_
          ) |>
          tryCatch(error = function(e) NA_integer_),
        event_player_3 =
          ifelse(!is.na(assist2PlayerId), assist2PlayerId, NA_integer_) |>
          tryCatch(error = function(e) NA_integer_),
        penalty_class = typeCode |>
          tryCatch(error = function(e) NA_integer_),
        penalty_in_minutes = duration |>
          tryCatch(error = function(e) NA_integer_),
        event_reason_1 = reason,
        event_reason_2 = secondaryReason,
        coords_x = xCoord,
        coords_y = yCoord,
        zone_code = zoneCode,
        away_goalie_api =
          ifelse(
            eventOwnerTeamId == pbp_json$homeTeam$id,
            goalieInNetId,
            NA_integer_
          ),
        home_goalie_api =
          ifelse(
            eventOwnerTeamId == pbp_json$awayTeam$id,
            goalieInNetId,
            NA_integer_
          )
      )
  } else {
    tibble::tibble()
  }
}
