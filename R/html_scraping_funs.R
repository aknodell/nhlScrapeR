get_game_rosters_raw_html <- function(gm_id, verbose = T) {
  if (verbose) {
    message("Getting rosters (HTML)")
  }
  .validate_gm_id_format(gm_id)

  season <-
    gm_id |>
    as.character() |>
    stringr::str_sub(end = 4)

  season <-
    "{season}{as.integer(season) + 1}" |>
    glue::glue()

  game_id <- gm_id |> stringr::str_sub(start = -5)

  "https://www.nhl.com/scores/htmlreports/{season}/RO0{game_id}.HTM" |>
    glue::glue() |>
    rvest::read_html() |>
    tryCatch(
      error = function(e) {
        if (e$message |> stringr::str_detect("HTTP error 404")) {
          stop("Roster report page not found for game ID {gm_id}" |> glue::glue())
        } else {
          e
        }
      }
    )
}

get_game_shifts_raw_html <- function(gm_id, side, verbose = T) {
  if (verbose) {
    message("Getting {side} shifts (HTML)" |> glue::glue())
  }
  .validate_gm_id_format(gm_id)

  .validate_side(side)

  season <-
    gm_id |>
    as.character() |>
    stringr::str_sub(end = 4)

  season <-
    "{season}{as.integer(season) + 1}" |>
    glue::glue()

  game_id <- gm_id |> stringr::str_sub(start = -5)

  "https://www.nhl.com/scores/htmlreports/{season}/T{ifelse(side == 'home', 'H', 'V')}0{game_id}.HTM" |>
    glue::glue() |>
    rvest::read_html() |>
    tryCatch(
      error = function(e) {
        if (e$message |> stringr::str_detect("HTTP error 404")) {
          stop("{stringr::str_to_title(side)} shift report page not found for game ID {gm_id}" |> glue::glue())
        } else {
          e
        }
      }
    )
}

get_game_pbp_raw_html <- function(gm_id, verbose = T) {
  if (verbose) {
    message("Getting play-by-play (HTML)")
  }
  .validate_gm_id_format(gm_id)

  season <-
    gm_id |>
    as.character() |>
    stringr::str_sub(end = 4)

  season <-
    "{season}{as.integer(season) + 1}" |>
    glue::glue()

  game_id <- gm_id |> stringr::str_sub(start = -5)

  "https://www.nhl.com/scores/htmlreports/{season}/PL0{game_id}.HTM" |>
    glue::glue() |>
    rvest::read_html() |>
    tryCatch(
      error = function(e) {
        if (e$message |> stringr::str_detect("HTTP error 404")) {
          stop("Play-by-play report page not found for game ID {gm_id}" |> glue::glue())
        } else {
          e
        }
      }
    )
}

extract_game_metadata_from_raw_html <- function(html, gm_id, verbose = T) {
  if (verbose) {
    message("Extracting metadata (HTML)")
  }

  .validate_html_object(html)

  tables <- html |> rvest::html_table()

  tibble::tibble(
    game_id = gm_id |> as.integer(),
    attendance =
      tables[[6]] |>
      dplyr::pull(X1) |>
      stringr::str_to_lower() |>
      purrr::keep(stringr::str_detect, pattern = "attendance") |>
      stringr::str_extract("(\\d+,)*\\d+") |>
      stringr::str_remove_all(",") |>
      as.integer()
  ) |>
    tryCatch(
      error = function(e) {
        stop("Error extracting metadata from HTML report: {e$message}" |> glue::glue())
      }
    )
}

extract_rosters_from_raw_roster_html <- function(roster_html, gm_id, verbose = T) {
  if (verbose) {
    message("Extracting rosters (HTML)")
  }

  .validate_html_object(roster_html)

  tables <- roster_html |> rvest::html_table()

  tables[[11]] |>
    tail(-1) |>
    dplyr::transmute(
      game_id = gm_id |> as.integer(),
      name = X3 |> stringr::str_remove("\\(.\\)") |> stringr::str_trim(),
      letter = X3 |> stringr::str_extract("\\(.\\)") |> stringr::str_remove_all("[\\(\\)]") |> stringr::str_trim(),
      venue = "away",
      team = tables[[9]] |> dplyr::pull(X1),
      sweater_number = X1 |> as.integer(),
      position_category = ifelse(X2 %in% c("D", "G"), X2, "F"),
      position = X2
    ) |>
    dplyr::bind_rows(
      tables[[12]] |>
        tail(-1) |>
        dplyr::transmute(
          game_id = gm_id |> as.integer(),
          name = X3 |> stringr::str_remove("\\(.\\)") |> stringr::str_trim(),
          letter = X3 |> stringr::str_extract("\\(.\\)") |> stringr::str_remove_all("[\\(\\)]") |> stringr::str_trim(),
          venue = "home",
          team = tables[[9]] |> dplyr::pull(X2),
          sweater_number = X1 |> as.integer(),
          position_category = ifelse(X2 %in% c("D", "G"), X2, "F"),
          position = X2
        )
    ) |>
    tryCatch(
      error = function(e) {
        stop("Error extracting rosters from HTML report: {e$message}" |> glue::glue())
      }
    )
}

extract_scratches_from_raw_roster_html <- function(roster_html, gm_id, verbose = T) {
  if (verbose) {
    message("Extracting scratches (HTML)")
  }

  .validate_html_object(roster_html)

  headers <-
    roster_html |>
    rvest::html_elements(".header") |>
    rvest::html_text2() |>
    stringr::str_to_upper() |>
    stringr::str_squish()

  tables <- roster_html |> rvest::html_table()

  if ("SCRATCHES" %in% headers) {
    away_scratches <- tables[[13]] |> tail(-1)
    home_scratches <- tables[[14]] |> tail(-1)

    if (nrow(away_scratches) > 0) {
      away_scratches <-
        away_scratches |>
        dplyr::transmute(
          game_id = gm_id |> as.integer(),
          name = X3 |> stringr::str_remove("\\(.\\)") |> stringr::str_trim(),
          letter = X3 |> stringr::str_extract("\\(.\\)") |> stringr::str_remove_all("[\\(\\)]") |> stringr::str_trim(),
          venue = "away",
          team = tables[[9]] |> dplyr::pull(X1),
          sweater_number = X1 |> as.integer(),
          position_category = ifelse(X2 %in% c("D", "G"), X2, "F"),
          position = X2
        ) |>
        tryCatch(
          error = function(e) {
            stop("Error extracting away scratches from HTML report: {e$message}" |> glue::glue())
          }
        )
    } else {
      away_scratches <- tibble::tibble()
    }

    if (nrow(home_scratches) > 0) {
      home_scratches <-
        home_scratches |>
        dplyr::transmute(
          game_id = gm_id |> as.integer(),
          name = X3 |> stringr::str_remove("\\(.\\)") |> stringr::str_trim(),
          letter = X3 |> stringr::str_extract("\\(.\\)") |> stringr::str_remove_all("[\\(\\)]") |> stringr::str_trim(),
          venue = "home",
          team = tables[[9]] |> dplyr::pull(X2),
          sweater_number = X1 |> as.integer(),
          position_category = ifelse(X2 %in% c("D", "G"), X2, "F"),
          position = X2
        ) |>
        tryCatch(
          error = function(e) {
            stop("Error extracting home scratches from HTML report: {e$message}" |> glue::glue())
          }
        )
    } else {
      home_scratches <- tibble::tibble()
    }

    dplyr::bind_rows(
      away_scratches,
      home_scratches
    )
  } else {
    tibble::tibble()
  }
}

extract_coaches_from_raw_roster_html <- function(roster_html, gm_id, verbose = T) {
  if (verbose) {
    message("Extracting coaches (HTML)")
  }

  .validate_html_object(roster_html)

  headers <-
    roster_html |>
    rvest::html_elements(".header") |>
    rvest::html_text2() |>
    stringr::str_to_upper() |>
    stringr::str_squish()

  tables <- roster_html |> rvest::html_table()

  if ("HEAD COACHES" %in% headers) {
    table_offset <- ifelse("SCRATCHES" %in% headers, 0, 2)

    away_coach <- tables[[15 - table_offset]]
    home_coach <- tables[[16 - table_offset]]

    if (nrow(away_coach) > 0) {
      away_coach <-
        away_coach |>
        dplyr::transmute(
          game_id = gm_id |> as.integer(),
          name = X1 |> stringr::str_squish(),
          venue = "away",
          team = tables[[9]]$X1
        ) |>
        tryCatch(
          error = function(e) {
            stop("Error extracting away coach from HTML report: {e$message}" |> glue::glue())
          }
        )
    } else {
      tibble::tibble()
    }

    if (nrow(home_coach) > 0) {
      home_coach <-
        home_coach |>
        dplyr::transmute(
          game_id = gm_id |> as.integer(),
          name = X1 |> stringr::str_squish(),
          venue = "home",
          team = tables[[9]]$X2
        ) |>
        tryCatch(
          error = function(e) {
            stop("Error extracting home coach from HTML report: {e$message}" |> glue::glue())
          }
        )
    } else {
      tibble::tibble()
    }

    dplyr::bind_rows(
      away_coach,
      home_coach
    )
  } else {
    tibble::tibble()
  }
}

extract_referees_from_raw_roster_html <- function(roster_html, gm_id, verbose = T) {
  if (verbose) {
    message("Extracting referees (HTML)")
  }

  .validate_html_object(roster_html)

  headers <-
    roster_html |>
    rvest::html_elements(".header") |>
    rvest::html_text2() |>
    stringr::str_to_upper() |>
    stringr::str_squish()

  tables <- roster_html |> rvest::html_table()

  if ("OFFICIALS" %in% headers) {
    table_offset <-
      ifelse("SCRATCHES" %in% headers, 0, 2) +
      ifelse("HEAD COACHES" %in% headers, 0, 2)

    refs <- tables[[18 - table_offset]]

    if (nrow(refs) > 0) {
      refs |>
        dplyr::transmute(
          game_id = gm_id |> as.integer(),
          name = X1 |> stringr::str_remove("#\\d+") |> stringr::str_trim()
        ) |>
        tryCatch(
          error = function(e) {
            stop("Error extracting referees from HTML report: {e$message}" |> glue::glue())
          }
        )
    } else {
      tibble::tibble()
    }
  } else {
    tibble::tibble()
  }
}

extract_linesmen_from_raw_roster_html <- function(roster_html, gm_id, verbose = T) {
  if (verbose) {
    message("Extracting linesmen (HTML)")
  }

  .validate_html_object(roster_html)

  headers <-
    roster_html |>
    rvest::html_elements(".header") |>
    rvest::html_text2() |>
    stringr::str_to_upper() |>
    stringr::str_squish()

  tables <- roster_html |> rvest::html_table()

  if ("OFFICIALS" %in% headers) {
    table_offset <-
      ifelse("SCRATCHES" %in% headers, 0, 2) +
      ifelse("HEAD COACHES" %in% headers, 0, 2)

    linesmen <- tables[[19 - table_offset]]

    if (nrow(linesmen) > 0) {
      linesmen |>
        dplyr::transmute(
          game_id = gm_id |> as.integer(),
          name = X1 |> stringr::str_remove("#\\d+") |> stringr::str_trim()
        ) |>
        tryCatch(
          error = function(e) {
            stop("Error extracting linesmen from HTML report: {e$message}" |> glue::glue())
          }
        )
    } else {
      tibble::tibble()
    }
  } else {
    tibble::tibble()
  }
}

extract_shifts_from_raw_shifts_html <- function(shifts_html, gm_id, side, verbose = T) {
  if (verbose) {
    message("Extracting {side} shifts (HTML)" |> glue::glue())
  }

  .validate_html_object(shifts_html)

  .validate_side(side)

  shifts_table <-
    shifts_html |>
    rvest::html_table() |>
    purrr::pluck(10)

  if (nrow(shifts_table) > 0) {
    shifts_table |>
      dplyr::select(X1:X5) |>
      dplyr::mutate(
        X1 = ifelse(stringr::str_detect(X1, "Shift|Per|Total|(\\d$)"), NA_character_, X1)
      ) |>
      tidyr::fill(X1, .direction = "down") |>
      dplyr::filter(stringr::str_count(X3, "\\d+:\\d+") == 2) |>
      dplyr::transmute(
        game_id = gm_id |> as.integer(),
        venue = side,
        sweater_number = X1 |> stringr::str_extract("\\d+") |> as.integer(),
        game_period =
          X2 |>
          stringr::str_replace_all("OT", "4") |>
          as.integer(),
        shift_start_time =
          purrr::map2_int(
            X3,
            game_period,
            function(t, p) {
              t |>
                stringr::str_trim() |>
                stringr::str_extract("^\\d+:\\d+") |>
                stringr::str_split(":") |>
                purrr::flatten_chr() |>
                as.integer() |>
                magrittr::multiply_by(c(60, 1)) |>
                sum() |>
                magrittr::add((p - 1) * 1200) |>
                as.integer()
            }
          ),
        shift_start_clock =
          purrr::map2_int(
            X3,
            game_period,
            function(t, p) {
              t |>
                stringr::str_trim() |>
                stringr::str_extract("\\d+:\\d+$") |>
                stringr::str_split(":") |>
                purrr::flatten_chr() |>
                as.integer() |>
                magrittr::multiply_by(c(-60, -1)) |>
                sum() |>
                magrittr::add(p * 1200) |>
                as.integer()
            }
          ),
        shift_end_time =
          purrr::map2_int(
            X4,
            game_period,
            function(t, p) {
              t |>
                stringr::str_trim() |>
                stringr::str_extract("^\\d+:\\d+") |>
                stringr::str_split(":") |>
                purrr::flatten_chr() |>
                as.integer() |>
                magrittr::multiply_by(c(60, 1)) |>
                sum() |>
                magrittr::add((p - 1) * 1200) |>
                as.integer()
            }
          ),
        shift_end_clock =
          purrr::map2_int(
            X4,
            game_period,
            function(t, p) {
              t |>
                stringr::str_trim() |>
                stringr::str_extract_all("\\d+:\\d+$") |>
                stringr::str_split(":") |>
                purrr::flatten_chr() |>
                as.integer() |>
                magrittr::multiply_by(c(-60, -1)) |>
                sum() |>
                magrittr::add(p * 1200) |>
                as.integer()
            }
          ),
        duration =
          purrr::map2_int(
            X5,
            game_period,
            function(t, p) {
              t |>
                stringr::str_extract("\\d+:\\d+") |>
                stringr::str_split(":") |>
                purrr::flatten_chr() |>
                as.integer() |>
                magrittr::multiply_by(c(60, 1)) |>
                sum() |>
                as.integer()
            }
          )
      ) |>
      tryCatch(
        error = function(e) {
          stop("Error extracting {stringr::str_to_title(side)} shifts from HTML report: {e$message}" |> glue::glue())
        }
      )
  } else {
    tibble::tibble()
  }
}

extract_pbp_from_raw_pbp_html <- function(pbp_html, gm_id, verbose = T) {
  if (verbose) {
    message("Extracting play-by-play (HTML)")
  }

  .validate_html_object(pbp_html)

  session <-
    gm_id |>
    as.character() |>
    stringr::str_sub(start = 6, end = 6) |>
    as.integer()

  purrr::map(
    c(".evenColor", ".oddColor"),
    function(class) {
      pbp_html |>
        rvest::html_elements(class) |>
        purrr::map(
          function(r) {
            tds <-
              r |>
              rvest::html_elements("td")

            event_details <-
              tibble::tibble(
                name = c("event_id", "game_period", "event_strength", "game_seconds", "event_type", "event_description"),
                value =
                  tds |>
                  head(6) |>
                  rvest::html_text2() |>
                  stringr::str_squish()
              )

            away_on <-
              tibble::tibble(
                value =
                  tds[7] |>
                  rvest::html_elements("font") |>
                  rvest::html_text2()
              ) |>
              tibble::rowid_to_column() |>
              dplyr::mutate(
                name =
                  "away_on_{rowid}" |>
                  glue::glue()
              )

            home_on <-
              tibble::tibble(
                value =
                  r |>
                  rvest::html_elements("td:nth-child(8)") |>
                  rvest::html_elements("table") |>
                  rvest::html_elements("font") |>
                  rvest::html_text2()
              ) |>
              tibble::rowid_to_column() |>
              dplyr::mutate(
                name =
                  "home_on_{rowid}" |>
                  glue::glue()
              )

            dplyr::bind_rows(
              event_details,
              home_on,
              away_on
            ) |>
              dplyr::select(-rowid) |>
              tidyr::pivot_wider(names_from = name, values_from = value)
          }
        ) |>
        dplyr::bind_rows()
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::transmute(
      game_id = as.integer(gm_id),
      event_id = as.integer(event_id),
      game_period = as.integer(game_period),
      game_seconds =
        purrr::map2_int(
          game_seconds,
          game_period,
          function(t, p) {
            t |>
              stringr::str_extract("^\\d+:\\d+") |>
              stringr::str_split(":") |>
              purrr::flatten_chr() |>
              as.integer() |>
              magrittr::multiply_by(c(60, 1)) |>
              sum() |>
              magrittr::add(
                ifelse(
                  session != 3 & p >= 5,
                  3900,
                  (p - 1) * 1200
                )
              )
          }
        ),
      event_strength =
        ifelse(stringr::str_trim(event_strength) == "", NA_character_, event_strength),
      event_type,
      event_description,
      dplyr::across(
        .cols = -c(event_id:event_description),
        .fns = function(x) {as.integer(x)}
      )
    ) |>
    dplyr::arrange(event_id) |>
    tryCatch(
      error = function(e) {
        stop("Error extracting play-by-play from HTML report: {e$message}" |> glue::glue())
      }
    )
}
