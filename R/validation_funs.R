.validate_gm_id_format <- function(gm_id) {
  gm_id <- gm_id |> as.character()

  if (stringr::str_detect(gm_id, "\\D")) {
    stop("Invalid game ID format: game ID must only contain number characters")
  }

  if (stringr::str_length(gm_id) != 10) {
    stop("Invalid game ID format: game ID must be 10 digits long")
  }

  season <- gm_id |> stringr::str_sub(end = 4)
  session <- gm_id |> stringr::str_sub(start = 5, end = 6)
  game <- gm_id |> stringr::str_sub(start = 7)

  if (season < "2009" | season > as.character(lubridate::year(lubridate::today()))) {
    stop("Invalid game ID format: season (digits 1-4) must be between 2009 and {lubridate::year(lubridate::today())}" |> glue::glue())
  }

  if (!session %in% c("01", "02", "03")) {
    stop("Invalid game ID format: session (digits 5-6) must be one of 01 (preseason), 02 (regular season) or 03 (playoffs)")
  }

  if (session == "03") {
    round <- game |> stringr::str_sub(start = 2, end = 2)
    series <- game |> stringr::str_sub(start = 3, end = 3)
    series_game <- game |> stringr::str_sub(start = 4)

    if (
      !(round %in% c("1", "2", "3", "4") | (season == "2019" & round == "0"))
    ) {
      stop("Invalid game ID format: playoff round (digit 8) must be between 1 and 4 (or 0 for bubble play-in round)")
    }

    if (
      round != "0" & as.integer(series) > (2 ** (4 - as.integer(round)))
    ) {
      stop("Invalid game ID format: playoff series (digit 9) must be between 1 and 8 for round 1, 1 and 4 for round 2, 1 and 2 for round 3, and 1 for round 4")
    }

    if (!series_game %in% as.character(1:7)) {
      stop("Invalid game ID format: playoff series game (digit 10) must be between 1 and 7")
    }
  }
}

.validate_html_object <- function(html) {
  if (!identical(class(html), c("xml_document", "xml_node"))) {
    stop("html argument must be an rvest html_document object")
  }
}

.validate_side <- function(side) {
  if (!side %in% c("home", "away")) {
    stop("Side must be either \"home\" or \"away\"")
  }
}
