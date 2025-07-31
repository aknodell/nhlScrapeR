.manually_added_html_events <-
  readr::read_csv(
    "data/manually_added_html_events.csv",
    col_types = readr::cols(
      .default = readr::col_integer(),
      event_type = readr::col_character()
    )
  )

.manually_add_html_events <- function(html_events, g_id) {
  html_events |>
    dplyr::bind_rows(
      .manually_added_html_events |>
        dplyr::filter(game_id == g_id)
    )
}

.manually_clean_html_events <- function(html_events, g_id) {
  if (g_id == 2022020673) {
    html_events <-
      html_events |>
      dplyr::filter(
        !(event_id == 208)
      )
  }
  if (g_id == 2020020860) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 334 ~ 3869,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2020020456) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 174 ~ 2269,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2019030232) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            game_seconds == 3570 ~ 3571,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2019021019) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            game_seconds == 1805 ~ 1806,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2019020575) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            game_seconds == 3515 ~ 3516,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2018021101) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            game_seconds == 3227 ~ 3228,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2018020786) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            game_seconds == 3760 ~ 3761,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2018020733) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 4 ~ 16,
            event_id == 5 ~ 38,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2018020630) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            game_seconds == 3874 ~ 3875,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2018020443) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            game_seconds == 1313 ~ 1315,
            game_seconds == 2577 ~ 2580,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2018020401) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            game_seconds == 1919 ~ 1920,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2017020935) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            game_seconds == 3899 ~ 3900,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2017020697) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            game_seconds == 2809 ~ 2811,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2017020623) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            game_seconds == 1867 ~ 1868,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2017020040) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            game_seconds == 3869 ~ 3871,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2017020025) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            game_seconds == 3246 ~ 3245,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2017020020) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            game_seconds == 1517 ~ 1513,
            game_seconds == 3378 ~ 3376,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2016020963) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 5 ~ 237,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2016020177) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 197 ~ 2758,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2016020078) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 2 ~ 37,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2015020267) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 3 ~ 29,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2014020878) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 2 ~ 701,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2014020356) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 288 ~ 3094,
            event_id == 292 ~ 3096,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2013030163) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 2 ~ 34,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2013020971) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_period =
          dplyr::case_when(
            event_id == 1 ~ 1,
            T ~ game_period
          ),
        game_seconds =
          dplyr::case_when(
            event_id == 1 ~ 0,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2012020058) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 1 ~ 53,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2011030242) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 257 ~ 2408,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2011020347) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 2 ~ 10,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2011020187) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 112 ~ 1238,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2009030321) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 389 ~ 3430,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2009030114) {
    html_events <-
      html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 2 ~ 0,
            T ~ game_seconds
          )
      )
  }

  html_events
}
