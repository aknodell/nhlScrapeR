.manually_clean_html_events <- function(html_events, g_id) {
  if (g_id == 2022020673) {
    html_events |>
      dplyr::filter(
        !(event_id == 208)
      ) |>
      tibble::add_row(
        game_id = g_id,
        game_period = 2,
        game_seconds = 1884,
        event_type = "BLOCK",
        event_id = 0
      )
  } else if (g_id == 2021021105) {
    html_events |>
      tibble::add_row(
        game_id = g_id,
        game_period = 4,
        game_seconds = 3844,
        event_type = "DELPEN"
      )
  } else if (g_id == 2020020865) {
    html_events |>
      tibble::add_row(
        game_id = g_id,
        game_period = 4,
        game_seconds = 3766,
        event_type = "GEND"
      )
  } else if (g_id == 2020020860) {
    html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 334 ~ 3869,
            T ~ game_seconds
          )
      ) |>
      tibble::add_row(
        game_id = g_id,
        game_period = 4,
        game_seconds = 3870,
        event_type = "GEND"
      )
  } else if (g_id == 2020020858) {
    html_events |>
      tibble::add_row(
        game_id = g_id,
        game_period = 3,
        game_seconds = 3600,
        event_type = "GEND"
      )
  } else if (g_id == 2020020833) {
    html_events |>
      tibble::add_row(
        game_id = g_id,
        game_period = 4,
        game_seconds = 3609,
        event_type = "GEND"
      )
  } else if (g_id == 2020020824) {
    html_events |>
      tibble::add_row(
        game_id = g_id,
        game_period = 5,
        game_seconds = 3900,
        event_type = "GEND"
      )
  } else if (g_id == 2020020810) {
    html_events |>
      tibble::add_row(
        game_id = g_id,
        game_period = 3,
        game_seconds = 3600,
        event_type = "GEND"
      )
  } else if (g_id == 2020020529) {
    html_events |>
      tibble::add_row(
        game_id = g_id,
        game_period = 4,
        game_seconds = 3773,
        event_type = "GEND"
      )
  } else if (g_id == 2020020526) {
    html_events |>
      tibble::add_row(
        game_id = g_id,
        game_period = 4,
        game_seconds = 3627,
        event_type = "GEND"
      )
  } else if (g_id == 2020020456) {
    html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 174 ~ 2269,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2019020047) {
    html_events |>
      tibble::add_row(
        game_id = g_id,
        game_period = 1,
        game_seconds = 1106,
        event_type = "DELPEN"
      ) |>
      tibble::add_row(
        game_id = g_id,
        game_period = 3,
        game_seconds = 2684,
        event_type = "DELPEN"
      )
  } else if (g_id == 2018020733) {
    html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 4 ~ 16,
            event_id == 5 ~ 38,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2017020008) {
    html_events |>
      tibble::add_row(
        game_id = g_id,
        game_period = 5,
        game_seconds = 3900,
        event_type = "GEND"
      )
  } else if (g_id == 2016020963) {
    html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 5 ~ 237,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2016020177) {
    html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 197 ~ 2758,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2016020078) {
    html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 2 ~ 37,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2015020995) {
    html_events |>
      tibble::add_row(
        game_id = g_id,
        game_period = 2,
        game_seconds = 1330,
        event_type = "PENL"
      )
  } else if (g_id == 2015020410) {
    html_events |>
      tibble::add_row(
        game_id = g_id,
        game_period = 4,
        game_seconds = 3873,
        event_type = "PEND"
      ) |>
      tibble::add_row(
        game_id = g_id,
        game_period = 4,
        game_seconds = 3873,
        event_type = "GEND"
      )
  } else if (g_id == 2015020267) {
    html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 3 ~ 29,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2014020878) {
    html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 2 ~ 701,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2014020356) {
    html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 288 ~ 3094,
            event_id == 292 ~ 3096,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2013030163) {
    html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 2 ~ 34,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2013020971) {
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
  } else if (g_id == 2012020058) {
    html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 1 ~ 53,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2011030242) {
    html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 257 ~ 2408,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2011020347) {
    html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 2 ~ 10,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2011020187) {
    html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 112 ~ 1238,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2009030321) {
    html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 389 ~ 3430,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2009030114) {
    html_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            event_id == 2 ~ 0,
            T ~ game_seconds
          )
      )
  } else {
    html_events
  }
}

.manually_clean_api_events <- function(api_events, g_id) {
  if (g_id == 2015020207) {
    api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            game_seconds == 3581 ~ 3580,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2014021127) {
    api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 754 ~ 3524,
            sort_order == 755 ~ 3525,
            sort_order == 756 ~ 3527,
            T ~ game_seconds
          ),
        event_player_1 =
          dplyr::case_when(
            sort_order == 755 ~ 8467875,
            sort_order == 756 ~ 8474009,
            T ~ event_player_1
          ),
        coords_y =
          dplyr::case_when(
            sort_order == 756 ~ 3,
            sort_order == 757 ~ -11,
            T ~ coords_y
          ),
        coords_x =
          dplyr::case_when(
            sort_order == 756 ~ 83,
            T ~ coords_x
          )
      )
  } else if (g_id == 2014020945) {
    api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 585 ~ 3469,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2014020859) {
    api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 697 ~ 3581,
            T ~ game_seconds
          ),
        event_player_1 =
          dplyr::case_when(
            sort_order == 697 ~ 8471707,
            T ~ event_player_1
          )
      )
  } else if (g_id == 2009030414) {
    api_events |>
      dplyr::filter(sort_order != 797)
  } else if (g_id == 2009030313) {
    api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 701 ~ 3541,
            sort_order == 705 ~ 3569,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2009030311) {
    api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 667 ~ 3555,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2009030246) {
    api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 579 ~ 3195,
            sort_order == 582 ~ 3220,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2009030241) {
    api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 554 ~ 2923,
            sort_order == 570 ~ 3011,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2009030234) {
    api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 701 ~ 3551,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2009030214) {
    api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 562 ~ 3441,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2009030185) {
    api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 548 ~ 3139,
            sort_order == 585 ~ 3334,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2009030184) {
    api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 674 ~ 3548,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2009030175) {
    api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 674 ~ 3520,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2009030165) {
    api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 707 ~ 3751,
            sort_order == 724 ~ 3847,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2009030154) {
    api_events |>
      dplyr::filter(sort_order != 837)
  } else if (g_id == 2009030153) {
    api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 688 ~ 3554,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2009030121) {
    api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 579 ~ 3588,
            sort_order == 580 ~ 3589,
            sort_order == 581 ~ 3589,
            T ~ game_seconds
          )
      )
  } else if (g_id == 2009030114) {
    api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 6 ~ 0,
            sort_order == 674 ~ 3391,
            T ~ game_seconds
          )
      )
  } else {
    api_events
  }
}
