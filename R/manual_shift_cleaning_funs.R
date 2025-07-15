.manually_clean_shifts <- function(s, g_id) {
  if (g_id == 2021021189) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = "home",
          sweater_number = c(3, 21, 24, 41),
          game_period = 4,
          shift_start_time = c(3656, 3692, 3656, 3600),
          shift_start_clock = c(3656, 3692, 3656, 3600),
          shift_end_time = 3700,
          shift_end_clock = 3700,
          duration = c(44, 8, 44, 100)
        )
      )
  } else if (g_id == 2021020452) {
    s |>
      dplyr::mutate(
        duration = ifelse(duration != shift_end_time - shift_start_time, duration + 1, duration)
      )
  } else if (g_id == 2021020427) {
    s |>
      dplyr::mutate(
        duration = ifelse(duration != shift_end_time - shift_start_time, duration + 1, duration)
      )
  } else if (g_id == 2021020416) {
    s |>
      dplyr::mutate(
        duration = ifelse(duration != shift_end_time - shift_start_time, duration + 1, duration)
      )
  } else if (g_id == 2021020326) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = "away",
          sweater_number = c(6, 43, 8, 53, 9, 23),
          game_period = 3,
          shift_start_time = c(3487, 3506, 3506, 3506, 3506, 3554),
          shift_start_clock = c(3487, 3506, 3506, 3506, 3506, 3554),
          shift_end_time = 3600,
          shift_end_clock = 3600,
          duration = c(113, 94, 94, 94, 94, 46)
        )
      )
  } else if (g_id == 2020020865) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home", "home", "home", "home", "away", "away", "away"),
          sweater_number = c(36, 49, 24, 33, 67, 42, 30),
          game_period = 4,
          shift_start_time = c(3713, 3713, 3713, 3600, 3761, 3722, 3600),
          shift_start_clock = c(3713, 3713, 3713, 3600, 3761, 3722, 3600),
          shift_end_time = 3766,
          shift_end_clock = 3766,
          duration = c(53, 53, 53, 166, 5, 44, 166)
        )
      ) |>
      dplyr::mutate(
        ## terry's last shift
        duration =
          ifelse(venue == "away" & sweater_number == 61 & game_period == 4, 55, duration)
      )
  } else if (g_id == 2020020860) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home", "home", "home", "home", "away", "away", "away", "away"),
          sweater_number = c(12, 88, 5, 60, 14, 3, 21, 29),
          game_period = 4,
          shift_start_time = c(3854, 3831, 3827, 3600, 3827, 3827, 3827, 3600),
          shift_start_clock = c(3854, 3831, 3827, 3600, 3827, 3827, 3827, 3600),
          shift_end_time = 3870,
          shift_end_clock = 3870,
          duration = c(16, 39, 43, 270, 43, 43, 43, 270)
        )
      )
  } else if (g_id == 2020020858) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home", "home", "home", "home", "home", "home", "away", "away", "away", "away", "away", "away"),
          sweater_number = c(31, 58, 8, 77, 11, 12, 68, 26, 37, 23, 57, 55),
          game_period = 3,
          shift_start_time = c(2400, 3464, 3489, 3575, 3582, 3582, 3477, 3463, 3525, 3537, 3582, 3596),
          shift_start_clock = c(2400, 3464, 3489, 3575, 3582, 3582, 3477, 3463, 3525, 3537, 3582, 3596),
          shift_end_time = 3600,
          shift_end_clock = 3600,
          duration = c(1200, 136, 111, 25, 18, 18, 153, 137, 75, 63, 18, 4)
        )
      )
  } else if (g_id == 2020020857) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(17),
          game_period = 1,
          shift_start_time = c(0),
          shift_start_clock = c(0),
          shift_end_time = 67,
          shift_end_clock = 67,
          duration = c(67)
        )
      )
  } else if (g_id == 2020020810) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c(
            "home", "home", "home", "home", "home", "home",
            "away", "away", "away", "away", "away", "away", "away", "away"
          ),
          sweater_number = c(
            30, 4, 32, 53, 46, 15,
            40, 40, 8, 8, 22, 9, 44, 55
          ),
          game_period = c(
            3, 3, 3, 3, 3, 3,
            2, 3, 2, 3, 3, 3, 3, 3
          ),
          shift_start_time = c(
            2400, 3520, 3530, 3552, 3552, 3567,
            1200, 2400, 1200, 3567, 3548, 3548, 3548, 3548
          ),
          shift_start_clock = c(
            2400, 3520, 3530, 3552, 3552, 3567,
            1200, 2400, 1200, 3567, 3548, 3548, 3548, 3548
          ),
          shift_end_time = c(
            3600, 3600, 3600, 3600, 3600, 3600,
            2400, 3600, 1244, 3600, 3600, 3600, 3600, 3600
          ),
          shift_end_clock = c(
            3600, 3600, 3600, 3600, 3600, 3600,
            2400, 3600, 1244, 3600, 3600, 3600, 3600, 3600
          ),
          duration = c(
            1200, 80, 70, 48, 48, 33,
            1200, 1200, 44, 33, 52, 52, 52, 52

          )
        )
      )
  } else if (g_id == 2020020762) {
    s |>
      dplyr::filter(
        !(shift_start_time == 1200 & shift_end_time == 0)
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(47),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = c(1200),
          shift_end_time = c(1246),
          shift_end_clock = c(1246),
          duration = c(46)
        )
      )
  } else if (g_id == 2020020748) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(20, 13, 7, 17, 49, 34),
          game_period = c(3),
          shift_start_time = c(3565),
          shift_start_clock = c(3565),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(35)
        )
      )
  } else if (g_id == 2020020526) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home", "home", "home", "home", "away", "away", "away", "away"),
          sweater_number = c(8, 14, 22, 30, 22, 29, 21, 41),
          game_period = c(4),
          shift_start_time = c(3600),
          shift_start_clock = c(3600),
          shift_end_time = c(3627),
          shift_end_clock = c(3627),
          duration = c(27)
        )
      )
  } else if (g_id == 2020020367) {
    s |>
      dplyr::filter(
        !(shift_start_time == 2400 & shift_end_time == 1200)
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(12, 34),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = c(2400),
          shift_end_time = c(2460),
          shift_end_clock = c(2460),
          duration = c(60)
        )
      )
  } else if (g_id == 2020020252) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(53, 23, 15, 22, 33, 67),
          game_period = c(3),
          shift_start_time = c(3535, 3575, 3598, 3598, 3598, 3598),
          shift_start_clock = c(3535, 3575, 3598, 3598, 3598, 3598),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(65, 25, 2, 2, 2, 2)
        )
      )
  } else if (g_id == 2020020124) {
    s |>
      dplyr::filter(
        !(shift_start_time == 3600 & shift_end_time == 2477)
      ) |>
      dplyr::mutate(
        duration =
          ifelse(venue == "home" & sweater_number == 53 & shift_start_time == 3430, 170, duration)
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(55),
          game_period = c(4),
          shift_start_time = c(3600),
          shift_start_clock = c(3600),
          shift_end_time = c(3676),
          shift_end_clock = c(3676),
          duration = c(76)
        )
      )
  } else if (g_id == 2019021076) {
    s |>
      dplyr::filter(
        !(venue == "home" & shift_start_time == 1880 & sweater_number == 4),
        !(venue == "home" & shift_start_time == 1880 & sweater_number == 28),
        !(venue == "away" & shift_start_time == 2180 & sweater_number == 24),
        !(venue == "away" & shift_start_time == 2204 & sweater_number == 21)
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 1845 & sweater_number == 4) ~ 51,
            (venue == "home" & shift_start_time == 1845 & sweater_number == 28) ~ 51,
            (venue == "away" & shift_start_time == 2136 & sweater_number == 21) ~ 83,
            T ~ duration
          )
      )
  } else if (g_id == 2019021053) {
    s |>
      dplyr::filter(
        !(venue == "away" & shift_start_time == 2332 & sweater_number == 14),
        !(venue == "away" & shift_start_time == 2332 & sweater_number == 22),
        !(venue == "away" & shift_start_time == 2332 & sweater_number == 55),
        !(venue == "away" & shift_start_time == 2332 & sweater_number == 81)
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 2317 & sweater_number == 88) ~ 67,
            (venue == "away" & shift_start_time == 2317 & sweater_number == 24) ~ 76,
            (venue == "away" & shift_start_time == 2317 & sweater_number == 98) ~ 76,
            (venue == "away" & shift_start_time == 1200 & sweater_number == 88) ~ 1200,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home", "away", "away", "away"),
          sweater_number = c(46, 55, 22, 55),
          game_period = c(2, 2, 3, 3),
          shift_start_time = c(2384, 2393, 2400, 2400),
          shift_start_clock = c(2384, 2393, 2400, 2400),
          shift_end_time = c(2400, 2400, 2429, 2429),
          shift_end_clock = c(2400, 2400, 2429, 2429),
          duration = c(16, 7, 29, 29)
        )
      )
  } else if (g_id == 2019021047) {
    s |>
      dplyr::filter(
        !(venue == "home" & shift_start_time == 2913 & sweater_number == 8),
        !(venue == "away" & shift_start_time == 2175 & sweater_number == 13),
        !(venue == "away" & shift_start_time == 2616 & sweater_number == 6)
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home", "away", "away"),
          sweater_number = c(23, 13, 6),
          game_period = c(3, 2, 3),
          shift_start_time = c(2913, 2178, 2618),
          shift_start_clock = c(2913, 2178, 2618),
          shift_end_time = c(2929, 2254, 2740),
          shift_end_clock = c(2929, 2254, 2740),
          duration = c(16, 76, 122)
        )
      )
  } else if (g_id == 2019021043) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(55, 33, 50, 6, 20, 21),
          game_period = c(3),
          shift_start_time = c(3527, 3543, 3543, 3586, 3586, 3594),
          shift_start_clock = c(3527, 3543, 3543, 3586, 3586, 3594),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(73, 57, 57, 14, 14, 6)
        )
      )
  } else if (g_id == 2019021019) {
    s |>
      dplyr::filter(
        !(venue == "home" & shift_start_time == 0 & sweater_number == 86),
        !(venue == "home" & shift_start_time == 37 & sweater_number == 17),
        !(venue == "home" & shift_start_time == 37 & sweater_number == 20),
        !(venue == "home" & shift_start_time == 2414 & sweater_number == 77)
      ) |>
      dplyr::mutate(
        shift_start_time =
          ifelse(venue == "away" & shift_start_time == 268 & sweater_number == 48, 227, shift_start_time),
        duration =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 208 & sweater_number == 21) ~ 60,
            (venue == "away" & shift_start_time == 208 & sweater_number == 46) ~ 60,
            (venue == "away" & shift_start_time == 227 & sweater_number == 48) ~ 64,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home", "home", "home", "away", "away", "away"),
          sweater_number = c(17, 20, 77, 25, 37, 40),
          game_period = c(1, 1, 3, 3, 3, 3),
          shift_start_time = c(0, 0, 2400, 2400, 2400, 2400),
          shift_start_clock = c(0, 0, 2400, 2400, 2400, 2400),
          shift_end_time = c(43, 43, 2439, 2439, 2439, 3600),
          shift_end_clock = c(43, 43, 2439, 2439, 2439, 3600),
          duration = c(43, 43, 39, 39, 39, 1200)
        )
      )
  } else if (g_id == 2019020963) {
    s |>
      dplyr::filter(
        !(venue == "home" & shift_start_time == 1200 & sweater_number == 45)
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(45, 45, 83, 39, 71),
          game_period = c(2, 3, 3, 3, 3),
          shift_start_time = c(1200, 2400, 2400, 2400, 2400),
          shift_start_clock = c(1200, 2400, 2400, 2400, 2400),
          shift_end_time = c(2400, 3052, 2456, 2456, 2456),
          shift_end_clock = c(2400, 3052, 2456, 2456, 2456),
          duration = c(1200, 652, 56, 56, 56)
        )
      )
  } else if (g_id == 2019020852) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(35, 4, 24, 91),
          game_period = c(4),
          shift_start_time = c(3600, 3668, 3691, 3737),
          shift_start_clock = c(3600, 3668, 3691, 3737),
          shift_end_time = c(3739),
          shift_end_clock = c(3739),
          duration = c(139, 71, 48, 2)
        )
      )
  } else if (g_id == 2019020842) {
    s |>
      dplyr::filter(
        !(venue == "away" & shift_start_time == 263 & sweater_number == 46),
        !(venue == "away" & shift_start_time == 270 & sweater_number == 24)
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 251 & sweater_number == 24) ~ 52,
            T ~ duration
          )
      )
  } else if (g_id == 2019020808) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(74),
          game_period = c(4),
          shift_start_time = c(3600),
          shift_start_clock = c(3600),
          shift_end_time = c(3637),
          shift_end_clock = c(3637),
          duration = c(37)
        )
      )
  } else if (g_id == 2019020775) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(39, 11, 24),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = c(2400),
          shift_end_time = c(3600, 2428, 2428),
          shift_end_clock = c(3600, 2428, 2428),
          duration = c(1200, 28, 28)
        )
      )
  } else if (g_id == 2019020772) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(10, 27, 17, 21, 72, 34),
          game_period = c(3),
          shift_start_time = c(3489, 3509, 3509, 3599, 3599, 3599),
          shift_start_clock = c(3489, 3509, 3509, 3599, 3599, 3599),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(111, 91, 91, 1, 1, 1)
        )
      )
  } else if (g_id == 2019020726) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 2400 & sweater_number == 50) ~ 1200,
            (venue == "home" & shift_start_time == 3535 & sweater_number == 27) ~ 65,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(10, 27, 50),
          game_period = c(3, 4, 4),
          shift_start_time = c(3575, 3600, 3600),
          shift_start_clock = c(3575, 3600, 3600),
          shift_end_time = c(3600, 3679, 3813),
          shift_end_clock = c(3600, 3679, 3813),
          duration = c(25, 79, 213)
        )
      )
  } else if (g_id == 2019020722) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 2400 & sweater_number == 30) ~ 1200,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(30, 23, 91),
          game_period = c(4),
          shift_start_time = c(3600),
          shift_start_clock = c(3600),
          shift_end_time = c(3714, 3642, 3624),
          shift_end_clock = c(3714, 3642, 3624),
          duration = c(114, 42, 24)
        )
      )
  } else if (g_id == 2019020710) {
    s |>
      dplyr::filter(
        !(venue == "home" & shift_start_time == 132 & sweater_number == 21),
        !(venue == "home" & shift_start_time == 2462 & sweater_number == 23),
        !(venue == "home" & shift_start_time == 2462 & sweater_number == 55),
        !(venue == "home" & shift_start_time == 3027 & sweater_number == 17),
        !(venue == "home" & shift_start_time == 3168 & sweater_number == 18),
        !(venue == "home" & shift_start_time == 3424 & sweater_number == 18),
        !(venue == "away" & shift_start_time == 3494 & sweater_number == 29),
        !(venue == "away" & shift_start_time == 3494 & sweater_number == 32)
      ) |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            venue == "home" & shift_start_time == 3446 & sweater_number == 72 ~ 3424,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 126 & sweater_number == 38) ~ 7,
            (venue == "home" & shift_start_time == 2430 & sweater_number == 23) ~ 43,
            (venue == "home" & shift_start_time == 2430 & sweater_number == 55) ~ 46,
            (venue == "home" & shift_start_time == 3015 & sweater_number == 17) ~ 89,
            venue == "home" & shift_start_time == 3424 & sweater_number == 72 ~ 70,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away", "home"),
          sweater_number = c(10, 18),
          game_period = c(3, 3),
          shift_start_time = c(3291, 3494),
          shift_start_clock = c(3291, 3494),
          shift_end_time = c(3347, 3511),
          shift_end_clock = c(3347, 3511),
          duration = c(56, 17)
        )
      )
  } else if (g_id == 2019020708) {
    s |>
      dplyr::filter(
        !(venue == "home" & shift_start_time == 2628 & sweater_number == 5),
        !(venue == "away" & shift_start_time == 2628 & sweater_number == 37)
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "home" & shift_end_time == 2628) ~ duration + 2,
            (venue == "away" & shift_end_time == 2628) ~ duration + 15,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(9, 13, 23, 35, 81, 98),
          game_period = c(3),
          shift_start_time = c(3599),
          shift_start_clock = c(3599),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(1)
        )
      )
  } else if (g_id == 2019020674) {
    s |>
      dplyr::mutate(
        game_period =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 1200 & sweater_number == 30) ~ 3,
            T ~ game_period
          ),
        shift_start_time =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 1200 & sweater_number == 30) ~ 2400,
            T ~ shift_start_time
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(30),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = c(1200),
          shift_end_time = c(2400),
          shift_end_clock = c(2400),
          duration = c(1200)
        )
      )
  } else if (g_id == 2019020665) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away", "home", "home"),
          sweater_number = c(1, 13, 21),
          game_period = c(2, 4, 4),
          shift_start_time = c(1200, 3600, 3600),
          shift_start_clock = c(1200, 3600, 3600),
          shift_end_time = c(2400, 3625, 3638),
          shift_end_clock = c(2400, 3625, 3638),
          duration = c(1200, 25, 38)
        )
      )
  } else if (g_id == 2019020628) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(9),
          game_period = c(4),
          shift_start_time = c(3600),
          shift_start_clock = c(3600),
          shift_end_time = c(3669),
          shift_end_clock = c(3669),
          duration = c(69)
        )
      )
  } else if (g_id == 2019020591) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(61, 67, 88, 19, 71, 81),
          game_period = c(3),
          shift_start_time = c(3530, 3541, 3570, 3594, 3594, 3594),
          shift_start_clock = c(3530, 3541, 3570, 3594, 3594, 3594),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(70, 59, 30, 6, 6, 6)
        )
      )
  } else if (g_id == 2019020580) {
    s |>
      dplyr::filter(
        !(venue == "home" & shift_start_time == 2046 & sweater_number == 88),
        !(venue == "home" & shift_start_time == 2145),
        !(venue == "home" & shift_start_time == 2153)
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 2118 & sweater_number == 3) ~ 50,
            (venue == "home" & shift_start_time == 2118 & sweater_number == 88) ~ 50,
            (venue == "home" & shift_start_time == 2118 & sweater_number == 26) ~ 68,
            (venue == "home" & shift_start_time == 2118 & sweater_number == 89) ~ 68,
            (venue == "home" & shift_start_time == 2118 & sweater_number == 28) ~ 68,
            T ~ duration
          )
      )
  } else if (g_id == 2019020549) {
    s |>
      dplyr::filter(
        !(venue == "home" & shift_start_time == 3487 & sweater_number == 44)
      ) |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 3286 & sweater_number == 63) ~ 3265,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 2427 & sweater_number == 21) ~ 15,
            (venue == "home" & shift_start_time == 3265 & sweater_number == 63) ~ duration + 21,
            T ~ duration
          )
      )
  } else if (g_id == 2019020535) {
    s |>
      dplyr::filter(
        !(venue == "away" & shift_start_time == 1816 & sweater_number == 15),
        !(venue == "away" & shift_start_time == 1816 & sweater_number == 24),
        !(venue == "away" & shift_start_time == 1827 & sweater_number == 48),
        !(venue == "away" & shift_start_time == 1889 & sweater_number == 44)
      ) |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 1484 & sweater_number == 42) ~ 1479,
            (venue == "away" & shift_start_time == 2210 & sweater_number == 5) ~ 2208,
            (venue == "away" & shift_start_time == 2842 & sweater_number == 67) ~ 2833,
            (venue == "home" & shift_start_time == 2844 & sweater_number == 8) ~ 2847,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 1479 & sweater_number == 42) ~ duration + 5,
            (venue == "away" & shift_end_time == 1806 & sweater_number == 5) ~ duration - 24,
            (venue == "away" & shift_end_time == 2208 & sweater_number == 5) ~ duration + 2,
            (venue == "away" & shift_end_time == 2629) ~ duration + 8,
            (venue == "home" & shift_end_time == 2833 & sweater_number == 67) ~ duration + 9,
            (venue == "home" & shift_end_time == 2847 & sweater_number == 8) ~ duration - 3,
            (venue == "away" & shift_end_time == 3552 & sweater_number == 33) ~ duration - 30,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(48, 33),
          game_period = c(2, 2),
          shift_start_time = c(1806, 1806),
          shift_start_clock = c(1806, 1806),
          shift_end_time = c(1836, 1836),
          shift_end_clock = c(1836, 1836),
          duration = c(30, 30)
        )
      )
  } else if (g_id == 2019020479) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(47, 6, 71, 57, 21),
          game_period = c(3),
          shift_start_time = c(2400, 3558, 3558, 3558, 3590),
          shift_start_clock = c(2400, 3558, 3558, 3558, 3590),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(1200, 42, 42, 42, 10)
        )
      )
  } else if (g_id == 2019020477) {
    s |>
      dplyr::filter(
        !(venue == "away" & shift_start_time == 472),
        !(venue == "away" & shift_end_time == 553),
        !(venue == "away" & shift_start_time == 3486 & sweater_number == 73)
      ) |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 780 & sweater_number == 29) ~ 778,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 423 & sweater_number != 71) ~ 130,
            (venue == "away" & shift_start_time == 435 & sweater_number == 81) ~ 118,
            (venue == "away" & shift_start_time == 778 & sweater_number == 29) ~ duration + 2,
            (venue == "away" & shift_start_time == 857 & sweater_number == 73) ~ 33,
            (venue == "away" & shift_end_time == 1761 & sweater_number == 81) ~ duration - 10,
            T ~ duration
          )
      )
  } else if (g_id == 2019020475) {
    s |>
      dplyr::filter(
        !(venue == "home" & shift_start_time == 2515 & sweater_number == 19)
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 2461 & sweater_number == 19) ~ 61,
            (venue == "away" & shift_start_time == 3299 & sweater_number == 73) ~ 11,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(74, 4, 14),
          game_period = c(3),
          shift_start_time = c(2400, 2400, 2400),
          shift_start_clock = c(2400, 2400, 2400),
          shift_end_time = c(3600, 2451, 2461),
          shift_end_clock = c(3600, 2451, 2461),
          duration = c(1200, 51, 61)
        )
      )
  } else if (g_id == 2019020457) {
    s |>
      dplyr::filter(
        !(venue == "home" & shift_start_time == 1200 & sweater_number == 2),
        !(venue == "away" & shift_start_time == 1200 & sweater_number == 71),
      ) |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 659 & sweater_number == 3) ~ 649,
            (venue == "away" & shift_start_time == 659 & sweater_number == 38) ~ 649,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 649 & sweater_number == 3) ~ duration + 10,
            (venue == "away" & shift_start_time == 649 & sweater_number == 39) ~ duration + 10,
            (venue == "away" & shift_end_time == 611 & sweater_number == 20) ~ duration - 26,
            (venue == "home" & shift_end_time == 963 & sweater_number == 3) ~ duration + 1,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(72),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = c(1200),
          shift_end_time = c(2400),
          shift_end_clock = c(2400),
          duration = c(1200)
        )
      )
  } else if (g_id == 2019020447) {
    s |>
      dplyr::filter(
        !(venue == "away" & shift_start_time == 689),
        !(venue == "away" & shift_start_time == 709),
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 666 & sweater_number == 56) ~ 63,
            (venue == "away" & shift_start_time == 666 & sweater_number == 68) ~ 80,
            (venue == "away" & shift_start_time == 679 & sweater_number == 12) ~ 50,
            (venue == "away" & shift_start_time == 679 & sweater_number == 88) ~ 50,
            (venue == "away" & shift_start_time == 679 & sweater_number == 17) ~ 67,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(7, 15),
          game_period = c(1),
          shift_start_time = c(249),
          shift_start_clock = c(249),
          shift_end_time = c(261),
          shift_end_clock = c(261),
          duration = c(12)
        )
      )
  } else if (g_id == 2019020418) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(19, 23, 77),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = c(1200),
          shift_end_time = c(2400, 1228, 1228),
          shift_end_clock = c(2400, 1228, 1228),
          duration = c(1200, 28, 28)
        )
      )
  } else if (g_id == 2019020410) {
    s |>
      dplyr::filter(
        !(venue == "away" & shift_start_time == 3506 & sweater_number == 72)
      )
  } else if (g_id == 2019020331) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 2400 & sweater_number == 41) ~ 1200,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(41, 44),
          game_period = c(4),
          shift_start_time = c(3600),
          shift_start_clock = c(3600),
          shift_end_time = c(3638, 3624),
          shift_end_clock = c(3638, 3634),
          duration = c(38, 24)
        )
      )
  } else if (g_id == 2019020316) {
    s |>
      dplyr::filter(
        !(venue == "away" & shift_start_time == 2489 & sweater_number != 14),
        !(venue == "away" & shift_start_time == 2666),
        !(venue == "away" & shift_start_time == 2671),
        !(venue == "away" & shift_start_time == 2973 & sweater_number != 44)
      ) |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 2578 & sweater_number == 44) ~ 2534,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 2458 & sweater_number == 4) ~ 76,
            (venue == "away" & shift_start_time == 2473 & sweater_number == 15) ~ 61,
            (venue == "away" & shift_start_time == 2473 & sweater_number == 33) ~ 61,
            (venue == "away" & shift_start_time == 2483 & sweater_number == 67) ~ 51,
            (venue == "away" & shift_start_time == 2534 & sweater_number == 29) ~ 60,
            (venue == "away" & shift_start_time == 2534 & sweater_number == 44) ~ 60,
            (venue == "away" & shift_start_time == 2633 & sweater_number == 49) ~ 56,
            (venue == "away" & shift_start_time == 2633 & sweater_number == 34) ~ 84,
            (venue == "away" & shift_start_time == 2664 & sweater_number == 5) ~ 34,
            (venue == "away" & shift_start_time == 2664 & sweater_number == 32) ~ 34,
            (venue == "away" & shift_start_time == 2664 & sweater_number == 15) ~ 53,
            (venue == "away" & shift_start_time == 2952 & sweater_number == 5) ~ 21,
            (venue == "away" & shift_start_time == 2952 & sweater_number == 2) ~ 43,
            (venue == "away" & shift_start_time == 2952 & sweater_number == 20) ~ 43,
            (venue == "away" & shift_start_time == 2952 & sweater_number == 38) ~ 43,
            (venue == "away" & shift_start_time == 2952 & sweater_number == 24) ~ 43,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(4),
          game_period = c(3),
          shift_start_time = c(2594),
          shift_start_clock = c(2594),
          shift_end_time = c(2664),
          shift_end_clock = c(2664),
          duration = c(70)
        )
      )
  } else if (g_id == 2019020259) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(40),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = c(2400),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(1200)
        )
      )
  } else if (g_id == 2019020234) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(31, 88, 52, 51, 19, 12),
          game_period = c(3),
          shift_start_time = c(2400, 3514, 3514, 3514, 3598, 3598),
          shift_start_clock = c(2400, 3514, 3514, 3514, 3598, 3598),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(1200, 86, 86, 86, 2, 2)
        )
      )
  } else if (g_id == 2019020221) {
    s |>
      dplyr::filter(
        !(venue == "home" & shift_start_time == 1170 & sweater_number == 40),
        !(venue == "home" & shift_start_time == 1123 & sweater_number == 48),
        !(venue == "away" & shift_start_time == 1123 & sweater_number == 10),
        !(venue == "away" & shift_start_time == 1127)
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 279 & sweater_number == 40) ~ 921,
            (venue == "home" & shift_start_time == 1099 & sweater_number == 55) ~ 71,
            (venue == "home" & shift_start_time == 1111 & sweater_number == 25) ~ 46,
            (venue == "home" & shift_start_time == 1111 & sweater_number == 16) ~ 59,
            (venue == "home" & shift_start_time == 1111 & sweater_number == 17) ~ 59,
            (venue == "away" & shift_start_time == 0 & sweater_number == 31) ~ 1200,
            (venue == "away" & shift_start_time == 1075 & sweater_number == 90) ~ 95,
            (venue == "away" & shift_start_time == 1111 & sweater_number == 13) ~ 69,
            (venue == "away" & shift_start_time == 1111 & sweater_number == 26) ~ 69,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(28, 63),
          game_period = c(1),
          shift_start_time = c(1123),
          shift_start_clock = c(1123),
          shift_end_time = c(1170),
          shift_end_clock = c(1170),
          duration = c(47)
        )
      )
  } else if (g_id == 2019020201) {
    s |>
      dplyr::filter(
        !(venue == "home" & shift_start_time == 2283 & sweater_number == 37),
        !(venue == "home" & shift_start_time == 2283 & sweater_number == 49)
      ) |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 2291 & sweater_number == 72) ~ 2283,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 2283 & sweater_number == 72) ~ duration + 8,
            T ~ duration
          )
      )
  } else if (g_id == 2019020178) {
    s |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 2260 & sweater_number == 8) ~ 2242,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 2242 & sweater_number == 8) ~ duration + 18,
            T ~ duration
          )
      )
  } else if (g_id == 2019020169) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 1200 & sweater_number == 39) ~ 1200,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(39, 29, 95, 72),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = c(2400),
          shift_end_time = c(3415, 2498, 2428, 2428),
          shift_end_clock = c(3415, 2498, 2428, 2428),
          duration = c(1015, 98, 28, 28)
        )
      )
  } else if (g_id == 2019020129) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(31),
          game_period = c(4),
          shift_start_time = c(3600),
          shift_start_clock = c(3600),
          shift_end_time = c(3717),
          shift_end_clock = c(3717),
          duration = c(117)
        )
      )
  } else if (g_id == 2019020072) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 0 & sweater_number == 39) ~ 1200,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(39),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = c(1200),
          shift_end_time = c(1260),
          shift_end_clock = c(1260),
          duration = c(60)
        )
      )
  } else if (g_id == 2019020030) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 1342 & sweater_number == 30) ~ 1058,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away", "away"),
          sweater_number = c(65, 30),
          game_period = c(1, 3),
          shift_start_time = c(743, 2400),
          shift_start_clock = c(743, 2400),
          shift_end_time = c(788, 3471),
          shift_end_clock = c(788, 3471),
          duration = c(45, 1071)
        )
      )
  } else if (g_id == 2019020021) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(72, 7, 19, 5, 55, 16),
          game_period = c(3),
          shift_start_time = c(2400, 3510, 3510, 3510, 3510, 3599),
          shift_start_clock = c(2400, 3510, 3510, 3510, 3510, 3599),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(1200, 90, 90, 90, 90, 1)
        )
      )
  } else if (g_id == 2019020019) {
    s |>
      dplyr::mutate(
        sweater_number =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 3520 & sweater_number == 15) ~ 94,
            T ~ sweater_number
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(30),
          game_period = c(4),
          shift_start_time = c(3600),
          shift_start_clock = c(3600),
          shift_end_time = c(3900),
          shift_end_clock = c(3900),
          duration = c(300)
        )
      )
  } else if (g_id == 2019020014) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(30, 77, 19, 8, 33, 34),
          game_period = c(3),
          shift_start_time = c(2400, 3482, 3521, 3536, 3592, 3592),
          shift_start_clock = c(2400, 3482, 3521, 3536, 3592, 3592),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(1200, 118, 79, 64, 8, 8)
        )
      )
  } else if (g_id == 2019020011) {
    s |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 2588) ~ 2587,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            (venue == "home" & shift_end_time == 2562) ~ duration + 2,
            (venue == "home" & shift_end_time == 2588) ~ duration - 1,
            (venue == "home" & shift_start_time == 2587) ~ duration + 1,
            T ~ duration
          )
      )
  } else if (g_id == 2018021052) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(37, 6, 9, 10, 11, 44),
          game_period = c(3),
          shift_start_time = c(2400, 3534, 3534, 3553, 3584, 3584),
          shift_start_clock = c(2400, 3534, 3534, 3553, 3584, 3584),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(1200, 66, 66, 47, 16, 16)
        )
      )
  } else if (g_id == 2018020963) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(6, 24, 11, 26, 90, 31),
          game_period = c(3),
          shift_start_time = c(3503, 3531, 3531, 3531, 3550, 3585),
          shift_start_clock = c(3503, 3531, 3531, 3531, 3550, 3585),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(97, 69, 69, 69, 50, 15)
        )
      )
  } else if (g_id == 2018020890) {
    s |>
      dplyr::filter(
        !(shift_start_time == 2400 & game_period == 2)
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(35),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = c(2400),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(1200)
        )
      )
  } else if (g_id == 2018020732) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(36, 8, 11, 44, 6, 77),
          game_period = c(3),
          shift_start_time = c(2400, 3497, 3561, 3581, 3581, 3598),
          shift_start_clock = c(2400, 3497, 3561, 3581, 3581, 3598),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(1200, 103, 39, 19, 19, 2)
        )
      )
  } else if (g_id == 2018020681) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(31, 4),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = c(1200),
          shift_end_time = c(2400, 1244),
          shift_end_clock = c(2400, 1244),
          duration = c(1200, 44)
        )
      )
  } else if (g_id == 2018020592) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(50, 41, 29, 10, 91, 17),
          game_period = c(3),
          shift_start_time = c(2400, 3536, 3543, 3546, 3546, 3546),
          shift_start_clock = c(2400, 3536, 3543, 3546, 3546, 3546),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(1200, 64, 57, 54, 54, 54)
        )
      )
  } else if (g_id == 2018020555) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(31, 15, 33, 29, 46, 45),
          game_period = c(3),
          shift_start_time = c(2400, 3585, 3588, 3588, 3588, 3588),
          shift_start_clock = c(2400, 3585, 3588, 3588, 3588, 3588),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(1200, 15, 12, 12, 12, 12)
        )
      )
  } else if (g_id == 2018020397) {
    s |>
      dplyr::filter(
        !(shift_start_time == 2400 & game_period == 2),
        !(venue == "away" & game_period == 2 & sweater_number == 35 & shift_start_time != 1200),
        !(venue == "away" & sweater_number == 8 & shift_start_time == 2363)
      ) |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 2399 & sweater_number == 59) ~ 2396,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 1200 & sweater_number == 35) ~ 1200,
            (venue == "away" & shift_start_time == 2396 & sweater_number == 59) ~ 4,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(42, 47),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = c(2400),
          shift_end_time = c(2441),
          shift_end_clock = c(2441),
          duration = c(41)
        )
      )
  } else if (g_id == 2018020164) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(31, 27, 28, 65),
          game_period = c(4),
          shift_start_time = c(3600, 3705, 3705, 3724),
          shift_start_clock = c(3600, 3705, 3705, 3724),
          shift_end_time = c(3732),
          shift_end_clock = c(3732),
          duration = c(132, 27, 27, 8)
        )
      )
  } else if (g_id == 2018020144) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(28, 21, 75, 27, 24, 40),
          game_period = c(3),
          shift_start_time = c(3515, 3529, 3536, 3552, 3565, 3565),
          shift_start_clock = c(3515, 3529, 3536, 3552, 3565, 3565),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(85, 71, 64, 48, 35, 35)
        )
      )
  } else if (g_id == 2018020086) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(90, 17, 27, 10, 91, 34),
          game_period = c(3),
          shift_start_time = c(3568, 3572, 3589, 3589, 3589, 3598),
          shift_start_clock = c(3568, 3572, 3589, 3589, 3589, 3598),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(32, 28, 11, 11, 11, 2)
        )
      )
  } else if (g_id == 2018020081) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(31, 6, 47, 53),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = c(1200),
          shift_end_time = c(2400, 1257, 1257, 1257),
          shift_end_clock = c(2400, 1257, 1257, 1257),
          duration = c(1200, 57, 57, 57)
        )
      )
  } else if (g_id == 2018020072) {
    s |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 2548 & sweater_number == 23) ~ 2563,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 2563 & sweater_number == 23) ~ duration - 15,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(33),
          game_period = c(3),
          shift_start_time = c(2548),
          shift_start_clock = c(2548),
          shift_end_time = c(2563),
          shift_end_clock = c(2563),
          duration = c(15)
        )
      )
  } else if (g_id == 2017021267) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(23, 33, 15, 36, 25, 32),
          game_period = c(3),
          shift_start_time = c(3491, 3493, 3592, 3592, 3592, 3596),
          shift_start_clock = c(3491, 3493, 3592, 3592, 3592, 3596),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(109, 107, 8, 8, 8, 4)
        )
      )
  } else if (g_id == 2017021083) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(35),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = c(1200),
          shift_end_time = c(2400),
          shift_end_clock = c(2400),
          duration = c(1200)
        )
      )
  } else if (g_id == 2017020820) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(18, 25, 6, 23, 36),
          game_period = c(3),
          shift_start_time = c(3570, 3570, 3570, 3592, 3592),
          shift_start_clock = c(3570, 3570, 3570, 3592, 3592),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(30, 30, 30, 8, 8)
        )
      )
  } else if (g_id == 2017020666) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(11, 93),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = c(2400),
          shift_end_time = c(2434),
          shift_end_clock = c(2434),
          duration = c(34)
        )
      )
  } else if (g_id == 2017020434) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(7, 9, 17, 44, 1),
          game_period = c(3),
          shift_start_time = c(3589, 3589, 3589, 3589, 3597),
          shift_start_clock = c(3589, 3589, 3589, 3589, 3597),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(11, 11, 11, 11, 3)
        )
      )
  } else if (g_id == 2016021194) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "away" & shift_end_time == 3895) ~ duration + 5,
            T ~ duration
          )
      )
  } else if (g_id == 2016021163) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(1, 14, 23, 24, 33, 44),
          game_period = c(3),
          shift_start_time = c(3575),
          shift_start_clock = c(3575),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(25)
        )
      )
  } else if (g_id == 2016020936) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(40, 42),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = c(1200),
          shift_end_time = c(2400),
          shift_end_clock = c(2400),
          duration = c(1200, 55)
        )
      )
  } else if (g_id == 2016020915) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(35),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = c(2400),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(1200)
        )
      )
  } else if (g_id == 2016020856) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(31, 96, 9, 14),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = c(1200),
          shift_end_time = c(2400, 1274, 1244, 1244),
          shift_end_clock = c(2400, 1274, 1244, 1244),
          duration = c(1200, 74, 44, 44)
        )
      )
  } else if (g_id == 2016020511) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(35, 47, 54, 88, 37),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = c(1200),
          shift_end_time = c(2400, 1245, 1245, 1233, 1233),
          shift_end_clock = c(2400, 1245, 1245, 1233, 1233),
          duration = c(1200, 45, 45, 33, 33)
        )
      )
  } else if (g_id == 2016020421) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(30, 3, 7, 81, 44, 39),
          game_period = c(3),
          shift_start_time = c(2666, 3588, 3588, 3588, 3588, 3588),
          shift_start_clock = c(2666, 3588, 3588, 3588, 3588, 3588),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(934, 12, 12, 12, 12, 12)
        )
      )
  } else if (g_id == 2016020419) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(37, 8, 44, 9, 13, 22),
          game_period = c(3),
          shift_start_time = c(2400, 3528, 3528, 3599, 3599, 3599),
          shift_start_clock = c(2400, 3528, 3528, 3599, 3599, 3599),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(1200, 72, 72, 1, 1, 1)
        )
      )
  } else if (g_id == 2016020163) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(25, 60, 5, 21, 44, 27),
          game_period = c(3),
          shift_start_time = c(3292, 3567, 3582, 3582, 3582, 3592),
          shift_start_clock = c(3292, 3567, 3582, 3582, 3582, 3592),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(308, 33, 18, 18, 18, 8)
        )
      )
  } else if (g_id == 2016020139) {
    s |>
      dplyr::filter(
        !(venue == "away" & game_period == 1 & shift_start_time == 1200)
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(1),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = c(1200),
          shift_end_time = c(2400),
          shift_end_clock = c(2400),
          duration = c(1200)
        )
      )
  } else if (g_id == 2016020099) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(17, 32, 28, 93, 53, 10),
          game_period = c(3),
          shift_start_time = c(3488, 3494, 3537, 3537, 3537, 3537),
          shift_start_clock = c(3488, 3494, 3537, 3537, 3537, 3537),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(112, 106, 63, 63, 63, 63)
        )
      )
  } else if (g_id == 2015021224) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 40 & game_period == 1 ~ 1200,
            venue == "home" & sweater_number == 40 & game_period == 2 ~ 606,
            T ~ duration
          ),
        shift_start_time =
          dplyr::case_when(
            venue == "home" & sweater_number == 40 & game_period == 2 ~ 1200,
            T ~ shift_start_time
          )
      )
  } else if (g_id == 2015021049) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(33, 82, 28, 2, 93, 26),
          game_period = c(3),
          shift_start_time = c(3244, 3521, 3582, 3582, 3582, 3582),
          shift_start_clock = c(3244, 3521, 3582, 3582, 3582, 3582),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(356, 79, 18, 18, 18, 18)
        )
      )
  } else if (g_id == 2015021003) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(13, 37, 11, 27),
          game_period = c(4),
          shift_start_time = c(3600, 3600, 3670, 3670),
          shift_start_clock = c(3600, 3600, 3670, 3670),
          shift_end_time = c(3672),
          shift_end_clock = c(3672),
          duration = c(72, 72, 2, 2)
        )
      )
  } else if (g_id == 2015020969) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(40),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = c(2400),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(1200)
        )
      )
  } else if (g_id == 2015020918) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(14, 24, 30, 34, 47, 54),
          game_period = c(3),
          shift_start_time = c(3586),
          shift_start_clock = c(3586),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(14)
        )
      )
  } else if (g_id == 2015020900) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 33 & game_period == 2 ~ 1200,
            T ~ duration
          ),
        shift_start_time =
          dplyr::case_when(
            venue == "home" & sweater_number == 33 & game_period == 2 ~ 1200,
            T ~ shift_start_time
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(33, 2, 4, 16, 19, 29),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = c(2400),
          shift_end_time = c(3386, 2443, 2443, 2443, 2443, 2443),
          shift_end_clock = c(3386, 2443, 2443, 2443, 2443, 2443),
          duration = c(986, 43, 43, 43, 43, 43)
        )
      )
  } else if (g_id == 2015020866) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 33 & game_period == 2 ~ 1200,
            T ~ duration
          ),
        shift_start_time =
          dplyr::case_when(
            venue == "home" & sweater_number == 33 & game_period == 2 ~ 1200,
            T ~ shift_start_time
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(33, 2, 19),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = c(2400),
          shift_end_time = c(3491, 2428, 2428),
          shift_end_clock = c(3491, 2428, 2428),
          duration = c(1091, 28, 28)
        )
      )
  } else if (g_id == 2015020849) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(16, 26, 12, 57, 33, 18),
          game_period = c(3),
          shift_start_time = c(3555, 3555, 3555, 3555, 3573, 3573),
          shift_start_clock = c(3555, 3555, 3555, 3555, 3573, 3573),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(45, 45, 45, 45, 27, 27)
        )
      )
  } else if (g_id == 2015020825) {
    s |>
      dplyr::filter(
        !(shift_start_time == 3600 & game_period == 3),
        !(venue == "away" & shift_start_time == 3302),
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "away" & sweater_number == 35 & shift_end_time == 3300 ~ 1200,
            venue == "away" & sweater_number == 39 & shift_end_time == 3300 ~ duration + 7,
            venue == "away" & sweater_number == 8 & shift_end_time == 3300 ~ duration + 10,
            venue == "away" & sweater_number == 16 & shift_end_time == 3300 ~ duration + 10,
            venue == "away" & sweater_number == 48 & shift_end_time == 3300 ~ duration + 10,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(12, 35, 6),
          game_period = c(3, 4, 4),
          shift_start_time = c(3300, 3600, 3600),
          shift_start_clock = c(3300, 3600, 3600),
          shift_end_time = c(3310, 3778, 3634),
          shift_end_clock = c(3310, 3778, 3634),
          duration = c(8, 178, 34)
        )
      )
  } else if (g_id == 2015020560) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(33, 19, 7, 88, 72),
          game_period = c(4),
          shift_start_time = c(3600, 3742, 3763, 3763, 3763),
          shift_start_clock = c(3600, 3742, 3763, 3763, 3763),
          shift_end_time = c(3809),
          shift_end_clock = c(3809),
          duration = c(209, 67, 46, 46, 46)
        )
      )
  } else if (g_id == 2015020504) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(31, 3, 33, 43, 83, 90),
          game_period = c(3),
          shift_start_time = c(2400, 3573, 3586, 3586, 3586, 3586),
          shift_start_clock = c(2400, 3573, 3586, 3586, 3586, 3586),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(1200, 27, 14, 14, 14, 14)
        )
      )
  } else if (g_id == 2015020306) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(1, 2, 15, 17, 37, 53),
          game_period = c(3),
          shift_start_time = c(3591),
          shift_start_clock = c(3591),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(9)
        )
      )
  } else if (g_id == 2015020260) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(93),
          game_period = c(3),
          shift_start_time = c(3550),
          shift_start_clock = c(3550),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(50)
        )
      )
  } else if (g_id == 2015020212) {
    s |>
      dplyr::filter(
        !(venue == "away" & sweater_number == 30 & shift_end_time == 1407),
        !(venue == "home" & sweater_number == 1 & shift_end_time == 1426),
      ) |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            venue == "away" & sweater_number == 30 & shift_end_time == 2400 ~ 1200,
            venue == "home" & sweater_number == 1 & shift_end_time == 2400 ~ 1200,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            venue == "away" & sweater_number == 30 & shift_end_time == 2400 ~ 1200,
            venue == "home" & sweater_number == 1 & shift_end_time == 2400 ~ 1200,
            venue == "away" & shift_end_time == 1407 ~ duration + 26,
            venue == "home" & shift_end_time == 1426 ~ duration + 7,
            T ~ duration
          )
      )
  } else if (g_id == 2015020008) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(19, 10, 28, 33, 51, 77),
          game_period = c(3),
          shift_start_time = c(3521, 3582, 3582, 3582, 3582, 3582),
          shift_start_clock = c(3521, 3582, 3582, 3582, 3582, 3582),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(79, 18, 18, 18, 18, 18)
        )
      )
  } else {
    s
  }
}
