u <- "https://github.com/JeffSackmann/tennis_slam_pointbypoint/raw/refs/heads/master/2024-wimbledon-points.csv"
dados_wb <- readr::read_csv(u)

u_meta <- "https://github.com/JeffSackmann/tennis_slam_pointbypoint/raw/refs/heads/master/2024-wimbledon-matches.csv"
dados_wb_meta <- readr::read_csv(u_meta)

dplyr::glimpse(dados_wb_meta)


readr::problems(dados_wb)

dados_wb |>
  janitor::clean_names() |>
  dplyr::glimpse()

table(dados_wb$Rally)

break_points <- dados_wb |>
  janitor::clean_names() |>
  dplyr::group_by(match_id) |>
  dplyr::summarise(
    dplyr::across(
      dplyr::contains("break_point"),
      sum
    )
  )

break_points_stats <- break_points |>
  tidyr::pivot_longer(-match_id) |>
  tidyr::separate_wider_position(
    name,
    widths = c(player = 2, name = 1e6),
    too_many = "drop",
    too_few = "align_start"
  ) |>
  tidyr::pivot_wider() |>
  dplyr::select(-break_point_missed) |>
  dplyr::mutate(
    tx_vitoria = break_point_won / break_point
  ) |>
  dplyr::arrange(dplyr::desc(tx_vitoria))


aux_join <- dados_wb_meta |>
  dplyr::select(match_id, p1 = player1, p2 = player2) |>
  tidyr::pivot_longer(
    -match_id, names_to = "player", values_to = "player_name"
  )

break_points_stats |>
  dplyr::inner_join(aux_join, by = c("match_id", "player")) |>
  dplyr::group_by(player_name) |>
  dplyr::summarise(
    dplyr::across(c(break_point, break_point_won), sum)
  ) |>
  dplyr::mutate(
    tx_vitoria = break_point_won / break_point
  ) |>
  dplyr::arrange(dplyr::desc(tx_vitoria)) |>
  dplyr::filter(break_point >= 10) |>
  View("a")


dados_wb

dados_wb_meta |>
  dplyr::filter(
    player1 == "Barbora Krejcikova",
    player2 == "Jasmine Paolini"
  ) |>
  dplyr::glimpse()

final_wta <- dados_wb |>
  janitor::clean_names() |>
  dplyr::filter(match_id == "2024-wimbledon-2701")

View(final_wta)

final_wta |>
  dplyr::count(point_server, point_winner) |>
  dplyr::filter(point_server != 0) |>
  dplyr::group_by(point_server) |>
  dplyr::mutate(
    tx_vitoria = n / sum(n)
  )

final_wta |>
  dplyr::transmute(
    point_winner,
    lag_point_winner = dplyr::lag(point_winner)
  ) |>
  dplyr::filter(lag_point_winner != 0) |>
  dplyr::count(lag_point_winner, point_winner) |>
  dplyr::group_by(lag_point_winner) |>
  dplyr::mutate(
    tx_vitoria = n / sum(n)
  )

