
da <- readr::read_csv("dados/csv/atp_tennis.csv") |>
  janitor::clean_names()

dplyr::glimpse(da)

da_2024 <- da |>
  dplyr::filter(date >= "2024-01-01")

da_2024 |>
  dplyr::glimpse()

da_2024 |>
  dplyr::count(series)


jogadores_long_2024 <- da_2024 |>
  dplyr::select(
    tournament:winner
  ) |>
  tidyr::pivot_longer(
    cols = c(player_1, player_2),
    names_to = "n_player",
    values_to = "player"
  )

mais_jogos <- jogadores_long_2024 |>
  dplyr::count(player, sort = TRUE)

mais_vitorias <- da_2024 |>
  dplyr::count(winner, sort = TRUE)

# win rate 2024

mais_vitorias |>
  dplyr::inner_join(
    mais_jogos,
    by = c("winner" = "player"),
    suffix = c("_vitorias", "_jogos")
  ) |>
  dplyr::mutate(
    win_rate = n_vitorias / n_jogos
  ) |>
  dplyr::arrange(dplyr::desc(win_rate))

# carreira

jogadores_long <- da |>
  dplyr::select(
    tournament:winner
  ) |>
  tidyr::pivot_longer(
    cols = c(player_1, player_2),
    names_to = "n_player",
    values_to = "player"
  )

mais_jogos <- jogadores_long |>
  dplyr::count(player, sort = TRUE)

mais_vitorias <- da |>
  dplyr::count(winner, sort = TRUE)

win_rate_historica <- mais_vitorias |>
  dplyr::inner_join(
    mais_jogos,
    by = c("winner" = "player"),
    suffix = c("_vitorias", "_jogos")
  ) |>
  dplyr::mutate(
    win_rate = n_vitorias / n_jogos
  ) |>
  dplyr::arrange(dplyr::desc(win_rate))

win_rate_historica |>
  dplyr::filter(n_jogos > 100)

# análise por superfície

mais_jogos <- jogadores_long |>
  dplyr::count(surface, player, sort = TRUE)

mais_vitorias <- da |>
  dplyr::count(surface, winner, sort = TRUE)

win_rate_superficie <- mais_vitorias |>
  dplyr::inner_join(
    mais_jogos,
    by = c("winner" = "player", "surface"),
    suffix = c("_vitorias", "_jogos")
  ) |>
  dplyr::mutate(
    win_rate = n_vitorias / n_jogos
  ) |>
  dplyr::arrange(dplyr::desc(win_rate))

win_rate_superficie |>
  dplyr::filter(n_jogos > 50)

da$score[1:10]
