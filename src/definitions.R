#Définition des horizons temporelles utile pour l'utilisation de fonctions

horizon_estim <-
  (seq.Date(
    from = as.Date(date_debut_estim) + 1,
    to = as.Date(date_fin_estim) + 1,
    by = "quarter",
  ) - 1) %>%
  as.character(.)

horizon_estim_a <- seq(from = 2006, to = 2016, by = 1)  %>%
  as.character(.)

horizon_prev <-
  (seq.Date(
    from = as.Date(date_debut_prev) + 1,
    to = as.Date(date_fin_prev) + 1,
    by = "quarter",
  ) - 1) %>%
  as.character(.)