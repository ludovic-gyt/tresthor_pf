#comparaison simulé/observé

# Bien expliqué les possibilités de chaque fonctions dans le rmd:
#   par exemple type ="g"--> croissance en pourcentage, "d" --> différence absolu d'un lag, "lvl" --> niveau


#Le but est de comprendre si on s'écarte de l'observé à cause d'un choc non anticipable ou bien à cause d'une imprécisionn d'estimation

date_debut_simulation <- "2010-03-31"
date_fin_simulation <- "2016-03-31"
  
simul_data <- lapply(names(info_equations), function(x) {
  y <- simulate_equation(
    thor_equation = get(x),
    database = dt_pf,
    start_sim = as.Date(date_debut_estim),
    end_sim = as.Date(date_fin_estim),
    index_time = "date",
    residual_var = info_equations[[x]]$residual_name
  )
})

lapply(1:length(info_equations), function(x) {
  simul_data[[x]][["date"]] <<- as.Date(simul_data[[x]][["date"]])
}) #double < car sinon ça ne marche pas
  
lapply(1:length(info_equations), function(x) {
  graph_sim_obs(
    simul_data[[x]] ,
    start_plot = as.Date(date_debut_simulation),
    end_plot = as.Date(date_fin_simulation),
    title = paste0(info_equations[[x]]$endogenous_name),
    type = "g"
  )
})

lapply(1:length(info_equations), function(x) {
  graph_sim_obs(
    simul_data[[x]] ,
    title = paste0(info_equations[[x]]$endogenous_name),
    start_plot = as.Date(date_debut_simulation),
    end_plot = as.Date(date_fin_simulation),
    type = "lvl"
  )
})
  
  