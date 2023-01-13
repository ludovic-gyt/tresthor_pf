date_debut_contribution <- "2014-03-31"
date_fin_contribution <- "2020-12-31"
date_debut_contribution_y <- "2014"
date_fin_contribution_y <- "2020"

contrib <- lapply(names(info_equations),function(x) {
  dyn_contribs(get(x),
               prev,
               as.Date(date_debut_estim),
               as.Date(date_fin_contribution),
               "date",
               info_equations[[x]][["residual_name"]]) %>%
    filter(date>=as.Date(date_debut_estim))
}) %>%
  setNames(.,names(info_equations)) %>%
  as.list()

contrib_an <- lapply(contrib, function(x) {
  yearly_contrib(x,
                 index_year=substr(x[,"date"],start=1,stop=4))
})


# Graphiques ----------------------------------------------------------------
lapply(names(info_equations),function(x) {
  contrib[[x]][["date"]] <<- as.Date(contrib[[x]][["date"]])}
)

graphiques_q<- lapply(names(info_equations),function(x) {
  graph_contrib(
    contrib[[x]],
    as.Date(date_debut_contribution),
    as.Date(date_fin_contribution),
    "date",
    paste0("Contributions trimestrielles : ",
           info_equations[[x]]$endogenous_name)
  )
}) %>%
  setNames(names(info_equations))

graphiques_a <- lapply(names(info_equations),function(x) {
  graph_contrib(
    contrib_an[[x]],
    date_debut_contribution_y,
    date_fin_contribution_y,
    "year",
    paste0("Contributions annuelles : ",info_equations[[x]]$endogenous_name)
  )
})  %>%
  setNames(.,names(info_equations))

graphiques_q
graphiques_a
