#import de la base de donnée principale 

dt_pf <- fread(file = "input/dt_pib_tresthor.csv")

#Supression variables inutiles

dt_pf[, c(
  "ID_PROD",
  "DATA_TYPE",
  "CS_S11",
  "CS_S13",
  "VA_NM",
  "VA_M",
  "P",
  "ATSLP",
  "SSLP",
  "P_S11",
  "PIB_E",
  "DTI",
  "DTE",
  "TVA",
  "CCF_S13",
  "CI",
  "CI_S13",
  "CI_S11"
  
) := NULL] 

#Renomination des variables

setnames(dt_pf, names(dt_pf[, !c("DATE")]), paste0(names(dt_pf[, !c("DATE")]), "_v")) #pour spécifier que ce sont des variables en valeur (variables nominales)
setnames(dt_pf, names(dt_pf), str_to_lower(names(dt_pf)))
setnames(dt_pf, "pib_r_v", "pib_v")

#Définition automatique des dates de début et de fin d'observation (en fonction du jeu de donnée dt_pf)

date_debut_obs <- dt_pf[1, date]
date_fin_obs    <- dt_pf[nrow(dt_pf), date]

#import de la base de données d'indicateurs exogènes

dt_indicateur <-
  fread("C:/Users/ludovicg/Documents/R/ludovic/input/dt_indicateur.csv")

dt_indicateur <- dt_indicateur[date >= date_debut_obs &
                                 date <= date_fin_obs, ]

setnames(dt_indicateur, names(dt_indicateur), str_to_lower(names(dt_indicateur)))
setnames(
  dt_indicateur,
  c(
    "heures",
    "fx_eurusd",
    "ipc_loyers_0411",
    "ipc_alim_01",
    "ipc_nrj_045",
    "brent_usd",
    "ind_ibtg_01.0"
  ),
  c(
    "h_trav",
    "tcen",
    "px_immo",
    "ipc_alim",
    "ipc_nrj",
    "brent",
    "ind_btp"
  )
)

#Import du prix du pétrole

dt_petrole <-
  setDT(read_excel("C:/Users/ludovicg/Documents/R/ludovic/input/Petrole.xlsx"))
dt_petrole[, Date := as.Date(Date)]
for (i in 1:nrow(dt_petrole))
  dt_petrole[i, trimestre := tr(Date)]
dt_petrole <-
  dt_petrole[, Petrole := mean(Petrole), by = trimestre][year(Date) >= year(date_debut_obs) &
                                                           Date <= as.Date(date_fin_obs), .SD[1], by = trimestre]
dt_pf[, petrole := rev(dt_petrole$Petrole)]

#Import du prix de l'essence

dt_essence <-
  setDT(read_excel("C:/Users/ludovicg/Documents/R/ludovic/input/Essence.xlsx"))
dt_essence[, Date := as.Date(Date)]
for (i in 1:nrow(dt_essence))
  dt_essence[i, trimestre := tr(Date)]
dt_essence <-
  dt_essence[, Essence := mean(Essence), by = trimestre][year(Date) >= year(date_debut_obs) &
                                                           Date <= as.Date(date_fin_obs), .SD[1], by = trimestre]
dt_pf[, essence := rev(dt_essence$Essence)]


##Déflateur PIB

dt_pib <-
  setDT(read_excel("C:/Users/ludovicg/Documents/R/ludovic/input/PIB.xlsx"))

dt_pib[, DATE := as.IDate(rev(seq.Date(
  from = as.IDate(date_debut_obs) + 1,
  by = "year",
  length.out = nrow(dt_pib)
) - 1))]
dt_pib[, dfl_pib := PIB_V / PIB]


dt_pf[, dfl_pib := predict(td(rev(dt_pib[DATE >= date_debut_obs &
                                           DATE <= date_fin_obs, dfl_pib]) ~ 1, to = 4, method = "denton-cholette"))]

#Merge des deux bases

dt_pf <- dt_indicateur[dt_pf,on = .(date)]

#Allègement de l'environnement

rm(dt_essence, dt_petrole, dt_pib)
