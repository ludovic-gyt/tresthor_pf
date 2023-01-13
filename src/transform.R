
#Hypothèse hors modèle (ceci doit être remplacé par les vraies données
#surtout que cela détermine le rdb observé à travers la pmc)


dt_pf[, c("dfl_fbcf_s13",
          "dfl_m",
          "dfl_x",
          "dfl_cf_s13") := 1][
            , pmc := 0.8]


#variables agrégées (bien et service) pour les imports et exports 
#afin de rester à un niveau analysable de l'économie.

dt_pf[, x_v := x_b_v + x_s_v][, m_v := m_s_v + m_b_v]

#Construction des variables manquante de l'emploi (pour estimation (construction hors modèle)

dt_pf[, rs_v := rs_s11_v + rs_s13_v]

dt_pf[, emploi := eff_sal_etp + total][,
       salaire := (msalb - ch_sal -cst) / 100000][,
       msalb := msalb / 100000][,
       ch_sal := ch_sal / 100000][, 
       ch_pat := ch_pat / 100000][,
       cst := cst / 100000]

#salaire est une variable test pour remplacer rs_v mais en fait elle ne corresponde pas (problème d'échelle en plus)
#On n'a pas les données sur le chômage


#Pas de variable heures travaillé par emploi car heures est construire sur eff_sal_etp*507 (nombre d'heure travailler par trimestre 35*52/4)

#Construction artificielle rdb_s14_v hors modèle

set.seed(123)

dt_pf[, pmc := 0.8][,
       rdb_s14_v:=cf_s14_v/pmc][,
       pertu:=rnorm(1:nrow(dt_pf),0,0.01) ][,
       rdb_s14_v:=rdb_s14_v+rdb_s14_v*pertu]

#Il nous faudrait l'épargne pour pouvoir vraiment déterminer le rdb, pour l'instant l'équation ne sert à rien  
#sans le rdb on ne peut pas répliquer la théorie du revenu permanent correctement

#Supression de lignes/création de lignes sur l'horizon de prévision

for (i in horizon_prev) {
  if (as.Date(i) %in% dt_pf$date) {
    dt_pf[date == as.Date(i), setdiff(names(dt_pf), "date") := NA]
  }
  else {
    dt_add <-
      data.table(matrix(as.numeric(NA), nrow = 1, ncol = ncol(dt_pf)))
    setnames(dt_add, new = names(dt_pf))
    dt_add[, date := as.IDate(i)]
    dt_pf <- rbind(dt_pf, dt_add)
  }
}

#Définition de l'index d'horizon

horizon_estim_index <- which(dt_pf$date%in%as.Date(horizon_estim))
horizon_prev_index <- which(dt_pf$date%in%as.Date(horizon_prev))
horizon_total_index <- 1:nrow(dt_pf)



#Travail du prévisionniste : Hypothèses variables exo sur la période de prévision

ggplot(data = dt_pf
       , aes(x =date, y=rdb_s14_v)) +
  geom_line() +
  ggtitle(paste0("Evolution des salaires")) #graph pour déterminer la tendance en hypothèse



series_stables <-
  c(
    "dfl_fbcf_s13",
    "dfl_m",
    "dfl_x",
    "fbcf_s13_v",
    "ipc_tot",
    "vs_v",
    "pmc",
    "dfl_cf_s13",
    "tcen",
    "petrole"
  ) #Séries plutôt stable


dt_pf[horizon_prev_index, c(series_stables) := dt_pf[rep(horizon_prev_index[1] -
                                                           1, length(horizon_prev_index)), c(series_stables), with = F]]


series_croissantes <- 
  c("x_v", 
    "cf_s13_v", 
    "rs_v", 
    "rs_s11_v",
    "eff_sal_etp",
    "brent",
    "msalb",
    "fbcf_s14_v",
    "cf_s14_v",
    "px_immo",
    "va_v",
    "ind_btp"
  ) #série avec tendance croissante


laps <-
  8 #Choix du nombre de trimestre pour établir le taux de croissance

n0 <- horizon_prev_index[1] - laps
n1 <- horizon_prev_index[1] - 1

x0 <- dt_pf[n0, c(series_croissantes), with = F]
x1 <- dt_pf[n1, c(series_croissantes), with = F]

variation <-
  (x1 / x0) ^ (1 / (n1 - n0))

for (h in horizon_prev_index) {
  dt_pf[h, (series_croissantes) := dt_pf[h - 1, c(series_croissantes), with =
                                           F] * (variation)]
}

#statut spécial d'hypothèse pour l'investissement des entreprises et les imports à cause 
#des avions achetés en 2018.

dt_pf[horizon_prev_index, fbcf_s11_v := 16000][horizon_prev_index, m_v := 55000]

#Ajout d'une variable tendance pour les MCE

dt_pf[,tendance:=1:nrow(dt_pf)]

#Indicatrice

#crise 2008

dt_pf[,dummy_crise_fi:=0][date>=as.Date("2008-03-31")&date<=as.Date("2009-12-31"),dummy_crise_fi:=1]

#Avion 

dt_pf[,dummy_18_q1:=0][date==as.Date("2018-03-31"),dummy_18_q1:=1]
dt_pf[,dummy_18_q2:=0][date==as.Date("2018-06-30"),dummy_18_q2:=1]
dt_pf[,dummy_18_q3:=0][date==as.Date("2018-09-30"),dummy_18_q3:=1]
dt_pf[,dummy_18_q4:=0][date==as.Date("2018-12-31"),dummy_18_q4:=1]

#ou


dt_pf[,dummy_avion:=0][date>=as.Date("2018-03-31")&date<=as.Date("2018-12-31"),dummy_avion:=1]

#Suppression base et variables encombrantes dans l'environnement

rm(dt_add,dt_indicateur,variation,x0,x1,n0,n1)
