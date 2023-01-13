#Méthode Mathieu pour mettre des NA a toutes les lignes de l'horizon prévisionnel sauf à la colonne date

#1eme méthode

for(j in setdiff(names(dt_pf),"date")) dt_pf[horizon_prev_index, (j):=NA]

#2eme méthode

monVecteur <- setdiff(names(dt_pf),"date")
dt_pf[horizon_prev_index,(monVecteur):=lapply(.SD, function(x) NA), .SDcols=monVecteur]

#Méthode pour répéter une donnée sur l'horizon prev de certaines colonne (méthode plus simple utilisé dans le programme)

dt_update <- data.table()
for (i in 1:8)
  dt_update <- rbind(dt_update, dt_pf[48])
dt_update[, date := as.Date(horizon_prev)]
for (j in series_stables)
  dt_pf[dt_update, (j) := get(paste0("i.", j)), on = .(date)]