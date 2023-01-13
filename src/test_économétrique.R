
#dt_pf seulement sur la période d'estimation :

dt_pf1 <- dt_pf[dt_pf$date >= date_debut_estim &
                  dt_pf$date <= date_fin_estim , ]
#dt_pf sur la période d'estimation mais sans 2017 et 2018 pollué par l'investissement d'avions:

dt_pf2 <- dt_pf1[1:48, ]

## Equation RDB :  eq_rdb_s14_v 


## Equation Revenu Disponible Brut : eq_rdb_s14_v

###  rdb_s14_v 


ggplot(data = dt_pf1
       , aes(x = date, y = log(rdb_s14_v))) +
  geom_line() +
  ggtitle(paste0("Evolution du rdb "))


a <- ur.df(
  log(dt_pf1$rdb_s14_v),
  lags = 6,
  selectlags = "AIC",
  type = "trend"
)
summary(a)
b <- ur.df(
  log(dt_pf1$rdb_s14_v),
  lags = 6,
  selectlags = "AIC",
  type = "drift"
)
summary(b)
c <- ur.df(
  log(dt_pf1$rdb_s14_v),
  lags = 6,
  selectlags = "AIC",
  type = "none"
)
summary(c)

###  rs_v 

ggplot(data = dt_pf1
       , aes(x = date, y = log(rs_v))) +
  geom_line() +
  ggtitle(paste0("Evolution des salaires"))

a <- ur.df(
  log(dt_pf1$rs_v),
  lags = 6,
  selectlags = "AIC",
  type = "trend"
)
summary(a)
b <- ur.df(
  log(dt_pf1$rs_v),
  lags = 6,
  selectlags = "AIC",
  type = "drift"
)
summary(b)
c <- ur.df(
  log(dt_pf1$rs_v),
  lags = 6,
  selectlags = "AIC",
  type = "none"
)
summary(c)

#Avec cette variable explicative, nous ne retenons ni la constante ni la dérive dans le modèle. Le modèle simple nous conduit à accepter l'hypothèse $H_0$ de non-stationarité. La série est ainsi intégrée d'ordre 1. 

### msalb

ggplot(data = dt_pf1
       , aes(x = date, y = msalb)) +
  geom_line() +
  ggtitle(paste0("Evolution de la masse salariale "))

a <- ur.df(
  log(dt_pf1$msalb),
  lags = 6,
  selectlags = "AIC",
  type = "trend"
)
summary(a)
b <- ur.df(
  log(dt_pf1$msalb),
  lags = 6,
  selectlags = "AIC",
  type = "drift"
)
summary(b)
c <- ur.df(
  log(dt_pf1$msalb),
  lags = 6,
  selectlags = "AIC",
  type = "none"
)
summary(c)


# msalb  est non-stationaire et intégrée d'ordre 1.

### Régression de la relation de long terme

reg_rdb_s14_v_lt1 <- lm(log(rdb_s14_v) ~ log(rs_v), dt_pf1)
summary(reg_rdb_s14_v_lt1)

reg_rdb_s14_v_lt2 <- lm(log(rdb_s14_v) ~ log(msalb), dt_pf1)
summary(reg_rdb_s14_v_lt2)

#Les 2 variables ont plus ou moins le même pouvoir explicatif.

### Test de cointégration

res1_rdb_s14_v <- resid(reg_rdb_s14_v_lt1)
reg_rdb_s14_v_res1 <-
  ur.df(res1_rdb_s14_v,
        lags = 6,
        type = "none",
        selectlags = "AIC")
summary(reg_rdb_s14_v_res1)
plot(res1_rdb_s14_v)

res2_rdb_s14_v <- resid(reg_rdb_s14_v_lt2)
reg_rdb_s14_v_res2 <-
  ur.df(res1_rdb_s14_v,
        lags = 6,
        type = "none",
        selectlags = "AIC")
summary(reg_rdb_s14_v_res2)
plot(res2_rdb_s14_v)

#En écartant le problème de statioraité de  rdb_s14_v , on peut conclure ici que les résidus 
#(des 2 régressions) sont stationnaires.  rdb_s14_v  est donc cointégré avec  rs_v  ainsi 
#qu'avec  msalb . 

### Détermination de la spécification


create_equation(
  equation_name = "eq_rdb_s14_v",
  formula = "delta(1,log(rdb_s14_v))=a_cst+a_0*(log(lag(rdb_s14_v,-1))-a_lt2*log(lag(rs_v,-1))-a_lt1)+a_1*delta(1,log(rs_v))+delta(1,af_eq_rdb_s14_v)-a_0*lag(af_eq_rdb_s14_v,-1)",
  coefflist = c("a_cst", "a_0", "a_1", "a_2", "a_lt2", "a_lt1"),
  endogenous = "rdb_s14_v"
)
estimation <-
  quick_estim(
    thor_equation = eq_rdb_s14_v ,
    database = dt_pf,
    estim_start = as.Date(date_debut_estim),
    estim_end = as.Date(date_fin_estim),
    coeff_lt = c("a_lt1", "a_lt2")
  )

create_equation(
  equation_name = "eq_rdb_s14_v",
  formula = "delta(1,log(rdb_s14_v))=a_cst+a_0*(log(lag(rdb_s14_v,-1))-a_lt2*log(lag(msalb,-1))-a_lt1)+a_1*delta(1,log(msalb))+delta(1,af_eq_rdb_s14_v)-a_0*lag(af_eq_rdb_s14_v,-1)",
  coefflist = c("a_cst", "a_0", "a_1", "a_2", "a_lt2", "a_lt1"),
  endogenous = "rdb_s14_v"
)

estimation <-
  quick_estim(
    thor_equation = eq_rdb_s14_v ,
    database = dt_pf,
    estim_start = as.Date(date_debut_estim),
    estim_end = as.Date(date_fin_estim),
    coeff_lt = c("a_lt1", "a_lt2")
  )


#Les résultats sont très similaires. On retient  rs_v  qui a plus de sens économique dans le cadre de cette spécification. En effet  msalb  contient en partie les charges sociales, non déterminantes du revenu disponible brut. Lors de précédents tests avec une autre configuration de déflateur et une autre méthode de trimestrialisation, la variable  ipc_tot  pouvait être incorporée dans la partie de court terme et permettait de capter une importante partie de la variance de la variable dépendante. Les mises à jour du code ont altérées cet effet explicatif. 

#S'il n'était finalement pas possible d'obtenir des données fiable pour  rdb_s14_v , une possibilité serait de ne conserver que le MCE  eq_cf_s14  et d'expliquer la consommation finale des ménages avec  rs_v . En effet, c'est ce que nous avons indirectement effectué avec  eq_rdb_s14_v  et il s'avère que la régression est satisfaisante. De plus,  eq_cf_s14  conserverait sa cohérence économique.

## Equation Consommation finale des ménages :  eq_cf_s14 

###  cf_s14 


ggplot(data = dt_pf1
       , aes(x = date, y = log(cf_s14))) +
  geom_line() +
  ggtitle(paste0("Evolution de la consommation finale des ménages"))

a <- ur.df(
  log(dt_pf1$cf_s14),
  lags = 6,
  selectlags = "AIC",
  type = "trend"
)
summary(a)
b <- ur.df(
  log(dt_pf1$cf_s14),
  lags = 6,
  selectlags = "AIC",
  type = "drift"
)
summary(b)


#La constante est significative et nous rejetons l'hypothèse nulle de non-stationarité. La série n'est pas intégrée d'ordre 1.

###  pa 



ggplot(data = dt_pf1
       , aes(x = date, y = log(pa))) +
  geom_line() +
  ggtitle(paste0("Evolution du pouvoir d'achat"))

a <- ur.df(log(dt_pf1$pa),
           lags = 6,
           selectlags = "AIC",
           type = "trend")
summary(a)


#Nous retenons le modèle le plus élaboré avec constante et dérive temporelle, et rejetons l'hypothèse de non-stationarité.  pa  n'est pas intégré d'ordre 1.

### Régression de la relation de long terme


reg_cf_s14_lt1 <- lm(log(cf_s14)~log(pa),dt_pf1)
summary(reg_cf_s14_lt1) 

#La régression est forcément bonne étant donné que la relation est artificielle.

### Test de cointégration


res1_cf_s14 <- resid(reg_cf_s14_lt1)
plot(res1_cf_s14)
reg_cf_s14_res1<-
  ur.df(res1_cf_s14,
        lags = 6,
        type = "none",
        selectlags = "AIC")
summary(reg_cf_s14_res1)

#Les 2 séries sont cointégrées. La valeur du test statistique est en effet presque égale à la valeur critique pour un seuil de 5 %. Nous considérons que nous pouvons rejeter l'hypothèse nulle de non-stationarité.

### Détermination de la spécification


create_equation(
  equation_name = "eq_cf_s14",
  formula="delta(1,log(cf_s14))=b_cst+b_0*(log(lag(cf_s14,-1))-b_lt2*log(lag(pa,-1))-b_lt1)+b_1*delta(1,log(pa))+delta(1,af_eq_cf_s14)-b_0*lag(af_eq_cf_s14,-1)",
  coefflist = c("b_cst","b_0","b_1","b_2","b_lt2","b_lt1"),
  endogenous = "cf_s14"
)
estimation <- quick_estim(thor_equation = eq_cf_s14 , database = dt_pf,
                          estim_start=as.Date(date_debut_estim),
                          estim_end=as.Date(date_fin_estim),
                          coeff_lt = c("b_lt1","b_lt2"),
                          const = T)


create_equation(
  equation_name = "eq_cf_s14",
  formula = "delta(1,log(cf_s14))=b_cst+b_0*(log(lag(cf_s14,-1))-b_lt2*log(lag(pa,-1))-b_lt1)+b_1*delta(1,log(pa))+b_2*delta(1,log(emploi))+delta(1,af_eq_cf_s14)-b_0*lag(af_eq_cf_s14,-1)",
  coefflist = c("b_cst", "b_0", "b_1", "b_2", "b_lt2", "b_lt1"),
  endogenous = "cf_s14"
)
estimation <-
  quick_estim(
    thor_equation = eq_cf_s14 ,
    database = dt_pf,
    estim_start = as.Date(date_debut_estim),
    estim_end = as.Date(date_fin_estim),
    coeff_lt = c("b_lt1", "b_lt2"),
    const = T
  )

#Dans les tests économétriques menés avec une précédente configuration, la variable  emploi  pouvait être incorporée dans la partie de court terme et permettait de capter une importante partie de la variance de la variable dépendante. Des modifications sur les déflateurs ont altéré cet effet explicatif mais nous le précison quand même à titre indicatif. Dans le modèle  OPALE , c'est le taux de chômage qui est utilisé pour expliqué les variations de court-terme. Il faudra donc s'interessé à la relation entre consommation des ménages et marché du travail quand les données seront définitives.

## Equation Investissement des entreprises :  eq_fbcf_s11 

###  fbcf_s11 

ggplot(data = dt_pf2
       , aes(x =date, y=log(fbcf_s11))) +
  geom_line() +
  ggtitle(paste0("Evolution de l'investissement des entreprises"))

a <- ur.df(log(dt_pf2$fbcf_s11), 
           lags = 6, 
           selectlags = "AIC", 
           type = "trend")
summary(a)
b <- ur.df(log(dt_pf2$fbcf_s11), 
           lags = 6, 
           selectlags = "AIC", 
           type = "drift")
summary(b)
c <- ur.df(log(dt_pf2$fbcf_s11), 
           lags = 6, 
           selectlags = "AIC", 
           type = "none")
summary(c)

#Nous acceptons l'hypothèse nulle de non-stationarité dans un modèle sans constante ni dérive temporelle.

###  pib 

ggplot(data = dt_pf1
       , aes(x = date, y = log(pib))) +
  geom_line() +
  ggtitle(paste0("Evolution du pib"))

a <- ur.df(
  log(dt_pf1$pib),
  lags = 6,
  selectlags = "AIC",
  type = "trend"
)
summary(a)
b <- ur.df(
  log(dt_pf1$pib),
  lags = 6,
  selectlags = "AIC",
  type = "drift"
)
summary(b)
c <- ur.df(
  log(dt_pf1$pib),
  lags = 6,
  selectlags = "AIC",
  type = "none"
)
summary(c)

#Nous acceptons l'hypothèse nulle de non-stationarité dans un modèle sans constante ni dérive temporelle.

###  va_v 

ggplot(data = dt_pf1
       , aes(x = date, y = log(va_v))) +
  geom_line() +
  ggtitle(paste0("Evolution de la valeur ajoutée "))

a <- ur.df(
  log(dt_pf1$va_v),
  lags = 6,
  selectlags = "AIC",
  type = "trend"
)
summary(a)
b <- ur.df(
  log(dt_pf1$va_v),
  lags = 6,
  selectlags = "AIC",
  type = "drift"
)
summary(b)
c <- ur.df(
  log(dt_pf1$va_v),
  lags = 6,
  selectlags = "AIC",
  type = "none"
)
summary(c)


#Nous acceptons l'hypothèse nulle de non-stationarité dans un modèle sans constante ni dérive temporelle.

### Regression de la relation de long terme

reg_fbcf_s11_lt1 <- lm(log(fbcf_s11) ~ log(pib), dt_pf2)
summary(reg_fbcf_s11_lt1)

reg_fbcf_s11_lt2 <- lm(log(fbcf_s11) ~ log(va_v), dt_pf2)
summary(reg_fbcf_s11_lt2)

#Le pib explique mieux l'investissement des entreprises. La valeur ajoutée est quand même une variable intéressante à revoir avec les données finales. Nous retenons pour l'instant uniquement le  pib .

### Tests de cointégration

res1_fbcf_s11 <- resid(reg_fbcf_s11_lt1)
plot(res1_fbcf_s11)
reg_fbcf_s11_res1 <-
  ur.df(res1_fbcf_s11,
        lags = 6,
        type = "none",
        selectlags = "AIC")
summary(reg_fbcf_s11_res1)


# fbcf_s11  est cointégrée avec chacune des 2 variables utilisées.

### Détermination de la spécification

create_equation(
  equation_name = "eq_fbcf_s11",
  formula = "delta(1,log(fbcf_s11))=c_cst+c_0*(log(lag(fbcf_s11,-1))-c_lt2*log(lag(pib,-1))-c_lt1)+c_1*delta(1,log(pib))+delta(1,af_eq_fbcf_s11)-c_0*lag(af_eq_fbcf_s11,-1)",
  coefflist = c(
    "c_cst",
    "c_0",
    "c_1",
    "c_2",
    "c_3",
    "c_4",
    "c_5",
    "c_lt2",
    "c_lt1"
  ),
  endogenous = "fbcf_s11"
)
estimation <-
  quick_estim(
    thor_equation = eq_fbcf_s11,
    database = dt_pf,
    estim_start = as.Date(date_debut_estim),
    estim_end = as.Date(date_fin_estim),
    coeff_lt = c("c_lt1", "c_lt2"),
    const = T
  )

#La relation n'est pas du tout satisfaisante. Nous essayons de résoudre le problème en inlcuant des indicatrices afin de prendre en compte les investissements d'avions. Toutefois, comme vu précédemment, la meilleure solution serait de retrancher ces avions directement dans les données d'origine.

create_equation(
  equation_name = "eq_fbcf_s11",
  formula = "delta(1,log(fbcf_s11))=c_cst+c_0*(log(lag(fbcf_s11,-1))-c_lt2*log(lag(pib,-1))-c_lt1)+c_1*delta(1,log(pib))+c_2*dummy_avion+delta(1,af_eq_fbcf_s11)-c_0*lag(af_eq_fbcf_s11,-1)",
  coefflist = c(
    "c_cst",
    "c_0",
    "c_1",
    "c_2",
    "c_lt2",
    "c_lt1"
  ),
  endogenous = "fbcf_s11"
)
estimation <-
  quick_estim(
    thor_equation = eq_fbcf_s11,
    database = dt_pf,
    estim_start = as.Date(date_debut_estim),
    estim_end = as.Date(date_fin_estim),
    coeff_lt = c("c_lt1", "c_lt2"),
    const = T
  )


#Inclure des indicatrices permet de faire automatiquement augmenter le  R^2^ . Cependant, cela ne permet pas de rendre  pib  intéressant pour expliquer les variations de court-terme de  fbcf_s11 . On décide de changer la période d'estimation pour ce MCE afin d'exclure 2018 et nous décalons  pib  d'un retard afin de prendre en compte le temps de réaction de l'investissement :



create_equation(
  equation_name = "eq_fbcf_s11",
  formula = "delta(1,log(fbcf_s11))=c_cst+c_0*(log(lag(fbcf_s11,-1))-c_lt2*log(lag(pib,-2))-c_lt1)+c_1*delta(1,log(lag(pib,-1)))+delta(1,af_eq_fbcf_s11)-c_0*lag(af_eq_fbcf_s11,-1)",
  coefflist = c(
    "c_cst",
    "c_0",
    "c_1",
    "c_2",
    "c_lt2",
    "c_lt1"
  ),
  endogenous = "fbcf_s11"
)
estimation <-
  quick_estim(
    thor_equation = eq_fbcf_s11,
    database = dt_pf,
    estim_start = as.Date(date_debut_estim),
    estim_end = as.Date("2017-12-31"),
    coeff_lt = c("c_lt1", "c_lt2"),
    const = T
  )


#Cette modification permet de mieux juger la relation de long-terme. Et  pib  permet de mieux expliquer les dynamique de court terme quand il est appréhender avec un retard. Toutefois le  R^2^  est faible.

#Notons qu'il serait intéressant d'étudier le taux d'utilisation des capacités et le taux d'intérêt pour afiner la spécification.

## Equation Investissement des ménages :  eq_fbcf_s14 

###  fbcf_s14 

ggplot(data = dt_pf1
       , aes(x = date, y = log(fbcf_s14))) +
  geom_line() +
  ggtitle(paste0("Evolution de l'investissement des ménages"))

a <- ur.df(
  log(dt_pf1$fbcf_s14),
  lags = 6,
  selectlags = "AIC",
  type = "trend"
)
summary(a)
b <- ur.df(
  log(dt_pf1$fbcf_s14),
  lags = 6,
  selectlags = "AIC",
  type = "drift"
)
summary(b)
c <- ur.df(
  log(dt_pf1$fbcf_s14),
  lags = 6,
  selectlags = "AIC",
  type = "none"
)
summary(c)

#Nous acceptons l'hypothèse nulle de non-stationarité dans un modèle sans constante ni dérive temporelle.

###  px_immo 


ggplot(data = dt_pf1
       , aes(x = date, y = log(px_immo))) +
  geom_line() +
  ggtitle(paste0("Evolution du prix de l'immobilier"))

a <- ur.df(
  log(dt_pf1$px_immo),
  lags = 6,
  selectlags = "AIC",
  type = "trend"
)
summary(a)
b <- ur.df(
  log(dt_pf1$px_immo),
  lags = 6,
  selectlags = "AIC",
  type = "drift"
)
summary(b)
c <- ur.df(
  log(dt_pf1$px_immo),
  lags = 6,
  selectlags = "AIC",
  type = "none"
)
summary(c)

#Nous acceptons l'hypothèse nulle de non-stationarité dans un modèle sans constante ni dérive temporelle.

###  ind_btp 

ggplot(data = dt_pf1
       , aes(x = date, y = log(ind_btp))) +
  geom_line() +
  ggtitle(paste0("Evolution de l'indicateur des prix du batiment"))

c <- ur.df(
  log(dt_pf1$ind_btp),
  lags = 6,
  selectlags = "AIC",
  type = "trend"
)
summary(c)
b <- ur.df(
  log(dt_pf1$ind_btp),
  lags = 6,
  selectlags = "AIC",
  type = "drift"
)
summary(b)
c <- ur.df(
  log(dt_pf1$ind_btp),
  lags = 6,
  selectlags = "AIC",
  type = "none"
)
summary(c)


#Nous pouvons difficilement accepter l'hypothèse nulle de non-stationarité (valeur du test statistique supérieur de très peu à la valeur critique au seuil de 5%) en retenant le modèle avec constante sans dérive temporelle.

### Regression de la relation de long terme

reg_fbcf_s14_lt1 <- lm(log(fbcf_s14) ~ log(px_immo), dt_pf1)
summary(reg_fbcf_s14_lt1)

reg_fbcf_s14_lt2 <- lm(log(fbcf_s14) ~ log(ind_btp), dt_pf1)
summary(reg_fbcf_s14_lt2)

#Les deux variables expliquent l'investissement des ménages. Il faut toutefois retenir que  ind_btp  n'est pas intégré d'ordre 1.

### Tests de cointégration


res1_fbcf_s14 <- resid(reg_fbcf_s14_lt1)
reg_fbcf_s14_res1 <-
  ur.df(res1_fbcf_s14,
        lags = 6,
        type = "none",
        selectlags = "AIC")
summary(reg_fbcf_s14_res1)
plot(res1_fbcf_s14)
res2_fbcf_s14 <- resid(reg_fbcf_s14_lt2)
reg_fbcf_s14_res2 <-
  ur.df(res2_fbcf_s14,
        lags = 6,
        type = "none",
        selectlags = "AIC")
summary(reg_fbcf_s14_res2)
plot(res2_fbcf_s14)


# fbcf_s14  est cointégrée avec chacune des 2 variables utilisées.
#Attention, les graphiques montrent que les résidus peuvent être temporairement non-stationnaire.

### Détermination de la spécification

create_equation(
  equation_name = "eq_fbcf_s14",
  formula = "delta(1,log(fbcf_s14))=d_cst+d_0*(log(lag(fbcf_s14,-1))-d_lt2*log(lag(px_immo,-1))-d_lt1)+d_1*delta(1,log(px_immo))+delta(1,af_eq_fbcf_s14)-d_0*lag(af_eq_fbcf_s14,-1)",
  coefflist = c(
    "d_cst",
    "d_0",
    "d_1",
    "d_2",
    "d_lt2",
    "d_lt1"
  ),
  endogenous = "fbcf_s14"
)
estimation <-
  quick_estim(
    thor_equation = eq_fbcf_s14,
    database = dt_pf,
    estim_start = as.Date(date_debut_estim),
    estim_end = as.Date(date_fin_estim),
    coeff_lt = c("d_lt1", "d_lt2"),
    const = T
  )

# px_immo  explique plutôt bien  fbcf_s14  à court terme. Cependant, la correction d'erreur n'est pas satisfaisante. Nous testons une autre spécification :


create_equation(
  equation_name = "eq_fbcf_s14",
  formula = "delta(1,log(fbcf_s14))=d_cst+d_0*(log(lag(fbcf_s14,-1))-d_lt2*log(lag(px_immo,-1))-d_lt3*tendance-d_lt1)+d_1*delta(1,log(px_immo))+d_2*delta(1,log(ind_btp))+delta(1,af_eq_fbcf_s14)-d_0*lag(af_eq_fbcf_s14,-1)",
  coefflist = c(
    "d_cst",
    "d_0",
    "d_1",
    "d_2",
    "d_lt1",
    "d_lt2",
    "d_lt3"
  ),
  endogenous = "fbcf_s14"
)
estimation <-
  quick_estim(
    thor_equation = eq_fbcf_s14,
    database = dt_pf,
    estim_start = as.Date(date_debut_estim),
    estim_end = as.Date(date_fin_estim),
    coeff_lt = c("d_lt1", "d_lt2","d_lt3" ),
    const = T
  )


#L'ajout, dans la partie de court terme de  ind_btp  n'améliore pas de façon significative les résultats de la régression, mais on le laisse à ce stade expérimental. Un ajout important concerne la partie de long terme. En effet, comme dans le cas Grande Bretagne de la DG du trésor, on inclue une tendance dans la partie de long terme. La correction d'erreur devient davantage significative grâce à cette modification. Ceci pourrait avoir encore plus de valeur ajoutée sans la variation inexpliquée de l'investissement des ménages en 2018. Le fait d'inclure une tendance est peut-être une pratique à adopter lorsque les résidus dérives dans la régression de long terme. Ceci est à considérer.

#Par exemple, si on test la cointégration avec l'ajout de la tendance, on observe une bien meilleure stationarité à la fois graphiquement et analytiquement. Nous ajoutons cette tendance uniquement dans ce MCE mais il est nécessaire d'étudier l'ajout d'une telle variable dans d'autres MCE dans la suite des recherches.


reg_fbcf_s14_lt3 <-
  lm(log(fbcf_s14) ~ log(px_immo) + tendance, dt_pf1)
summary(reg_fbcf_s14_lt3)
res3_fbcf_s14 <- resid(reg_fbcf_s14_lt3)
reg_fbcf_s14_res3 <-
  ur.df(res3_fbcf_s14,
        lags = 6,
        type = "none",
        selectlags = "AIC")
summary(reg_fbcf_s14_res3)
plot(res3_fbcf_s14)

## Equation Salaire moyen par tête des salariés :  eq_smpt_s11 

###  smpt_s11 


ggplot(data = dt_pf1
       , aes(x = date, y = log(smpt_s11))) +
  geom_line() +
  ggtitle(paste0("Evolution du salaire moyen par tête des salariés"))

a <- ur.df(
  log(dt_pf1$smpt_s11),
  lags = 6,
  selectlags = "AIC",
  type = "trend"
)
summary(a)


#Nous rejetons l'hypothèse nulle de non-stationarité dans un modèle avec constante et dérive temporelle.

###  dfl_cf_s14 

#Le déflateur de la consommation finale des méngages permet d'expliquer la dynamique du salaire en bouclant le prix au salaire.

ggplot(data = dt_pf1
       , aes(x = date, y = log(dfl_cf_s14))) +
  geom_line() +
  ggtitle(paste0("Evolution du déflateur de la consommation finale des ménages"))

a <- ur.df(
  log(dt_pf1$dfl_cf_s14),
  lags = 6,
  selectlags = "AIC",
  type = "trend"
)
summary(a)
b <- ur.df(
  log(dt_pf1$dfl_cf_s14),
  lags = 6,
  selectlags = "AIC",
  type = "drift"
)
summary(b)


#Nous rejetons l'hypothèse nulle de non-stationarité dans un modèle avec constante sans dérive temporelle.

###  productivite 

ggplot(data = dt_pf1
       , aes(x = date, y = log(productivite))) +
  geom_line() +
  ggtitle(paste0("Evolution de la productivité"))

a <- ur.df(
  log(dt_pf1$productivite),
  lags = 6,
  selectlags = "AIC",
  type = "trend"
)
summary(a)
b <- ur.df(
  log(dt_pf1$productivite),
  lags = 6,
  selectlags = "AIC",
  type = "drift"
)
summary(b)
c <- ur.df(
  log(dt_pf1$productivite),
  lags = 6,
  selectlags = "AIC",
  type = "none"
)
summary(c)


#Nous acceptons l'hypothèse nulle de non-stationarité en retenant le modèle sans constante ni dérive temporelle.

### Regression de la relation de long terme

reg_smpt_s11_lt1 <- lm(log(smpt_s11) ~ log(dfl_cf_s14), dt_pf1)
summary(reg_smpt_s11_lt1)

reg_smpt_s11_lt2 <- lm(log(smpt_s11) ~ log(productivite), dt_pf1)
summary(reg_smpt_s11_lt2)

# dfl_cf_s14  est une très bonne variable explicative pour la partie de long terme.  productivite  est également intéressante.

### Tests de cointégration


res1_smpt_s11 <- resid(reg_smpt_s11_lt1)
reg_smpt_s11_res1 <-
  ur.df(res1_smpt_s11,
        lags = 6,
        type = "none",
        selectlags = "AIC")
summary(reg_smpt_s11_res1)
plot(res1_smpt_s11)
res2_smpt_s11 <- resid(reg_smpt_s11_lt2)
reg_smpt_s11_res2 <-
  ur.df(res2_smpt_s11,
        lags = 6,
        type = "none",
        selectlags = "AIC")
summary(reg_smpt_s11_res2)
plot(res2_smpt_s11)
 

#smpt_s11  est cointégrée avec  productivité . Nous pourrions néanmoins, graphiquement, douter de la stationarité jusqu'à la 20ème observation. Concernant la cointégration avec  dfl_cf_14 , on ne peut rejeter l'hypothèse de non-stationarité qu'à un niveau de 10%. Ceci se confirme graphiquement. 

### Détermination de la spécification

create_equation(
  equation_name = "eq_smpt_s11",
  formula="delta(1,log(smpt_s11))=e_cst+e_0*(log(lag(smpt_s11,-1))-e_lt2*log(lag(dfl_cf_s14,-1))-e_lt1)+e_1*delta(1,log(dfl_cf_s14))+delta(1,af_eq_smpt_s11)-e_0*lag(af_eq_smpt_s11,-1)",
  coefflist = c("e_cst","e_0","e_1","e_2","e_lt2","e_lt1"),
  endogenous = "smpt_s11"
)
estimation <- quick_estim(thor_equation = eq_smpt_s11 , database = dt_pf,
                          estim_start=as.Date(date_debut_estim),
                          estim_end=as.Date(date_fin_estim),
                          coeff_lt = c("e_lt1","e_lt2"),
                          const = T)
 

#Les coefficients sont significatifs mais le  R^2^  n'est pas satisfaisant. On décide d'ajouter  productivite  et une indicatrice :
 
 
create_equation(
  equation_name = "eq_smpt_s11",
  formula="delta(1,log(smpt_s11))=e_cst+e_0*(log(lag(smpt_s11,-1))-e_lt2*log(lag(dfl_cf_s14,-1))-e_lt3*dummy_crise_fi-e_lt1)+e_1*delta(1,log(dfl_cf_s14))+e_2*delta(1,log(productivite))+e_3*dummy_crise_fi+delta(1,af_eq_smpt_s11)-e_0*lag(af_eq_smpt_s11,-1)",
  coefflist = c("e_cst","e_0","e_1","e_2","e_3","e_lt1","e_lt2","e_lt3"),
  endogenous = "smpt_s11"
)
estimation <- quick_estim(thor_equation = eq_smpt_s11 , database = dt_pf,
                          estim_start=as.Date(date_debut_estim),
                          estim_end=as.Date(date_fin_estim),
                          coeff_lt = c("e_lt1","e_lt2","e_lt3"),
                          const = T)
 

#L'indicatrice et la productivité ne permettent pas d'améliorer la régression. Pour ce qui est de l'indicatrice, son rôle minime peut être du au fait que le déflateur de la consommation explique déjà le choc de 2008. Dans d'autres configuration la variable pourrait quand même être utile. En effet, avec des scénarios de déflateurs différents, nous avions conclu qu'il n'y avait pas eu de rattrapage total du niveau d'avant crise. Par conséquent il était utile d'inclure l'indicatrice dans la partie de long terme en plus de l'inclure dans la partie de court terme afin de gérer le choc de court terme. Nous laissons  productivite  dans le modèle à ce stade expérimentale. Il serait intéressant d'insérer le taux de chômage dans la spécification quand celui-ci sera disponible.

## Equation Emploi :  eq_emploi 

###  emploi 

 
ggplot(data = dt_pf1
       , aes(x =date, y=log(emploi))) +
  geom_line() +
  ggtitle(paste0("Evolution de l'emploi"))

a <- ur.df(log(dt_pf1$emploi), 
           lags = 6, 
           selectlags = "AIC", 
           type = "trend")
summary(a)
 

#Nous acceptons l'hypothèse nulle de non-stationarité dans un modèle avec constante et dérive temporelle.

###  pib 

#Nous avons déjà étudié la variable  pib  dans  eq_fbcf_s11 . Nous avions accepté l'hypothèse nulle de non-stationarité dans un modèle sans constante ni dérive temporelle.

### Regression de la relation de long terme

reg_emploi_lt1 <- lm(log(emploi) ~ log(pib), dt_pf1)
summary(reg_emploi_lt1)

#pib  est une variable explicative acceptable pour la partie de long terme. Graphiquement, nous avons pu voir une tendance commune, cependant les pertubations du pib biaisent le  R^2^ .

### Tests de cointégration

 
res1_emploi <- resid(reg_emploi_lt1)
plot(res1_emploi)
reg_emploi_res1 <-
  ur.df(res1_emploi,
        lags = 6,
        type = "none",
        selectlags = "AIC")
summary(reg_emploi_res1)
 
#L'hypothèse nulle d'absence de cointégration ne peut pas être rejetée. Ceci se confirme graphiquement.

### Détermination de la spécification
 
create_equation(
  equation_name = "eq_emploi",
  formula = "delta(1,log(emploi))=f_cst+f_0*(log(lag(emploi,-1))-f_lt2*log(lag(pib,-1))-f_lt1)+f_1*delta(1,log(pib))+delta(1,af_eq_emploi)-f_0*lag(af_eq_emploi,-1)",
  coefflist = c("f_cst", "f_0", "f_1", "f_2", "f_lt2", "f_lt1"),
  endogenous = "emploi"
)
estimation <-
  quick_estim(
    thor_equation = eq_emploi ,
    database = dt_pf,
    estim_start = as.Date(date_debut_estim),
    estim_end = as.Date(date_fin_estim),
    coeff_lt = c("f_lt1", "f_lt2"),
    const = T
  )

#La régression est satisfaisante malgré un faible  R^2^  (probablement du aux oscillations inexpliquées). On décide d'ajouter des retards de la variable  emploi  afin d'augmenter le  R^2^  : 
 
 
create_equation(
  equation_name = "eq_emploi",
  formula = "delta(1,log(emploi))=f_cst+f_0*(log(lag(emploi,-1))-f_lt2*log(lag(pib,-1))-f_lt1)+f_1*delta(1,log(lag(emploi,-1)))+f_2*delta(1,log(pib))+delta(1,af_eq_emploi)-f_0*lag(af_eq_emploi,-1)",
  coefflist = c("f_cst", "f_0", "f_1", "f_2", "f_3", "f_lt2", "f_lt1"),
  endogenous = "emploi"
)
estimation <-
  quick_estim(
    thor_equation = eq_emploi ,
    database = dt_pf,
    estim_start = as.Date(date_debut_estim),
    estim_end = as.Date(date_fin_estim),
    coeff_lt = c("f_lt1", "f_lt2"),
    const = T
  )
 
#Nous retenons cette spécification plutôt satisfaisante à ce stade. Avec  eq_fbcf_s11 , deux équations sont expliquées en majeure partie par  pib . Notons que grâce aux données de la CPS il serait peut-être possible de trouver les heures travaillées par emploi, variable explicative possiblement très intéressante dans cette spécification. 

## Equation Indice des prix à l'alimentation :  eq_ipc_alim 

###  ipc_alim 

 
ggplot(data = dt_pf1
       , aes(x =date, y=log(ipc_alim))) +
  geom_line() +
  ggtitle(paste0("Evolution de l'indice des prix à la consommation pour l'alimentation"))

a <- ur.df(log(dt_pf1$ipc_alim), 
           lags = 6, 
           selectlags = "AIC", 
           type = "trend")
summary(a)
b <- ur.df(log(dt_pf1$ipc_alim), 
           lags = 6, 
           selectlags = "AIC", 
           type = "drift")
summary(b)
 
#Nous ne pouvons pas accepter l'hypothèse nulle de non-stationarité dans un modèle avec constante sans dérive temporelle. Nous pouvons en effet la rejeter au seuil de 10%. la valeur du test est très proche du seuil de 5%.

###  brent 

#Attention :  brent  n'est surement pas l'indicateur le plus approprié à l'économie Polynésienne. Il s'agirait de choisir un indicateur du prix du baril américain.

  
ggplot(data = dt_pf1
       , aes(x =date, y=log(brent))) +
  geom_line() +
  ggtitle(paste0("Evolution du prix du baril"))

a <- ur.df(log(dt_pf1$brent), 
           lags = 6, 
           selectlags = "AIC", 
           type = "trend")
summary(a)
b <- ur.df(log(dt_pf1$brent), 
           lags = 6, 
           selectlags = "AIC", 
           type = "drift")
summary(b)
c <- ur.df(log(dt_pf1$brent), 
           lags = 6, 
           selectlags = "AIC", 
           type = "none")
summary(c)
 

#Nous acceptons l'hypothèse nulle de non-stationarité dans un modèle sans constante ni dérive temporelle.

###  tcen 

#Le taux de change nominal peut expliquer les variations de l'indice des prix à la consommation, surtout dans une économie basée sur l'importation.

  
ggplot(data = dt_pf1
       , aes(x =date, y=log(tcen))) +
  geom_line() +
  ggtitle(paste0("Evolution du prix du taux de change nominal"))

a <- ur.df(log(dt_pf1$tcen), 
           lags = 6, 
           selectlags = "AIC", 
           type = "trend")
summary(a)
 

#Nous rejetons l'hypothèse nulle de non-stationarité dans un modèle avec constante et dérive temporelle.

### Regression de la relation de long terme


 
reg_ipc_alim_lt1 <- lm(log(ipc_alim) ~ log(brent), dt_pf1)
summary(reg_ipc_alim_lt1)

reg_ipc_alim_lt2 <- lm(log(ipc_alim) ~ log(tcen), dt_pf1)
summary(reg_ipc_alim_lt2)

reg_ipc_alim_lt3 <-
  lm(log(ipc_alim) ~ log(brent) + log(tcen), dt_pf1)
summary(reg_ipc_alim_lt3)
 

#La combinaison des deux variables permet d'expliquer la moitié de la variance de la variable dépendante. Les coefficients sont de plus significatifs. Néanmoins le signe du coefficient de  brent  n'est pas cohérent. De plus, il s'agirait de revoir le sens du taux de change afin de déterminer la cohérence du coefficient devant  tcen .

### Tests de cointégration

 
res1_ipc_alim <- resid(reg_ipc_alim_lt1)
reg_ipc_alim_res1 <-
  ur.df(res1_ipc_alim,
        lags = 6,
        type = "none",
        selectlags = "AIC")
summary(reg_ipc_alim_res1)
plot(res1_ipc_alim)

res2_ipc_alim <- resid(reg_ipc_alim_lt2)
reg_ipc_alim_res2 <-
  ur.df(res2_ipc_alim,
        lags = 6,
        type = "none",
        selectlags = "AIC")
summary(reg_ipc_alim_res2)
plot(res2_ipc_alim)

res3_ipc_alim <- resid(reg_ipc_alim_lt3)
reg_ipc_alim_res3 <-
  ur.df(res3_ipc_alim,
        lags = 6,
        type = "none",
        selectlags = "AIC")
summary(reg_ipc_alim_res3)
plot(res3_ipc_alim)
 

#Individuellement,  brent  n'est pas cointérgré avec  ipc_alim . Cependant l'hypothèse nulle d'absence de cointégration est rejetée pour les 2 autres spécifications.

### Détermination de la spécification


 
create_equation(
  equation_name = "eq_ipc_alim",
  formula = "delta(1,log(ipc_alim))=g_cst+g_0*(log(lag(ipc_alim,-1))-g_lt1-g_lt2*log(lag(brent,-1))-g_lt3*log(lag(tcen,-1)))+g_1*delta(1,brent)+g_2*delta(1,tcen)+delta(1,af_eq_ipc_alim)-g_0*lag(af_eq_ipc_alim,-1)",
  coefflist = c("g_cst", "g_0", "g_1", "g_2", "g_3", "g_lt2", "g_lt1", "g_lt3"),
  endogenous = "ipc_alim"
)

estimation <-
  quick_estim(
    thor_equation = eq_ipc_alim ,
    database = dt_pf,
    estim_start = as.Date(date_debut_estim),
    estim_end = as.Date(date_fin_estim),
    coeff_lt = c("g_lt1", "g_lt2", "g_lt3"),
    const = T
  )

 

#La régression n'est pas satisfaisante malgré un faible  R^2^ . Même si la relation de long terme est très bonne, nous ne pouvons pas expliquer les relations de court terme. Une solution possible est de ne pas passer par un MCE ou de ne pas inclure cette équation. Nous ne retenons pas cet MCE à ce stade. Afin de l'inclure, il faudrait trouver des indicateurs propre à la Polynésie, c'est à dire des indicateurs avec des impacts direct. Nous la laissons dans le modèle et dans le programme afin de se donner la possibilité de résoudre le problème.

## Equation Indice des prix à l'énergie :  eq_ipc_nrj 

###  ipc_nrj 

 
ggplot(data = dt_pf1
       , aes(x =date, y=log(ipc_nrj))) +
  geom_line() +
  ggtitle(paste0("Evolution de l'indice des prix à la consommation pour l'énergie"))

a <- ur.df(log(dt_pf1$ipc_nrj), 
           lags = 6, 
           selectlags = "AIC", 
           type = "trend")
summary(a)
b <- ur.df(log(dt_pf1$ipc_nrj), 
           lags = 6, 
           selectlags = "AIC", 
           type = "drift")
summary(b)
c <- ur.df(log(dt_pf1$ipc_nrj), 
           lags = 6, 
           selectlags = "AIC", 
           type = "none")
summary(c)
 

#Nous acceptons l'hypothèse nulle de non-stationarité dans un modèle sans constante ni dérive temporelle.

###  petrole 

#Nous étudions maintenant la variable  petrole  directement issu du site de l'ISPF afin d'obtenir des données davantage en lien avec l'économie Polynésienne. Le prix à la pompe étant fixé en Polynésie, il est en effet difficile de se baser sur des indicateurs comme le brent.

  
ggplot(data = dt_pf1
       , aes(x =date, y=log(petrole))) +
  geom_line() +
  ggtitle(paste0("Evolution du prix du baril"))

a <- ur.df(log(dt_pf1$petrole), 
           lags = 6, 
           selectlags = "AIC", 
           type = "trend")
summary(a)
b <- ur.df(log(dt_pf1$petrole), 
           lags = 6, 
           selectlags = "AIC", 
           type = "drift")
summary(b)
c <- ur.df(log(dt_pf1$petrole), 
           lags = 6, 
           selectlags = "AIC", 
           type = "none")
summary(c)
 

#Nous acceptons l'hypothèse nulle de non-stationarité dans un modèle sans constante ni dérive temporelle.


### Regression de la relation de long terme


 
reg_ipc_nrj_lt1 <- lm(log(ipc_nrj) ~ log(petrole), dt_pf1)
summary(reg_ipc_nrj_lt1)

reg_ipc_nrj_lt2 <- lm(log(ipc_nrj) ~ log(brent), dt_pf1)
summary(reg_ipc_nrj_lt2)

reg_ipc_nrj_lt3 <- lm(log(ipc_nrj) ~ log(petrole) + log(brent), dt_pf1)
summary(reg_ipc_nrj_lt3)
 

#La combination des deux variables permet d'expliquer 3/4 de la variance de la variable dépendante. Les coefficients sont de plus significatif et du signe cohérent. Les deux variables se complètent car elle permettent d'avoir deux niveaux différents d'analyse des chocs et des variations.

### Tests de cointégration

 
res1_ipc_nrj <- resid(reg_ipc_nrj_lt1)
reg_ipc_nrj_res1 <-
  ur.df(res1_ipc_nrj,
        lags = 6,
        type = "none",
        selectlags = "AIC")
summary(reg_ipc_nrj_res1)
plot(res1_ipc_nrj)

res2_ipc_nrj <- resid(reg_ipc_nrj_lt2)
reg_ipc_nrj_res2 <-
  ur.df(res2_ipc_nrj,
        lags = 6,
        type = "none",
        selectlags = "AIC")
summary(reg_ipc_nrj_res2)
plot(res2_ipc_nrj)

res3_ipc_nrj <- resid(reg_ipc_nrj_lt3)
reg_ipc_nrj_res3 <-
  ur.df(res3_ipc_nrj,
        lags = 6,
        type = "none",
        selectlags = "AIC")
summary(reg_ipc_nrj_res3)
plot(res3_ipc_nrj)
 

#Individuellement, les variables ne sont pas cointégrées avec  ipc_nrj . Cependant, l'hypothèse nulle d'absence de cointégration est rejetée pour la combinaison des deux variables explicatives.

### Détermination de la spécification


 
create_equation(
  equation_name = "eq_ipc_nrj",
  formula = "delta(1,log(ipc_nrj))=h_cst+h_0*(log(lag(ipc_nrj,-1))-h_lt1-h_lt2*log(lag(petrole,-1))-h_lt3*log(lag(brent,-1)))+h_1*delta(1,log(petrole))+h_2*delta(1,log(brent))+delta(1,af_eq_ipc_nrj)-h_0*lag(af_eq_ipc_nrj,-1)",
  coefflist = c("h_cst", "h_0", "h_1", "h_2", "h_3", "h_lt2", "h_lt1", "h_lt3"),
  endogenous = "ipc_nrj"
)

estimation <-
  quick_estim(
    thor_equation = eq_ipc_nrj ,
    database = dt_pf,
    estim_start = as.Date(date_debut_estim),
    estim_end = as.Date(date_fin_estim),
    coeff_lt = c("h_lt1", "h_lt2", "h_lt3"),
    const = T
  )
 
#La spécification est très satisfaisante.


## Equation Indice des prix totaux :  eq_ipc_tot 

###  ipc_tot 

 
ggplot(data = dt_pf1
       , aes(x =date, y=log(ipc_tot))) +
  geom_line() +
  ggtitle(paste0("Evolution de l'indice des prix à la consommation"))

a <- ur.df(log(dt_pf1$ipc_tot), 
           lags = 6, 
           selectlags = "AIC", 
           type = "trend")
summary(a)
b <- ur.df(log(dt_pf1$ipc_tot), 
           lags = 6, 
           selectlags = "AIC", 
           type = "drift")
summary(b)
 

#Nous rejetons l'hypothèse nulle de non-stationarité dans un modèle avec constante sans dérive temporelle.

###  csu_s11 


ggplot(data = dt_pf1
       , aes(x =date, y=log(csu_s11))) +
  geom_line() +
  ggtitle(paste0("Evolution du coût salariale unitaire"))

a <- ur.df(log(dt_pf1$csu_s11), 
           lags = 6, 
           selectlags = "AIC", 
           type = "trend")
summary(a)
 

#Nous rejetons l'hypothèse nulle de non-stationarité dans un modèle avec constante et dérive temporelle. 


### Regression de la relation de long terme


 
reg_ipc_tot_lt1 <- lm(log(ipc_tot) ~ log(csu_s11), dt_pf1)
summary(reg_ipc_tot_lt1)
 

#Le coût salarié unitaire explique plutôt bien l'indice des prix à la consommation en niveau.

### Tests de cointégration

 
res1_ipc_tot <- resid(reg_ipc_tot_lt1)
plot(res1_ipc_tot)
reg_ipc_tot_res1 <-
  ur.df(res1_ipc_tot,
        lags = 6,
        type = "none",
        selectlags = "AIC")
summary(reg_ipc_tot_res1)

 

#Nous rejetons l'hypothèse nulle d'absence de cointégration uniquement au seuil de 10 %. Graphiquement les résidus ont l'air de suivre une tendance. 

### Détermination de la spécification


 
create_equation(
  equation_name = "eq_ipc_tot",
  formula = "delta(1,log(ipc_tot))=i_cst+i_0*(log(lag(ipc_tot,-1))-i_lt1-i_lt2*log(lag(csu_s11,-1)))+i_1*delta(1,log(csu_s11))+delta(1,af_eq_ipc_tot)-i_0*lag(af_eq_ipc_tot,-1)",
  coefflist = c("i_cst", "i_0", "i_1", "i_2", "i_3", "i_lt2", "i_lt1", "i_lt3"),
  endogenous = "ipc_tot"
)

estimation <-
  quick_estim(
    thor_equation = eq_ipc_tot ,
    database = dt_pf,
    estim_start = as.Date(date_debut_estim),
    estim_end = as.Date(date_fin_estim),
    coeff_lt = c("i_lt1", "i_lt2"),
    const = T
  )
 

#La spécification est plutôt satisfaisante mais on souhaite améliorer l'explication des dynamiques de court terme grâce à  ipc_alim  et  ipc_nrj .

 
create_equation(
  equation_name = "eq_ipc_tot",
  formula = "delta(1,log(ipc_tot))=i_cst+i_0*(log(lag(ipc_tot,-1))-i_lt1-i_lt2*log(lag(csu_s11,-1)))+i_1*delta(1,log(csu_s11))+i_2*delta(1,log(ipc_nrj))+i_3*delta(1,log(ipc_alim))+delta(1,af_eq_ipc_tot)-i_0*lag(af_eq_ipc_tot,-1)",
  coefflist = c("i_cst", "i_0", "i_1", "i_2", "i_3", "i_lt2", "i_lt1", "i_lt3"),
  endogenous = "ipc_tot"
)

estimation <-
  quick_estim(
    thor_equation = eq_ipc_tot ,
    database = dt_pf,
    estim_start = as.Date(date_debut_estim),
    estim_end = as.Date(date_fin_estim),
    coeff_lt = c("i_lt1", "i_lt2"),
    const = T
  )
 

#Cela a pour conséquence de diminuer la significativité du coefficient de  csu_s11 . cependant l'inclusion de ces dynamiques de court terme permet d'expliquer une très large partie de la variance de  ipc_tot . Nous pouvons questionner cet ajout étant donné que  ipc_tot  est comptablement déterminé sur la base de  ipc_alim  et  ipc_nrj . Il est toutefois intéressant de les garder si ces deux dernières variables sont endogénéisées. En effet cela permet de laisser de la liberté au modèle tout en gardant la logique comptable au sein même des équations économétriques pour l'exercice de prévision.

## Equation importations :  eq_m 

###  m 

#A cause de l'investissement d'avions, nous allons travailler ici avec  dt_pf2  pour exclure 2018 de notre période d'analyse tout comme pour  eq_fbcf_s11 .

 
ggplot(data = dt_pf2
       , aes(x =date, y=log(m))) +
  geom_line() +
  ggtitle(paste0("Evolution des importations"))

a <- ur.df(log(dt_pf2$m), 
           lags = 6, 
           selectlags = "AIC", 
           type = "trend")
summary(a)
b <- ur.df(log(dt_pf2$m), 
           lags = 6, 
           selectlags = "AIC", 
           type = "drift")
summary(b)
c <- ur.df(log(dt_pf2$m), 
           lags = 6, 
           selectlags = "AIC", 
           type = "none")
summary(c)
 

#Nous acceptons l'hypothèse nulle de non-stationarité dans un modèle sans constante ni dérive temporelle.

###  df 

#La demande finale, i.e. tout les postes de la demande du PIB sauf les importations, est censée être un bon indicateur de la demande de resssources extérieure.

  
ggplot(data = dt_pf2
       , aes(x =date, y=log(df))) +
  geom_line() +
  ggtitle(paste0("Evolution de la demande finale"))

a <- ur.df(log(dt_pf2$df), 
           lags = 6, 
           selectlags = "AIC", 
           type = "trend")
summary(a)
b <- ur.df(log(dt_pf2$df), 
           lags = 6, 
           selectlags = "AIC", 
           type = "drift")
summary(b)
c <- ur.df(log(dt_pf2$df), 
           lags = 6, 
           selectlags = "AIC", 
           type = "none")
summary(c)
 

#Nous acceptons l'hypothèse nulle de non-stationarité dans un modèle sans constante ni dérive temporelle.


### Regression de la relation de long terme


 
reg_m_lt1 <- lm(log(m) ~ log(df), dt_pf2)
summary(reg_m_lt1)
 

#La régression est acceptable ( R^2^  faible à cause des ocsillations).

### Tests de cointégration

 
res1_m <- resid(reg_m_lt1)
plot(res1_m)
reg_m_res1 <-
  ur.df(res1_m,
        lags = 6,
        type = "none",
        selectlags = "AIC")
summary(reg_m_res1)

 

#Nous rejetons l'hypothèse nulle d'absence de cointégration.

### Détermination de la spécification

#Nous estimons sans l'année 2018.

 
create_equation(
  equation_name = "eq_m",
  formula = "delta(1,log(m))=j_cst+j_0*(log(lag(m,-1))-j_lt1-j_lt2*log(lag(df,-1)))+j_1*delta(1,log(df))+delta(1,af_eq_m)-j_0*lag(af_eq_m,-1)",
  coefflist = c("j_cst", "j_0", "j_1", "j_2", "j_3", "j_lt2", "j_lt1", "j_lt3"),
  endogenous = "m"
)

estimation <-
  quick_estim(
    thor_equation = eq_m ,
    database = dt_pf,
    estim_start = as.Date(date_debut_estim),
    estim_end = as.Date("2017-12-31"),
    coeff_lt = c("j_lt1", "j_lt2"),
    const = T
  )
 

#La spécification est plutôt satisfaisante mais on souhaite l'améliorer en ajoutant  tcen , le taux de change, et  petrole .

 
create_equation(
  equation_name = "eq_m",
  formula = "delta(1,log(m))=j_cst+j_0*(log(lag(m,-1))-j_lt1-j_lt2*log(lag(df,-1)))+j_1*delta(1,log(df))+j_2*delta(1,log(tcen))+delta(1,af_eq_m)-j_0*lag(af_eq_m,-1)",
  coefflist = c("j_cst", "j_0", "j_1", "j_2", "j_3", "j_lt2", "j_lt1", "j_lt3"),
  endogenous = "m"
)

estimation <-
  quick_estim(
    thor_equation = eq_m ,
    database = dt_pf,
    estim_start = as.Date(date_debut_estim),
    estim_end = as.Date("2017-12-31"),
    coeff_lt = c("j_lt1", "j_lt2"),
    const = T
  )
 

#Nous gardons cette seconde spécification car elle augmente le  R^2^  ajusté même si le coefficient de  tcen  n'est pas significatif.

#Pour comparer les régressions et mettre en page, la fonction stargazer est intéressante

#Par exemple pour eq_ipc_nrj

stargazer(reg_ipc_nrj_lt1,reg_ipc_nrj_lt2,reg_ipc_nrj_lt3,
          title="Regression Results",
          align=TRUE,
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          type="text")

#code latex pour présentation

stargazer(reg_ipc_nrj_lt1,reg_ipc_nrj_lt2,reg_ipc_nrj_lt3,
          title="Regression Results",
          align=TRUE,
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          type="latex")

#Autres fonction pour test ADF

#ADF

library(aTSA)
aTSA::adf.test(dt_pf$rs_s11_v, nlag = NULL, output = TRUE)

dt_pf1 <- copy(dt_pf)
setDT(dt_pf1)
dt_pf1[,year:= (substr(as.character(date),1,4))]
length(names(dt_pf1))

dt_pfa <- dt_pf1[,year:= (substr(as.character(date),1,4))][,lapply(.SD,sum),.SDcols=names(dt_pf1[,!c("date","year")]), by="year"]

library(tseries)
TestADF <- map(dt_pfa[year%in%horizon_estim_a,2:ncol(dt_pfa)], ~tseries::adf.test(.x))
map(names(TestADF), ~TestADF[[.x]][["alternative"]])
