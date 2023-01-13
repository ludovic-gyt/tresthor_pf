source("src/functions.R")


#Initialisation des périodes


date_debut_obs <- "2005-03-31"
date_fin_obs    <- "2016-12-31"
date_debut_estim <- "2006-03-31"
date_fin_estim <- "2016-12-31"
date_debut_prev <- "2017-03-31"
date_fin_prev   <- "2018-12-31"

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


horizon_obs <-
  (seq.Date(
    from = as.Date(date_debut_obs) + 1,
    to = as.Date(date_fin_obs) + 1,
    by = "quarter",
  ) - 1) %>%
  as.character(.)



#Retraitement base de donnée


#dt_pib_tresthor <- read_csv("input/dt_pib_tresthor.csv")
dt_pf <- fread(file = "input/dt_pib_tresthor.csv")


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
  "PIB_E"
) := NULL] #supression variables inutiles

dt_pf[, x := X_B + X_S][, m := M_B + M_S][, rs := RS_S11 + RS_S13] #Ajout variables

setnames(dt_pf, "PIB_R", "PIB")
setnames(dt_pf, names(dt_pf), str_to_lower(names(dt_pf)))
setnames(dt_pf, names(dt_pf[, !c("date")]), paste0(names(dt_pf[, !c("date")]), "_v"))

#Hypothèse hors modèle (ceci doit être remplacé par les vraies données)

dt_pf[, c("dfl_fbcf_s11",
          "dfl_fbcf_s13",
          "dfl_fbcf_s14",
          "dfl_m",
          "dfl_x",
          "dfl_cf_s13") := 1][, pmc := 0.8][,ajust_pension := 0.1]


#Définition de l'index d'horizon

horizon_prev_index <- which(dt_pf$date%in%as.Date(horizon_prev))
horizon_estim_index <- which(dt_pf$date%in%as.Date(horizon_estim))
horizon_obs_index <- which(dt_pf$date%in%as.Date(horizon_obs))
horizon_total_index <- 1:nrow(dt_pf)

#Import Ipc


dt_pf[, ipc_tot := fread("input/dt_x12_econ1016.csv")[SERIE == "IND_11" &
                                                        DATE >= date_debut_obs &
                                                        DATE <= date_fin_prev &
                                                        ID_TYPE == "obs",
                                                      value]][,
                                                              ipc_tot_vt := vt(ipc_tot)]


#Supression donnée 2017&2018 pour test de prévision

#dt_pf[which(dt_pf$date>=date_debut_prev),!(c("date"))] <- NA 

dt_pf[horizon_prev_index, setdiff(names(dt_pf),"date"):=NA] 

#2eme méthode

# for(j in setdiff(names(dt_pf),"date")) dt_pf[horizon_prev_index, (j):=NA]

#3eme méthode

# monVecteur <- setdiff(names(dt_pf),"date")
# dt_pf[horizon_prev_index,(monVecteur):=lapply(.SD, function(x) NA), .SDcols=monVecteur]

#Travail du préviosioniste : Hypothèses variables exo sur la période de prévision

# dt_pf <- as.data.frame(dt_pf)
# 
# rownames(dt_pf) <- dt_pf$date

ggplot(data = dt_pf, aes(x =date, y=fbcf_s11_v)) +
  geom_line() +
  ggtitle(paste0("Evolution du pib ")) #graph pour déterminer la tendance en hypothèse


series_croissantes <- 
  c("x_v", 
    "cf_s13_v", 
    "rs_v", 
    "rs_s11_v"
    ) #série avec tendance croissante

series_stables <-
  c(
    "dfl_fbcf_s11",
    "dfl_fbcf_s13",
    "dfl_fbcf_s14",
    "dfl_m",
    "dfl_x",
    "fbcf_s11_v",
    "fbcf_s13_v",
    "fbcf_s14_v",
    "ipc_tot",
    "ipc_tot_vt",
    "m_v",
    "vs_v",
    "cf_s14_v",
    "pmc",
    "dfl_cf_s13",
    "ajust_pension"
  ) #Séries plutôt stable



# dt_pf[horizon_prev_index,.SD, .SDcols =series_stables] <- dt_pf[date_fin_obs,c(series_stables)] 

# dt_update <- data.table()
# for (i in 1:8)
#   dt_update <- rbind(dt_update, dt_pf[48])
# dt_update[, date := as.Date(horizon_prev)]
# for (j in series_stables)
#   dt_pf[dt_update, (j) := get(paste0("i.", j)), on = .(date)]

dt_pf[horizon_prev_index, c(series_stables) := dt_pf[rep(horizon_prev_index[1] -
                                                           1, length(horizon_prev_index)), c(series_stables), with = F]]

# dt_pf[horizon_prev_index,dfl_fbcf_s11:=dt_pf[date==date_fin_obs,dfl_fbcf_s11]][
#   horizon_prev_index,dfl_fbcf_s13:=dt_pf[date==date_fin_obs,dfl_fbcf_s13]][
#     horizon_prev_index,dfl_fbcf_s14:=dt_pf[date==date_fin_obs,dfl_fbcf_s14]][
#       horizon_prev_index,dfl_m:=dt_pf[date==date_fin_obs,dfl_m]][
#         horizon_prev_index,dfl_x:=dt_pf[date==date_fin_obs,dfl_x]][
#           horizon_prev_index,fbcf_s11_v:=dt_pf[date==date_fin_obs,fbcf_s11_v]][
#             horizon_prev_index,fbcf_s13_v:=dt_pf[date==date_fin_obs,fbcf_s13_v]][
#               horizon_prev_index,fbcf_s14_v:=dt_pf[date==date_fin_obs,fbcf_s14_v]][
#                 horizon_prev_index,ipc_tot:=dt_pf[date==date_fin_obs,ipc_tot]][
#                   horizon_prev_index,ipc_tot_vt:=dt_pf[date==date_fin_obs,ipc_tot_vt]][
#                     horizon_prev_index,m_v:=dt_pf[date==date_fin_obs,m_v]][
#                       horizon_prev_index,vs_v:=dt_pf[date==date_fin_obs,vs_v]][
#                         horizon_prev_index,cf_s14_v:=dt_pf[date==date_fin_obs,cf_s14_v]][
#                           horizon_prev_index,pmc:=dt_pf[date==date_fin_obs,pmc]][
#                             horizon_prev_index,dfl_cf_s13:=dt_pf[date==date_fin_obs,dfl_cf_s13]][
#                               horizon_prev_index,ajust_pension:=dt_pf[date==date_fin_obs,ajust_pension]]
# 


 

# n0  <-  horizon_prev_index[1] - 4
# n1  <-  horizon_prev_index[1] - 1
 # for (s in series_croissantes) {
 #   x0 <- dt_pf[n0, c(s),with=F]
 #   x1 <- dt_pf[n1, c(s),with=F]
 #   var <- (x1 / x0) ^ (1 / (n1 - n0)) - 1
 #   for (h in horizon_prev_index) {
 #     dt_pf[h, (s):=dt_pf[h-1, c(s),with=F]*(1 + var)]
 #   }
 # }


tx_variation <-
  (dt_pf[horizon_prev_index[1] - 1, c(series_croissantes), with = F] /dt_pf[horizon_prev_index[1] - 4, c(series_croissantes), with = F])^ (1 / (horizon_prev_index[1] - 1 - horizon_prev_index[1] - 4)) - 1

for (h in horizon_prev_index) {
  dt_pf[h, (series_croissantes) := dt_pf[h - 1, c(series_croissantes), with =
                                           F] * (1 + tx_variation)]
}
rm(tx_variation)

#création modèle

create_model("PF_rdb_mod", model_source = "C:/Users/ludovicg/Documents/R/tresthor-test/input/tresthor_base_rdb_mce.txt")


#Inclusion des variables du modèle manquantes dans le data frame

dt_pf[,setdiff(names(PF_rdb_mod@var_map),names(dt_pf)):=as.numeric(NA)]

#Initialisation des variables endogènes

set.seed(123)

dt_pf[, m := m_v / dfl_m ][
  ,x := x_v / dfl_x ][
    , fbcf_s11 := fbcf_s11_v/ dfl_fbcf_s11][
      ,fbcf_s13 := fbcf_s13_v / dfl_fbcf_s13 ][
        ,fbcf_s14:=fbcf_s14_v / dfl_fbcf_s14 ][
          ,fbcf_v:=fbcf_s11_v+fbcf_s13_v+fbcf_s14_v][
            ,dihs_v:=cf_v+fbcf_v][
              ,di_v:=dihs_v+vs_v][
                ,df_v:=di_v+x_v][
                  ,pib_v:=cf_v+fbcf_v+x_v-m_v+vs_v][
                    ,tx_inv:=fbcf_v/pib_v][
                      ,fbcf:=fbcf_s11+fbcf_s13+fbcf_s14][
                         ,dfl_cf_s14:=1][
                           ,dfl_cf_s14:=lag(dfl_cf_s14,1)*(1+ipc_tot_vt)][
                             ,cf_s13:=cf_s13_v/dfl_cf_s13][
                               ,cf_s14:=cf_s14_v/dfl_cf_s14][
                                 ,cf_v:=cf_s13_v+cf_s14_v][
                                   ,cf:=cf_s13+cf_s14][
                                     ,dihs:=dihs_v][
                                       ,dihs:=lag(dihs,1)*(((cf/lag(cf,1)-1)*lag(cf_v,1)+(fbcf/lag(fbcf,1)-1)*lag(fbcf_v,1))/lag(dihs_v,1)+1)][
                                          ,cont_cf_s13:=(cf_s13/lag(cf_s13,1)-1)*(lag(cf_s13_v,1)/lag(pib_v,1))][
                                            ,cont_cf_s14:=(cf_s14/lag(cf_s14,1)-1)*(lag(cf_s14_v,1)/lag(pib_v,1))][
                                              ,cont_cf:=cont_cf_s13+cont_cf_s14][
                                                ,cont_fbcf_s11:=(fbcf_s11/lag(fbcf_s11,1)-1)*(lag(fbcf_s11_v,1)/lag(pib_v,1))][
                                                  ,cont_fbcf_s13:=(fbcf_s13/lag(fbcf_s13,1)-1)*(lag(fbcf_s13_v,1)/lag(pib_v,1))][
                                                    ,cont_fbcf_s14:=(fbcf_s14/lag(fbcf_s14,1)-1)*(lag(fbcf_s14_v,1)/lag(pib_v,1))][
                                                      ,cont_fbcf:=cont_fbcf_s11+cont_fbcf_s13+cont_fbcf_s14][
                                                        ,cont_dihs:=cont_cf+cont_fbcf][
                                                          ,cont_vs:=(vs_v-lag(vs_v,1))/(lag(pib_v,1))][
                                                            ,cont_di:=cont_dihs+cont_vs][
                                                              ,di:=di_v][
                                                                ,di:=(cont_di/(lag(di_v,1)/lag(pib_v,1))+1)*lag(di,1)][
                                                                  ,cont_m:=-(m/lag(m,1)-1)*(lag(m_v,1)/lag(pib_v,1))][
                                                                    ,cont_x:=(x/lag(x,1)-1)*(lag(x_v,1)/lag(pib_v,1))][
                                                                      ,cont_df:=cont_di+cont_x][
                                                                        ,cont_xm:=cont_x+cont_m][
                                                                          ,df:=di+x][
                                                                            ,df:=lag(df,1)*(((di/lag(di,1)-1)*lag(di_v,1)+(x/lag(x,1)-1)*lag(x_v,1))/lag(df_v,1)+1)][
                                                                              ,dt_pib:=cont_cf+cont_fbcf+cont_x+cont_m+cont_vs][
                                                                                ,pib:=pib_v][
                                                                                  ,pib:=lag(pib,1)*(dt_pib+1)][
                                                                                    ,dfl_pib:=pib_v/pib][
                                                                                      ,rdb_s14_v:=cf_s14_v/pmc][
                                                                                        ,pertu:=rnorm(1:nrow(dt_pf),0,0.1)][
                                                                                          ,rdb_s14_v:=rdb_s14_v+rdb_s14_v*pertu][
                                                                                            ,epargne:=(rdb_s14_v)-cf_s14_v][
                                                                                              ,tx_epargne:=epargne/rdb_s14_v][
                                                                                                ,pa:=rdb_s14_v/dfl_cf_s14]
# setDT(dt_pf)
# set.seed(123)
# dt_pf[,pertu:=rnorm(1:nrow(dt_pf),0,0.1)][ ,pa:=rdb_s14_v/dfl_cf_s14][,pa:=pa+pa*pertu]
# dt_pf[,pa]

#dt_pf[,pertu1:=rnorm(1:nrow(dt_pf),0,0.1)][,fbcf_s11:=fbcf_s11+fbcf_s11*pertu]

# ggplot(data = dt_pf, aes(x =date, y=cf_s14_v)) +
#   geom_line() +
#   ggtitle(paste0("Evolution de l'I ")) 
# 
# ggplot(data = dt_pf, aes(x =date, y=cf_s14_v)) +
#   geom_line() +
#   ggtitle(paste0("Evolution de l'I "))


#Info equation


info_equations <- list(
  
  eq_rdb_s14_v=
    
    list(endogenous_name="rdb_s14_v",
         
         residual_name="af_eq_rdb_s14_v",
         
         coeff_lt=c("e_lt1","e_lt2"),
         
         estim_start=as.Date("2006-03-31"),
         
         estim_end=as.Date("2016-12-31"),
         
         const=T)
  ,
   eq_fbcf_s11=
     
     list(endogenous_name="fbcf_s11",
          
          residual_name="af_eq_fbcf_s11",
          
          coeff_lt=c("j_lt1","j_lt2"),           
          
          estim_start=as.Date("2006-03-31"),
          
          estim_end=as.Date("2016-12-31"),
          
          const=T)
   ,
  eq_cf_s14=

    list(endogenous_name="cf_s14",

         residual_name="af_eq_cf_s14",

         coeff_lt=c("i_lt1","i_lt2"),

         estim_start=as.Date("2006-06-30"),

         estim_end=as.Date("2016-12-31"),

         const=T)
)

dt_pf[,paste0("afusr_",names(info_equations)):=0]


dt_pf[,(paste0("af_",names(info_equations))):=lapply(.SD, function(x) cumsum(x)), .SDcols=paste0("afusr_",names(info_equations))]




#Estimation

setDF(dt_pf)

dt_pf <- quick_estim_all(info_equations,PF_rdb_mod,dt_pf,"date")

# create_equation(
#   equation_name = "eq_cf_s14",
#   formula="delta(1,log(cf_s14))=i_cst+i_0*(log(lag(cf_s14,1))-i_lt2*log(lag(pa,1))-i_lt1)+i_1*delta(1,log(pa))+delta(1,af_eq_cf_s14)-i_0*lag(af_eq_cf_s14,1)",
#   coefflist = c("i_cst","i_0","i_1","i_lt2","i_lt1"),
#   endogenous = "cf_s14"
# )
# 
# 
# dt_pf1 <- quick_estim(thor_equation = eq_cf_s14 , database = dt_pf,
#                       estim_start = as.Date("2006-06-30"),
#                       estim_end = as.Date(date_fin_obs),
#                       coeff_lt = c("i_lt1","i_lt2"),
#                       const = T)



#ADF

# library(aTSA)
# aTSA::adf.test(dt_pf$rs_s11_v, nlag = NULL, output = TRUE)
# 
# dt_pf1 <- copy(dt_pf)
# setDT(dt_pf1)
# dt_pf1[,year:= (substr(as.character(date),1,4))]
# length(names(dt_pf1))
# 
# dt_pfa <- dt_pf1[,year:= (substr(as.character(date),1,4))][,lapply(.SD,sum),.SDcols=names(dt_pf1[,!c("date","year")]), by="year"]
# 
# library(tseries)
# TestADF <- map(dt_pfa[year%in%horizon_estim_a,2:ncol(dt_pfa)], ~tseries::adf.test(.x))
# map(names(TestADF), ~TestADF[[.x]][["alternative"]])


#Fonction de vérification

 
 #data_model_checks(thor_model=PF_rdb_mod,database=dt_pf,quiet=F)


 # horizon_test <-
 #   (seq.Date(
 #     as.Date("2006-04-01"),
 #     to = as.Date("2017-01-01"),
 #     by = "quarter"
 #   ) - 1) %>%
 #   as.Date(.)
 # 
 # time_solver_test_run(
 #   PF_rdb_mod,
 #   database = dt_pf,
 #   index_time = "date",
 #   times = as.Date(horizon_test)
 # )
 
 # na_report_variables_times(
 #   dt_pf,
 #   times = as.Date(horizon_test),
 #   variables=names(dt_pf),
 #   index_time = "date"
 # )

donness_recalculee <- thor_solver(
  PF_rdb_mod,
  first_period = as.Date("2006-03-31"),
  last_period = as.Date("2016-12-31"),
  database = dt_pf,
  index_time = "date"
)

 
 
# create_equation("eq_fbcf_s11",
#                 formula = PF_rdb_mod@equation_list[["equation"]][[39]],
#                 endogenous = "fbcf_s11")
#  
# solution <- thor_equation_solver(
#   equation = eq_fbcf_s11,
#   first_period = as.Date("2006-03-31"),
#   last_period = as.Date("2016-12-31"),
#   database = dt_pf,
#   index_time = "date"
# )
#  
#  create_equation(
#    equation_name = "eq_cf_s14",
#    formula="delta(1,log(cf_s14))=i_cst+i_0*(log(lag(cf_s14,1))-i_lt2*log(lag(pa,1))-i_lt1)+i_1*delta(1,log(pa))+delta(1,af_eq_cf_s14)-i_0*lag(af_eq_cf_s14,1)",
#    coefflist = c("i_cst","i_0","i_1","i_lt2","i_lt1"),
#    endogenous = "cf_s14"
#  )
#  
#  solution1 <- thor_equation_solver(
#    equation = eq_cf_s14,
#    first_period = as.Date("2006-03-31"),
#    last_period = as.Date("2016-12-31"),
#    database = dt_pf,
#    index_time = "date"
#  )



#Simulation


# simul_data <- simulate_equation(eq_rdb_s14_v, database = donness_recalculee, 
#                                 start_sim = as.Date("2006-03-31"),
#                                 end_sim = as.Date(date_fin_obs),
#                                 index_time = "date",
#                                 residual_var="af_eq_rdb_s14_v")
# 
# dt_pf$date <- as.IDate(dt_pf$date)
# 
# graph_sim_obs(simul_data , start_plot = as.Date("2010-03-31"),type = "lvl")
# 
# graph_sim_obs(simul_data , start_plot = as.Date("2010-03-31"),type = "g")
# 
# simul_data1 <- simulate_equation(eq_cf_s14, database = donness_recalculee, 
#                                  start_sim = as.Date("2006-03-31"),
#                                  end_sim = as.Date(date_fin_obs),
#                                  index_time = "date",
#                                  residual_var="af_eq_cf_s14")
# 
# graph_sim_obs(simul_data1 , start_plot = as.Date("2010-03-31"),type = "lvl")
# 
# graph_sim_obs(simul_data1 , start_plot = as.Date("2010-03-31"),type = "g")

simulation <- lapply(names(info_equations),function(x) {
  y <- simulate_equation(
    thor_equation=get(x),
    database=dt_pf,
    start_sim=as.Date("2006-03-31"),
    end_sim=as.Date(date_fin_obs),
    index_time="date",
    residual_var=info_equations[[x]]$residual_name) %>%
    as.data.frame() %>%
    .[,c("date","residual")]
  colnames(y) <- c("date",info_equations[[x]]$residual_name)
  return(y)}
) %>%
  Reduce(function(...) merge(..., all=TRUE),.,
         dt_pf[,which(!colnames(dt_pf)%in%lapply(info_equations, 
                                                 function(x) x$residual_name))])




#Prévision

model_endo_exo_switch(base_model=PF_rdb_mod,
                      new_model_name="PF_rdb_mod_switch",
                      "vs_v",
                      "cont_vs",
                      algo = TRUE,
                      rcpp = FALSE)
simulation[horizon_prev_index,c("cont_vs","af_eq_rdb_s14_v","af_eq_fbcf_s11","af_eq_cf_s14")] <- 0 #contribution aux variations de stock nulles en prévision

my_prev <- thor_solver(model=PF_rdb_mod_switch,
                       first_period=as.Date(date_debut_prev),
                       last_period=as.Date(date_fin_prev),
                       database=simulation,
                       index_time = "date",
                       rcpp = FALSE)



ggplot(data = dt_pf, aes(x =date, y=cf_s14)) +
  geom_line() +
  ggtitle(paste0("Evolution de l'I ")) 

ggplot(data = donness_recalculee, aes(x =date, y=cf_s14)) +
  geom_line() +
  ggtitle(paste0("Evolution de l'I ")) 

ggplot(data = simulation, aes(x =date, y=cf_s14)) +
  geom_line() +
  ggtitle(paste0("Evolution de l'I ")) 

ggplot(data = my_prev, aes(x =date, y=cf_s14)) +
  geom_line() +
  ggtitle(paste0("Evolution de l'I ")) 

# Tableau

series <- c("pib",
            "cf","cf_s13","cf_s14",
            "fbcf","fbcf_s13","fbcf_s11","fbcf_s14",
            "m","x",
            "ipc_tot")

labels <- c("PIB",
            "Consommation","..... publiques", "..... ménage",
            "FBCF","..... publique",".... entreprises", "..... ménages",
            "Importations","Exportations","IPC")



df_tableau <- my_prev[,-1] %>%
  lapply(function(x)vt(x)*100) %>%
  as.data.frame() %>%
  .[(nrow(dt_pf)-4*4+1):nrow(dt_pf),series] %>%
  t() %>%
  as.data.frame() %>%
  round(digits=1) %>%
  format(decimal.mark = ",",
         digits = 1)

rownames(df_tableau) <- labels
df_tableau <- rownames_to_column(df_tableau)  

header_annee <- c("",year(my_prev$date[(nrow(dt_pf)-4*4+1):nrow(dt_pf)])) %>% as.list()
names(header_annee) <- colnames(df_tableau)  

header_trimestre <- c("",paste0("T",(month(my_prev$date[(nrow(dt_pf)-4*4+1):nrow(dt_pf)])-1)/12*4+1)) %>% as.list()
names(header_trimestre) <- colnames(df_tableau)  


df_tableau %>%
  flextable() %>%
  set_header_labels(values=header_annee) %>%
  merge_h(part="header") %>%
  theme_zebra() %>%
  align(align="center",j=2:ncol(df_tableau),part="all")#on pourrait essayé la fonction de traduction en data table ici


#Contribution


my_contrib <- lapply(names(info_equations),function(x) {
  dyn_contribs(get(x),
               my_prev,
               as.Date("2010-03-31"),
               as.Date("2018-12-31"),
               "date",
               info_equations[[x]][["residual_name"]]) %>%
    filter(date>=as.Date("2010-03-31"))
}) %>%
  setNames(.,names(info_equations)) %>%
  as.list()

my_contrib_an <- lapply(my_contrib, function(x) {
  yearly_contrib(x,
                 index_year=substr(x[,"date"],start=1,stop=4))
})


# Graphiques ----------------------------------------------------------------
lapply(names(info_equations),function(x) {
  my_contrib[[x]][["date"]] <- as.Date(my_contrib[[x]][["date"]])})#Commment faire passer cette fonction

my_contrib[["eq_rdb_s14_v"]][["date"]] <- as.Date(my_contrib[["eq_rdb_s14_v"]][["date"]])
my_contrib[["eq_fbcf_s11"]][["date"]] <- as.Date(my_contrib[["eq_fbcf_s11"]][["date"]])
my_contrib[["eq_cf_s14"]][["date"]] <- as.Date(my_contrib[["eq_cf_s14"]][["date"]])

graphiques_q<- lapply(names(info_equations),function(x) {
  graph_contrib(
    my_contrib[[x]],
    as.Date("2016-03-31"),
    as.Date("2018-12-31"),
    "date",
    paste0("Contributions trimestrielles : ",
           info_equations[[x]]$endogenous_name)
  )
}) %>%
  setNames(names(info_equations))

graphiques_a <- lapply(names(info_equations),function(x) {
  graph_contrib(
    my_contrib_an[[x]],
    "2011",
    "2018",
    "year",
    paste0("Contributions annuelles : ",info_equations[[x]]$endogenous_name)
  )
})  %>%
  setNames(.,names(info_equations))

