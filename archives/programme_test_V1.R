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

horizon_prev <-
  (seq.Date(
    from = as.Date(date_debut_prev) + 1,
    to = as.Date(date_fin_prev) + 1,
    by = "quarter",
  ) - 1) %>%
  as.character(.)

horizon_estim_a <- seq(from = 2006, to = 2016, by = 1)  %>%
  as.character(.)

#Retraitement base de donnée


#dt_pib_tresthor <- read_csv("input/dt_pib_tresthor.csv")
dt_pf <- fread(file = "input/dt_pib_tresthor.csv")

dt_pf <- select(dt_pf,-(c(ID_PROD,DATA_TYPE)))

dt_pf$x_v <- dt_pf$X_B+dt_pf$X_S #exports totaux
dt_pf$m_v <- dt_pf$M_B+dt_pf$M_S #imports totaux
dt_pf$rs_v <- dt_pf$RS_S11+dt_pf$RS_S13

dt_pf <- select(dt_pf,-(c(X_S,X_B,M_S,M_B,CS_S11,CS_S13,CCF_S13,VA_NM,VA_M,P,ATSLP,SSLP,P_S11,PIB_E))) #supression variables inutiles

names(dt_pf)
colnames(dt_pf) <- c("date","dti","dte","tva","rs_s11_v","rs_s13_v","va","ci_v",
                     "fbcf_s14_v","fbcf_s11_v","fbcf_s13_v","fbcf_v","vs_v","cf_v",
                     "cf_s13_v","ci_s13_v","ci_s11_v","cf_s14_v","pib_v","x_v","m_v","rs_v") #rennomage en fonction du modèle
names(dt_pf)

dt_pf$dfl_fbcf_s11 <-1 #définition arbitraire des déflateurs pour l'investissement, les import et les exports
dt_pf$dfl_fbcf_s13 <-1 
dt_pf$dfl_fbcf_s14 <-1 
dt_pf$dfl_m <-1 
dt_pf$dfl_x <-1 

dt_pf$dfl_cf_s13 <- 1


dt_pf$pmc<-0.8 #définition de la propention marginale à consommer

dt_pf$ajust_pension <- 0


dt_x12_econ1016 <- read_csv("input/dt_x12_econ1016.csv")      #création de ipc tot
dt_ipc<- dt_x12_econ1016 %>% filter(SERIE =="IND_11"& DATE>=date_debut_obs&DATE<=date_fin_prev&ID_TYPE=="obs") %>%  select(value) 
dt_pf$ipc_tot <- dt_ipc$value

dt_pf$ipc_tot_vt <- vt(dt_pf$ipc_tot)

dt_pf$afusr_eq_rdb_s14_v <- 0
dt_pf$afusr_eq_fbcf_s11 <- 0
dt_pf$afusr_eq_cf_s14 <- 0



#Test de prévision
#dt_pf <- as.data.table(dt_pf)
#dt_pf[which(dt_pf$date>=date_debut_prev),!(c("date"))] <- NA 

dt_pf[which(dt_pf$date>=date_debut_prev),2:last(col(dt_pf))] <- NA #supression des données sur la période de prévision pour test

#while(last(dt_pf$date)<as.Date(date_fin_prev)){
#  temp <- dt_pf[nrow(dt_pf),]
#  temp <- lapply(temp,function(x)x<-NA) %>% as.data.frame(.)
#  temp$date <- ceiling_date(as.Date(last(dt_pf$date)), "quarter")
#  rownames(temp) <- temp$date
#  dt_pf <- rbind(dt_pf,temp)
#}
#rm(temp)

#dt_pf$date[49]<-"2017-03-31"
#dt_pf$date[50]<-"2017-06-30"
#dt_pf$date[51]<-"2017-09-30"
#dt_pf$date[52]<-"2017-12-31"
#dt_pf$date[53]<-"2018-03-31"
#dt_pf$date[54]<-"2018-06-30"
#dt_pf$date[55]<-"2018-09-30"
#dt_pf$date[56]<-"2018-12-31"


#Hypothèses variables exo sur la période de prévision

dt_pf <- as.data.frame(dt_pf)

rownames(dt_pf) <- dt_pf$date

plot(y=dt_pf$pib,x=dt_pf$date) # Graph pour déterminer la tendance de la série


series_croissantes <- c("x_v","cf_s13_v","rs_v","rs_s11_v") #série avec tendance croissante
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
    "ajust_pension",
    "afusr_eq_rdb_s14_v",
    "afusr_eq_fbcf_s11"
  ) #Séries plutôt stable

dt_pf[horizon_prev,series_stables] <- dt_pf[date_fin_obs,series_stables] #Hypothèse de niveau constant pour les variables stables sur la période de prévision

horizon_prev_index <- which(rownames(dt_pf)%in%horizon_prev) 
n0=horizon_prev_index[1]-8
n1=horizon_prev_index[1]-1

for (s in series_croissantes){
  x0 <- dt_pf[n0,s]
  x1 <- dt_pf[n1,s]
  var <- (x1/x0)^(1/(n1-n0))-1          #Hypothèse de croissance constante des exports sur la période de prévision
  for(h in horizon_prev_index){
    dt_pf[h,s] <- dt_pf[h-1,s]*(1+var)
  }
}






#création modèle


#create_model("PF_exo_mod", model_source = "C:/Users/ludovicg/Documents/R/tresthor-test/input/tresthor_base_allexogenous_modified.txt")
#create_model("PF_rdb_mod", model_source = "C:/Users/ludovicg/Documents/R/tresthor-test/input/tresthor_base_rdb.txt")
create_model("PF_rdb_mod", model_source = "C:/Users/ludovicg/Documents/R/tresthor-test/input/tresthor_base_rdb_mce.txt")

#Inclusion des variables endogènes dans le data frame


endo<-data.frame(matrix(NA,nrow(dt_pf),length(setdiff(names(PF_rdb_mod@var_map),names(dt_pf)))))
names(endo)<-setdiff(names(PF_rdb_mod@var_map),names(dt_pf))
dt_pf <- cbind(dt_pf,endo)

#Verifications

data_model_checks(thor_model=PF_rdb_mod,database=dt_pf,quiet=T) #check model

#Initialisation des variables endogènes


dt_pf <- dt_pf %>% mutate(m=m_v/dfl_m*100) #ini import en volume
dt_pf <- dt_pf %>% mutate(x=x_v/dfl_x*100) #ini export en volume
dt_pf <- dt_pf %>% mutate(fbcf_s11=fbcf_s11_v/dfl_fbcf_s11*100) #ini fbcf s11 en volume
dt_pf <- dt_pf %>% mutate(fbcf_s13=fbcf_s13_v/dfl_fbcf_s13*100) #ini fbcf s13 en volume
dt_pf <- dt_pf %>% mutate(fbcf_s14=fbcf_s14_v/dfl_fbcf_s14*100) #ini fbcf s14 en volume
dt_pf <- dt_pf %>% mutate(fbcf_v=fbcf_s11_v+fbcf_s13_v+fbcf_s14_v) #ini fbcf en valeur
dt_pf <- dt_pf %>% mutate(dihs_v=cf_v+fbcf_v) #ini demande intérieur hors stock en valeur
dt_pf <- dt_pf %>% mutate(di_v=dihs_v+vs_v) #ini demande intérieure en valeur
dt_pf <- dt_pf %>% mutate(df_v=di_v+x_v) #ini demande finale en valeur
dt_pf <- dt_pf %>% mutate(pib_v=cf_v+fbcf_v+x_v-m_v+vs_v) #ini pib en valeur
dt_pf <- dt_pf %>% mutate(tx_inv=fbcf_v/pib_v) #ini taux d'investissement
dt_pf <- dt_pf %>% mutate(fbcf=fbcf_s11+fbcf_s13+fbcf_s14) #ini fbcf en volume
dt_pf <- dt_pf %>% mutate(dfl_cf=1) 
dt_pf <- dt_pf %>% mutate(dfl_cf=lag(dfl_cf,1)*(ipc_tot/100)) #ini déflateur consommation finale



dt_pf <- dt_pf %>% mutate(cf_s14=cf_s14_v) #ini de la conso finale des ménages en volume sur celle en valeur avant de la déterminer par le déflateur
dt_pf <- dt_pf %>% mutate(dfl_cf_s14=1) #ini du déflateur de la conso finale des ménages avant de le déterminer par son lag et l'IPC
dt_pf <- dt_pf %>% mutate(dfl_cf_s14=lag(dfl_cf_s14,1)*(1+ipc_tot_vt)) #ini du déflateur de la conso finale des ménages
dt_pf <- dt_pf %>% mutate(cf_s14=cf_s14_v/dfl_cf_s14*100) #ini de la conso finale des ménages
dt_pf <- dt_pf %>% mutate(cf_s13=cf_s13_v/dfl_cf_s13*100) #ini de la conso des APU
dt_pf <- dt_pf %>% mutate(cf_v=cf_s13_v+cf_s14_v) #ini de la conso finale en valeur
dt_pf <- dt_pf %>% mutate(cf=cf_s13+cf_s14) #ini consommation finale en volume

dt_pf <- dt_pf %>% mutate(dihs=dihs_v) #ini de la demande intérieure hors stock en volume sur celle en valeur avant de la caler sur la vraie formule
dt_pf <- dt_pf %>% mutate(dihs=lag(dihs,1)*(((cf/lag(cf,1)-1)*lag(cf_v,1)+(fbcf/lag(fbcf,1)-1)*lag(fbcf_v,1))/lag(dihs_v,1)+1)) #ini demande intérieur hors stock en volume

dt_pf <- dt_pf %>% mutate(cf_s13=cf_s13_v)
dt_pf <- dt_pf %>% mutate(cf_s14=cf_s14_v)
dt_pf <- dt_pf %>% mutate(cont_cf_s13=(cf_s13/lag(cf_s13,1)-1)*(lag(cf_s13_v,1)/lag(pib_v,1)))
dt_pf <- dt_pf %>% mutate(cont_cf_s14=(cf_s14/lag(cf_s14,1)-1)*(lag(cf_s14_v,1)/lag(pib_v,1)))
dt_pf <- dt_pf %>% mutate(cont_cf=cont_cf_s13+cont_cf_s14)#ini contribution consommation finale
dt_pf <- dt_pf %>% mutate(cont_fbcf_s11=(fbcf_s11/lag(fbcf_s11,1)-1)*(lag(fbcf_s11_v,1)/lag(pib_v,1))) #ini contribution fbcf s11
dt_pf <- dt_pf %>% mutate(cont_fbcf_s13=(fbcf_s13/lag(fbcf_s13,1)-1)*(lag(fbcf_s13_v,1)/lag(pib_v,1))) #ini contribution fbcf s13
dt_pf <- dt_pf %>% mutate(cont_fbcf_s14=(fbcf_s14/lag(fbcf_s14,1)-1)*(lag(fbcf_s14_v,1)/lag(pib_v,1))) #ini contribution fbcf s14
dt_pf <- dt_pf %>% mutate(cont_fbcf=cont_fbcf_s11+cont_fbcf_s13+cont_fbcf_s14) #ini contribution fbcf 
dt_pf <- dt_pf %>% mutate(cont_dihs=cont_cf+cont_fbcf) #ini contribution demande intérieure hors stock

dt_pf <- dt_pf %>% mutate(cont_vs=(vs_v-lag(vs_v,1))/(lag(pib_v,1))) #ini contribution variation stock


dt_pf <- dt_pf %>% mutate(cont_di=cont_dihs+cont_vs) #ini contribution demande intérieure

dt_pf <- dt_pf %>% mutate(di=di_v) #ini de la demande intérieure en volume sur celle en valeur avant de la caler sur la vraie formule en lag
dt_pf <- dt_pf %>% mutate(di=(cont_di/(lag(di_v,1)/lag(pib_v,1))+1)*lag(di,1)) #ini demande intérieure 

dt_pf <- dt_pf %>% mutate(cont_m=-(m/lag(m,1)-1)*(lag(m_v,1)/lag(pib_v,1))) #ini contribution import 
dt_pf <- dt_pf %>% mutate(cont_x=(x/lag(x,1)-1)*(lag(x_v,1)/lag(pib_v,1))) #ini contribution export

dt_pf <- dt_pf %>% mutate(cont_df=cont_di+cont_x) #ini contribution demande finale

dt_pf <- dt_pf %>% mutate(cont_xm=cont_x+cont_m) #ini contribution export/import

dt_pf <- dt_pf %>% mutate(df=di+x) #ini demande finale en volume
dt_pf <- dt_pf %>% mutate(df=lag(df,1)*(((di/lag(di,1)-1)*lag(di_v,1)+(x/lag(x,1)-1)*lag(x_v,1))/lag(df_v,1)+1))  #ini demande finale en volume

dt_pf <- dt_pf %>% mutate(dt_pib=cont_cf+cont_fbcf+cont_x+cont_m+cont_vs) #ini croissance pib
dt_pf <- dt_pf %>% mutate(pib=pib_v)
dt_pf <- dt_pf %>% mutate(pib=lag(pib,1)*(dt_pib+1)) #ini pib en volume
dt_pf <- dt_pf %>% mutate(dfl_pib=pib_v/pib*100) #ini déflateur pib

dt_pf <- dt_pf %>% mutate(rdb_s14_v=cf_s14_v/pmc) #ini du rdb en valeur
dt_pf <- dt_pf %>% mutate(epargne=(rdb_s14_v)-cf_s14_v) #ini de l'épargne
dt_pf <- dt_pf %>% mutate(tx_epargne=epargne/rdb_s14_v) # ini du taux d'épargne

dt_pf <- dt_pf %>% mutate(pa=rdb_s14_v/dfl_cf_s14*100)#ini du pouvoir d'achat

dt_pf <- dt_pf %>% mutate(af_eq_rdb_s14_v=0)
dt_pf <- dt_pf %>% mutate(af_eq_rdb_s14_v=lag(af_eq_rdb_s14_v,1)+afusr_eq_rdb_s14_v)

dt_pf <- dt_pf %>% mutate(af_eq_fbcf_s11=0)
dt_pf <- dt_pf %>% mutate(af_eq_fbcf_s11=lag(af_eq_fbcf_s11,1)+afusr_eq_fbcf_s11)

dt_pf <- dt_pf %>% mutate(af_eq_cf_s14=0)
dt_pf <- dt_pf %>% mutate(af_eq_cf_s14=lag(af_eq_cf_s14,1)+afusr_eq_cf_s14)



#Info equation


info_equations <- list(

 eq_rdb_s14_v=

      list(endogenous_name="rdb_s14_v",

                residual_name="af_eq_rdb_s14_v",

                coeff_lt=c("e_lt1","e_lt2"),

                estim_start=as.Date("2006-03-31"),

                estim_end=as.Date(date_fin_obs),

                const=T)
 ,
  eq_fbcf_s11=
    
    list(endogenous_name="fbcf_s11",
         
         residual_name="af_eq_fbcf_s11",
         
         coeff_lt=NULL,           
         
         estim_start=as.Date("2006-03-31"),
         
         estim_end=as.Date("2016-12-31"),
         
         const=T)
 ,
 eq_cf_s14=
   
   list(endogenous_name="cf_s14",
        
        residual_name="af_eq_cf_s14",
        
        coeff_lt=NULL,           
        
        estim_start=as.Date("2006-03-31"),
        
        estim_end=as.Date("2016-12-31"),
        
        const=T)
  )
  
#Estimation

plot(y=dt_pf$cf_s14,x=dt_pf$date) 
plot(y=dt_pf$pa,x=dt_pf$date) 
plot(y=dt_pf$rdb_s14,x=dt_pf$date) 


create_equation(
  equation_name = "eq_cf_s14",
  formula="delta(1,log(cf_s14))=i_cst+i_0*(log(lag(cf_s14,1))-i_lt2*log(lag(pa,1))-i_lt1)+i_1*delta(1,log(pa))+delta(1,af_eq_cf_s14)-i_0*lag(af_eq_cf_s14,1)",
  coefflist = c("i_cst","i_0","i_1","i_lt2","i_lt1"),
  endogenous = "cf_s14"
)

dt_pf1 <- quick_estim(thor_equation = eq_cf_s14 , database = dt_pf,
                      estim_start = as.Date("2006-03-31"),
                      estim_end = as.Date(date_fin_obs),
                      coeff_lt = c("i_lt1","i_lt2"),
                      const = T)

dt_pf <- quick_estim_all(info_equations,PF_rdb_mod,dt_pf,"date")

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


# create_equation(
#  equation_name = "eq_rdb_s14_v",
#  formula="delta(1,log(rdb_s14_v))=e_cst+e_0*(log(lag(rdb_s14_v,1))-e_lt2*log(lag(rs_s11_v,1))-e_lt1)+e_1*delta(1,log(rs_s11_v))+delta(1,af_eq_rdb_s14_v)-e_0*lag(af_eq_rdb_s14_v,1)",
#  coefflist = c("e_cst","e_0","e_1","e_lt2","e_lt1"),
#  endogenous = "rdb_s14_v")
#
#create_equation(
#  equation_name = "eq_fbcf_s11",
#  formula="delta(1,log(fbcf_s11))=j_cst+j_0*(log(lag(fbcf_s11,-1))-j_lt2*log(lag(pib,-1))-j_lt1)+j_1*delta(1,log(pib))+delta(1,af_eq_fbcf_s11)-j_0*lag(af_eq_fbcf_s11,-1)",
#  coefflist = c("j_cst","j_0","j_1","j_lt2","j_lt1"),
#  endogenous = "fbcf_s11"
#)
#
#dt_pf1 <- quick_estim(thor_equation = eq_rdb_s14_v , database = dt_pf,
#                      estim_start = as.Date("2006-03-31"),
#                      estim_end = as.Date(date_fin_obs),
#                      coeff_lt = c("e_lt1","e_lt2"),
#                      const = T) #Pourquoi on n'a pas le même résultat>il faut bien mettre const=pareil que dans info equation 
#dt_pf1 <- dt_pf1[,c("e_cst","e_0","e_1","e_lt2","e_lt1")]
#dt_pf<-quick_estim(thor_equation = eq_fbcf_s11 , database = dt_pf,
#            estim_start = as.Date("2006-03-31"),
#            estim_end = as.Date(date_fin_obs),
#            coeff_lt = c("j_lt1","j_lt2"),
#            const = T)
#
#dt_pf <- cbind(dt_pf,dt_pf1)
#
#dt_pf[,c("e_cst","e_0","e_1","e_lt2","e_lt1")] <- dt_pf1

#Thor solver


horizon_test <- (seq.Date(as.Date("2010-04-01"), by="quarter", length.out = 12)-1 ) %>% 
  as.Date(.)

all(horizon_test %in% dt_pf$date)

dt_pf$date <- dt_pf[, "date"]
if (all(horizon_test %in% dt_pf$date) == FALSE) {
  stop(paste0("At least one of ", times, " was not found in ", 
              index_time, "."))
}

time_solver_test_run(PF_rdb_mod, 
                      database = dt_pf, 
                      index_time = "date",
                     times= horizon_prev)

donness_recalculee <- thor_solver(PF_rdb_mod, 
                                  first_period = as.Date("2010-03-31"), 
                                  last_period = as.Date("2012-09-30"),
                                  database = dt_pf, 
                                  index_time = "date",
                                  rcpp = FALSE) 


#a <- donness_recalculee==dt_pf

#Simulation

#create_equation(equation_name = "ec_pib_val",
                #formula = "pib_v=cf_v+fbcf_v+x_v-m_v+vs_v",
                #endogenous = "pib_v",coefflist =  NULL)

simul_data <- simulate_equation(eq_rdb_s14_v, database = dt_pf, 
                                start_sim = as.Date("2006-03-31"),
                                end_sim = as.Date(date_fin_obs),
                                index_time = "date",
                                residual_var="af_eq_rdb_s14_v")

graph_sim_obs(simul_data , start_plot = as.Date("2010-03-31"),type = "lvl")

graph_sim_obs(simul_data , start_plot = as.Date("2010-03-31"),type = "g")

simul_data1 <- simulate_equation(eq_pib, database = dt_pf, 
                                start_sim = as.Date("2006-03-31"),
                                end_sim = as.Date(date_fin_obs),
                                index_time = "date",
                                residual_var="residu")

graph_sim_obs(simul_data1 , start_plot = as.Date("2010-03-31"),type = "lvl")

graph_sim_obs(simul_data1 , start_plot = as.Date("2010-03-31"),type = "g")

dt_pf <- lapply(names(info_equations),function(x) {
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




#dyn_contrib <- dyn_contribs(ec_pib_val, database = dt_pf, 
#                            start_sim = as.Date("2006-03-31"),
#                            end_sim = as.Date(date_fin_prev),
#                            index_time = "date",
#                            residual_var="vs_v")

#Prévision

model_endo_exo_switch(base_model=PF_rdb_mod,
                      new_model_name="PF_rdb_mod_switch",
                      "vs_v",
                      "cont_vs",
                      algo = TRUE,
                      rcpp = FALSE)
dt_pf[horizon_prev_index,c("cont_vs","af_eq_rdb_s14_v","af_eq_fbcf_s11")] <- 0 #contribution aux variations de stock nulles en prévision

my_prev <- thor_solver(model=PF_rdb_mod_switch,
                       first_period=as.Date(date_debut_prev),
                       last_period=as.Date(date_fin_prev),
                       database=dt_pf,
                       index_time = "date",
                       rcpp = FALSE)

#Mise en forme

series <- c("pib",
            "cf","cf_s13","cf_s14",
            "fbcf","fbcf_s13","fbcf_s11","fbcf_s14",
            "m","x",
            "ipc_tot")

labels <- c("PIB",
            "Consommation","..... publiques", "..... ménage",
            "FBCF","..... publique",".... entreprises", "..... ménages",
            "Importations","Exportations","IPC")

# data.frame pour le tableau

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

# Dates

header_annee <- c("",year(my_prev$date[(nrow(dt_pf)-4*4+1):nrow(dt_pf)])) %>% as.list()
names(header_annee) <- colnames(df_tableau)  

header_trimestre <- c("",paste0("T",(month(my_prev$date[(nrow(dt_pf)-4*4+1):nrow(dt_pf)])-1)/12*4+1)) %>% as.list()
names(header_trimestre) <- colnames(df_tableau)  

# Tableau flextable

df_tableau %>%
  flextable() %>%
  set_header_labels(values=header_trimestre) %>%
  add_header(values=header_annee) %>%
  merge_h(part="header") %>%
  theme_zebra() %>%
  align(align="center",j=2:ncol(df_tableau),part="all")

df_tableau %>%
  flextable() %>%
  set_header_labels(values=header_annee) %>%
  merge_h(part="header") %>%
  theme_zebra() %>%
  align(align="center",j=2:ncol(df_tableau),part="all")


my_contrib <- sapply(names(info_equations),function(x) {
  dyn_contribs(get(x),
               my_prev,
               as.Date("2006-03-31"),
               as.Date(date_fin_prev),
               "date",
               info_equations[[x]]$residual_name) 
}) %>%
  setNames(.,names(info_equations)) %>%
  as.list()

graphiques_q<- lapply(names(info_equations),function(x) {
  graph_contrib(
    my_contrib[[x]],
    as.Date("2006-03-31"),
    as.Date(date_fin_prev),
    "date",
    paste0("Contributions trimestrielles : ",
           info_equations[[x]]$endogenous_name)
  )
}) %>%
  setNames(names(info_equations))



my_contrib <- dyn_contribs(eq_rdb_s14_v,
             my_prev,
             as.Date("2006-03-31"),
             as.Date(date_fin_prev),
             "date",
             "af_eq_rdb_s14_v")

mycontrib_an <- yearly_contrib(my_contrib,
               index_year=substr(my_contrib[,"date"],start=1,stop=4))

graph_contrib(
  my_contrib,
  as.Date("2015-03-31"),
  as.Date(date_fin_prev),
  "date",
  "Contributions trimestrielle RDB")

graph_contrib(
  mycontrib_an,
  "2010",
  "2018",
  "year",
  "Contributions annuelles du RDB ")




my_contrib1 <- dyn_contribs(eq_fbcf_s11,
                           dt_pf,
                           as.Date("2006-03-31"),
                           as.Date("2013-12-31"),
                           "date",
                           "af_eq_fbcf_s11")

mycontrib_an1 <- yearly_contrib(my_contrib1,
                               index_year=substr(my_contrib1[,"date"],start=1,stop=4))

graph_contrib(
  my_contrib1,
  as.Date("2010-03-31"),
  as.Date("2013-12-31"),
  "date",
  "Contributions trimestrielles fcbf")

graph_contrib(
  mycontrib_an1,
  "2007",
  "2013",
  "year",
  "Contributions annuelles du RDB")

