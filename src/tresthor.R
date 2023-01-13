#création modèle

create_model("PF_model", model_source = "C:/Users/ludovicg/Documents/R/ludovic/input/PF_model.txt")

#Inclusion des variables du modèle manquantes dans le data frame

dt_pf[,setdiff(names(PF_model@var_map),names(dt_pf)):=as.numeric(NA)]

#Initialisation des variables endogènes 

dt_pf[,ipc_tot_vt := vt(ipc_tot)][,
       dfl_fbcf_s11:=ipc_tot/100][,
       dfl_fbcf_s14:=px_immo/100][,
       dfl_cf_s14:=ipc_tot/100][,
       cf_s14:=cf_s14_v/dfl_cf_s14][,
       cf_v:=cf_s13_v+cf_s14_v][,
       fbcf_s11:=fbcf_s11_v/dfl_fbcf_s11][,
       fbcf_s14:=fbcf_s14_v/dfl_fbcf_s14][,
       fbcf_v:=fbcf_s11_v+fbcf_s13_v+fbcf_s14_v][,
       m:=m_v/dfl_m][,
       pib_v:=cf_v+fbcf_v+x_v-m_v+vs_v][,
       cf_s13:=cf_s13_v/dfl_cf_s13][,
       cf:=cf_s13+cf_s14][, 
       fbcf_s13:=fbcf_s13_v/dfl_fbcf_s13][,
       fbcf:=fbcf_s11+fbcf_s13+fbcf_s14][,
       x:=x_v/dfl_x][,
       cont_cf_s13:=(cf_s13/lag(cf_s13,1)-1)*(lag(cf_s13_v,1)/lag(pib_v,1))][,
       cont_cf_s14:=(cf_s14/lag(cf_s14,1)-1)*(lag(cf_s14_v,1)/lag(pib_v,1))][,
       cont_cf:=cont_cf_s13+cont_cf_s14][,
       cont_fbcf_s11:=(fbcf_s11/lag(fbcf_s11,1)-1)*(lag(fbcf_s11_v,1)/lag(pib_v,1))][,
       cont_fbcf_s13:=(fbcf_s13/lag(fbcf_s13,1)-1)*(lag(fbcf_s13_v,1)/lag(pib_v,1))][,
       cont_fbcf_s14:=(fbcf_s14/lag(fbcf_s14,1)-1)*(lag(fbcf_s14_v,1)/lag(pib_v,1))][,
       cont_fbcf:=cont_fbcf_s11+cont_fbcf_s13+cont_fbcf_s14][,
       cont_x:=(x/lag(x,1)-1)*(lag(x_v,1)/lag(pib_v,1))][,
       cont_m:=-(m/lag(m,1)-1)*(lag(m_v,1)/lag(pib_v,1))][,
       cont_xm:=cont_x+cont_m][,
       cont_vs:=(vs_v-lag(vs_v,1))/lag(pib_v,1)][,
       dt_pib:=cont_cf+cont_fbcf+cont_x+cont_m+cont_vs][,
       pib:=cf+fbcf+x-m+vs_v][,
       dfl_pib:=pib_v/pib][,
       dihs_v:=cf_v+fbcf_v][,
       di_v:=dihs_v+vs_v][,
       df_v:=di_v+x_v][,
       dihs:=cf+fbcf][,
       di:=dihs+vs_v][,
       df:=di+x][,
       cont_dihs:=cont_cf+cont_fbcf][,
       cont_di:=cont_dihs+cont_vs][,
       cont_df:=cont_di+cont_x][,
       epargne:=(rdb_s14_v)-cf_s14_v][,
       tx_epargne:=epargne/rdb_s14_v][,
       pa:=rdb_s14_v/dfl_cf_s14][,
       productivite:=pib/emploi][,
       smpt_s11:=rs_s11_v/eff_sal_etp][,
       csu_s11:=smpt_s11/productivite][,
       tx_marge_macro:=(pib_v-rs_s11_v)/pib_v][,
       tx_inv:=fbcf_v/pib_v]



#Info equation


info_equations <- list(
  
  eq_rdb_s14_v=
    
    list(endogenous_name="rdb_s14_v",
         
         residual_name="af_eq_rdb_s14_v",
         
         coeff_lt=c("a_lt1","a_lt2"),
         
         estim_start=as.Date(date_debut_estim),
         
         estim_end=as.Date(date_fin_estim),
         
         const=T)
  ,
  eq_cf_s14=
    
    list(endogenous_name="cf_s14",
         
         residual_name="af_eq_cf_s14",
         
         coeff_lt=c("b_lt1","b_lt2"),
         
         estim_start=as.Date(date_debut_estim),
         
         estim_end=as.Date(date_fin_estim),
         
         const=T)
  ,  
  eq_fbcf_s11=
    
    list(endogenous_name="fbcf_s11",
         
         residual_name="af_eq_fbcf_s11",
         
         coeff_lt=c("c_lt1","c_lt2"),           
         
         estim_start=as.Date(date_debut_estim),
         
         estim_end=as.Date(date_fin_estim),
         
         const=T)
  ,
  eq_fbcf_s14=
    
    list(endogenous_name="fbcf_s14",
         
         residual_name="af_eq_fbcf_s14",
         
         coeff_lt=c("d_lt1","d_lt2","d_lt3"),
         
         estim_start=as.Date(date_debut_estim),
         
         estim_end=as.Date(date_fin_estim),
         
         const=T)
  ,
  eq_smpt_s11=
    
    list(endogenous_name="smpt_s11",
         
         residual_name="af_eq_smpt_s11",
         
         coeff_lt=c("e_lt1","e_lt2"),
         
         estim_start=as.Date(date_debut_estim),
         
         estim_end=as.Date(date_fin_estim),
         
         const=T)
  ,
  eq_emploi=
    
    list(endogenous_name="emploi",
         
         residual_name="af_eq_emploi",
         
         coeff_lt=c("f_lt1","f_lt2"),
         
         estim_start=as.Date(date_debut_estim),
         
         estim_end=as.Date(date_fin_estim),
         
         const=T)
  ,
  eq_ipc_alim=
    
    list(endogenous_name="ipc_alim",
         
         residual_name="af_eq_ipc_alim",
         
         coeff_lt=c("g_lt1","g_lt2","g_lt3"),
         
         estim_start=as.Date(date_debut_estim),
         
         estim_end=as.Date(date_fin_estim),
         
         const=T)
  ,
  eq_ipc_nrj=
    
    list(endogenous_name="ipc_nrj",
         
         residual_name="af_eq_ipc_nrj",
         
         coeff_lt=c("h_lt1","h_lt2","h_lt3"),
         
         estim_start=as.Date(date_debut_estim),
         
         estim_end=as.Date(date_fin_estim),
         
         const=T)
  ,
  eq_ipc_tot=
    
    list(endogenous_name="ipc_tot",
         
         residual_name="af_eq_ipc_tot",
         
         coeff_lt=c("i_lt1","i_lt2"),
         
         estim_start=as.Date(date_debut_estim),
         
         estim_end=as.Date(date_fin_estim),
         
         const=T)
  ,
  eq_m=
    
    list(endogenous_name="m",
         
         residual_name="af_eq_m",
         
         coeff_lt=c("j_lt1","j_lt2"),
         
         estim_start=as.Date(date_debut_estim),
         
         estim_end=as.Date("2016-12-31"),
         
         const=T)
)

#Définition des résidus exogènes et endogènes

dt_pf[,paste0("afusr_",names(info_equations)):=0]


dt_pf[,(paste0("af_",names(info_equations))):=lapply(.SD, function(x) cumsum(x)), .SDcols=paste0("afusr_",names(info_equations))]

#transformer dt en dataframe pour tresthor + ordre alphabétique des colonnes pour plus d'accessibilité

setDF(dt_pf)
dt_pf <- dt_pf[,order(names(dt_pf))]


#Estimation

dt_pf <- quick_estim_all(info_equations,PF_model,dt_pf,"date")

#On n'a pas les p value du LT parfois, raison inconnue

#Fonction de vérification


# data_model_checks(thor_model=PF_model,database=dt_pf,quiet=F)


# horizon_test <-
#   (seq.Date(
#     as.Date(date_debut_estim)+1,
#     to = as.Date(date_fin_estim)+1,
#     by = "quarter"
#   ) - 1) %>%
#   as.Date(.)
# 
# time_solver_test_run(
#   PF_model,
#   database = dt_pf,
#   index_time = "date",
#   times = as.Date(horizon_test)
# )
# 
# na_report_variables_times(
#   dt_pf,
#   times = as.Date(horizon_test),
#   variables=names(dt_pf),
#   index_time = "date"
# )

#Recalcul des données sur la période passée (il est possible de s'éloigner de la réalité)(étape non nécessaire)

donnees_recalculee <- thor_solver(
  PF_model,
  first_period = as.Date(date_debut_estim),
  last_period = as.Date(date_fin_estim),
  database = dt_pf,
  index_time = "date"
)


#Possibilité d'utiliser le solver pour sortir des données concernant une seule équation

solution <- thor_equation_solver(
  equation = eq_fbcf_s11,
  first_period = as.Date("2006-03-31"),
  last_period = as.Date("2016-12-31"),
  database = dt_pf,
  index_time = "date"
)


#Simulation


simulation <- lapply(names(info_equations),function(x) {
  y <- simulate_equation(
    thor_equation=get(x),
    database=dt_pf,
    start_sim=as.Date(date_debut_estim),
    end_sim=as.Date(date_fin_estim),
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

model_endo_exo_switch(base_model = PF_model,
                      new_model_name = "PF_rdb_mod_switch",
                      "vs_v",
                      "cont_vs")#Verifier l'utiliter de cette étape

simulation[horizon_prev_index, c("cont_vs")] <- 0 #contribution aux variations de stock nulles en prévision


prev <- thor_solver(
  model = PF_rdb_mod_switch,
  first_period = as.Date(date_debut_prev),
  last_period = as.Date(date_fin_prev),
  database = simulation,
  index_time = "date",
  rcpp = FALSE
)

#Ecriture de la base de donnée avec la prévision dans les outputs

#fwrite(prev,"output/prevision.csv")


