#Méthode bis : interpolation pour trimestrialisons plus lisse

#Import de données eretes brut

dt_an<-fread(file ="C:/Users/ludovicg/Documents/R/ludovic/input/dt_pib_eretes.csv")

dt_an[, c(
  "ID_PROD",
  "DATA_TYPE",
  "CF_S14_M",
  "CF_S14_NM",
  "CF_S15",
  "CI",
  "CSI",
  "CSE",
  "DTI",
  "DTE",
  "EBE",
  "ISP",
  "M",
  "MDC",
  "MDT",
  "MSB",
  "N_ETP",
  "SP",
  "TVA",
  "X",
  "XM_B",
  "XM_S",
  "MSB_S11",
  "MSB_S13",
  "CS_S11",
  "CS_S13",
  "VA_NM",
  "VA_M",
  "P",
  "ATSLP",
  "SSLP",
  "PIB_E",
  "RS",
  "CCF_S13"
) := NULL]

setnames(dt_an, names(dt_an[,!c("DATE")]), paste0(names(dt_an[,!c("DATE")]), "_v"))
setnames(dt_an, names(dt_an), str_to_lower(names(dt_an)))
setnames(dt_an, "pib_r_v", "pib_v")

#Ajout fbcf du site de l'ispf (détournement de méthode)

# dt_fbcf <- read_excel("C:/Users/ludovicg/Documents/R/ludovic/input/FBCF.xlsx")
# setDT(dt_fbcf)
# dt_fbcf[,Date:=as.Date(Date)]
# 
# for(i in c("FBCF_S13","FBCF_S14","FBCF_S11")) dt_an[,(i):=dt_fbcf[,get(i)]*1000]

#Fin détournement de méthode

# #Detournement de méthode 2
# 
# for(i in c("FBCF_S13","FBCF_S14","FBCF_S11")) dt_an[,(i):=dt_pf[c(which(substr(dt_pf$DATE,6,7)%in%c("03"))),get(i)]]
# 
# #fin de détournement de méthode

#Trimestrialisation par interpolation

dt_an[,date:=as.IDate(seq.Date(from=as.IDate(date_debut_obs)+1,by="year",length.out=nrow(dt_an))-1)]


dt_an[nrow(dt_an),date:=as.IDate(date_fin_obs)]

date2<-(seq.Date(from=as.IDate(date_debut_obs)+1,by="quarter",length.out=nrow(dt_an)*4)-1)

dt_tr <- setDT(lapply(dt_an,freqconv_repeat,times=4))

for(j in setdiff(names(dt_an),c("date"))) dt_tr[, (j):=approx(x=dt_an$date, y=dt_an[,get(j)], xout = date2, method="linear", ties="ordered")$y/4][,date:=date2]

#ajout dans dt_tr les variables manquantes (notamment fbcf si on passe pas par le détournement de méthode)

for(j in setdiff(names(dt_pf),names(dt_tr))) dt_tr[,(j):=dt_pf[,get(j)]]

dt_pf <- dt_tr

dt_pf$date <- as.IDate(dt_pf$date)

rm(dt_tr,dt_an)

