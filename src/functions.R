library(data.table)
library(DiagrammeR)
library(dplyr)
library(dbplyr)
library(DBI)
library(knitr)
library(tidyverse)
library(rdbnomics)
library(tresthor)
library(x12)
library(lubridate)
library(jsonlite)
library(tools)
library(tempdisagg)         # Methods for Temporal Disaggregation and Interpolation of Time Series
library(tsbox)              # required by tempdisagg
library(textreg)
library(flextable)
library(readxl)
library(urca)
library(stargazer)

# Fonctions --------------------------------------------------------------------

#trimestre en cours

tr <- function(x) {
  return(if (month(x) <= 3) {
    paste0(year(x),"_","Q1")
  }
  else if (month(x) > 3 & month(x) <= 6) {
    paste0(year(x),"_","Q2")
  }
  else if (month(x) > 6 & month(x) <= 9) {
    paste0(year(x),"_","Q3")
  }
  else {
    paste0(year(x),"_","Q4")
  })
}


# Variation trimestrielle
vt <- function(x){
  return(x/lag(x,n=1)-1)
}

#Moyenne mobile

ma <- function(x,n) if(length(x) >= n) stats::filter(x,rep(1 / n, n), sides=2) else NA

# Premiere obs non NA
firstNonNA <- function(x){
  i <- 1
  y <- x[i]
  while(is.na(x[i])){
    i <- i+1
  }
  return(i)
}

# Niveau a partir de vt
getLevel <- function(x) {
  # Fonction qui prend en input un taux de croissance et retourne un niveau (indice)
  # Attention pas de trous dans les donnees, juste des NA au debut et a la fin de la serie, sinon ca ne fonctionne pas.
  nobs <- length(na.omit(x))
  x[1] <- NA
  i <- firstNonNA(x)
  y <- c(1,1+na.omit(x))
  res <- x
  res[(i-1):(i+nobs-1)]<-cumprod(y)
  return(res)
}

# Fonction qui repete chaque observation de v times fois
freqconv_repeat <- function(v,times){
  n=length(v)
  y <- rep(NA,n*times)
  for(i in 1:n){
    for(j in ((i-1)*times+1):(i*times)){
      y[j]<-v[i]
    }
  }
  return(y)
}

# Fonction qui approxime une somme de volumes chaines
AgregVolch <- function(val, volch, annref = 2014){
  
  if (any(!(names(val) %in% names(volch))) | any(!(names(volch) %in% names(val)))) stop("Les noms de variables ne correspondent pas")
  
  val$agreg <- rowSums(val)
  
  for (i in c("val", "volch")){
    
    t <- eval(parse(text = i))
    
    t$annee <- as.numeric(substr(rownames(t), 1, 4))
    
    t.ann <- aggregate(subset(t, select = -c(annee)), by = list(t$annee), FUN = sum)
    rownames(t.ann) <- t.ann$Group.1
    assign(paste0(i, "_ann"), subset(t.ann, select = -c(Group.1)))
  }
  
  prixch_ann <- subset(val_ann, select = -c(agreg)) / volch_ann
  
  volpap <- volch
  for (i in 1:dim(volpap)[1]){
    volpap[i, ] <- volch[i, ] * prixch_ann[as.character(as.numeric(substr(rownames(subset(volpap, rownames(volpap) == rownames(volpap)[i])), 1, 4)) - 1), ]
  }
  volpap$agreg <- rowSums(volpap)
  volpap$annee <- as.numeric(substr(rownames(volpap), 1, 4))
  
  volpap_ann <- aggregate(subset(volpap, select = -c(annee)), by = list(volpap$annee), FUN = sum)
  rownames(volpap_ann) <- volpap_ann$Group.1
  volpap_ann <- subset(volpap_ann, select = -c(Group.1))
  
  volch_ann <- val_ann
  for (i in (annref + 1):max(as.numeric(rownames(volch_ann)))){
    volch_ann[as.character(i), ] <- volch_ann[as.character(i - 1), ] * volpap_ann[as.character(i), ] / val_ann[as.character(i - 1), ]
  }
  for (i in (annref - 1):(min(as.numeric(rownames(volch_ann))))){
    volch_ann[as.character(i), ] <- volch_ann[as.character(i + 1), ] / volpap_ann[as.character(i + 1), ] * val_ann[as.character(i), ]
  }
  
  prixch_ann$agreg <- val_ann$agreg / volch_ann$agreg
  
  for (i in 1:dim(volch)[1]){
    volch[i, "agreg"] <- volpap[i, "agreg"] / prixch_ann[as.character(as.numeric(substr(rownames(subset(volpap, rownames(volpap) == rownames(volpap)[i])), 1, 4)) - 1), "agreg"]
  }
  return(subset(volch, select = c(agreg)))
  
}



# Equations From JSON Input ---------------------------------------------------------------
CreateEquationsFromJSON <- function(.GlobalEnv) {
  listEquationsFiles <- list.files("input", "*.json", full.names = T)
  
  invisible(lapply(listEquationsFiles, FUN=function(x) {
    objectName <- basename(file_path_sans_ext(x))
    message(sprintf("Création de l'objet %s", objectName))
    assign(objectName, fromJSON(x), envir = globalenv() )
  }))
}


# Tableaux ----------------------------------------------------------------
MiseEnFormeTableauResultat <- function(tab){
  X <- copy(tab)
  setDT(X)
  Y <- suppressWarnings(melt(X, id.vars = c("date"), ))
  nomenc <- fread("input/NKL.series.csv")
  nomenc[,ordre:=.I]
  Y <- merge(Y, nomenc, by.x = "variable", by.y="serie")
  dtMax <- max(Y$date)
  dtMin <- dtMax %m+% years(-4)
  Z <- Y[date>dtMin & date<=dtMax, .(date, serie_label, ordre, value, date, Trimestre=gsub("\\.", "T", quarter(date, with_year = T)))]
}

# CSV ---------------------------------------------------------------------

readCSVFile <- function(i){
  fread(sprintf("%s/graphe%s.csv", projectFolder,i), encoding = "UTF-8", header = T)
}


readCSVFileAndMelt <- function(i, keysList, str_wrap_cle=30, str_wrap_variable=30, 
                               isOrdered=T, isVariableOrdered=F, isVariableRevOrdered=F){
  graphe <- fread(sprintf("%s/graphe%s.csv", projectFolder,i), encoding = "UTF-8", header = T)
  
  if(isOrdered) {
    #conserve l'ordre d'origine
    graphe[,(keysList):=stringr::str_wrap(get(keysList), str_wrap_cle)]
    r <- graphe[,get(keysList)]
    graphe[,(keysList):=factor(get(keysList), levels=r, ordered=T)]
  }
  
  graphe <- melt(graphe,id.vars=keysList)
  #graphe[,(keysList):=stringr::str_wrap(get(keysList), str_wrap_cle)]
  if (isVariableOrdered) {
    graphe[,variable:=factor(stringr::str_wrap(variable, str_wrap_variable), ordered = T)]
    if (isVariableRevOrdered)
      graphe[,variable:=forcats::fct_rev(variable)]
  } else
    graphe[,variable:=stringr::str_wrap(variable, str_wrap_variable)]
  graphe
}



# Code ggplot2 ------------------------------------------------------------

#ispfPalette <- c("#0071B2", "#A349A4", "#F07D17", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7")
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7", "#345687")

windowsFonts("Roboto" = windowsFont("Roboto Light"))


createGenericGraphe <- function(i, keyList, includeGraphe=F,
                                originalOrder=T, isVariableOrdered=F, isVariableRevOrdered=F,
                                legendPosition="bottom", legendDirection="horizontal",
                                str_wrap_cle=15, str_wrap_variable=15,
                                ylabelFunction=scales::number_format(accuracy = 1),
                                axis.text.x.angle=0){
  
  message(sprintf("Création du graphique %s",i))
  graphe <- readCSVFileAndMelt(i, keyList, str_wrap_cle, str_wrap_variable, isOrdered = originalOrder,
                               isVariableOrdered=isVariableOrdered, isVariableRevOrdered=isVariableRevOrdered)
  if (includeGraphe)
    g <- ggplot(graphe, aes_string(x=keyList, y="value", fill="variable", colour="variable"))
  else 
    g <- ggplot()
  g <- g +
    scale_y_continuous(labels = ylabelFunction)+
    scale_fill_manual(values=ispfPalette)+
    scale_colour_manual(values=ispfPalette)+
    theme_ispf()+
    ylab("")+  xlab("")+
    theme(legend.position = legendPosition, legend.direction = legendDirection,
          axis.text.x = element_text(angle = axis.text.x.angle, hjust = 1))
  g
}


theme_ispf <- function (base_size = 8, base_family = "Roboto Light") 
{
  bgcolor <- "#FFFFFF"
  ret <- theme(rect = element_rect(fill = bgcolor, linetype = 0, colour = NA), 
               text = element_text(size = base_size, family = base_family), 
               title = element_text(size = base_size,hjust = 0.5, family = "Roboto Light"), 
               plot.title = element_text(hjust = 0.5, family = "Roboto Light"), 
               axis.title.x = element_blank(),
               axis.title.y = element_text(hjust = 0.5, family = base_family),
               panel.grid.major.y = element_line(colour = "#D8D8D8"), 
               panel.grid.minor.y = element_blank(),
               panel.grid.major.x = element_blank(), 
               panel.grid.minor.x = element_blank(), 
               panel.border = element_blank(), 
               panel.background = element_blank(),
               legend.key = element_rect(fill = "#FFFFFF00"),
               plot.margin=grid::unit(c(0,0,0,0), "mm"),
               legend.position = "bottom",
               legend.direction = "horizontal",
               legend.title = element_blank(),
               legend.margin=margin(t = 0, unit='cm'),
               legend.key.width=unit(0.2, "cm"),
               legend.text = element_text(size = base_size, family = base_family))
  ret
}

saveGrapheFiles <- function(i, largeurCM=9, hauteurCM=8){
  ggplot <- eval(parse(text=paste0("g",i)))
  pdfFile <- paste0("output/plots/graphe", i, ".pdf")
  svgFile <- paste0("output/plots/graphe", i, ".svg")
  suppressMessages(ggsave(pdfFile, ggplot, width = largeurCM, height=hauteurCM, units = "cm", device=cairo_pdf))  
  suppressMessages(ggsave(svgFile, ggplot, width = 5, height=4, units = "in", device=svg))
  
  #On supprime la taille d'affichage pour que le SVG s'adapte à l'écran
  #https://la-cascade.io/rendre-svg-responsif/
  f <- readLines(svgFile)
  f[2] <- "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" viewBox=\"0 0 360 288\" version=\"1.1\">"
  writeLines(f, svgFile)
}
