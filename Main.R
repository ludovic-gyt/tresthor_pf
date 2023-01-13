#Paramètre utilisateur:

date_debut_estim <- "2006-03-31"
date_fin_estim <- "2018-12-31"
date_debut_prev <- "2019-03-31"
date_fin_prev   <- "2020-12-31" #Attention au problème des avions à la fin de transform si on change la période de prévision

#source
Donnee_lissee <- F
source("src/functions.R")
source("src/definitions.R")
source("src/extract.R")
if (Donnee_lissee){
  message("Les données trimestrialisées par interpolation linéaire seront utilisées")
  source("src/donnees_alternatives.R")
} else{message("Les données trimestrialisées par la méthode Denton-Cholette seront utilisées")}
source("src/transform.R")
source("src/tresthor.R")

#Représentations visuelles

source("src/tableau_croissance.R")
source("src/contribution.R")
source("src/simulation.R")

#Tests économétriques

source("src/test_économétrique.R")

#Construction dictionnaire de variable

source("src/dictionnaire.R")
