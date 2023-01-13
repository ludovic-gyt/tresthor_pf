#Dictionnaire de variables

#Ne peut être lancé que à la fin du main 

#Il faut ajouter les unités à l'occasion

dictio <- data.table("Noms_des_variables" = names(dt_pf),
           "label1" = 1,"label2" = 2,"label3" = 3)
dictio <- as.data.table(t(dictio))
setnames(dictio,names(dt_pf))


for(i in names(dt_pf)) dictio[2, (i) := if (is.na(str_split(i, "_")[[1]][1])){
  ""}
  else if (str_split(i, "_")[[1]][1] == "af") {
    "Résidus endogène"
  } else if (str_split(i, "_")[[1]][1] == "afusr") {
    "Résidus exogène"
  }
  else if (str_split(i, "_")[[1]][1] == "brent") {
    "Référence de prix pour le pétrole d'Europe, d'Afrique et du Moyen-Orient"
  }
  else if (str_split(i, "_")[[1]][1] == "cf") {
    "Consommation finale"
  }
  else if (i == "ch_pat") {
    "Charge patronale"
  }
  else if (i == "ch_sal") {
    "Charge salariale"
  }
  else if (i == "conso_elek_bt") {
    "Consommation d'électricité des entreprises"
  }
  else if (i == "conso_elek_mt") {
    "Consommation d'électricité des ménages"
  }
  else if (str_split(i, "_")[[1]][1] == "civils") {
      "Nombre d'employés civil"
  }
  else if (str_split(i, "_")[[1]][1] == "cont") {
    "Contribution au PIB"
  }
  else if (str_split(i, "_")[[1]][1] == "cst") {
    "Contribution de solidarité territoriale"
  }
  else if (i == "csu_s11") {
    "Coût salariale unitaire moyen"
  }
  else if (str_split(i, "_")[[1]][1] == "date") {
    "Dernier jour du trimestre pendant lequel sont issues les données"
  }
  else if (str_split(i, "_")[[1]][1] == "défense") {
    "Nombre d'employés militaires"
  }
  else if (str_split(i, "_")[[1]][1] == "df") {
    "Demande finale"
  }
  else if (str_split(i, "_")[[1]][1] == "civils") {
    "Nombre d'employés civil"
  }
  else if (str_split(i, "_")[[1]][1] == "dfl") {
    "Déflateur"
  }
  else if (str_split(i, "_")[[1]][1] == "di") {
    "Femande intérieure"
  }
  else if (str_split(i, "_")[[1]][1] == "dihs") {
    "Femande intérieure hors stocks"
  }
  else if (i== "dt_pib") {
    "Taux de croissance du PIB réel"
  }
  else if (i == "dummy_avion") {
    "Indicatrice de l'achat d'avion"
  }
  else if (i == "dummy_crise_fi") {
    "Indicatrice de la crise financière"
  }
  else if (str_split(i, "_")[[1]][1] == "dummy") {
    "Indicatrice"
  }
  else if (i == "eff_sal_etp") {
    "Nombre de salarié en équivalent temps plein"
  }
  else if (str_split(i, "_")[[1]][1] == "emploi") {
    "Nombre total d'emploi"
  }
  else if (str_split(i, "_")[[1]][1] == "epargne") {
    "Epargne totale"
  }
  else if (str_split(i, "_")[[1]][1] == "essence") {
    "Prix de l'essence"
  } 
  else if (str_split(i, "_")[[1]][1] == "fbcf") {
    "Formation brut de capital fixe"
  }
  else if (i == "h_trav") {
    "Nombre d'heures travaillées totales"
  }
  else if (i == "i_esm_commerce") {
    "Indice de l'emploi salarié marchand dans le secteur du commerce"
  }
  else if (i == "i_esm_construction") {
    "Indice de l'emploi salarié marchand dans le secteur du construction"
  }
  else if (i == "i_esm_hôtellerierestauration") {
    "Indice de l'emploi salarié marchand dans le secteur de l'hôtellerie/restauration"
  }
  else if (i == "i_esm_industrie") {
    "Indice de l'emploi salarié marchand dans le secteur de l'industrie"
  }
  else if (i == "i_esm_secteurmarchand") {
    "Indice de l'emploi salarié marchand dans le secteur marchand"
  }
  else if (i == "i_esm_services") {
    "Indice de l'emploi salarié marchand dans le secteur des services"
  }
  else if (i == "ind_btp") {
    "Indice du secteur du btp"
  }
  else if (str_split(i, "_")[[1]][1] == "ipc") {
    "Indice des prix à la consommation"
  }
  else if (str_split(i, "_")[[1]][1]== "m") {
    "Importations"
  }
  else if (str_split(i, "_")[[1]][1]== "msalb") {
    "Masse salariale"
  }
  else if (i== "nbr_sal") {
    "Nombre totale de salariés"
  }
  else if (str_split(i, "_")[[1]][1]== "pa") {
    "Pouvoir d'achat"
  }
  else if (str_split(i, "_")[[1]][1]== "pertu") {
    "Pertubation aléatoire (pour la phase expérimentale)"
  }
  else if (str_split(i, "_")[[1]][1]== "petrole") {
    "Prix du pétole en Polynésie"
  }
  else if (str_split(i, "_")[[1]][1]== "productivite") {
    "Taux de productivite (Richesse crée par emploi)"
  }
  else if (str_split(i, "_")[[1]][1]== "pib") {
    "Produit intérieur brut"
  }
  else if (i== "px_immo") {
    "Indice de prix de l'immobilier en nominal"
  }
  else if (str_split(i, "_")[[1]][1]== "pmc") {
    "Propension marginale à consommer"
  }
  else if (str_split(i, "_")[[1]][1]== "rdb") {
    "Revenu disponible brut"
  }
  else if (str_split(i, "_")[[1]][1]== "rs") {
    "Revenu salarié"
  }
  else if (str_split(i, "_")[[1]][1]== "salaire") {
    "Salaire (calculé à partir de la différence de msalb et les cotisations et la cst)"
  }
  else if (str_split(i, "_")[[1]][1]== "smpt") {
    "Salarie moyen par tête"
  }
  else if (str_split(i, "_")[[1]][1]== "tendance") {
    "Vecteur allant de 1 au nombre de trimestre total"
  }
  else if (str_split(i, "_")[[1]][1]== "tcen") {
    "Taux de change en nominal"
  }
  else if (str_split(i, "_")[[1]][1]== "total") {
    "Nombre d'emploi total"
  }
  else if (i == "tx_epargne") {
    "Taux d'épargne (revenu disponible brut moins la consommation des ménages)"
  }
  else if (i == "tx_inv") {
    "Taux d'investissement relativement au PIB"
  }
  else if (i == "tx_marge_macro") {
    "Excédent brut d'exploitation relativement au PIB"
  }
  else if (str_split(i, "_")[[1]][1] == "va") {
    "Valeur ajoutée"
  }
  else if (str_split(i, "_")[[1]][1] == "vs") {
    "Variation de stock"
  }
  else if (str_split(i, "_")[[1]][1] == "x") {
    "Exports"
  }
  else if (str_split(i, "_")[[1]][1] == "a") {
    "Coefficient de l'équation de l'indice des prix à l'alimentation"
  }
  else if (str_split(i, "_")[[1]][1] == "b") {
    "Coefficient de l'équation de l'indice des prix à l'énergie"
  }
  else if (str_split(i, "_")[[1]][1] == "c") {
    "Coefficient de l'équation de l'indice des prix total"
  }
  else if (str_split(i, "_")[[1]][1] == "d") {
    "Coefficient de l'équation de l'emploi"
  }
  else if (str_split(i, "_")[[1]][1] == "e") {
    "Coefficient de l'équation du revenu disponible brut"
  }
  else if (str_split(i, "_")[[1]][1] == "f") {
    "Coefficient de l'équation des importations"
  }
  else if (str_split(i, "_")[[1]][1] == "g") {
    "Coefficient de l'équation de l'investissement des ménages"
  }
  else if (str_split(i, "_")[[1]][1] == "h") {
    "Coefficient de l'équation du salaire moyen par tête"
  }
  else if (str_split(i, "_")[[1]][1] == "i") {
    "Coefficient de l'équation de la consommation des ménages"
  }
  else if (str_split(i, "_")[[1]][1] == "j") {
    "Coefficient de l'équation de l'investissement des entreprises"
  }
  else {
    ""
  }]

for(i in names(dt_pf)) dictio[3, (i) := if (is.na(str_split(i, "_")[[1]][2])) {
    ""
  }
  else if (str_split(i, "_")[[1]][2] == "eq") {
    "de l'équation économétrique"
  }
  else if (str_split(i, "_")[[1]][2] == "s11") {
    "des entreprises"
  }
  else if (str_split(i, "_")[[1]][2] == "s13") {
    "des administrations"
  }
  else if (str_split(i, "_")[[1]][2] == "s14") {
    "des ménages"
  }
  else if (str_split(i, "_")[[1]][2] == "v") {
    "en valeur"
  }
  else if (str_split(i, "_")[[1]][2] == "cf") {
    "de la consommation"
  }
  else if (str_split(i, "_")[[1]][2] == "df") {
    "de la demande finale"
  }
  else if (str_split(i, "_")[[1]][2] == "di") {
    "de la demande intérieure"
  }
  else if (str_split(i, "_")[[1]][2] == "dihs") {
    "de la demande intérieure hors stock"
  }
  else if (str_split(i, "_")[[1]][2] == "fbcf") {
    "de la formation brut de capital fixe"
  }
  else if (str_split(i, "_")[[1]][2] == "m") {
    "des importations"
  }
  else if (str_split(i, "_")[[1]][2] == "vs") {
    "des variations de stock"
  }
  else if (str_split(i, "_")[[1]][2] == "x") {
    "des exportations"
  }
  else if (str_split(i, "_")[[1]][2] == "xm") {
    "des exportations et des importations"
  }
  else if (str_split(i, "_")[[1]][2] == "vs") {
    "des variations de stock"
  }
  else if (str_split(i, "_")[[1]][2] == "18") {
    "de l'année 2018"
  }
  else if (str_split(i, "_")[[1]][2] == "alim") {
    "de l'alimentation"
  }
  else if (str_split(i, "_")[[1]][2] == "elekt") {
    "de l'électricité"
  }
  else if (str_split(i, "_")[[1]][2] == "h") {
    "du tabac"
  }
  else if (str_split(i, "_")[[1]][2] == "inf") {
    "sous-jacent"
  }
  else if (str_split(i, "_")[[1]][2] == "nrj") {
    "de l'énergie"
  }
  else if (str_split(i, "_")[[1]][2] == "tot") {
    "total"
  }
  else if (str_split(i, "_")[[1]][2] == "b") {
    "des biens"
  }
  else if (str_split(i, "_")[[1]][2] == "s") {
    "des services"
  }
  else {
    ""
  }]

for(i in names(dt_pf))
  dictio[4, (i) := if (is.na(str_split(i, "_")[[1]][3])) {
    ""
  }
  else if (str_split(i, "_")[[1]][1] == "af") {
    gsub("af_", "", i)
  } 
  else if (str_split(i, "_")[[1]][1] == "afusr") {
    gsub("afusr_", "", i)
  }
  else if (str_split(i, "_")[[1]][3] == "v") {
   "en valeur"
  } 
  else if (str_split(i, "_")[[1]][3] == "s11") {
    "des entreprises"
  }
  else if (str_split(i, "_")[[1]][3] == "s13") {
    "des administrations"
  }
  else if (str_split(i, "_")[[1]][3] == "s14") {
    "des ménages"
  }
  else if (str_split(i, "_")[[1]][3] == "q1") {
    "au premier trimestre"
  }
  else if (str_split(i, "_")[[1]][3] == "q2") {
    "au deuxième trimestre"
  }
  else if (str_split(i, "_")[[1]][3] == "q3") {
    "au troisième trimestre"
  }
  else if (str_split(i, "_")[[1]][3] == "q4") {
    "au quatrième trimestre"
  }
  else if (str_split(i, "_")[[1]][3] == "vt") {
    "en taux de croissance"
  }
  else {
    ""
  }]


dictio <- as.data.table(t(dictio))
dictio[,Label:=paste(V2,V3,V4)]
setnames(old = "V1", new= "Variable", dictio)
dictio <- dictio[,c("Variable","Label")]

fwrite(dictio,"output/dictionnaire.csv")
