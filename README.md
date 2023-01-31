# Documentation

## Mission du stage

Établir une prévision macroéconomique de moyen terme à travers un modèle économique.

### Type de modèle choisi

**Modèle Opale** : modèle utilisé par la Direction Générale du Trésor afin de réaliser des prévisions macroéconomiques sur l’économie française à l’horizon de 1 à 2 an, incluant deux types d'équations :  
* **Les équations comptables** : servent à assurer l'équilibre comptable du modèle (par exemple : l'équilibre du PIB avec ses composantes calculées par le modèle). 
* **Les équations comportementales (économétriques)** : vont quant à elles définir les mécanismes et dynamiques de l'économie (par exemple la consommation des ménages dépend entre-autres du pouvoir d'achat).

La modélisation est centrée sur la prévision des postes de la demande, cela signifie que le modèle sertt principalement à réaliser des prévisions sur la consommation, l’investissement, les imports et les exports en plus de l’emploi et du salaire moyen.  

**Comment prévoir ?** : En utilisant des équations économétriques de comportement à correction d’erreur. Les modèles à correction d’erreur sont un type d’équation particulier qui permet de dissocier la dynamique de court terme de la dynamique de long terme d’une série.


### Outil choisi 

Package Tresthor sous R :  Package disponible en open source et créé sur la base théorique du modèle Opale pour une utilisation pratique sous le langage informatique R

### Objectif 

Construire un modèle économique adapté à l’économie polynésienne à partir de la structure du modèle Opale. Puis, à partir du modèle créé, construire un programme sous R qui permet de réaliser une prévision de court terme en utilisant le package Tresthor.

### Supports de base  
* Manuels : https://www.tresor.economie.gouv.fr/Articles/2021/06/30/tresthor-le-nouvel-outil-de-la-dg-tresor-pour-realiser-des-previsions-macroeconomiques
  * Manuel d’utilisation de Tresthor
  * Guide d’utilisation d’Opale avec Tresthor
  * Exemple d’utilisation dans un cadre simplifié (sur la base de donnée issue de la Grande Bretagne)
* Document de travail  de la DG  Trésor retraçant la construction de la maquette Opale : https://www.tresor.economie.gouv.fr/Articles/2017/05/19/la-maquette-de-prevision-opale-2017

### Intérêt du projet : 

* Établir une prévision macro-économiques des grands agrégats de la demande (et donc du PIB) ainsi que du niveau d’emploi, du salaire moyen et de l’inflation.
* Réaliser des exercices de type post-mortem  afin de connaitre les mécanismes économiques en place. Cela signifie d'éxécuter le modèle sur le passé afin d’analyser la différence entre ce qui s’est vraiment passé et ce que le modèle aurait prévu avec les observations réelles des variables exogènes.
* Analyser le modèle en réponse à des chocs exogènes : il est possible de réaliser des scénarios de prévisions en incluant des chocs dans les résidus des équations économétriques ou en testant différant scénario de prévision (à travers les variables exogènes) afin d’analyser la réponse du modèle. Par exemple : quel est l’incidence d’un choc du prix du pétrole sur l’investissement des ménages?


### Synthèse graphique du projet 


[DTPLYR](https://dtplyr.tidyverse.org/)

``` mermaid

graph TB

J[Modélisateur]-- tests économétriques --> B[Modèle];
B -- input --> D[Tresthor];
M[Les comptes définitifs] -- sont fournis au -->H[Data scientist];
O[Des indicateurs économiques] -- sont receuillis par le --> H;
H -- retraitements --> C[Base de données];
C -- input --> D;
D --> F[Estimation];
F --> K[Simulation];
K --> G[Prévision];
I[Prévisionniste] -- hypothèse de scénario --> G;
L[Utilisateur] -- paramètre et exécute la  --> G
G --> N[Représentations visuelles]

```
