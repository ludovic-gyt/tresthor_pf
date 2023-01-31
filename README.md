# La doc de mon projet

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
