# La doc de mon projet

[DTPLYR](https://dtplyr.tidyverse.org/)

``` mermaid
flowchart TB

subgraph Présentation
   Checking --> A[Up-date présentation]
end

subgraph MCE
   Etape-1A -- Inclure fbcf entreprises, conso ménage, rdb --> Etape-2A
   Etape-2A -- Inclure fcbf ménage --> Etape-3A 
   Etape-3A -- Inclure emploi, smtp, inflation --> Etape-4A 
   Etape-4A -- Si KO --> Etape-3A
   Etape-4A -- Inclure export et import --> FIN-A
end

subgraph Amélioration
  Etape-1B --  Inclure les indicatrices --> Etape-2B
  Etape-2B --  Afiner déflateurs avec PIB réel--> Etape-3B
  Etape-3B --  Essayer d'autres formes de MCE --> Etape-4B
  Etape-4B --  Ajouter auto retard MCE --> Etape-5B
  Etape-5B --  Comparer avec les MCE Opale --> Etape-6B
  Etape-6B --  Améliorer données RDB --> Etape-7B
  Etape-7B --  Revoir test ADF et inclusion tendance --> Etape-8B
  Etape-8B --  Trouver fbcf annuel pour tout lisser --> Etape-9B
  Etape-9B --  Revoir partage volume valeur dans les MCE --> Etape-10B
  Etape-10B --  Control maj A partout -->Etape-11B
  Etape-11B --  Tester MCE avec import désagrégé --> Etape-12B
  Etape-12B --  Lire lubridate --> Etape-13B
  Etape-13B --  Mise à jour spec avec rmd test -->FIN-B
end  

subgraph Documentation
  Etape-1C -- Lister les indicatrices --> Etape-2C
  Etape-2C -- Rédaction processus de vérification MCE --> Etape-3C
  Etape-3C -- Rédaction méthode trimestrialisation --> Etape-4C
  Etape-4C -- Structuration markdown --> Etape-5C
  Etape-5C -- Passer les équations en latex --> Etape-6C
  Etape-6C -- Processus d'intégration d'une nouvelle MCE --> Etape-7C
  Etape-7C -- Dictionnaire de variables --> Etape-8C
  Etape-8C -- Reporting de bug rencontrés --> Etape-9C
  Etape-9C -- Rédaction note d'instruction -->   Etape-10C
  Etape-10C -- Néttoyer dossier et fichiers --> Etape-11C
  Etape-11C -- passer texte rmd sous correcteur -->Etape-11C
  Etape-11C -- Faire un diagramme tresthor données modèle -->FIN-C
end

FIN-A --> Checking
FIN-B --> Checking
FIN-A -- Si checking OK --> Etape-1B
Etape-2C --> Etape-1B
FIN-C --> Checking

```
