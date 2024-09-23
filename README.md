# Scripts d'analyse de données RhoMéO Flore

*Max de Bry d'Arcy, Asters-CEN74 2024*


## Le script Import_donnees.R
sert à fusionner plusieurs tableaux (csv) de données RhoMéO Flore d'un même site à des années différentes et à corriger d'éventuelles erreurs de saisie
En entrée, un ou plusieurs tableaux exporté depuis la base Access RhoMéO avec les colonnes "abundance", "cd_nom", "date", "nom_complet", "phisionomy", "trackingPoint"
En sortie, un tableau "lot_donnees.csv" nettoyé.

### Le script Analyse_site
sert à calculer les indices RhoMéO pour un site, créer des graphiques et faire des tests statistiques pour évaluer l'évolution des indices RhoMéO d'une année à l'autre

Il permet de créer les graphiques : 
  - Indices RhoMéO site entier / année
  - Indices RhoMéO par placette / année
  - Nombre d'espèces rencontrées / valeur d'indice
  - Plus grandes variations de recouvrement / espèce
  - Indices de Jaccard (similarité) / placette d'une année à une autre
  - AFC (analyse factorielle des correspondances) : association placettes - espèces rencontrées ensemble

Et de réaliser les tests de Wilcoxon et de comparaison médiane-seuil. /\ le test du V de Cramer n'est pas utilisé


### Le script Analyse_multi_site
a permis la comparaison d'un réseau de sites au plateau des Bornes. Il est peu adapté à d'autres jeux de données et analyse.
Il permet notamment de creer des modeles mixtes pour evaluer les variations des indices RhoMeO selon plusieurs variables (année, physionomie, modalités de gestion)




La visualisation des données utilise les packages tidyverse ggplot2 et dplyr (Wickham et al. 2019), ggrepel (Slowikowski et al. 2018), pour les AFC les packages
factoMineR (Lê, Josse, Husson 2008) et factoextra (Kassambara 2016), pour les corrélations le package corrplot (Wei et al. 2017). 
Les modèles mixtes utilisent le package lmerTest (Kuznetsova, Brockhoff, Christensen 2017) et pour les diagnostics le package DHARMa (Hartig, 2017)
