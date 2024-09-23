### Import de donnees issues du protocole RhoMeO

# Max de Bry d'Arcy, 2024


# Ce code sert a corriger des erreurs dans les jeux de donnees : il cree 
# a partir d'un ou de plusieurs fichiers bruts un fichiet "lot_donnees" corrige pret
# pour l'analyse.

# Le jeu de donnees est de type RhoMeO Flore issu de l'export avec la base Access

# Le jeu de donnees doit etre dans un dossier "Data"

## Import des bibliotheques ------
library(dplyr)


# Import des fichiers (.csv)----
# s'il y a plusieurs fichiers en entree (un par annee de suivi), remplacer
# ici par les noms de fichiers. attention si erreur les fichiers ne sont 
# peut etre pas encodes en format UTF - 8

table <- rbind(read.csv2("Data/NOM DU FICHIER"),
               read.csv2("Data/NOM DU FICHIER"),
               read.csv2("Data/NOM DU FICHIER"))

# S'il n'y a qu'un seul fichier, executer : 
# table <- read.csv2("Data/ NOM DU FICHIER")


# Import de la table flore : 
# flore_bassin est la table de donnees utilisee pour le calcul des indices RhoMeO, elle est
# disponible ici : https://rhomeo-bao.fr/?q=calculette dans "listes de reference"
# elle est susceptible d'etre mise a jour par le programme RhoMeO,
# Attention a toujours utiliser une liste adaptee a la region de l'etude!

# Pour trouver dans l'intranet d'ASTERS
especes <- read.csv2("N:/Thematiques/Suivi-Protocoles/RhoMeO/04-Listes-reference-rhomeo/flore_bassin.csv")

# Si la table est dans le dossier "Tables"
especes <- read.csv2("Tables/flore_bassin.csv")



# Selection des placettes presentes toutes les annees -------
# Si certaines placettes n'ont pas ete refaites tous les ans
# (ex. site de la Combe du feu)
# permet de ne garder que les placettes presentes toutes les annees

placettes <- unique( table %>% select(date, trackingPoint) %>%
  mutate(year = as.factor(substr(date,nchar(date)-3,nchar(date)))))
placettes <- placettes %>%
  group_by(trackingPoint) %>%
  filter(n_distinct(year) == length(unique(placettes$year))) %>%
  pull(trackingPoint) %>%
  unique()

# placettes presentes tous les ans :
print(placettes)

# correction de la table
table <- table %>% filter (trackingPoint %in% placettes)



# Correction d'erreurs dans "cd_nom" et "nom_complet" ------

# liste des especes rencontrees dans tout le site
#    (on peut eventuellement voir deja des erreurs) : 
View(unique(table %>% select (nom_complet)))

# verifier si presence de NA (pour "remarks" c'est normal si pas de remarques entrees)
summary(is.na(table))

# Valeurs de nom_complet qui ont plusieurs cd_nom : souvent des erreurs 
anomalies <- table %>% group_by(nom_complet) %>%
  summarise(nombre = n_distinct(cd_nom)) %>%
  filter(nombre > 1) %>%
  pull(nom_complet)
anomalies

# On corrige les especes mal notees : 
# Remplacer ici les cd_nom d'origine par le cd_nom corrige
# (le bloc est un ensemble des erreurs rencontrees pour le plateau des Bornes, remplir
#    avec toutes les corrections a faire)

table$cd_nom[table$cd_nom=="88719"] <- "88720"    # Carex nigra (et pas C. nigra all)
table$cd_nom[table$cd_nom=="162406"] <- "95922"   # Eleocharis palustris
table$cd_nom[table$cd_nom=="161115"] <- "96136"   # Epilobium angustifolium
table$cd_nom[table$cd_nom=="127338"] <- "127337"   # Trifolium hybridum
table$cd_nom[table$cd_nom=="98462"] <- "98460"   # Festuca pratensis
table$cd_nom[table$cd_nom=="98651"] <- "134664"   # Ficaria verna
table$cd_nom[table$cd_nom=="96526"] <- "96519"   # Equisetum limosum = fluviatile
table$nom_complet[table$cd_nom=="96519"] <- "Equisetum fluviatile"
table$cd_nom[table$cd_nom=="85852"] <- "124797" #Stachys officinalis (et pas Betonica)
table$nom_complet[table$cd_nom=="124797"] <- "Stachys officinalis"
table$cd_nom[table$cd_nom=="97433"] <- "97434" #Eupatorium cannabinum 


# verifier que cd_nom renvoie a une especes dont les indices RhoMeo sont connus
#   (parfois c'est une erreur de cd_nom)

tab_merge <- merge(table, especes, by = "cd_nom")
tab_merge %>% filter(is.na(cc)|is.na(humidite)|is.na(nutriment)) # si on a "0 lignes" c'est normal
                                          # sinon le tableau renvoie les especes a indices inconnus
# Si on trouve des especes a indices inconnus, il faut verifier que le nom rentre dans RhoMeO est bien
# le nom courant de l'espece, parfois un des synonymes a des indices RhoMeO donc il faut corriger. 
# Parfois les indices sont simplement non renseignes (especes de milieu sec par ex), dans ce cas il n
# y a rien a corriger.



# Enregistrement de la table sous le nom "lot_donnees.csv" pour utilisation du code "analyse_site" 
write.csv2(table, "Data/lot_donnees.csv", row.names = F)








