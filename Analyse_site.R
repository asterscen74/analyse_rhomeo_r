# Analyse de donnees issues d un protocole RhoMeO pour des ZH restaurees
----------------------------------------------------------------------------------------------------
# Max de Bry d'Arcy
# stage de M2 a ASTERS-CEN 74, 2024
----------------------------------------------------------------------------------------------------
# Import et mise a jour des bibliotheques   -------------
# au besoin executer install.packages("X") pour installer le package X

library(tidyverse)
library(ggrepel)
library(ggpubr)
library(gridExtra)
library(rlang)
library(FactoMineR)
library(factoextra)
library(viridis)

#  Le dossier du projet doit contenir * un dossier "Tables", qui contient
#          - "correspondance_transects.csv" = correspondances numero de placette-transect
#          - "flore_bassin.csv" = table de flore avec les cd_nom et les valeurs indicatrices
#              (pas besoin de placer flore_bassin dans "Tables" si acces au NAS d'ASTERS)
#
#  et   * un dossier "Data" avec le fichier de donnees "lot_donnees.csv" encode en UTF-8
#             issu de l'export depuis le formulaire Access export_calculette de l'outil RhomeO.
#             Il est possible de passer d'abord par le code "import_donnees" pour 

# Import et mise en forme des Données ---------------------------------------------------------------

# Import des donnees : adapter le chemin au besoin
data_brut <- read.csv2("Data/lot_donnees.csv")

# A partir d'ici, possible de tout executer sans modifier jusqu'a la partie "graphiques" (ligne ~165)

# Mise en forme
data_brut <- data_brut %>%
  rename(location = trackingPoint) %>% 
  mutate(year = substr(date,nchar(date)-3,nchar(date)),
         num = substr(location, nchar(location)-1, nchar(location))) %>%
  mutate_at(vars(year, physionomy, cd_nom, location, num), as.factor)%>%
  arrange(year) %>%
  mutate(num_an = dense_rank(year)) %>%   # Numero d'annee de suivi
  group_by(location)%>%
  mutate(phy_n1 = first(physionomy[num_an==1]))%>%  # Physionomie la premiere annee
  ungroup()

#conversion des abondances (indice Braun-Blanquet) en recouvrement (en %)
conversion = data.frame(abundance = c("+", "1", "2", "3", "4", "5"), rec = c(0.005, 0.025, 0.125, 0.375, 0.625, 0.875))
data_brut <- merge(data_brut, conversion, by = "abundance")
rm(conversion)


# Import de la table de calcul des indices RhoMeO
rhomeo <- read.csv2("Tables/flore_bassin.csv") %>%
  select(cd_nom, cc, humidite, nutriment)%>%
  mutate(cd_nom = as.factor(cd_nom))

# Calcul des indices RhoMeO (resultat_site) : executer sans modifier
data_brut <- merge(data_brut, rhomeo, by = "cd_nom", all.x = F)
data_brut <- data_brut[complete.cases(data_brut$humidite),]   # on ne retient que les especes a indices connus

resultat_site <- data_brut %>% 
  group_by(location, year) %>%
  summarise(
    S = sum(!is.na(rec)),
    I02 = sum(rec*humidite)/sum(rec), 
    I06 = sum(rec*nutriment)/sum(rec),
    I08 = (sum(rec*cc)/sum(rec))* sqrt(S),
    Cmoy = (sum(rec*cc)/sum(rec)),
    physionomy = first(physionomy),
    phy_n1 = first(phy_n1),
    num_an = first(num_an),
    num = first(num))%>%
  ungroup() %>%
  mutate(location = as.character(location),
         location = as.factor(location))


###  Ajout d'une variable "nom_red" : nom reduit pour faciliter l'affichage par espece
#       (4 lettres du nom de genre et 4 lettres du nom d'especes)

get_4l <- function(word) {return(substr(word,1,4))}  # fonction qui garde les 4 premieres lettres d'un mot
parts <- strsplit(data_brut$nom_complet," ")
parts <- lapply(parts, function(x) {return(x[1:2])})  # on ne garde que les 2 premiers mots du nom d'espece
lettres <- lapply(parts, function(x) {return (sapply(x, get_4l))})
data_brut$nom_red <- sapply(1:nrow(data_brut), function(i) {return( paste(lettres[[i]], collapse = "."))})

# Verifier que les valeurs de nom_red sont uniques (parfois les 4 premieres lettres
#   sont identiques entre 2 especes differentes)
anomalies <- data_brut %>% group_by(nom_red) %>% summarise(nombre = n_distinct(nom_complet)) %>%
  filter(nombre > 1) %>%  pull(nom_red)
anomalies

# choix manuel de nom_red differents au besoin (choix d'un nom reduit a partir du cd_nom)
data_brut$nom_red[data_brut$cd_nom=="103329"] <- "Hype.tetp"   # Hypericum tetrapterum
data_brut$nom_red[data_brut$cd_nom=="159984"] <- "Hype.tetg"   # Hypericum tetragonum
data_brut$nom_red[data_brut$cd_nom=="88753"] <- "Care.panl"   # Carex paniculata (pas panicea)

# Nettoyage variables
rm(lettres, parts, get_4l, anomalies)

###  Calcul des distributions des indices en nombre d'especes sur tout le site
data_calcul <- data_brut[!duplicated(data_brut[c("cd_nom","year")]),]

distrib_I02 <- data_calcul %>%
  group_by(year,humidite) %>%
  summarise(year = first(year),
            Nombre = n()) %>%
  rename(Indice = humidite) %>%
  ungroup()
distrib_I06 <- data_calcul %>%
  group_by(year,nutriment) %>%
  summarise(year = first(year),
            Nombre = n()) %>%
  rename(Indice = nutriment) %>%
  ungroup()
distrib_Cmoy <- data_calcul %>%
  group_by(year,cc) %>%
  summarise(year = first(year),
            Nombre = n()) %>%
  rename(Indice = cc) %>%
  ungroup()
distrib = list(I02 = distrib_I02, I06 = distrib_I06, Cmoy = distrib_Cmoy)
  
rm(data_calcul, rhomeo, distrib_I02, distrib_I06, distrib_Cmoy)

# Import des informations sur les transects : au besoin les rajouter a la main
# en rentrant :
# transect = c("T001", "T001", ... "T002") # dans l'ordre des lignes de resultat_site
# resultat_site <- cbind(resultat_site, transect)

transects <- read.csv2("Tables/correspondance_transects.csv") %>% mutate(
  location = as.factor(location))
transects <- unique(transects)

# Remarque : ici les noms de transects ont la forme "146floT001", on passe a "T001"
# Si pas besoin, enlever l'argument " %>% mutate(...)"
resultat_site <- merge(resultat_site, transects, by = "location") %>% 
  mutate(transect = substr(transect,nchar(transect)-3,nchar(transect)))
rm(transects)

# Pour une analyse d un seul transect : 
resultat_transect1 = resultat_site %>% filter(transect=="T001")
resultat_transect2 = resultat_site %>% filter(transect=="T002") # s'il y a 2 transects


## Graphiques des indices par site  -------------------------------------------------------

# IMPORTANT : dans mon analyse, je fais le choix d'etudier Cmoy (conservatisme moyen)
# et pas I08 (indice RhoMeO de qualite floristique) qui vaut Cmoy * sqrt(S)  ou S = nb d'especes de la placette
# car je suspecte une diff. de prospection d'une annee a l'autre donc un biais sur S

# Pour etudier I08 au lieu de Cmoy, il suffit de remplacer dans la suite du code
# (les 2 sont calcules dans "resultat_site")

# Tableau de donnees (temporaire) pour affichage graph 
#BOXPLOT pour l'ensemble du site, 
#si besoin transects par transects alors modifier la ligne suivante et mettre resultat_transect1 
temp <- resultat_site %>% 
  pivot_longer(cols = c(I02, I06, Cmoy), names_to = "Indice",
                                       values_to = "Valeur")

# Affichage du graphique
plt <- ggplot(temp, aes(x = year, y = Valeur, fill = year)) +
    geom_boxplot(alpha = 0.7) +
    facet_wrap(~Indice, scales = "free_y",axes = "all_y", 
               labeller = labeller(Indice = c("I02" = "Engorgement",
  "I06" = "Fertilité", "Cmoy" = "Conservatisme"))) +
  theme_minimal()+
    labs(x = "Année", title = "Variation des Indices RhoMeO du site")+
    theme(plot.title = element_text(size = 14, face = "bold"),
          strip.text = element_text(size = 9, face = "bold"))+
  guides(fill = "none")+
  scale_fill_viridis(discrete = TRUE, option = "D")

plot(plt)

# Sauvegarde (au besoin changer le chemin d'acces et la taille d'image)
ggsave(paste0(nom_dossier, "/Graphiques/indices_site.png"), plt, width = 8, height = 5)

# nettoyage variables
rm(temp, plt)


##  Graphiques des indices placette par placette ----------------------------------------

# fonction qui donne un graph sans la physionomie (plt1) et un avec la physionomie (plt2)
graph_transect <- function(indice){
  plt1 <- ggplot(resultat_site, aes(num, !!sym(indice), color = year, group = year)) +
    geom_line() +
    geom_point() + 
    labs(x = "Numéro de placette", color = "Année", 
         title = paste0("Variation de ", indice, " par placette"))+
    facet_wrap(~transect, scales = "free_x")+
    theme(plot.title = element_text(size = 14, face = "bold"),
          strip.text = element_text(size = 9, face = "bold"),
          legend.title = element_text(size = 9, face = "bold"))+
    scale_color_viridis(discrete = TRUE, option = "D")
  plt2 <- ggplot(resultat_site, aes(num, !!sym(indice), color = year, group = year, shape = physionomy)) +
    geom_line() +
    geom_point(size = 4) + 
    labs(x = "Num Placette", shape = "Physionomie", 
         color = "Année", 
         title = paste0("Variation de ", indice, " par placette"))+
    facet_wrap(~transect, scales = "free_x")+
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.title = element_text(size = 9, face = "bold"))+
    scale_color_viridis(discrete = TRUE, option = "D")
  return(list(plt1 = plt1, plt2 = plt2))
  }



# pour afficher chaque graph de chaque indice
plot(graph_transect("I02")$plt1)
plot(graph_transect("I02")$plt2)

plot(graph_transect("I06")$plt1)
plot(graph_transect("I06")$plt2)

plot(graph_transect("Cmoy")$plt1)
plot(graph_transect("Cmoy")$plt2)

#sauvegarde   (la taille d'image peut être adaptée en changeant "width" et "height")
for (i in c("I02", "I06", "Cmoy")){
  ggsave(paste0("Graphiques/", i, "_placettes.png"), graph_transect(i)$plt1, width = 5, height = 3) 
  ggsave(paste0("Graphiques/", i, "_placettes_physio.png"), graph_transect(i)$plt2, width = 5, height = 3)}


# Nettoyage variables
rm(i, graph_transect)


## Distribution des indices en nombre d'especes rencontrees -----------------------------------
# C'est-a-dire combien d'especes ont ete observees pour chaque valeur d'indice dans le site, par exemple
# pour I06 combien d'especes I06=2 (oligotrophe) jusqu'a combien d'especes I06 = 6 (nitrophile)

# choix des deux annees
indice <- "Cmoy"

# selection tableau correspondant
tab = distrib[[indice]]

# Graphique
plt <- ggplot(tab, aes(Indice, Nombre, color = year, group = year)) +
  geom_line() +
  geom_point() + 
  labs(x = indice, y = "Effectifs", title = paste0("Répartition des espèces par ", indice)) + 
  scale_x_continuous(breaks = tab$Indice)+
  scale_color_viridis(discrete = TRUE, option = "D", name = "Année")+
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 9, face = "bold"),
        panel.border = element_rect(color = "grey"))

plt

# Sauvegarde
ggsave(paste0(nom_dossier, "/Graphiques/", indice, "_repartition.png"), plt)

rm(plt, indice, tab)


# Condition d'application test Wilcoxon -------------------
# Il faut verifier que la difference entre indices les 2 annees  souhaitees est unimodale (1 seul grand pic)
# Si c'est le cas on peut ensuite appliquer le test de Wilcoxon
# Sinon le test est inadapte : preferer seulement une interpretation graphique

# Choix des deux annees
n1 = "2015"
n2 = "2024"

diff = data.frame(location = unique(resultat_site$location))
for (i in c("I02", "I06", "Cmoy")) {
  temp = resultat_site%>%filter(year==n2)%>%arrange(location)%>%pull(i)-
    resultat_site%>%filter(year==n1)%>%arrange(location)%>%pull(i)
  diff[[i]] = temp }

diff <- diff %>% pivot_longer(cols = c("I02", "I06", "Cmoy"), names_to = "Indice",values_to = "diff")

# Ces graph representent les differences entre indice l'annee 2 et 1
#   S'ils ont un seul pic (repartition unimodale), le test de Wilcoxon est applicable
ggplot(diff, aes(x = diff))+
  geom_density()+
  facet_wrap(~Indice, axes = "all_y")+
  labs(title = "Densités des différences d'indices année n2 - n1")+
  theme(plot.title = element_text(size = 14, face = "bold"))

rm(diff,temp, i, n1, n2)


# Test de Wilcoxon ----------------------------------------------
# Pour comparer la variation des 3 indices floristiques a l'echelle du site entre deux annees

# D'abord choisir les 2 annees a comparer

n1 = "2015"
n2 = "2024"

# Creation des tableaux indice / an
tab_02 <- resultat_site %>% select(location, year, I02) %>%
  pivot_wider(names_from = year, values_from = I02) %>%
  select(location, all_of(n1), all_of(n2)) %>%
  filter(complete.cases(.))

tab_06 <- resultat_site %>% select(location, year, I06) %>%
  pivot_wider(names_from = year, values_from = I06) %>%
  select(location, all_of(n1), all_of(n2)) %>%
  filter(complete.cases(.))

tab_08 <- resultat_site %>% select(location, year, Cmoy) %>%
  pivot_wider(names_from = year, values_from = Cmoy) %>%
  select(location, all_of(n1), all_of(n2)) %>%
  filter(complete.cases(.))

tests_stat = data.frame(Indice = c("Cmoy", "I02", "I06"),
  p_W = c(wilcox.test(tab_08[[n1]], tab_08[[n2]], paired = TRUE)$p.value,
          wilcox.test(tab_02[[n1]], tab_02[[n2]], paired = TRUE)$p.value,
          wilcox.test(tab_06[[n1]], tab_06[[n2]], paired = TRUE)$p.value))%>%
  mutate(signif = ifelse(p_W>0.05, "non significatif", "significatif"))

resultats_mediane <- resultat_site %>% 
  pivot_longer(cols = c(I02,I06,Cmoy),names_to = "Indice", values_to = "Val") %>%
  group_by(year, Indice) %>%
  summarise(Value = median(Val))
tests_stat <-cbind(tests_stat, resultats_mediane %>% filter (year == n2) %>% pull(Value) - 
                     resultats_mediane %>% filter (year == n1) %>% pull(Value))
names(tests_stat)[4] = "Diff_mediane"

tests_stat = tests_stat %>% mutate(
  Seuil_mediane = c(0.3, 0.2, 1.5),
  signif_mediane = ifelse(abs(Diff_mediane) >= Seuil_mediane, "significatif", "non significatif"))


# Nettoyage variables
rm(tab_02, tab_06, tab_08, resultats_mediane)

# Affichage du resultat
view(tests_stat)

# Ce tableau presente la significativite sous 2 manieres : p-val du test de Wilcoxon < 0.05
#  Et difference de mediane > seuil 


#### export de la table de tests (format image ou csv au choix)
tab = tableGrob(tests_stat)

ggsave(paste0(nom_dossier, "/Resultats/stat_", nom_dossier, ".png"), plot = tab, width = 10, height = 3)
write.csv2(tests_stat, paste0(nom_dossier, "/Resultats/stat_", nom_dossier, ".csv"))

rm(tab, n1, n2)
  

#### Variations par espece -----------------------------------

# pour observer le nombre d'especes par date par placette: 
xtabs(~location + year, data = data_brut)
ggplot(resultat_site, aes(x = num, y = S, fill = year))+
  geom_col(position = "dodge")+
  labs(x = "Numéro de placette", fill = "Année", title = "Nombre d'espèces par placette par an")+
  facet_wrap(~transect, scales = "free_x")+
  theme_minimal()+
  theme(plot.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 9, face = "bold"),
        legend.title = element_text(size = 9, face = "bold"))+
  scale_fill_viridis(discrete = TRUE, option = "D")
  

# Pour representer les especes dont le recouvrement varie le + entre 2 annees
# Choix annees
n1 ="2015"
n2 = "2024"

#Selection de certaines placettes au besoin, ou une seule physionomie... 
# pour selectionner certaines placettes au choix, executer : 
data = data_brut%>%filter(num %in% c("01", "02", "03"))

# pour avoir le site entier executer :
data = data_brut  



effectif <- length(unique(data$location))

combinaisons <- expand.grid(nom_complet = unique(data$nom_complet),
                            location = unique(data$location),
                            year = unique(data$year))
data <- merge(combinaisons, data, by = c("nom_complet", "location", "year"), all.x = T)
data$rec[is.na(data$rec)] = 0 

classement <-data %>%
  filter(year== n1 | year==n2) %>%
  group_by(nom_complet, year) %>%
  summarise(rec_moyen = 100*sum(rec)/effectif)%>%
  group_by(nom_complet) %>%
  reframe(diff = diff(rec_moyen), nom = first(nom_complet)) %>%
  arrange(desc(abs(diff))) %>%
  head(10)                                 # Pour avoir + ou - d'especes changer ici dans head()

# Plot
plt <- ggplot(classement, aes(y = reorder(nom, diff), x = diff, fill = ifelse(diff>0, "positive", "negative"))) +
  geom_bar(stat = "identity", position = "identity", width = 0.5) +
  scale_fill_manual(values = c("negative" = "lightblue", "positive" = "lightgreen")) + 
  labs(title = paste0("Espèces à plus grande évolution de recouvrement de ",n1," à ",n2),
    x = "Evolution du recouvrement moyen (%)")+
  theme_bw()+
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0, vjust = 0, size = 14, face = "bold")) +
  guides(fill = "none", ylab = "non")

plt

# Sauvegarde
ggsave(paste0("Graphiques/var_esp",n1,"-",n2,".png"), plt,width = 8, height = 5)


# Nettoyage
rm(data, classement, plt, effectif, n1, n2 ,combinaisons)


#  Similarite des placettes d'une annee a l'autre (indices de Jaccard) -------

# Indice de Jaccard entre 2 corteges = especes en commun / especes au total
# va de 0 (releves completement differents) a 1 (releves identiques)
# Attention cet indice ne prend pas en compte le recouvrement mais seulement presence/absence

n1 = "2015"
n2 = "2019"

jaccard <- data_brut %>%
  filter(year %in% c(n1,n2)) %>%
  group_by(year, num) %>%
  summarise(composition = list(unique(nom_complet))) %>%
  pivot_wider(names_from = year, values_from = composition) %>%
  rename(n1 = 2, n2 = 3) %>%
  rowwise() %>%
  mutate(similarite = length(intersect(n1,n2))/length(union(n1,n2))) %>%
  select(num, similarite)

plt <- ggplot(jaccard, aes(x = num, y = similarite)) +
  labs(title = paste0("Similarité des placettes de ",n1," à ",n2),
       x = "Numéro de placette", y = "Similarité (ind. de Jaccard)")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0, vjust = 0, size = 14, face = "bold")) +
  geom_col(fill = "lightblue", color = "black")
  
plt

# Sauvegarde
ggsave(paste0("Graphiques/jaccard",n1,"-",n2,".png"), plt, width = 8, height = 5)


rm(jaccard, plt, n1, n2)




 #### Representation du cortege vegetal par AFC ---------------------------------

# cette analyse permet une bonne visualisation des affinites especes - placettes
# et permet de mettre en evidence des variations de composition d'une ou un petit groupe 
# de placettes

# Choix de placettes (optionnel)
# permet de ne selectionner qu'un ensemble de placettes coherent geographiquement
# ou ayant des compositions proches, ce qui permet de mieux cerner des 
# variations du cortege (on evite de comparer une saulaie a un BM par exemple)

placettes = c("02", "03")

# Sinon, pour selectionner le site en entier :
placettes = unique(data_brut$num)


# jeu de donnees
tab <- data_brut %>% 
  filter(num %in% placettes) %>%
  mutate(location = as.character(location),
         id_plot = paste0(substr(location,nchar(location)-1,nchar(location)),"_",substr(as.character(year),3,4)),
         id_plot = as.factor(id_plot),
         location = as.factor(location)) %>%
  select(id_plot, year, physionomy, phy_n1, nom_red, rec)
var_rhomeo <- resultat_site %>% 
  mutate(location = as.character(location),
         id_plot = paste0(substr(location,nchar(location)-1,nchar(location)),"_",substr(as.character(year),3,4)),
         id_plot = as.factor(id_plot),
         location = as.factor(location))%>%
  select(id_plot, I02, I06, Cmoy)

tab <- merge(tab, var_rhomeo, by = "id_plot")


# Pour ne garder qu'une seule physionomie
# tab <- tab %>% filter(phy_n1=="PH")

# Creation de la table de contingence
cont = reshape(tab[, c("id_plot", "nom_red", "rec")], idvar = 'id_plot', timevar = 'nom_red',
                  varying = list(as.character(unique(tab$nom_red))), direction = 'wide')
cont[is.na(cont)]=0
rownames(cont) = cont$id_plot
cont = cont[,-1]
# si on prefere un tableau en presence / absence plutot que recouvrement, executer : cont[cont>0]=1
data = cbind(cont, unique(tab %>% select(id_plot,year,physionomy, I02, I06, Cmoy))[-1])

# Calcul de l'AFC
AFC = CA(data, quali.sup = c("year", "physionomy"), quanti.sup = c("I02", "I06", "Cmoy"), graph = F)

# valeurs propres et inertie : regarder a partir de combien d'axes l'inertie diminue nettement
#   ou critere variance > 20
head(AFC$eig)

fviz_screeplot(AFC, choice = "variance", ylab = "Variance expliquée (%)",
               xlab = "Axes factoriels", main = "Variance expliquée par axes factoriels")


### Representations graphiques

# selon les axes 1 et 2 : 
G1 <- fviz_ca_row(AFC, repel = T, habillage = data$year,
                  addEllipses=F)+
  labs(title= "Placettes") +
  theme_classic()
G2 <- fviz_ca_col(AFC, repel = T,  col.col= "cos2")+
  scale_color_gradient2(low = "white", mid = "blue", high = "red", midpoint = 0.5, space = "Lab") +
  labs(title = "Espèces")+
  theme_classic()
ggarrange(G1, G2, ncol = 2)

# Sauvegarde
ggsave("Graphiques/AFC_axes_1-2.png",ggarrange(G1, G2, ncol = 2), width = 10, height = 5)

# selon les axes 3 et 4 : 
G1 <- fviz_ca_row(AFC, axes = c(3,4), repel = T, habillage = data$year,
                  addEllipses=F)+
  labs(title = "Placettes")
G2 <- fviz_ca_col (AFC, axes = c(3,4), repel = T,  col.col= "cos2")+
    scale_color_gradient2(low = "lightblue", mid = "blue", high = "red", midpoint = 0.5, space = "Lab") +
    labs(title = "Espèces")
ggarrange(G1, G2, ncol = 2)

#rapports de correlation entre variables qualitatives supplementaires et les axes 1 - 4
dimdesc(AFC)$'Dim 1'$quali
dimdesc(AFC)$'Dim 2'$quali
dimdesc(AFC)$'Dim 3'$quali
dimdesc(AFC, axes = 1:4)$'Dim 4'$quali

# modalites de var. quali. sup excentrees significativement sur les axes 1 - 4
dimdesc(AFC)$'Dim 1'$category
dimdesc(AFC)$'Dim 2'$category
dimdesc(AFC)$'Dim 3'$category
dimdesc(AFC, axes = 1:4)$'Dim 4'$category


#####  Projection des indices floristiques sur l'AFC
plot(AFC, choix = "quanti.sup", title = "Indices RhoMeO des placettes sur les axes 1 et 2")

# axes 3 et 4
plot(AFC, choix = "quanti.sup", axes = c(3,4),
     title = "Indices RhoMeO des placettes sur les axes 3 et 4")


# Nettoyage des variables
rm(AFC, cont, data, G1, G2, var_rhomeo, placettes, tab)





#### export des tableaux --------------

# Donnees brutes
write.csv2(data_brut, "donnees_calcul.csv",row.names = F )

# Resultats
write.csv2(resultat_site,"resultat.csv" ,row.names = F )







