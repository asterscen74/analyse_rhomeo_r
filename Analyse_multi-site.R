####### Analyse de releves Flore du protocole RhoMeO sur une serie de sites 
#       du plateau des Bornes

# Max de Bry d'Arcy
# Asters CEN-74, 2024

# ce code a servi a l'analyse multi-site sur le plateau des Bornes



# Import des donnees ---------------------------------------------------------

# Installation des packages
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(ggpubr)
library(extrafont)
library(viridis)
library(svglite)

# Definition d'un theme pour les graphiques 
loadfonts(device = "win")
theme_set(theme_bw(base_family = "Times New Roman"))

# Import des resultats de tous les sites (releves complets et indices)

# Les releves de donnees complet, nommes "lot_donnees" sont crees par le script
#   "import donnees", et les indices RhoMeO sont calcules par le script "Analyse_site"

# la fonction "mutate" permet de rajouter une variable "Site" aux tableaux de donnees
ind1 = read.csv2("Beulet/Resultats/indices_Beulet.csv") %>% mutate(Site = "Beulet")
ind2 = read.csv2("Chenet/Resultats/indices_Chenet.csv")%>% mutate(Site = "Chenet")
ind3 = read.csv2("Combe_du_feu/Resultats/indices_Combe_du_feu.csv")%>% mutate(Site = "Combe_du_feu")
ind4 = read.csv2("Dralet/Resultats/indices_Dralet.csv")%>% mutate(Site = "Dralet")
ind5 = read.csv2("la_Croix/Resultats/indices_la_Croix.csv")%>% mutate(Site = "la_Croix")
ind6 = read.csv2("Roguet/Resultats/indices_Roguet.csv")%>% mutate(Site = "Roguet")

tab1 <- read.csv2("Beulet/Data/lot_donnees.csv") %>% mutate(Site = "Beulet")
tab2 <- read.csv2("Chenet/Data/lot_donnees.csv")%>% mutate(Site = "Chenet")
tab3 <- read.csv2("Combe_du_feu/Data/lot_donnees.csv")%>% mutate(Site = "Combe_du_feu")
tab4 <- read.csv2("Dralet/Data/lot_donnees.csv")%>% mutate(Site = "Dralet")
tab5 <- read.csv2("la_Croix/Data/lot_donnees.csv")%>% mutate(Site = "la_Croix")
tab6 <- read.csv2("Roguet/Data/lot_donnees.csv")%>% mutate(Site = "Roguet")


# tableaux totaux (avec nom de site)
# Tableau de donnees brutes
tab_total <- rbind(tab1,tab2, tab3, tab4, tab5, tab6) %>%
  rename(location = trackingPoint) %>%
  mutate(id_plot = paste0((location), "_", substr(date,9,10)),
  id_plot = as.factor(id_plot),
  year = substr(date,7,10))

#conversion des abondances (indice Braun-Blanquet) en recouvrement (en %)
conversion = data.frame(abundance = c("+", "1", "2", "3", "4", "5"), rec = c(0.005, 0.025, 0.125, 0.375, 0.625, 0.875))
tab_total <- merge(tab_total, conversion, by = "abundance")
rm(conversion)


## Tableau des indices RhomeO par placette pour chaque site
indices <- rbind(ind1,ind2, ind3, ind4, ind5, ind6) %>%
  mutate_at(vars(location, year, physionomy, transect, Site), as.factor) %>%
  mutate(id_plot = paste0(as.character(location), "_", substr(as.character(year),3,4)),
         id_plot = as.factor(id_plot))

# nettoyage
rm(ind1, ind2, ind3, ind4, ind5, ind6)
rm(tab1, tab2, tab3, tab4, tab5, tab6)

#ajout des indicateurs floristiques de chaque espece a tab_total
rhomeo <- read.csv2("Tables/flore_bassin.csv") %>% select(cd_nom, cc, humidite, nutriment)
tab_total <- merge(tab_total, rhomeo)
rm(rhomeo)


# definition d'une variable num_an qui vaut le numero d'annee de suivi :
indices <- indices %>% mutate(num_an = ifelse(year=="2014"|year=="2015","1",ifelse(year=="2024",3,2)))

# Correction pour la combe du feu : pour homogeneiser on ne prend pas les releves 2010
#   --> a discuter ça : a la Combe du feu les + gros travaux ont ete faits en 2015,
#       donc l'annee 2014 peut servir de n0 comme l'annee 2015
#       pour les autres sites, meme si pour une analyse du site seul
#       on garde un maximum de donnees y compris 2010

indices <- indices %>% filter(!year=="2010")
tab_total <- tab_total %>% 
  filter(!year=="2010")

# Creation d'une variable "phy_n1" qui correspond a la physionomie la premiere annee de suivi
temp <- indices%>%filter(num_an==1)%>%select(location,physionomy) %>% rename(phy_n1 = physionomy)
tab_total <- merge(tab_total, temp, by = "location")
rm(temp)

###  Creation d'une variable "nom_red" : nom reduit pour faciliter l'affichage par espece

parts <- strsplit(tab_total$nom_complet," ")   # separer les 2 mots du nom d'espece
parts <- lapply(parts, function(x) {return(x[1:2])})    # prendre seulement les 2 premiers mots au besoin
lettres <- lapply(parts, function(x) {return (sapply(x, function(word) {return(substr(word,1,4))}))})   #garder les 4 premieres lettres

tab_total$nom_red <- sapply(1:nrow(tab_total), function(i) {
  return( paste(lettres[[i]], collapse = "."))})

# choix manuel de nom_red differents au besoin
tab_total$nom_red[tab_total$cd_nom=="103329"] <- "Hype.tetp"   # Hypericum tetrapterum
tab_total$nom_red[tab_total$cd_nom=="159984"] <- "Hype.tetg"   # Hypericum tetragonum
tab_total$nom_red[tab_total$cd_nom=="88753"] <- "Care.panl"   # Carex paniculata (pas panicea)
rm(lettres, parts)

# Import des donnees distance
dist <- read.csv2("Tables/distance.csv") %>% mutate (location = as.factor(location), distance = as.numeric(distance))
indices <- merge(indices, dist, by = "location")
rm(dist)


# Import des donnees sur les travaux, issues d'interpretation QGIS
#  Variable "travaux" : 0 = pas de travaux
#                       1 = travaux d'entretien
#                       2 = travaux de restauration
# Variable "depot" : 0 = pas de depot de matiere organique
#                   1 a 5 = nombre d'annee depuis le dernier depot (travaux sans export)
# variable "EEE" : gestion des plantes invasives (solidage) : 0 = pas de gestion
#                                                             1 = gestion

travaux <-read.csv2("Tables/travaux_placette.csv") %>%
  filter(year!="2010")%>%
  mutate_all(as.factor)%>%
  mutate(id_plot = paste0(as.character(location), "_", substr(as.character(year),3,4)),
         id_plot = as.factor(id_plot),
         num_an = ifelse(year=="2014"|year=="2015","1",ifelse(year=="2024","3","2")))%>%
  group_by(location)%>%
  mutate(restau = case_when(
    travaux[num_an=="2"]=="2"~ "n1 à n2",
    TRUE ~ "aucune"))%>%
  ungroup()%>%
  select(id_plot, travaux, restau, EEE, depot)


# Creation d'un tableau fusionne avec les indices et les travaux
ind_trav = merge(indices,travaux, by = "id_plot", all.x = T)
rm(travaux)


# Creer une variable sympa pour les noms de placettes, ex "Beu06_24" Beulet 6 2024
tab_total <- tab_total %>% mutate(
  plot = paste0(substr(Site,1,3),
                substr(location,nchar(location)-1,nchar(location)),"_",
                substr(year,3,4)))
ind_trav <- merge(ind_trav, unique(tab_total%>%select(id_plot, plot)), by = "id_plot")


# depot simplifie (a mettre dans autre bloc)
ind_trav <- ind_trav %>% mutate(
  depot_simpl = ifelse(depot==0, "aucun", ifelse(depot==1|depot==2, "1 à 2 ans", "3 à 5 ans")),
  depot_simpl = as.factor(depot_simpl))

# ind_trav <- ind_trav %>% mutate (depot = as.integer(depot)-1,
#   travaux_depot = ifelse(travaux==0, "aucun", 
#                          ifelse(travaux==1, ifelse(depot>0, "Entretien SE", "Entretien AE"), 
#                                 "Restauration")),
#   depot = as.factor(depot),
#   travaux_depot = as.factor (travaux_depot))



# Variation des indices RhoMeO sur tous les sites ------------------------------

# Evolution temporelle site par site 
plt1 <- ggplot(indices, aes(x = Site, y = I02, fill = num_an)) +
  geom_boxplot() +
  labs(title = paste0("A : Indice d'engorgement"),
       fill = "Année de suivi n°") +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 8)) +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  stat_summary(aes(shape = "Moyenne"), fun.y = mean, geom = "point", position = position_dodge(0.75), size = 2, color = "red") +
  scale_shape_manual(values = c("Moyenne" = 18)) +
  guides(shape = guide_legend(title = NULL, override.aes = list(color = "red", size = 3)))

plt2 <- ggplot(indices, aes(x = Site, y = I06, fill = num_an)) +
  geom_boxplot() +
  labs(title = paste0("B : Indice de fertilité"),
       fill = "Année de suivi n°") +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 8)) +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  stat_summary(aes(shape = "Moyenne"), fun.y = mean, geom = "point", position = position_dodge(0.75), size = 2, color = "red") +
  scale_shape_manual(values = c("Moyenne" = 18)) +
  guides(shape = guide_legend(title = NULL, override.aes = list(color = "red", size = 3)))

plt3 <- ggplot(indices, aes(x = Site, y = Cmoy, fill = num_an)) +
  geom_boxplot() +
  labs(title = paste0("C : Conservatisme moyen"),
       fill = "Année de suivi n°") +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 8)) +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  stat_summary(aes(shape = "Moyenne"), fun.y = mean, geom = "point", position = position_dodge(0.75), size = 2, color = "red") +
  scale_shape_manual(values = c("Moyenne" = 18)) +
  guides(shape = guide_legend(title = NULL, override.aes = list(color = "red", size = 3)))

plt_combine <- ggarrange(plt1, plt2, plt3, ncol = 1, nrow = 3)
ggsave("tous_sites/var_sites.svg",device = "svg",  plt_combine, width = 6, height = 8)
plt_combine

# Par physionomie
data_summary = function(x) {
  m = mean(x)
  ymin = m-sd(x)
  ymax = m+sd(x)
  return(c(y = m, ymin = ymin, ymax = ymax))}

plt <- ggplot(indices, aes(x = phy_n1, y = !!sym(indice))) +
  geom_violin() +
  labs(title = paste0("Variation ", legend, " sur tous les sites"),
       x = "Physionomie")+
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 8))+
  geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge", dotsize = 1/4)+
  stat_summary(fun.data = data_summary, geom = "pointrange", color = "red", size = 0.5, position = position_dodge(0.9))
plt

ggsave(paste0("tous_sites/",indice,"_physio_tot.png"), plt, width = 8, height = 4)

# evolutions : attention si l'on separe selon "physionomy" les placettes ne sont pas
# les memes d'une annee a l'autre. Si on separe selon "phy_n1", on etudie les
# physionomies de la premiere annee sans prendre compte des changements de physio.
indice = "I02"
legend = ifelse(indice=="I02", "de l'indice d'engorgement", ifelse(indice=="I06","de l'indice de fertilité", "du conservatisme moyen")) 

plt <- ggplot(indices, aes(x = num_an, y = !!sym(indice), fill = num_an)) +
  geom_boxplot() +
  facet_wrap(~phy_n1, scales = "free_y",axes = "all_y") +
  labs(x = "Année de suivi",
       title = paste0("Variation ", legend," par physionomie sur tous les sites"))+
  theme(plot.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 8))+
  scale_fill_viridis(discrete = TRUE, option = "D")+
  guides(fill = "none")

plt

rm(data_summary, legend, indice)

### Graphs descriptif du jeu de donnees ----------------------------------

## Graph de caracterisation des physio :
# Richesses spes
plt <- ggplot(indices, aes(x = physionomy, y = S))+
  geom_violin(fill = "grey")+
  geom_boxplot(width = 0.1, fill = "white")+
  labs(x = "Physionomie", 
       title = "Richesse spécifique")+
  theme(plot.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 8))

# Engorgement
plt02 <- ggplot(indices, aes(x = physionomy, y = I02))+
  geom_violin(fill = "grey")+
  geom_boxplot(width = 0.1, fill = "white")+
  labs(x = "Physionomie", 
       title = "Indice d'Engorgement")+
  theme(plot.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 8))

# Fertilite
plt06 <- ggplot(indices, aes(x = physionomy, y = I06))+
  geom_violin(fill = "grey")+
  geom_boxplot(width = 0.1, fill = "white")+
  labs(x = "Physionomie", 
       title = "Indice de Fertilité")+
  theme(plot.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 8))

# Conservatisme
plt08 <- ggplot(indices, aes(x = physionomy, y = Cmoy))+
  geom_violin(fill = "grey")+
  geom_boxplot(width = 0.1, fill = "white")+
  labs(x = "Physionomie", 
       title = "Conservatisme moyen")+
  theme(plot.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 8))

plt_combine <- annotate_figure(
  ggarrange(plt, plt02, plt06, plt08, ncol = 2, nrow = 2, common.legend = T, legend = "top"),
  top = text_grob("Caractérisation des physionomies de l'ensemble des sites", color = "black", face = "bold", size = 14 ,family = "Times New Roman"))

plot(plt_combine)
ggsave("tous_sites/carac_physio.png", plt_combine, width = 6, height = 6)
#ggsave("tous_sites/carac_physio.svg", plot = plt_combine , device = "svg", width = 6, height = 6)


rm(plt, plt02, plt06, plt08, plt_combine)

# effectifs par physio par an :
table(indices$num_an, indices$physionomy)

# stats (ecart type, mediane)
stats <- indices %>% group_by(physionomy)%>%
  summarise(
    med_S = median(S, na.rm = TRUE),
    s2_S = sd(S, na.rm = TRUE),
    med_02 = median(I02, na.rm = TRUE),
    s2_02 = sd(I02, na.rm = TRUE),
    med_06 = median(I06, na.rm = TRUE),
    s2_06 = sd(I06, na.rm = TRUE),
    med_08 = median(Cmoy, na.rm = TRUE),
    s2_08 = sd(Cmoy, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  mutate_at(vars(2:9), round, 2)
  
summary(indices$I02)
sd(indices$S)

### Graph de comparaison par annee :
plt <- ggplot(indices, aes(x = num_an, y = S, fill = num_an))+
  geom_violin()+
  geom_boxplot(width = 0.1, fill = "white")+
  labs(x = "Année de suivi", 
       title = "Richesse spécifique",
       fill = "Année")+
  theme(plot.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 8))+
  scale_fill_viridis(discrete = T, option = "D")+
  guides(fill = "none")
plt
ggsave("tous_sites/S.svg", plot = plt , device = "svg", width = 4, height = 3)


# Engorgement
plt02 <- ggplot(indices, aes(x = num_an, y = I02, fill = num_an))+
  geom_violin()+
  geom_boxplot(width = 0.1, fill = "white")+
  labs(x = "Année de suivi", 
       title = "Indice d'Engorgement")+
  theme(plot.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 8))+
  scale_fill_viridis(discrete = T, option = "D")+
  guides(fill = "none")

# Fertilite
plt06 <- ggplot(indices, aes(x = num_an, y = I06, fill = num_an))+
  geom_violin()+
  geom_boxplot(width = 0.1, fill = "white")+
  labs(x = "Année de suivi", 
       title = "Indice de Fertilité")+
  theme(plot.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 8))+
  scale_fill_viridis(discrete = T, option = "D")+
  guides(fill = "none")

# Conservatisme
plt08 <- ggplot(indices, aes(x = num_an, y = Cmoy, fill = num_an))+
  geom_violin()+
  geom_boxplot(width = 0.1, fill = "white")+
  labs(x = "Année de suivi", 
       title = "Conservatisme moyen")+
  theme(plot.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 8))+
  scale_fill_viridis(discrete = T, option = "D")+
  guides(fill = "none")

plt_combine <- annotate_figure(
  ggarrange(plt, plt02, plt06, plt08, ncol = 2, nrow = 2, common.legend = T, legend = "top"),
  top = text_grob("Comparaison selon l'année de suivi pour l'ensemble des sites", color = "black", face = "bold", size = 14 ,family = "Times New Roman"))

plot(plt_combine)

#ggsave("tous_sites/par_annee.svg", plot = plt_combine , device = "svg", width = 6, height = 6)
rm(plt, plt02, plt06, plt08, plt_combine)

# stats (ecart type, mediane)
stats <- indices %>% group_by(num_an)%>%
  summarise(
    med_S = median(S, na.rm = TRUE),
    s2_S = sd(S, na.rm = TRUE),
    med_02 = median(I02, na.rm = TRUE),
    s2_02 = sd(I02, na.rm = TRUE),
    med_06 = median(I06, na.rm = TRUE),
    s2_06 = sd(I06, na.rm = TRUE),
    med_08 = median(Cmoy, na.rm = TRUE),
    s2_08 = sd(Cmoy, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  mutate_at(vars(2:9), round, 2)

view(stats)


# pour test Wilcoxon pour justifier texte
temp <- indices %>%
  select(location, S, num_an, I02, I06, Cmoy) %>%
  pivot_wider(names_from = num_an, values_from = c("S","I02", "I06", "Cmoy"), values_fill = F)

WS <- wilcox.test(temp$S_2, temp$S_3, paired = TRUE)
W02 <- wilcox.test(temp$I02_2, temp$I02_3, paired = TRUE)
W06 <- wilcox.test(temp$I06_2, temp$I06_3, paired = TRUE)
W08 <- wilcox.test(temp$Cmoy_2, temp$Cmoy_3, paired = TRUE)
tab_W <- data.frame( Indice = c("S", "I02", "I06", "Cmoy"),
  W = c(WS$statistic, W02$statistic, W06$statistic, W08$statistic), 
  p_val = c(WS$p.value, W02$p.value, W06$p.value, W08$p.value),
  Effectif = c(nrow(temp), nrow(temp), nrow(temp), nrow(temp)) )

summary(temp$Cmoy_3)

rm(W02, W06, W08, WS, tab_W, stats, temp)


#### Especes en liste rouge ---------------------------------------

LR <- read.csv2("Tables/liste_rouge_RA.csv")
corresp <- read.csv2("Tables/flore_bassin.csv") %>% select(cd_nom, cd_ref)

liste_sp <- tab_total %>%
  group_by(cd_nom, year) %>%
  summarise(n_obs = n(),
            nom_complet = first(nom_complet))

liste_sp <- merge(liste_sp, corresp, by = "cd_nom")
liste_sp <- merge(liste_sp, LR, by = "cd_ref", all.x = T, all.y = F)
rm(corresp, LR)

sp_vulnerables <- liste_sp %>% filter(UICN!="LC"|National!="")

rm(sp_vulnerables, liste_sp)


### Impact des travaux sur les indices RhoMeO ----------------------------


  
# Graphique : évolution d'un indice RhoMeO selon la date de restauration ---------
indice = "Cmoy"
legend = ifelse(indice=="I02", "de l'indice d'engorgement", ifelse(indice=="I06","de l'indice de fertilité", "du conservatisme moyen")) 

# ajout possible filtre : ind_trav%>% filter(phy_n1=="BM"|phy_n1=="PH")

plt <- ggplot(ind_trav, aes(x = num_an, y = !!sym(indice), fill = num_an)) +
  geom_violin() +
  geom_boxplot(width = 0.1, fill = "white")+
  facet_wrap(~annee_restau, axes = "all_y") +
  labs(x = "Année de suivi n°", 
       title = paste0("Variation ", legend," sur tous les sites selon la date de première restauration"))+
  theme(plot.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 8))+
  scale_fill_brewer(palette = "Purples")+
  guides(fill = "none")
plt

# ou 
plt <- ggplot(ind_trav, aes(x = travaux, y = !!sym(indice) , fill = travaux))+
  geom_violin()+
  #geom_boxplot(width = 0.1, fill = "white")+
  labs(x = "Type de travaux" ,
       title = paste0("Variation ", legend," sur tous les sites selon les travaux"))+
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 8))+
  scale_fill_brewer(palette = "Greys")+
  geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge", dotsize = 1/3)+
  guides(fill = "none")

plot(plt)


ggsave(paste0("tous_sites/",indice,"_restau.png"), plt, width = 8, height = 4)

rm(indice, legend, plt)

### Graphique : comparaison indice fertilite selon le depot ou non de MO ------

plt <- ggplot(ind_trav, aes(x = depot_simpl, y = I06 , fill = depot_simpl))+
  geom_violin()+
  #geom_boxplot(width = 0.1, fill = "white")+
  labs(x = "Année depuis les travaux sans exportation", y = "Indice de fertilité I06" ,
       title = "Indice de fertilité selon la date de dépôt")+
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 8))+
  scale_fill_brewer(palette = "Greys")+
  geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge", dotsize = 1/3)+
  guides(fill = "none")

plot(plt)




ggsave("fertilite_depot.png", plt, width = 8, height = 4)


### Graphique : comparaison conservatisme selon lutte contre EEE ------
#  c'est peu interessant comme resultat on n'a pas les donnees pour faire du
#  avant / apres gestion des especes invasives : il faudrait plus de finesse temporaire
# dans l'echantillonage mais ce n'est pas le but du protocole RhoMeO

plt <- ggplot(ind_trav, aes(x = EEE, y = Cmoy , fill = EEE))+
  geom_violin()+
  labs(x = "présence/absence de lutte contre les eee", y = "Conservatisme Cmoy" ,
       title = "Conservatisme selon la gestion des invasives")+
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 8))+
  scale_fill_brewer(palette = "Greys")+
  geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge", dotsize = 1/4)+
  guides(fill = "none")

plot(plt)

ggsave("conservatisme_cc.png", plt, width = 8, height = 4)

rm(plt)

###  Tests statistiques pour evaluer la variation des 3 indices RhoMeO -----------
# library(car)
# library(rstatix)
library(lmerTest) # modeles mixtes (fonction lmer)
# library(MuMIn) # fonction rsquared GLMM
# library(lattice) # tests (fonction step)
# library(AICcmodavg) # comparaison AIC
library(outliers) # valeurs influentes
library(DHARMa) # tests de diagnostic modeles

### Modele mixte-------
# : evaluation de l'effet "annee" et travaux sur chacun des indices
# avec un effet considere aleatoire qui est la difference des placettes et des sites


# I02 ----------
mm <- lmer(I02 ~ num_an +phy_n1+ restau:num_an+ depot_simpl +phy_n1:num_an + distance+(1|Site)+(1|location), data = ind_trav)
step(mm)

mm <- lmer(I02 ~ num_an +phy_n1+ restau:num_an+ phy_n1:num_an +(1|location), data = ind_trav)
summary(mm)

# R2m ( R² marginal) variance expliquee par effets fixes
# R2c (R² conditionnel) variance expliquee par effets fixes et aleat. R2c tjrs > R2m
r.squaredGLMM(mm)

# Histogramme des residus
residus <- residuals(mm, type = "pearson", scaled = T)
ind_trav$res = residus


#Q-Q plot des residus
hist(residus, main = "Histogramme des résidus", xlab = "Résidus")
qqnorm(residus)
qqline(residus)

# Pour I02, les residus ont une repartition normale.
## QQ plot des effets aleatoires (ranef)
coeff <- ranef(mm)$location
qqnorm(coeff$'(Intercept)')
qqline(coeff$'(Intercept)') 

# Le test de Grubbs sert a identifier un outlier, et la probabilite qu'il influence le modele
grubbs.test(ind_trav$res, type = 10, opposite = F, two.sided = F)

# Boucle pour eliminer les outliers
data_clean <- ind_trav
outliers <- data.frame()
while(grubbs.test(data_clean$res, type = 10, opposite = F, two.sided = F)$p.value<0.05){
  max <- which.max(abs(data_clean$res))         #- selection du + grand residu (ecart predit-reel)
  outliers <- rbind(outliers, data_clean[max,]) #- la table outliers prend cette ligne
  data_clean <- data_clean[-max,]               #- on la retire de data_clean
}
outliers # observer les valeurs influentes : pas d'outlier pour I02

# Residus vs valeurs predictes
plot(fitted(mm), residus, main = "Résidus vs valeurs prédites", xlab = "Valeurs prédites", ylab = "Résidus")
abline(h=0, col = "red")


# Conclusion : interpreter les coeff du summary du modele selectionne
summary(mm)
# I02 diminue significativement l'annee 3 du suivi.
# Les fourres humides, prairies humides et megaphorbiaies ont un I02 plus bas que les BM
# les placettes restaurees entre annee 1&2 ont un engorgement + eleve que les autres l'annee 3
# (remarquer que ce coeff compense a peine celui de n3 : l'engorgement ne diminue pas mais n'augmente pas)


# Diagramme a barres des effets fixes : 
effets_fixes <- as.data.frame(summary(mm)$coefficients)%>%
  filter(`Pr(>|t|)`<0.05)

df <- data.frame(Variable = rownames(effets_fixes),
                 Coefficient = effets_fixes[,"Estimate"],
                 IC_inf = effets_fixes[,"Estimate"]-1.96*effets_fixes[,"Std. Error"],
                 IC_sup = effets_fixes[,"Estimate"]+1.96*effets_fixes[,"Std. Error"])
ggplot(df, aes(x = Variable, y = Coefficient, fill = ifelse(Coefficient>0, "positive", "negative"))) + 
  geom_bar(stat = "identity", position = "identity", width = 0.5)+
  scale_fill_manual(values = c("negative" = "lightsalmon", "positive" = "lightskyblue1")) +
  geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup), width = 0.2)+
  labs(title = "Effets fixes du modèle", y = "Coefficient", x = "Variable")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 9, face = "bold" ))+
  guides(fill = "none")


## Graph des effets aleatoires

# effets_aleatoires <- ranef(mm)$Site[1] 
# # Créer un dataframe pour ggplot 
# df_effets_aleatoires <- data.frame(Groupe = rownames(effets_aleatoires), 
#                                    Effet = effets_aleatoires[, "(Intercept)"])
# # Plot 
# ggplot(df_effets_aleatoires, aes(x = reorder(Groupe, Effet), y = Effet)) + 
#   geom_point() + 
#   coord_flip() + 
#   labs(title = "Effets aléatoires par groupe", y = "Effet", x = "Groupe") 


  
# Graphiques de diagnostic DHARMa


residus <- simulateResiduals(mm, plot = T)
testResiduals(residus)


# I06 ----------------
mm <- lmer(I06 ~ num_an + restau:num_an +depot_simpl + phy_n1+ phy_n1:num_an + distance +
             (1|Site)+(1|location), data = ind_trav)
step(mm)


mm <- lmer(I06 ~ num_an + phy_n1 + (1 | location) + distance + num_an:phy_n1, data = ind_trav)
summary(mm)

#Q-Q plot des residus
hist(residus, main = "Histogramme des résidus", xlab = "Résidus")
qqnorm(residus)
qqline(residus) # pour I06, peut etre des outliers (residus en dessous de la norme a l'extremite)

# QQ plot effets aleatoires
coeff <- ranef(mm)$location
qqnorm(coeff$'(Intercept)')
qqline(coeff$'(Intercept)') 

# Valeurs influentes
data_clean <- ind_trav
outliers <- data.frame()
while(grubbs.test(data_clean$res, type = 10, opposite = F, two.sided = F)$p.value<0.05){
  max <- which.max(abs(data_clean$res))         #- selection du + grand residu (ecart predit-reel)
  outliers <- rbind(outliers, data_clean[max,]) #- la table outliers prend cette ligne
  data_clean <- data_clean[-max,]}               #- on la retire de data_clean
outliers  # pas d' outliers selon le test de Grubbs


# Conclusions
summary(mm)
# hausse significative de I06 l'annee 3.
# I06 plus haut dans les placettes avec au depart une physionomie GH ,MG, PH que BM
# Les placettes restaurees ont une fertilite moins elevee que les autres (pas en fonction de l'entretien)

# Diagramme a barres des effets fixes : 
effets_fixes <- as.data.frame(summary(mm)$coefficients)%>%
  filter(`Pr(>|t|)`<0.05)

df <- data.frame(Variable = rownames(effets_fixes),
                 Coefficient = effets_fixes[,"Estimate"],
                 IC_inf = effets_fixes[,"Estimate"]-1.96*effets_fixes[,"Std. Error"],
                 IC_sup = effets_fixes[,"Estimate"]+1.96*effets_fixes[,"Std. Error"])
ggplot(df, aes(x = Variable, y = Coefficient, fill = ifelse(Coefficient>0, "positive", "negative"))) + 
  geom_bar(stat = "identity", position = "identity", width = 0.5)+
  scale_fill_manual(values = c("negative" = "lightsalmon", "positive" = "lightskyblue1")) +
  geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup), width = 0.2)+
  labs(title = "Effets fixes du modèle", y = "Coefficient", x = "Variable")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 9, face = "bold" ))+
  guides(fill = "none")


# Graphiques de diagnostic DHARMa
residus <- simulateResiduals(mm, plot = T, seed =F )
testResiduals(residus)
plotResiduals(residus, form = ind_trav$Site)

# testDispersion(residus)
# testOutliers(residus)

library(performance)
check_heteroscedasticity(mm)



# Cmoy ------------
mm <- lmer(Cmoy ~ num_an + restau:num_an +depot_simpl +phy_n1+EEE+
             phy_n1:num_an +distance+(1|Site)+(1|location), data = ind_trav)

step(mm)

mm <- lmer(Cmoy ~ num_an + phy_n1 + (1 | location) + num_an:restau, data = ind_trav)
summary(mm)


#Q-Q plot des residus
qqnorm(residus)
qqline(residus)

# QQ plot effets aleatoires
coeff <- ranef(mm)$location
qqnorm(coeff$'(Intercept)')
qqline(coeff$'(Intercept)') # Les effets aleatoires ont des residus lineaires

# Valeurs influentes
data_clean <- ind_trav
outliers <- data.frame()
while(grubbs.test(data_clean$res, type = 10, opposite = F, two.sided = F)$p.value<0.05){
  max <- which.max(abs(data_clean$res))         #- selection du + grand residu (ecart predit-reel)
  outliers <- rbind(outliers, data_clean[max,]) #- la table outliers prend cette ligne
  data_clean <- data_clean[-max,]}               #- on la retire de data_clean
outliers  # 2 outliers selon le test de Grubbs : placette 7 Beulet (solidagaie pure en 2015)
# et placettes  10 la Croix (mégaphorbiaie)



summary(mm)

# Conservatisme plus bas l'annee 3 
# plus bas dans les placettes a composition initiale de FU, GH, MG et PH (que BM)
# plus haut l'annee 3 dans les placettes restaurees de n1 a n2
# noter que le modele surevalue I08 des placettes ou I08 est bas.

# Diagramme a barres des effets fixes : 
effets_fixes <- as.data.frame(summary(mm)$coefficients)%>%
  filter(`Pr(>|t|)`<0.05)

df <- data.frame(Variable = rownames(effets_fixes),
                 Coefficient = effets_fixes[,"Estimate"],
                 IC_inf = effets_fixes[,"Estimate"]-1.96*effets_fixes[,"Std. Error"],
                 IC_sup = effets_fixes[,"Estimate"]+1.96*effets_fixes[,"Std. Error"])
ggplot(df, aes(x = Variable, y = Coefficient, fill = ifelse(Coefficient>0, "positive", "negative"))) + 
  geom_bar(stat = "identity", position = "identity", width = 0.5)+
  scale_fill_manual(values = c("negative" = "lightsalmon", "positive" = "lightskyblue1")) +
  geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup), width = 0.2)+
  labs(title = "Effets fixes du modèle", y = "Coefficient", x = "Variable")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 9, face = "bold" ))+
  guides(fill = "none")


rm(df, effets_fixes, mm, outliers, residus)

# Graphiques de diagnostic DHARMa
residus <- simulateResiduals(mm, plot = T)
testResiduals(residus)

#Comparaison IQF (I08) vs Cmoy -------------------
# Cmoy = conservatisme moyen (par placette)
# S = richesse specifique = nb d'especes dans une placette
# I08 (indice de qualite floristique) = Cmoy * sqrt(S)

# Problematique : doit on multiplier par sqrt(S) ? Est ce que cela induit un biais du 
# a un effort de prospection different selon les annees ?

plt1 <- ggplot(ind_trav, aes(x = num_an, y = I08, fill = num_an))+
  geom_boxplot()+
  labs(x = "Année", title = "I08 par an tous sites confondus")+
  theme_bw()+
  guides(fill = "none")+
  scale_fill_viridis(discrete = TRUE, option = "D")
plt2 <- ggplot(ind_trav, aes(x = num_an, y = Cmoy, fill = num_an))+
  geom_boxplot()+
  labs(x = "Année", title = "Cmoy par an tous sites confondus")+
  theme_bw()+
  guides(fill = "none")+
  scale_fill_viridis(discrete = TRUE, option = "D")
plt3 <- ggplot(ind_trav, aes(x = num_an, y = S, fill = num_an))+
  geom_boxplot()+
  labs(x = "Année", title = "S par an tous sites confondus")+
  theme_bw()+
  guides(fill = "none")+
  scale_fill_viridis(discrete = TRUE, option = "D")
ggarrange(plt1, plt2, plt3, ncol = 3, nrow = 1)
rm(plt1, plt2, plt3)

### Conclusion : il y a un biais de prospection : la richesse est significativement + haute la 3e annee
# de suivi que les autres annees sur l'ensemble des sites. 
# I08 ne doit pas etre interprete tel quel, c'est Cmoy qui sera retenu dans les analyses


# Comparaison 3 sites piezo (Combe du feu, Beulet, Chenet)-------
indice = "I02"
indices_piezo <- indices %>% filter(Site %in% c("Beulet", "Combe_du_feu", "Chenet")&physionomy %in% c("BM", "PH"))

plt <- ggplot(indices_piezo, aes(x = Site, y = !!sym(indice), fill = num_an)) +
  geom_boxplot() +
  labs(title = paste0("Variation de l'indice ", indice," sur tous les sites"))
plt




####### AFC multi_site ---------


# variables travaux (?)
vars = ind_trav %>% 
  select(plot,num_an, restau, depot_simpl, I02, I06, Cmoy, distance) 

tab <- tab_total %>%
  mutate_at(vars(nom_complet, location, phy_n1, Site,year), as.factor)
tab = merge(tab, vars, by = "plot")

tab <- tab %>% filter(phy_n1=="PH"&Site!="Dralet"&!id_plot%in%c("1908flo002_18"))
tab <- tab %>% filter(phy_n1=="BM"&!location%in% c("146flo002", "1916flo014"))
# tab <- tab %>% filter(phy_n1=="MG"&!location%in% c("1921flo007", "1921flo008"))
# tab <- tab %>% filter(phy_n1 == "FU")
# tab <- tab %>% filter(phy_n1 == "MC")


# Eliminer les sp rares : 
esp_courantes = tab %>% group_by(nom_red) %>%
  summarise(freq = n())%>%
  filter(freq>3) %>%
  pull(nom_red)

tab = tab %>% filter(nom_red %in% esp_courantes)

# Tableau de contingence
cont = reshape(tab[, c("plot", "nom_red", "rec")], idvar = 'plot', timevar = 'nom_red',
                  varying = list(as.character(unique(tab$nom_red))), direction = 'wide')
cont[is.na(cont)]=0
rownames(cont) = cont$plot
cont = cont[,-1]
data = cbind(cont, unique(tab %>% select(plot,phy_n1,Site,restau, depot_simpl,num_an, I02, I06, Cmoy, distance))[-1])
# data= t(cont)

### Calcul de l'AFC
AFC = CA(data, quali.sup = c("phy_n1", "Site", "restau", "depot_simpl","num_an"),quanti.sup = c("I02", "I06", "Cmoy", "distance"), graph = F, ncp = Inf)


# valeurs propres et inertie : regarder a partir de combien d'axes l'inertie diminue nettement
head(AFC$eig)
G3 <- fviz_screeplot(AFC, choice = "variance", ylab = "Variance expliquée (%)",
               xlab = "Axes factoriels", main = "(C) Explication de la variance par les axes factoriels")+
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 14, face = "bold"))
G3

# Representation de l'AFC sur les axes 1 et 2 : 
G1 <- fviz_ca_row(AFC, repel = T, habillage = data$num_an)+
  labs(title= "Placettes")
G2 <- fviz_ca_col(AFC, repel = T,  col.col= "cos2")+
  scale_color_gradient2(low = "lightblue", mid = "blue", high = "red", midpoint = 0.5, space = "Lab") +
  labs(title = "(B) Espèces sur les axes 1 et 2")+
  theme(text = element_text(family = "Times New Roman"),
  plot.title = element_text(size = 14, face = "bold"))
plot(ggarrange(G1, G2, ncol = 2, nrow = 1))

# Indices floristiques
G4 <- plot(AFC, choix = "quanti.sup", title = "(D) Indices floristiques sur les axes 1 et 2")+
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 14, face = "bold"))
G4


# Classification ascendance hierarchique : clustering par methode HCPC
res_HCPC <- HCPC(AFC, nb.clust = -1, graph = F)

# Representation clusters sur axes 1 et 2 
G1 <- fviz_cluster(res_HCPC, data, ellipse.type = "norm", 
                   ggtheme = theme_minimal(),repel = T, show.clust.cent = F) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed")+
  labs(title = "(A) Placettes par cluster sur les axes 1 et 2")+
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 14, face = "bold"))

# Graphique bilan
plt_combine <- ggarrange(G1, G2, G3, G4, ncol = 2, nrow = 2) 
plot(plt_combine)
# ggsave("tous_sites/AFC/AFC_BM.svg", plot = plt_combine , device = "svg", width = 12, height = 12)

## Grapiques de la CAH
# Inerties
svg("tous_sites/AFC/inertie_BM.svg", width = 8, height = 6)
plot(res_HCPC, choice = "bar")
dev.off()

# Dendrogramme
G1 <- fviz_dend(res_HCPC, palette = c("3","2","4"))+
  labs(title = "(A) Dendrogramme des clusters")+
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 20, face = "bold"))
G1
# ggsave("tous_sites/AFC/dendro_PH.svg", plot = G1 , device = "svg", width = 10, height = 6)
ggsave("tous_sites/AFC/dendro_BM.jpg", plot = G1 , width = 10, height = 6)


# Description des clusters
res_HCPC$desc.var$frequency    # par especes courantes
res_HCPC$desc.var$category     # par variables categorielles
res_HCPC$desc.var$quanti       # par variables quantitatives (indices floristiques)




# selon les axes 3 et 4 : 
G1 <- fviz_ca_row(AFC, axes = c(3,4), repel = T, habillage = data$Site)+
  labs(title = "Placettes")
G2 <- fviz_ca_col (AFC, axes = c(3,4), repel = T,  col.col= "cos2")+
  scale_color_gradient2(low = "lightblue", mid = "blue", high = "red", midpoint = 0.5, space = "Lab") +
  labs(title = "Espèces")
ggarrange(G1, G2, ncol = 2, nrow = 1)



# Valeurs indicatrices
data = unique(tab %>% select(nom_red,humidite,nutriment,cc))
data = data [,-1]
cont_t <- t(cont)
cont_t <- cont_t[order(row.names(cont_t)),]
data = cbind(cont_t, data)

# AFC
AFCt = CA(data, quanti.sup = c("humidite", "nutriment", "cc"), graph = F)


# plot des valeurs indicatrices sur les plans factoriels
plot(AFC, choix = "quanti.sup", title = "Influence des indices floristiques (axes 1-2)")
plot(AFC, choix = "quanti.sup", axes = c(3,4), title = "Influence des indices floristiques (axes 3-4)")

#correlations avec les axes
dimdesc(AFCt)$'Dim 1'$quanti
dimdesc(AFCt)$'Dim 2'$quanti
dimdesc(AFCt)$'Dim 3'$quanti
dimdesc(AFCt, axes = 1:4)$'Dim 4'$quanti

# nettoyage !
rm(esp_courantes,AFC, coeff, cont, data, data_clean, G1, G2, G3, G4, plt_combine, res_HCPC, tab, vars)




### Correlations entre les indices -----------------------------
pairs(indices[, c("I02", "I06", "Cmoy")])


library(corrplot)
# Calcul de la matrice de corrélation
# Méthode de Spearman (adaptée à données non normales, pour corrélation monotone pas forcément linéaire)
cor_matrix <- cor(indices[, c("I02", "I06", "Cmoy")], method = "spearman")

# Visualisation de la matrice de corrélation
svg("tous_sites/correlation_plot.svg", width = 5, height = 5)
par(family = "Times New Roman")
plt <- corrplot(cor_matrix, method = "circle", type = "upper", addCoef.col = "black", tl.col = "black", 
                col = colorRampPalette(c("red", "cyan3"))(200))
title(main = "Matrice de Corrélation", font.main = 1)
dev.off()

cor.test(indices$I02, indices$I06, method = "spearman")
cor.test(indices$I02, indices$Cmoy, method = "spearman")
cor.test(indices$Cmoy, indices$I06, method = "spearman")


# # Charger le package
# library(GGally)
# # Matrice de scatterplots
# ggpairs(indices[, c("I02", "I06", "Cmoy")])




# distances ------



ggplot(indices, aes(x = distance, y = Cmoy))+
  geom_point(color = "blue", size = 1.2)


