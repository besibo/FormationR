# ---- Analyse en composantes principales : ACP ----

# Mise en mémoire des packages
library(tidyverse)
library(ade4)       # Pour toutes les analyses multivariées
library(ggrepel)

# Utilisation d'un thème "fait maison"
source("ggplot_theme.R")
theme_set(theme_benoit())

# Accès aux données
data("olympic")
dat <- olympic$tab
dat

# 1. Examen des données brutes
dat

# 2. Examen des corrélations
round(cor(dat), 3)

# 3. Réalisation de l'ACP
nvar <- ncol(dat)
res <- dudi.pca(dat, scannf = FALSE, nf = nvar)

help <- inertia.dudi(res, col.inertia = TRUE)

# 4. Examen des valeurs propres
help$tot.inertia

# 5. Calcul de la valeur seuil
100 / nvar

# 6. Quelles variables retenir : examen des contributions absolues des variables
round(help$col.abs, 2)

# contributions relatives : lire en lignes !
abs(round(help$col.rel, 2))

# 7 et 8. Cercle des corrélations
s.corcircle(res$co, xax = 1, yax = 2)
s.corcircle(res$co, xax = 1, yax = 3)

# 9 et 10. Graphique des individus
s.label(res$li, xax = 1, yax = 2)

par(mfrow = c(1, 2))
s.corcircle(res$co, xax = 1, yax = 2)
s.label(res$li, xax = 1, yax = 2)
par(mfrow = c(1, 1))

out <- res$li %>% 
  mutate(score = olympic$score) %>% 
  as_tibble()

out

out %>% 
  ggplot(aes(x = Axis1, y = Axis3)) +
  geom_point(aes(fill = score), shape = 21) +
  geom_text_repel(aes(label = 1:nrow(out))) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  scale_fill_viridis_c(option = "B")


# ---- Quelques notes sur les polices dans les graphiques ----
# Première façøn de faire
library(systemfonts)
system_fonts() %>% 
  filter(family == "Gill Sans") %>% 
  pull(name)

# Deuxième façon de faire
library(extrafont)
font_import()
fonttable() %>% view()

# Package pour ajouter du markdown dans les titres (couleurs, gras, italique...)
library(ggtext)

out %>%
  ggplot(aes(x = Axis1, y = Axis3)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_point(aes(fill = score), shape = 21) +
  geom_text_repel(aes(label = 1:nrow(out)), family = "ArcherPro-Book") +
  scale_fill_viridis_c(option = "B") +
  theme_bw() +
  labs(
    x = "Axe 1 (34%)",
    y = "Axe 2 (26%)",
    fill = "Score",
    title = "Mon titre **principal**",
    subtitle = "Mon sous-titre",
    caption = "La source des données"
  ) +
  theme(plot.title = element_markdown())

library(glue)
data <- tibble(
  bactname = c("Staphylococcaceae", "Moraxella", "Streptococcus", "Acinetobacter"),
  OTUname = c("OTU 1", "OTU 2", "OTU 3", "OTU 4"),
  value = c(-0.5, 0.5, 2, 3)
)

data %>% mutate(
  color = c("#009E73", "#D55E00", "#0072B2", "#000000"),
  name = glue("<i style='color:{color}'>{bactname}</i> ({OTUname})"),
  name = fct_reorder(name, value)
)


# ---- Autre exemple d'ACP : le tableau PBDE ----
library(janitor)

# Importation et mise en forme des données
BD <- read_delim(
  "Data/BD.csv",
  delim = "\t",
  escape_double = FALSE,
  locale = locale(decimal_mark = ","),
  trim_ws = TRUE
) %>% 
  clean_names()

names(BD)
BD

BD %>% 
  count(esp_id, sex)

# Récupération des données pour l'ACP :
# Uniquement les PBDE, et suppression des NAs
tab_acp <- BD %>% 
  filter(!is.na(pbde28)) %>% 
  select(contains("pbde"))

# Récupération des données pour l'interprétation
tab_var <- BD %>% 
  filter(!is.na(pbde28)) %>% 
  select(!contains("pbde"))


# 1. Examen des données brutes
tab_acp

# 2. Examen des corrélations
round(cor(tab_acp), 3)

# 3. Réalisation de l'ACP
nvar <- ncol(tab_acp)
res <- dudi.pca(tab_acp, scannf = FALSE, nf = nvar)

help <- inertia.dudi(res, col.inertia = TRUE)

# 4. Examen des valeurs propres
help$tot.inertia

# 5. Calcul de la valeur seuil
100 / nvar

# 6. Quelles variables retenir : examen des contributions absolues des variables
round(help$col.abs, 2)

# contributions relatives : lire en lignes !
abs(round(help$col.rel, 2))

# 7 et 8. Cercle des corrélations
s.corcircle(res$co, xax = 1, yax = 2)

# 9 et 10. Graphique des individus
s.label(res$li, xax = 1, yax = 2)

# interprétation
global <- tab_var %>% 
  bind_cols(res$li)

global %>% 
  ggplot(aes(x = Axis1, y = Axis2, color = esp_id)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_point() +
  theme_bw()

global %>% 
  ggplot(aes(x = long)) +
  geom_density(fill = "grey20", alpha = 0.2) +
  facet_wrap(~esp_id, ncol = 1)

# Est-ce que la taile a une influence ? Non...
global <- global %>% 
  group_by(esp_id) %>% 
  mutate(taille = cut_number(long, 4),
         taille = fct_collapse(taille,
                               petit = levels(taille)[c(1,5)],
                               moyen = levels(taille)[c(2,6)],
                               grand = levels(taille)[c(3,7)],
                               tres_grand = levels(taille)[c(4,8)]))
  
  
global %>% 
  ggplot(aes(x = taille, y = long)) +
  geom_jitter(width = 0.2, height = 0) +
  facet_wrap(~esp_id)


global %>% 
  ggplot(aes(x = Axis1, y = Axis2, color = taille)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_point() +
  theme_bw() +
  facet_grid(taille~esp_id)


# Qu'en est-il de la géographie
global %>% 
  ggplot(aes(x = ech_long, y = ech_lat, color = esp_id)) +
  geom_point() +
  coord_map()

library(sf)

# Pour installer un package hébergé chez github
install.packages("remotes")
remotes::install_github("ropensci/rnaturalearthhires")

# Pour les traits de côte
library(rnaturalearth)
library(rnaturalearthhires)

# Création d'un objet géographique
glob_geo <- global %>% 
  filter(!is.na(ech_long), 
         !is.na(ech_lat)) %>% 
  st_as_sf(coords = c("ech_long", "ech_lat"), crs = "EPSG:4326") 

# Détermination de l'emprise géographique
st_bbox(glob_geo)

# Récupération des données bathymétriques
library(marmap)
bathy <- getNOAA.bathy(-5, 2, 43, 51, resolution = 1,
                       keep = TRUE)

bathy_geo <- as.xyz(bathy)
head(bathy_geo)

# Récupération des traits de côte
pays <- ne_countries(country = c("France", "Spain", "United Kingdom"),
                     scale = "large", returnclass = "sf")

# Création des cartes
ggplot(data = pays) +
  geom_contour(data = bathy_geo, aes(x = V1, y = V2, z = V3),
               binwidth = 200, color = "grey90", linewidth = 0.3) +
  geom_contour(data = bathy_geo, aes(x = V1, y = V2, z = V3),
               binwidth = 1000, color = "grey70", linewidth = 0.4) +
  geom_sf() +
  geom_sf(data = glob_geo, aes(color = Axis2, size = Axis2),
          alpha = 0.5) +
  coord_sf(xlim = c(-5, 2), ylim = c(43, 51)) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal() +
  facet_wrap(~esp_id)
