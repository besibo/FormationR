# Mise en mémoire des packages
library(tidyverse)
library(ggrepel)
library(ade4)

# ---- Analyse Factorielle des Correspondances : AFC ----

# Les données
data(doubs)
doubs

glimpse(doubs)
dat <- doubs$fish

# Visualisation de la zone d'étude
doubs$xy %>% 
  ggplot(aes(x, y)) +
  geom_path() +
  geom_label(aes(label = 1:30)) +
  coord_quickmap()

# 1. Regarder les données brutes
dat
# Somme des lignes
apply(dat, 1, sum)

# Somme des colonnes
apply(dat, 2, sum)

# 2. Faire l'AFC
# Nombre d'axes factoriels
nvar <- min(dim(dat)) - 1
res <- dudi.coa(dat, scannf = FALSE, nf = nvar)

# 3. Regarder les valeurs propres
inertia.dudi(res)

# 4 et 5. Visualiser les résultats et interpréter
scatter(res)

res

ggplot(data = res$li, aes(x = Axis3, y = Axis4)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_point() +
  geom_text_repel(aes(label = 1:30)) +
  geom_point(data = res$co, shape = 23,
             aes(x = Comp3, y = Comp4), fill = "red") +
  geom_text_repel(data = res$co, 
                  aes(x = Comp3, y = Comp4,
                      label = rownames(res$co))) +
  theme_bw()


# ---- ANOVA : Analysis Of Variance ----
# Importation des données
lumi <- read_csv("Data/chap15e1KneesWhoSayNight.csv")
lumi

# Description statistique des données
library(skimr)
lumi %>% 
  group_by(treatment) %>% 
  skim()

lumi %>% count(treatment)

# Exploration graphique
lumi %>% 
  ggplot(aes(x = treatment, y = shift)) +
  geom_violin() +
  geom_boxplot(alpha = 0.3, width = 0.1) +
  geom_jitter(width = 0.2, height = 0)

# Réalisation de l'ANOVA
res <- lumi %>% 
  aov(shift ~ treatment, data = .)

# Syntaxe équivalente
res <- aov(shift ~ treatment, data = lumi)

# Examen des résidus
par(mfrow = c(2,2))
plot(res)
par(mfrow = c(1,1))

# Résultats de l'ANOVA
summary(res)
anova(res)

# On rejette H0 : au moins une moyenne est différente des autres

# Tests a posteriori ou Post-Hoc
library(broom)
tidy(TukeyHSD(res))   # Test de Tukey


# ---- Analyse de variance covariance ----
# Importation des données
base <- read_delim(
  "Data/base.txt",
  delim = "\t",
  escape_double = FALSE,
  locale = locale(decimal_mark = ","),
  trim_ws = TRUE
)

# Visualisation des données
base %>% 
  filter(Age < 24) %>% 
  ggplot(aes(x = Age, y = CdK)) +
  geom_point(shape = 21, aes(fill = Zone)) +
  geom_smooth(method = "lm", aes(color = Zone)) +
  theme_bw()

# Réalisation de l'ANCOVA
ancov <- base %>% 
  filter(Age < 24) %>% 
  lm(CdK ~ Age * Zone, data = .)

# Affichage des résultats : méthode des contrastes
ancov
summary(ancov)

# Ré-organisation des catégories du facteur Zone
base <- base %>% 
  mutate(Zone = fct_relevel(Zone, "OI"))

# Régression linéaire pour la zone OI
base %>% 
  filter(Zone == "OI") %>% 
  lm(CdK~Age, data = .) %>% 
  plot()

