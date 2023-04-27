# ---- Rappels sur le tidverse ----

# Mise en mémoire de spakcages et des données
library(tidyverse)
library(palmerpenguins)
library(ggrepel)

penguins

# ---- calcul de résumés ----
penguins %>% 
  filter(!is.na(bill_length_mm)) %>% 
  summarise(moyenne_lon_bec = mean(bill_length_mm))

penguins %>% 
  summarise(moyenne_lon_bec = mean(bill_length_mm, na.rm = TRUE),
            variance = var(bill_length_mm, na.rm = TRUE))

penguins %>% 
  group_by(species, sex) %>% 
  summarise(moyenne = mean(bill_length_mm, na.rm = TRUE),
            variance = var(bill_length_mm, na.rm = TRUE),
            n_obs = n())

penguins %>% 
  group_by(species, sex) %>% 
  summarise(n_obs = n())

# Réagencement du tableau de résumé
penguins %>% 
  count(species, sex) %>% 
  filter(!is.na(sex)) %>% 
  pivot_wider(names_from = sex,
              values_from = n)

penguins %>% 
  count(species, sex) %>% 
  pivot_wider(names_from = sex,
              values_from = n) %>% 
  rename(missing = `NA`)

# Calcul de l'erreur standard grâce à mutate()
penguins %>% 
  group_by(species, sex) %>% 
  summarise(moyenne = mean(bill_depth_mm, na.rm = TRUE),
            variance = var(bill_depth_mm, na.rm = TRUE),
            n_obs = n()) %>% 
  mutate(s_e = sqrt(variance / n_obs))

# ---- Le tableau sharks : importation et mise en forme ----
library(readxl)
Sharks1 <- read_excel("Data/Sharks.xlsx", sheet = "Sphyrna lewini 1") %>% 
  select(Date:`Autres Poissons`) %>% 
  mutate(Id = "Indiv_1")

Sharks2 <- read_excel("Data/Sharks.xlsx", sheet = "Sphyrna lewini 2") %>% 
  select(Date:`Autres Poissons`) %>% 
  mutate(Id = "Indiv_2", 
         Truite = as.numeric(Truite))

Sharks3 <- read_excel("Data/Sharks.xlsx", sheet = "Sphyrna lewini 3") %>% 
  select(Date:`Autres Poissons`) %>% 
  mutate(Id = "Indiv_3")

Sharks4 <- read_excel("Data/Sharks.xlsx", sheet = "Sphyrna lewini 4") %>% 
  select(Date:`Autres Poissons`) %>% 
  mutate(Id = "Indiv_4") %>% 
  mutate_if(is_logical, as.numeric)

# Vérification que les noms des variables des 4 tableaux sont les mêmes
identical(names(Sharks1), names(Sharks2))
identical(names(Sharks1), names(Sharks3))
identical(names(Sharks1), names(Sharks4))

Sharks_raw <- bind_rows(Sharks1, Sharks2, Sharks3, Sharks4)

Sharks <- Sharks_raw %>% 
  pivot_longer(cols = Lieu:`Autres Poissons`,
               names_to = "Proie",
               values_to = "N_conso") %>% 
  # replace_na(list(N_conso = 0)) %>% 
  mutate(Id = as.factor(Id))

# Visualisation synthétique des données
Sharks %>% 
  ggplot(aes(x = Date, y = Proie, 
             size = N_conso, color = Proie)) +
  geom_point(alpha = 0.4) +
  facet_wrap(~ Id, ncol = 1) +
  theme_bw()

# Utilisation de reframe() pour calculer plusieurs valeurs à la fois
Sharks %>% 
  group_by(Id) %>% 
  reframe(quantile(N_conso, na.rm = TRUE))

penguins %>% 
  group_by(species, sex) %>% 
  reframe(mean_cl_normal(bill_length_mm))

# Alternative à group_by() : l'argument .by
penguins %>% 
  summarise(moyenne = mean(bill_length_mm, na.rm = TRUE),
            variance = var(bill_length_mm, na.rm = TRUE),
            n_obs = n(),
            .by = c(species, sex))


# ---- Régression linéaire : visualisation et utilisation de map() ----

# Une droite de régression par catégorie
penguins %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = body_mass_g, y = bill_depth_mm,
             color = species, shape = sex)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Une seule droite de régression par espèce
penguins %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = body_mass_g, y = bill_depth_mm)) +
  geom_point(aes(color = species, shape = sex)) +
  geom_smooth(aes(color = species),
              method = "lm", se = FALSE)

# Utilisation de nest/map/unnest
library(broom)
penguins %>% 
  filter(!is.na(sex)) %>% 
  nest(data = -c(species, sex)) %>% 
  mutate(regression = map(data, 
                          ~lm(.x$bill_depth_mm ~ .x$body_mass_g)),
         tidied = map(regression, ~tidy(.x))) %>% 
  unnest(tidied) %>% 
  filter(p.value <= 0.05)

# ---- Travail avec plusieurs tableaux : les joints ----
library(nycflights13)
flights
names(flights)

flights %>% 
  group_by(origin) %>% 
  summarise(n_vols = n())

flights %>% 
  count(carrier) %>% 
  arrange(desc(n)) %>% 
  left_join(airlines)

airlines


# ---- Création de fonctions ----
err_std <- function(x, ...) {
 
  stopifnot(is.numeric(x))
  
   variance <- var(x, ...)
   effectif <- length(x)
   es <- sqrt(variance / effectif)
  
   return(es)
}

vec <- c(34, 423, 2, 3, 34, 5, 78, NA)

err_std(x = vec)
err_std(x = vec, na.rm = TRUE)

var(vec, na.rm = TRUE)


# ---- le principe des fonction génériques : exemple de summary() ----
summary(vec)
summary(penguins)
res <- lm(bill_length_mm ~ body_mass_g, data = penguins)
summary(res)

library(marmap)
data(florida)
summary(florida)


# ---- Les boucles "for" ----

# Modèle dynamique
# Création d'un objet vide qui contiendra les résultats
N <- numeric()

# Condition initiale
N0 <- 5

# Valeur de paramètre
r <- 0.01   # taux de croissance instantannée
K <- 30

N[1] <- N0
N

for (i in 2:500) {
  N[i] <- N[i-1] + r * N[i-1] - r / K * N[i-1]^2
}

N
plot(N)


# 2. Le paradoxe de l'anniversaire


# n : le nombre de personnes qui se trouvent dans une pièce
# rep : le nombre de fois ou on réalise l'expérience
# La fonction renvoie la probabilité qu'au moins 2 personnes
# dans la pièce aient la même date d'anniversaire
proba <- function(n, rep = 1000) {
  
  res <- numeric()
  
  for (i in 1:rep) {
    groupe <- sample(1:365, n, replace = TRUE)
    res[i] <- length(groupe) != length(unique(groupe))
  }
  
  # Fréquence des dates dupliquées
  mean(res)
}

# Test de la fonction
proba(n = 10, rep = 10000)

# Application de la fonction poru plusieurs valeurs de n avec map_dbl()
out <- tibble(n = 2:50) %>% 
  mutate(proba = map_dbl(n, ~proba(.x, rep = 10000)))

out

# Visualisation des résultats
out %>% 
  ggplot(aes(x = n, y = proba)) +
  geom_line() +
  geom_point() +
  geom_smooth(se = FALSE) +
  geom_hline(yintercept = 0.5, linetype = 2) +
  geom_vline(xintercept = 23, linetype = 2) +
  geom_text_repel(data = out %>% filter(n == 23),
                  aes(label = n), nudge_x = 1, nudge_y = 0.1)
