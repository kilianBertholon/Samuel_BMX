library(dplyr)
library(ggplot2)
library(lme4)

# Charger les données
base <- read.csv2("Analyse_BMX/dataset/base_bmx.csv")

# Convertir les variables en facteurs
base$Epreuves <- as.factor(base$Epreuves)
base$Saison <- as.factor(base$Saison)
base$Typologie_Epreuves <- as.factor(base$Typologie_Epreuves)
base$Categorie <- as.factor(base$Categorie)

base$Temps_T1 <- as.numeric(base$Temps_T1)
base$Temps_T2 <- as.numeric(base$Temps_T2)
base$Temps_T3 <- as.numeric(base$Temps_T3)
base$Temps_T4 <- as.numeric(base$Temps_T4)
base$Temps_Final <- as.numeric(base$Temps_Final)

base$Temps_T1_seul <- base$Temps_T1 
base$Temps_T2_seul <- base$Temps_T2 - base$Temps_T1
base$Temps_T3_seul <- base$Temps_T3 - base$Temps_T2
base$Temps_T4_seul <- base$Temps_T4 - base$Temps_T3
base$Temps_T5 <- base$Temps_Final - base$Temps_T4

base_clean <- base[complete.cases(base[, (length(base) - 4):length(base)]), ]
base_clean <- base_clean %>%
  filter(Temps_T4_seul >= 0 & Temps_T5 >= 0 & Temps_T4_seul >= 4 & Temps_T5 >= 4)

base_clean <- base_clean %>%
  group_by(Categorie, Epreuves, Saison, Typologie_Epreuves) %>%
  mutate_at(vars(Temps_T1_seul:Temps_T5), scale)

ggplot(base_clean) +
  geom_density(aes(x = Temps_T1_seul), fill = "blue", alpha = 0.5) +
  geom_density(aes(x = Temps_T2_seul), fill = "red", alpha = 0.5) +
  geom_density(aes(x = Temps_T3_seul), fill = "green", alpha = 0.5) +
  geom_density(aes(x = Temps_T4_seul), fill = "purple", alpha = 0.5) +
  geom_density(aes(x = Temps_T5), fill = "orange", alpha = 0.5) +
  xlab("Temps Intermédiaires") +
  ylab("Densité") +
  ggtitle("Courbe de densité des temps intermédiaires")

base_clean$Categorie <- as.factor(base_clean$Categorie)
base_clean$Epreuves <- as.factor(base_clean$Epreuves)
base_clean$Saison <- as.factor(base_clean$Saison)
base_clean$Typologie_Epreuves <- as.factor(base_clean$Typologie_Epreuves)
base_clean$Position_Start <- as.factor(base_clean$Position_Start)
classement <- c(1, 4, 2, 3, 6, 5, 8, 7)


# Encodage des rangs en tant que facteur avec les niveaux ordonnés
base_clean$Rang_T1 <- factor(base_clean$Rang_T1, levels = classement, ordered = TRUE)
base_clean$Rang_T2 <- factor(base_clean$Rang_T2, levels = classement, ordered = TRUE)
base_clean$Rang_T3 <- factor(base_clean$Rang_T3, levels = classement, ordered = TRUE)
base_clean$Rang_T4 <- factor(base_clean$Rang_T4, levels = classement, ordered = TRUE)
# base_clean$Classement_final <- factor(base_clean$Classement_final, levels = classement, ordered = TRUE)

base_model <- base_clean |> select(Position_Start, nom, Rang_T1, Rang_T2, Rang_T3, Rang_T4, Classement_final ,Temps_T1_seul,Temps_T2_seul, Temps_T3_seul, Temps_T4_seul, Temps_T5,Saison, Manche, Categorie, Typologie_Epreuves )
base_model$Classement_final <- as.numeric(base_model$Classement_final)

model <- lmerTest::lmer(Classement_final ~  Temps_T1_seul + Temps_T2_seul + Temps_T3_seul + Temps_T4_seul + Temps_T5 + (1 | Epreuves) + (1| Categorie) + (1| nom) + (1| Manche) , data = base_model)

# Extraire les coefficients estimés
coefficients <- fixef(model)

# Créer un data frame pour le graphique
data <- data.frame(Temps_Intermediaire = c("T1", "T2", "T3", "T4", "T5"),
                   Coefficient = coefficients[c("Temps_T1_seul", "Temps_T2_seul", "Temps_T3_seul", "Temps_T4_seul", "Temps_T5")])

# Créer le graphique en barres
ggplot(data, aes(x = Temps_Intermediaire, y = Coefficient)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Temps intermédiaire", y = "Coefficient estimé") +
  ggtitle("Importance des temps intermédiaires sur le classement final") +
  theme_minimal()

random_effects_epr <- ranef(model)$Epreuves
random_effects_cat <- ranef(model)$Categorie
random_effects_nom <- round(ranef(model)$nom,2) 
random_effects_Manche <- ranef(model)$Manche
# random_effects_Pos <- ranef(model)$Position_Start


