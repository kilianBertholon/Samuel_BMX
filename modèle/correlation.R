base <- read.csv2("Analyse_BMX/dataset/base_bmx.csv")

base <- base[complete.cases(base) & !grepl("DNS|DNF", base$Temps_Final), ]
base <- base[complete.cases(base) & !grepl("DNS|DNF", base$Temps_T1), ]
base <- base[complete.cases(base) & !grepl("DNS|DNF", base$Temps_T2), ]
base <- base[complete.cases(base) & !grepl("DNS|DNF", base$Temps_T3), ]
base <- base[complete.cases(base) & !grepl("DNS|DNF", base$Temps_T4), ]

base$Position_Start <- as.numeric(base$Position_Start)
base$Rang_T1 <- as.numeric(base$Rang_T1, na.rm = TRUE)
base$Rang_T2 <- as.numeric(base$Rang_T2, na.rm = TRUE)
base$Rang_T3 <- as.numeric(base$Rang_T3, na.rm = TRUE)
base$Rang_T4 <- as.numeric(base$Rang_T4, na.rm = TRUE)
base$Classement_final <- as.numeric(base$Classement_final, na.rm = TRUE)

base <- base[complete.cases(base),]

correlation_matrix <- round(cor(base[,c("Position_Start", "Rang_T1", "Rang_T2", "Rang_T3", "Rang_T4", "Classement_final")], method = "kendall"), 2)

library(corrplot)
library(ggcorrplot)
matrice_cor_Spear <- ggcorrplot(
  correlation_matrix,
  type = "lower",
  lab = TRUE,
  lab_size = 4,
  method = "square",
  title = "Matrice de corrélation",
  ggtheme = ggplot2::theme_gray
) + scale_fill_gradientn(colors = c("#001EFF", "#FFFF00", "#FF002B"),
                         limits = c(0,1), name="Kendall correlation", label=scales::comma)

###Voir pour faire" corrélation pour chaque manche et ou chaque piste
library(dplyr)
differente_manche <- unique(base$Manche)
differente_epreuves <- unique(base$Epreuves)

Epreuves_Sakarya <- base |> filter(Epreuves == "Sakarya")
correlation_matrix_Sakarya <- round(cor(Epreuves_Sakarya[,c("Position_Start", "Rang_T1", "Rang_T2", "Rang_T3", "Rang_T4", "Classement_final")], method = "kendall"), 2)

Epreuves_Papendal <- base |> filter(Epreuves == "Papendal")
correlation_matrix_Papendal <- round(cor(Epreuves_Papendal[,c("Position_Start", "Rang_T1", "Rang_T2", "Rang_T3", "Rang_T4", "Classement_final")], method = "kendall"), 2)

Epreuves_Manchester <- base |> filter(Epreuves == "Manchester")
correlation_matrix_Manchester <- round(cor(Epreuves_Manchester[,c("Position_Start", "Rang_T1", "Rang_T2", "Rang_T3", "Rang_T4", "Classement_final")], method = "kendall"), 2)

Epreuves_Glasgow <- base |> filter(Epreuves == "Glasgow")
correlation_matrix_Glasgow  <- round(cor(Epreuves_Glasgow[,c("Position_Start", "Rang_T1", "Rang_T2", "Rang_T3", "Rang_T4", "Classement_final")], method = "kendall"), 2)

Epreuves_Santiago_del_Estero <- base |> filter(Epreuves == "Santiago del Estero")
correlation_matrix_Santiago_del_Estero <- round(cor(Epreuves_Santiago_del_Estero[,c("Position_Start", "Rang_T1", "Rang_T2", "Rang_T3", "Rang_T4", "Classement_final")], method = "kendall"), 2)

Epreuves_Zolder <- base |> filter(Epreuves == "Zolder")
correlation_matrix_Zolder <- round(cor(Epreuves_Zolder[,c("Position_Start", "Rang_T1", "Rang_T2", "Rang_T3", "Rang_T4", "Classement_final")], method = "kendall"), 2)

Epreuves_Bogota <- base |> filter(Epreuves == "Bogota")
correlation_matrix_Bogota <- round(cor(Epreuves_Bogota[,c("Position_Start", "Rang_T1", "Rang_T2", "Rang_T3", "Rang_T4", "Classement_final")], method = "kendall"), 2)

Epreuves_RockHill <- base |> filter(Epreuves == "RockHill")
correlation_matrix_RockHill <- round(cor(Epreuves_RockHill[,c("Position_Start", "Rang_T1", "Rang_T2", "Rang_T3", "Rang_T4", "Classement_final")], method = "kendall"), 2)

recap <- data.frame(c("Sakarya", "Papendal", "Manchester", "Glascow", "Santiago_del_Estero", "Zolder", "Bogota", "RockHill", "all"))
recap <- recap |> mutate(Position_Start = c(0.33,0.71,0.43, 0.80, 0.61,0.53, 0.68, 0.51, 0.58)) |>
  mutate(T1 = c(0.44,0.43,0.51, 0.49, 0.55, 0.56, 0.53, 0.50, 0.51)) |>
  mutate(T2 = c(0.58,0.59,0.57, 0.61, 0.70, 0.77, 0.68, 0.64, 0.65)) |>
  mutate(T3 = c(0.81,0.69,0.83, 0.80, 0.85, 0.87, 0.84, 0.81, 0.81)) |>
  mutate(T4 = c(0.88,0.79,0.91, 0.87, 0.92,0.94, 0.92, 0.93, 0.90)) |>
  mutate(n = c(1515,1710, 694, 933, 1450, 1991, 2264, 614, 11256))

recap <- recap |> rename( "Localisation" =c..Sakarya....Papendal....Manchester....Glascow....Santiago_del_Estero... )


####----------------------------------- Même choses pour les manches 
Epreuves_round1 <- base |> filter(Manche == "Round1")
correlation_matrix_round1 <- round(cor(Epreuves_round1[,c("Position_Start", "Rang_T1", "Rang_T2", "Rang_T3", "Rang_T4", "Classement_final")], method = "kendall"), 2)

Epreuves_Repechage <- base |> filter(Manche == "Repechage")
correlation_matrix_Repechage <- round(cor(Epreuves_Repechage[,c("Position_Start", "Rang_T1", "Rang_T2", "Rang_T3", "Rang_T4", "Classement_final")], method = "kendall"), 2)

Epreuves_Huitieme_final <- base |> filter(Manche == "Huitieme-final")
correlation_matrix_Huitieme_final <- round(cor(Epreuves_Huitieme_final[,c("Position_Start", "Rang_T1", "Rang_T2", "Rang_T3", "Rang_T4", "Classement_final")], method = "kendall"), 2)

Epreuves_Quart_final <- base |> filter(Manche == "Quart-final")
correlation_matrix_Quart_final <- round(cor(Epreuves_Quart_final[,c("Position_Start", "Rang_T1", "Rang_T2", "Rang_T3", "Rang_T4", "Classement_final")], method = "kendall"), 2)

Epreuves_Demi_final <- base |> filter(Manche == "Demi-final")
correlation_matrix_Demi_final <- round(cor(Epreuves_Demi_final[,c("Position_Start", "Rang_T1", "Rang_T2", "Rang_T3", "Rang_T4", "Classement_final")], method = "kendall"), 2)

Epreuves_Finale <- base |> filter(Manche == "Finale")
correlation_matrix_Finale <- round(cor(Epreuves_Finale[,c("Position_Start", "Rang_T1", "Rang_T2", "Rang_T3", "Rang_T4", "Classement_final")], method = "kendall"), 2)

Epreuves_Trente_deuxieme_final <- base |> filter(Manche == "Trente-deuxieme-final")
correlation_matrix_Trente_deuxieme_final <- round(cor(Epreuves_Trente_deuxieme_final[,c("Position_Start", "Rang_T1", "Rang_T2", "Rang_T3", "Rang_T4", "Classement_final")], method = "kendall"), 2)

Epreuves_Seizieme_final <- base |> filter(Manche == "Seizieme-final")
correlation_matrix_Seizieme_final <- round(cor(Epreuves_Seizieme_final[,c("Position_Start", "Rang_T1", "Rang_T2", "Rang_T3", "Rang_T4", "Classement_final")], method = "kendall"), 2)

library(dplyr)
###--------------------------------------Par sexe 
base_homme <- base |> filter(Categorie == "MenElite" | Categorie == "MenUnder23")
correlation_matrix_homme <- round(cor(base_homme[,c("Position_Start", "Rang_T1", "Rang_T2", "Rang_T3", "Rang_T4", "Classement_final")], method = "kendall"), 2)


base_femme <- base |> filter(Categorie == "WomenElite" | Categorie == "WomenUnder23")
correlation_matrix_femme <- round(cor(base_femme[,c("Position_Start", "Rang_T1", "Rang_T2", "Rang_T3", "Rang_T4", "Classement_final")], method = "kendall"), 2)

library(ggplot2)
library(reshape2)
recap_hf <- data.frame(Men = c(0.54,0.50,0.64,0.80,0.89),
                       Women = c(0.81, 0.56, 0.73,0.86,0.91),
                       All = c(0.58,0.51,0.65,0.81,0.9))
rownames(recap_hf) <- c("Grille_départ", "T1", "T2", "T3", "T4")

# Convertir les noms de ligne en une colonne séparée
recap_hf$Intermediaires <- rownames(recap_hf)

# Transformer le dataframe en format long
recap_hf_long <- melt(recap_hf, id.vars = "Intermediaires", variable.name = "Sexe", value.name = "Tau_b")

# Convertir la colonne "Intermediaires" en un facteur ordonné
recap_hf_long$Intermediaires <- factor(recap_hf_long$Intermediaires, levels = c("Grille_départ", "T1", "T2", "T3", "T4"))

# Créer le graphique
# Créer le graphique
ggplot(recap_hf_long, aes(x = Intermediaires, y = Tau_b, group = Sexe, color = Sexe, shape = Sexe)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_hline(aes(yintercept = 0.5), linetype = "dashed", color = "black") +
  geom_hline(aes(yintercept = 0.7), linetype = "dashed", color = "black") +
  geom_hline(aes(yintercept = 1), linetype = "dashed", color = "black") +
  annotate("text", x = 5, y = 0.5, label = "Corrélation modérée", hjust = 0.5, vjust = -1.5) +
  annotate("text", x = 5, y = 0.7, label = "Corrélation forte", hjust = 0.5, vjust = -1.5 ) +
  annotate("text", x = 5, y = 1, label = "Corrélation parfaite", hjust = 0.5, vjust = 1.5 ) +
  scale_color_manual(values = c("Men" = "black", "Women" = "gray", "All" = "red")) +
  theme_bw() +
  theme(text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 14),
        legend.position = "bottom") +
  labs(x = "Intermédiaires", y = "Tau b de Kendall", title = "Evolution des valeurs du tau b de Kendall au cours d'une course", color = "Sexe", shape = "Sexe")

##### Calcul différence significatives entre les pistes 
Sakarya <- recap[1,2:6]
Papendal <- recap[2,2:6]
Manchester <- recap[3,2:6]
Glascow <- recap[4,2:6]
Santiago_del_estero <- recap[5,2:6]
Zolder <- recap[6,2:6]
Bogota <- recap[7,2:6]
Rockhill <- recap[8,2:6]

n_Sakarya <- 1515
n_Papendal <- 1710
n_Manchester <- 694
n_Glascow <- 933
n_Santiago <- 1450
n_Zolder <- 1991
n_Bogota <- 2264
n_RockHill <- 614
n_Sakarya <- 1515

correlations <- list(Sakarya, Papendal, Manchester, Glascow, Santiago_del_estero, Zolder, Bogota,Rockhill)

sizes <- list(n_Sakarya, n_Papendal, n_Manchester, n_Glascow, n_Santiago, n_Zolder, n_Bogota, n_RockHill)



comparaison_Sakarya_Bogota <- data.frame(Intermediaires = c("Grille", "T1", "T2", "T3", "T4"),
                                         P_Value = c(0.0001, 0.0003, 0.0001, 0.005, 0.0001))
comparaison_Sakarya_Rockhill <- data.frame(Intermediaires = c("Grille", "T1", "T2", "T3", "T4"),
                                         P_Value = c(0.0001, 0.100, 0.400, 1, 0.0001))
comparaison_Sakarya_Zolder <- data.frame(Intermediaires = c("Grille", "T1", "T2", "T3", "T4"),
                                           P_Value = c(0.0001, 0.0001, 0.0001, 0.0001, 0.0001))
comparaison_Sakarya_Santiago <- data.frame(Intermediaires = c("Grille", "T1", "T2", "T3", "T4"),
                                          P_Value = c(0.0, 0.0001, 0.0001, 0.0004, 0.0001))
comparaison_Sakarya_Papendal <- data.frame(Intermediaires = c("Grille", "T1", "T2", "T3", "T4"),
                                           P_Value = c(0, 0.727, 0.668, 0.00001, 0))

comparaison_Sakarya_Manchester <- data.frame(Intermediaires = c("Grille", "T1", "T2", "T3", "T4"),
                                           P_Value = c(0.01, 0.487, 0.748, 0.18, 0.0009))
comparaison_Sakarya_Glasgow <- data.frame(Intermediaires = c("Grille", "T1", "T2", "T3", "T4"),
                                             P_Value = c(0.0, 0.12, 0.265, 0.49, 0.30))


# Créer une fonction pour calculer la corrélation de Kendall
# Créer une fonction pour calculer la corrélation de Kendall et la p-valeur
calculate_kendall_cor <- function(df, start, end) {
  correlations <- c()
  p_values <- c()  # pour enregistrer les p-valeurs
  intermediaries <- c("Position_Start", "Rang_T1", "Rang_T2", "Rang_T3", "Rang_T4")
  for(interm in intermediaries) {
    subset_df <- df[df[[interm]] >= start & df[[interm]] <= end, ]
    cor_test <- cor.test(subset_df[[interm]], subset_df$Classement_final, method = "kendall")  # utiliser cor.test à la place de cor
    correlations <- round(c(correlations, cor_test$estimate),3)  # utiliser cor_test$estimate pour obtenir la corrélation
    p_values <- c(p_values, cor_test$p.value)  # ajouter la p-valeur à la liste des p-valeurs
  }
  return(list(correlations = correlations, p_values = p_values))  # retourner une liste contenant les corrélations et les p-valeurs
}


# Calculer la corrélation de Kendall pour le top 3 à chaque intermédiaire
correlations_top3 <- calculate_kendall_cor(base, 1, 3)

# Calculer la corrélation de Kendall pour les coureurs en 4ème à 8ème position à chaque intermédiaire
correlations_4_to_8 <- calculate_kendall_cor(base, 4, 8)

recap_top3 <- c(0.557,	0.217,	0.461	,0.531,	0.638)
recap_homme <- c(0.675,	0.317,	0.407,	0.602	,0.771)

cor_hf <- data.frame(recap_top3, recap_homme)
test <- Epreuves_Glasgow|> filter(Rang_T1 == 1 | Rang_T1 == 2 | Rang_T1 == 3)
n_size <- list(797,913)

# Convertir les coefficients de corrélation de Kendall (Tau) en coefficients de corrélation de Pearson (r)
r_values <- lapply(cor_hf, function(tau) sin(pi/2 * tau))

# Puis utilisez la fonction lapply pour transformer toutes les corrélations en scores z
z_scores <- lapply(cor_hf, function(r) 0.5 * log((1 + r) / (1 - r)))

# Calculez l'erreur standard pour chaque piste
se_diff <- lapply(n_size, function(n) sqrt(1/(n - 3)))


# Choisir deux pistes et un point intermédiaire pour comparer
i <- 1
j <- 2
k <-  # Par exemple, pour le premier point intermédiaire

# Calculer la différence entre les scores z
z_diff <- z_scores[[i]][k] - z_scores[[j]][k]

# Calculer la différence standard d'erreur
se_diff_ij <- sqrt(se_diff[[i]]^2 + se_diff[[j]]^2)

# Calculer le score z pour la différence
z_score <- as.numeric(z_diff / se_diff_ij)

# Calculer la valeur p pour le test
p_value <- 2 * (1 - pnorm(abs(z_score)))
p_value_ajust <- p.adjust(p_value, method = "bonferroni")

print(p_value_ajust)




base_1 <- base |> filter(Position_Start == 1)
base_2 <- base |> filter(Position_Start == 2)
base_3 <- base |> filter(Position_Start == 3)
base_4 <- base |> filter(Position_Start == 4)
base_5 <- base |> filter(Position_Start == 5)
base_6 <- base |> filter(Position_Start == 6)
base_7 <- base |> filter(Position_Start == 7)
base_8 <- base |> filter(Position_Start == 8)

base_1_1 <- base_1 |> filter(Classement_final == 1 | Classement_final ==  2| Classement_final == 3)
result_1 <- nrow(base_1_1)/nrow(base_1)

base_2_2 <- base_2 |> filter(Classement_final == 1 | Classement_final ==  2| Classement_final == 3)
result_2 <- nrow(base_2_2)/nrow(base_2)

base_3_3 <- base_3 |> filter(Classement_final == 1 | Classement_final ==  2| Classement_final == 3)
result_3 <- nrow(base_3_3)/nrow(base_3)

base_4_4 <- base_4 |> filter(Classement_final == 1 | Classement_final ==  2| Classement_final == 3)
result_4 <- nrow(base_4_4)/nrow(base_4)

base_5_5 <- base_5 |> filter(Classement_final == 1 | Classement_final ==  2| Classement_final == 3)
result_5 <- nrow(base_5_5)/nrow(base_5)

base_6_6 <- base_6 |> filter(Classement_final == 1 | Classement_final ==  2| Classement_final == 3)
result_6 <- nrow(base_6_6)/nrow(base_6)

base_7_7 <- base_7 |> filter(Classement_final == 1 | Classement_final ==  2| Classement_final == 3)
result_7 <- nrow(base_7_7)/nrow(base_7)

base_8_8 <- base_8 |> filter(Classement_final == 1 | Classement_final ==  2| Classement_final == 3)
result_8 <- nrow(base_8_8)/nrow(base_8)

