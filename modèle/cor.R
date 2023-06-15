library(dplyr)
library(report)
library(corrr)
library(apaTables)
library(rempsyc)
library(psych)
library(gt)
library(ggplot2)
###Matrice de corrélation 
base_donnee <- read.csv2("dataset/base_bmx.csv", sep =";")
base_donnee_rang <- base_donnee |> select(Position_Start, Rang_T1, Rang_T2, Rang_T3, Rang_T4, Classement_final, Categorie, Epreuves, Manche)

base_donnee_rang$Position_Start <- as.numeric(base_donnee_rang$Position_Start)
base_donnee_rang$Rang_T3 <- as.numeric(base_donnee_rang$Rang_T3)
base_donnee_rang$Rang_T4 <- as.numeric(base_donnee_rang$Rang_T4)
base_donnee_rang$Rang_T1 <- as.numeric(base_donnee_rang$Rang_T1)
base_donnee_rang$Rang_T2 <- as.numeric(base_donnee_rang$Rang_T2)
base_donnee_rang$Classement_final <- as.numeric(base_donnee_rang$Classement_final)

base_donnee_rang <- base_donnee_rang[complete.cases(base_donnee_rang)& !grepl("DNS|DNF", base_donnee_rang$Rang_T4), ]

correlation_matrix <- cor(base_donnee_rang[,c("Position_Start", "Rang_T1", "Rang_T2", "Rang_T3", "Rang_T4", "Classement_final")], method = "kendall")

cor_t2_fin <- cor.test(base_donnee_rang$Rang_T2, base_donnee_rang$Classement_final, method = "kendall", conf.level = .95)

datasummary_correlation(base_donnee_rang[,c("Position_Start", "Rang_T1", "Rang_T2", "Rang_T3", "Rang_T4", "Classement_final")], method = "kendall")



data <- base_donnee_rang[,c("Position_Start", "Rang_T1", "Rang_T2", "Rang_T3", "Rang_T4", "Classement_final")]

test2 <- rstatix::cor_mat(data, method = "kendall")
test3 <- rstatix::cor_mark_significant(test2)
test5 <- as.data.frame(test3)

base_donnee_Posi <- base_donnee_rang |> select(Position_Start, Classement_final)
base_donnee_Posi_1 <- base_donnee_Posi |> filter(Position_Start == 1) |> mutate(mean = mean(Classement_final))
base_donnee_Posi_2 <- base_donnee_Posi |> filter(Position_Start == 2) |> mutate(mean = mean(Classement_final))
base_donnee_Posi_3 <- base_donnee_Posi|> filter(Position_Start == 3) |> mutate(mean = mean(Classement_final))
base_donnee_Posi_4 <- base_donnee_Posi|> filter(Position_Start == 4) |> mutate(mean = mean(Classement_final))
base_donnee_Posi_5 <- base_donnee_Posi|> filter(Position_Start == 5) |> mutate(mean = mean(Classement_final))
base_donnee_Posi_6 <- base_donnee_Posi|> filter(Position_Start == 6) |> mutate(mean = mean(Classement_final))
base_donnee_Posi_7 <- base_donnee_Posi|> filter(Position_Start == 7) |> mutate(mean = mean(Classement_final))
base_donnee_Posi_8 <- base_donnee_Posi|> filter(Position_Start == 8) |> mutate(mean = mean(Classement_final))

recap <- as.data.frame(c(1,2,3,4,5,6,7,8))
recap <- recap |>mutate(moyenne = c(base_donnee_Posi_1[1,3],
                                    base_donnee_Posi_2[1,3],
                                    base_donnee_Posi_3[1,3],
                                    base_donnee_Posi_4[1,3],
                                    base_donnee_Posi_5[1,3],
                                    base_donnee_Posi_6[1,3],
                                    base_donnee_Posi_7[1,3],
                                    base_donnee_Posi_8[1,3]))
colnames(recap) <- c("Position de départ", "Moyenne Classement Final")


base_donnee_rang_T1 <- base_donnee_rang |> select(Rang_T1, Classement_final)
base_donnee_rang_T1_1 <- base_donnee_rang_T1 |> filter(Rang_T1 == 1) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T1_2 <- base_donnee_rang_T1 |> filter(Rang_T1 == 2) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T1_3 <- base_donnee_rang_T1 |> filter(Rang_T1 == 3) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T1_4 <- base_donnee_rang_T1 |> filter(Rang_T1 == 4) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T1_5 <- base_donnee_rang_T1 |> filter(Rang_T1 == 5) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T1_6 <- base_donnee_rang_T1 |> filter(Rang_T1 == 6) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T1_7 <- base_donnee_rang_T1 |> filter(Rang_T1 == 7) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T1_8 <- base_donnee_rang_T1 |> filter(Rang_T1 == 8) |> mutate(mean = mean(Classement_final))

recap_T1 <- as.data.frame(c(1,2,3,4,5,6,7,8))
recap_T1 <- recap_T1 |>mutate(moyenne = c(base_donnee_rang_T1_1[1,3],
                                    base_donnee_rang_T1_2[1,3],
                                    base_donnee_rang_T1_3[1,3],
                                    base_donnee_rang_T1_4[1,3],
                                    base_donnee_rang_T1_5[1,3],
                                    base_donnee_rang_T1_6[1,3],
                                    base_donnee_rang_T1_7[1,3],
                                    base_donnee_rang_T1_8[1,3]))
colnames(recap_T1) <- c("Position à T1", "Moyenne Classement Final")


base_donnee_rang_T2 <- base_donnee_rang |> select(Rang_T2, Classement_final)
base_donnee_rang_T2_1 <- base_donnee_rang_T2 |> filter(Rang_T2 == 1) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T2_2 <- base_donnee_rang_T2 |> filter(Rang_T2 == 2) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T2_3 <- base_donnee_rang_T2 |> filter(Rang_T2 == 3) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T2_4 <- base_donnee_rang_T2 |> filter(Rang_T2 == 4) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T2_5 <- base_donnee_rang_T2 |> filter(Rang_T2 == 5) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T2_6 <- base_donnee_rang_T2 |> filter(Rang_T2 == 6) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T2_7 <- base_donnee_rang_T2 |> filter(Rang_T2 == 7) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T2_8 <- base_donnee_rang_T2 |> filter(Rang_T2 == 8) |> mutate(mean = mean(Classement_final))

recap_T2 <- as.data.frame(c(1,2,3,4,5,6,7,8))
recap_T2 <- recap_T2 |>mutate(moyenne = c(base_donnee_rang_T2_1[1,3],
                                          base_donnee_rang_T2_2[1,3],
                                          base_donnee_rang_T2_3[1,3],
                                          base_donnee_rang_T2_4[1,3],
                                          base_donnee_rang_T2_5[1,3],
                                          base_donnee_rang_T2_6[1,3],
                                          base_donnee_rang_T2_7[1,3],
                                          base_donnee_rang_T2_8[1,3]))
colnames(recap_T2) <- c("Position à T2", "Moyenne Classement Final")

base_donnee_rang_T3 <- base_donnee_rang |> select(Rang_T3, Classement_final)
base_donnee_rang_T3_1 <- base_donnee_rang_T3 |> filter(Rang_T3 == 1) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T3_2 <- base_donnee_rang_T3 |> filter(Rang_T3 == 2) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T3_3 <- base_donnee_rang_T3 |> filter(Rang_T3 == 3) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T3_4 <- base_donnee_rang_T3 |> filter(Rang_T3 == 4) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T3_5 <- base_donnee_rang_T3 |> filter(Rang_T3 == 5) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T3_6 <- base_donnee_rang_T3 |> filter(Rang_T3 == 6) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T3_7 <- base_donnee_rang_T3 |> filter(Rang_T3 == 7) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T3_8 <- base_donnee_rang_T3 |> filter(Rang_T3 == 8) |> mutate(mean = mean(Classement_final))

recap_T3 <- as.data.frame(c(1,2,3,4,5,6,7,8))
recap_T3 <- recap_T3 |>mutate(moyenne = c(base_donnee_rang_T3_1[1,3],
                                          base_donnee_rang_T3_2[1,3],
                                          base_donnee_rang_T3_3[1,3],
                                          base_donnee_rang_T3_4[1,3],
                                          base_donnee_rang_T3_5[1,3],
                                          base_donnee_rang_T3_6[1,3],
                                          base_donnee_rang_T3_7[1,3],
                                          base_donnee_rang_T3_8[1,3]))
colnames(recap_T3) <- c("Position à T3", "Moyenne Classement Final")

base_donnee_rang_T4 <- base_donnee_rang |> select(Rang_T4, Classement_final)
base_donnee_rang_T4_1 <- base_donnee_rang_T4 |> filter(Rang_T4 == 1) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T4_2 <- base_donnee_rang_T4 |> filter(Rang_T4 == 2) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T4_3 <- base_donnee_rang_T4 |> filter(Rang_T4 == 3) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T4_4 <- base_donnee_rang_T4 |> filter(Rang_T4 == 4) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T4_5 <- base_donnee_rang_T4 |> filter(Rang_T4 == 5) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T4_6 <- base_donnee_rang_T4 |> filter(Rang_T4 == 6) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T4_7 <- base_donnee_rang_T4 |> filter(Rang_T4 == 7) |> mutate(mean = mean(Classement_final))
base_donnee_rang_T4_8 <- base_donnee_rang_T4 |> filter(Rang_T4 == 8) |> mutate(mean = mean(Classement_final))

recap_T4 <- as.data.frame(c(1,2,3,4,5,6,7,8))
recap_T4 <- recap_T4 |>mutate(moyenne = c(base_donnee_rang_T4_1[1,3],
                                          base_donnee_rang_T4_2[1,3],
                                          base_donnee_rang_T4_3[1,3],
                                          base_donnee_rang_T4_4[1,3],
                                          base_donnee_rang_T4_5[1,3],
                                          base_donnee_rang_T4_6[1,3],
                                          base_donnee_rang_T4_7[1,3],
                                          base_donnee_rang_T4_8[1,3]))
colnames(recap_T4) <- c("Position à T4", "Moyenne Classement Final")
###graph de suivi de place à faire



###distribution des places en graphes
ggplot(base_donnee_Posi, aes(x = Classement_final)) +
  geom_histogram(binwidth = 1, color = "black", fill = "white") +
  facet_wrap(~ Position_Start, nrow = 2) +
  labs(x = "Classement Final", y = "Count", title = "Distribution des Classements Finaux par rapport à la grille de départ") +
  theme_minimal()

#Rang T1
ggplot(base_donnee_rang_T1, aes(x = Classement_final)) +
  geom_histogram(binwidth = 1, color = "black", fill = "white") +
  facet_wrap(~ Rang_T1, nrow = 2) +
  labs(x = "Classement Final", y = "Count", title = "Distribution des Classements Finaux par Position à T4") +
  theme_minimal()

#Rang T1
ggplot(base_donnee_rang_T2, aes(x = Classement_final)) +
  geom_histogram(binwidth = 1, color = "black", fill = "white") +
  facet_wrap(~ Rang_T2, nrow = 2) +
  labs(x = "Classement Final", y = "Count", title = "Distribution des Classements Finaux par Position à T4") +
  theme_minimal()

#Rang T3
ggplot(base_donnee_rang_T3, aes(x = Classement_final)) +
  geom_histogram(binwidth = 1, color = "black", fill = "white") +
  facet_wrap(~ Rang_T3, nrow = 2) +
  labs(x = "Classement Final", y = "Count", title = "Distribution des Classements Finaux par Position à T4") +
  theme_minimal()

#Rang_T4
ggplot(base_donnee_rang_T4, aes(x = Classement_final)) +
  geom_histogram(binwidth = 1, color = "black", fill = "white") +
  facet_wrap(~ Rang_T4, nrow = 2) +
  labs(x = "Classement Final", y = "Count", title = "Distribution des Classements Finaux par Position à T4") +
  theme_minimal()

###Evolution corrélation kendall tau 
data_graphe <- test2[6,]
data_graphe <- as.data.frame(t(data_graphe))
colnames(data_graphe) <- "Classement_final"
data_graphe <- data_graphe[2:6,]
data_graphe <- as.data.frame(data_graphe)
data_graphe <- data_graphe |> mutate("Intermediaire" = c("Position_Start", "T1","T2", "T3", "T4"))



ggplot(data_graphe, aes(x = Intermediaire, y = data_graphe, group = 1)) +
  geom_point(shape = 16, color = "steelblue", size = 3) +
  geom_line(color = "steelblue") +
  labs(x = "", y = "Valeur", title = "Évolution des valeurs de corrélation de Kendall de tau b") +
  theme_minimal()
