library(ggplot2)
library(dplyr)
library(tidyr)

data <- read.csv2("Analyse_BMX/dataset/base_bmx_modif.csv", sep = ";", header =  TRUE) 
filtered_data <- data[complete.cases(data) | data %in% c("DNS", "DNF"), ]

filtered_data_2 <- filtered_data %>%
  filter(Saison == 2022) %>%
  filter(Epreuves == "Bogota") %>%
  filter(Categorie == "MenElite") 

athlete <- filtered_data_2 %>%
  filter(nom == "DAUDET Joris") %>%
  filter(Typologie_Epreuves == "CDM 8") %>%
  filter(Manche == "Demi-final") %>%
  select(Temps_T1, Temps_T2, Temps_T3, Temps_T4, Temps_Final)

athlete_2 <- athlete %>%
  tidyr::pivot_longer(everything(), names_to = "intermediaires", values_to = "time_athlete")

temps_intermediaires <- as.data.frame(c("T1", "T2", "T3", "T4", "Final"))

temps_intermediaires <- cbind(temps_intermediaires, athlete_2)
colnames(temps_intermediaires) <- c("intermediaires", "a", "time_athlete")
temps_intermediaires <- temps_intermediaires %>%
  select(intermediaires, time_athlete)

T1 <- filtered_data_2 %>%
   select(Temps_T1) 
T1 <- as.numeric(T1$Temps_T1)
T1 <- T1[complete.cases(T1)]  # Garder uniquement les observations complètes
T1 <- as.data.frame(T1)
T1 <- T1 |> arrange(T1)


T2 <- filtered_data_2 %>%
  select(Temps_T2) 
T2 <- as.numeric(T2$Temps_T2)
T2 <- T2[complete.cases(T2)]  # Garder uniquement les observations complètes
T2 <- as.data.frame(T2)
T2 <- T2 |> arrange(T2)


T3 <- filtered_data_2 %>%
  select(Temps_T3) 
T3 <- as.numeric(T3$Temps_T3)
T3 <- T3[complete.cases(T3)]  # Garder uniquement les observations complètes
T3 <- as.data.frame(T3)
T3 <- T3 |> arrange(T3)


T4 <- filtered_data_2 %>%
  select(Temps_T4) 
T4 <- as.numeric(T4$Temps_T4)
T4 <- T4[complete.cases(T4)]  # Garder uniquement les observations complètes
T4 <- as.data.frame(T4)
T4 <- T4 |> arrange(T4)


Final <- filtered_data_2 %>%
  select(Temps_Final) 
Final <- as.numeric(Final$Temps_Final)
Final <- Final[complete.cases(Final)]  # Garder uniquement les observations complètes
Final <- as.data.frame(Final)
Final <- Final |> arrange(Final)


T1_1 <- T1[1,1]
T1_8 <- T1[8,1]
T1_10 <- T1[10,1]
T1_16 <- T1[16,1]
T1_32 <- T1[32,1]
T1_50 <- T1[50,1]
T1_75 <- T1[75,1]

T2_1 <- T2[1,1]
T2_8 <- T2[8,1]
T2_10 <- T2[10,1]
T2_16 <- T2[16,1]
T2_32 <- T2[32,1]
T2_50 <- T2[50,1]
T2_75 <- T2[75,1]

T3_1 <- T3[1,1]
T3_8 <- T3[8,1]
T3_10 <- T3[10,1]
T3_16 <- T3[16,1]
T3_32 <- T3[32,1]
T3_50 <- T3[50,1]
T3_75 <- T3[75,1]

T4_1 <- T4[1,1]
T4_8 <- T4[8,1]
T4_10 <- T4[10,1]
T4_16 <- T4[16,1]
T4_32 <- T4[32,1]
T4_50 <- T4[50,1]
T4_75 <- T4[75,1]

Final_1 <- Final[1,1]
Final_8 <- Final[8,1]
Final_10 <- Final[10,1]
Final_16 <- Final[16,1]
Final_32 <- Final[32,1]
Final_50 <- Final[50,1]
Final_75 <- Final[75,1]

Meilleur <- as.data.frame(c(T1_1, T2_1, T3_1, T4_1, Final_1))
top_8 <- as.data.frame(c(T1_8, T2_8, T3_8, T4_8, Final_8))
top_10 <- as.data.frame(c(T1_10, T2_10, T3_10, T4_10, Final_10))
top_16 <- as.data.frame(c(T1_16, T2_16, T3_16, T4_16, Final_16))
top_32 <- as.data.frame(c(T1_32, T2_32, T3_32, T4_32, Final_32))
top_50 <- as.data.frame(c(T1_50, T2_50, T3_50, T4_50, Final_50))
top_75 <- as.data.frame(c(T1_75, T2_75, T3_75, T4_75, Final_75))

temps_intermediaires <- cbind(temps_intermediaires, Meilleur, top_8, top_10, top_16, top_32, top_50, top_75)
colnames(temps_intermediaires) <- c("intermediaires", "time_athlete", "Meilleur", "top_8", "top_10", "top_16", "top_32", "top_50", "top_75")

ordre_type <- c("T1", "T2", "T3", "T4", "Final")
temps_intermediaires$intermediaires <- factor((temps_intermediaires$intermediaires), levels = ordre_type)
temps_intermediaires$time_athlete <- as.numeric(temps_intermediaires$time_athlete)

p <- ggplot(temps_intermediaires) +
  geom_hline(aes(yintercept = top_8),
             linetype = "dashed",
             color = "black") +
  geom_hline(aes(yintercept = top_10),
             linetype = "dashed",
             color = "black") +
  geom_hline(aes(yintercept = top_16),
             linetype = "dashed",
             color = "black") +
  geom_hline(aes(yintercept = top_32),
             linetype = "dashed",
             color = "black") +
  geom_hline(aes(yintercept = top_50),
             linetype = "dashed",
             color = "black") +
  geom_hline(aes(yintercept = top_75),
             linetype = "dashed",
             color = "black") +
  geom_point(aes(x = "T3", y = Meilleur), color = "black") +
  geom_rect(aes(
    xmin = -Inf,
    xmax = Inf,
    ymin = Meilleur,
    ymax = top_8,
    alpha = 0.1
  ),
  fill = "#00FF00") +
  geom_rect(aes(
    xmin = -Inf,
    xmax = Inf,
    ymin = top_8,
    ymax = top_10,
    alpha = 0.1
  ),
  fill = "#CCFFCC") +
  geom_rect(aes(
    xmin = -Inf,
    xmax = Inf,
    ymin = top_10,
    ymax = top_16,
    alpha = 0.1
  ),
  fill = "#FFF333") +
  geom_rect(aes(
    xmin = -Inf,
    xmax = Inf,
    ymin = top_16,
    ymax = top_32,
    alpha = 0.1
  ),
  fill = "#FF9933") +
  geom_rect(aes(
    xmin = -Inf,
    xmax = Inf,
    ymin = top_32,
    ymax = top_50,
    alpha = 0.1
  ),
  fill = "#FF0000") +
  geom_rect(aes(
    xmin = -Inf,
    xmax = Inf,
    ymin = top_50,
    ymax = top_75,
    alpha = 0.1
  ),
  fill = "#660000") +
  labs(x = NULL, y = "Temps (s)") +
  ggtitle("Analyse des temps intermediaires") +
  theme_bw() +
  facet_wrap(
    ~ intermediaires,
    scales = "free_y",
    strip.position = "top",
    ncol = 5
  ) +
  xlab(NULL) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5
    )
  )
p <- p + geom_hline(aes(yintercept = time_athlete), color = "black")




#################################################################################
######""
######                      Passage au temps par intermédiaires 
#################################################################################
temps_par_zone <- filtered_data
temps_par_zone$Temps_T1 <- as.numeric(temps_par_zone$Temps_T1)
temps_par_zone$Temps_T2 <- as.numeric(temps_par_zone$Temps_T2)
temps_par_zone$Temps_T3 <- as.numeric(temps_par_zone$Temps_T3)
temps_par_zone$Temps_T4 <- as.numeric(temps_par_zone$Temps_T4)
temps_par_zone$Temps_Final <- as.numeric(temps_par_zone$Temps_Final)


temps_par_zone$Temps_T2 <- temps_par_zone$Temps_T2 - temps_par_zone$Temps_T1
temps_par_zone$Temps_T3 <- temps_par_zone$Temps_T3 - temps_par_zone$Temps_T1 - temps_par_zone$Temps_T2
temps_par_zone$Temps_T4 <- temps_par_zone$Temps_T4 - temps_par_zone$Temps_T1 - temps_par_zone$Temps_T2 - temps_par_zone$Temps_T3
temps_par_zone$Temps_Final <- temps_par_zone$Temps_Final - temps_par_zone$Temps_T1 - temps_par_zone$Temps_T2 - temps_par_zone$Temps_T3 - temps_par_zone$Temps_T4


temps_par_zone_2 <- temps_par_zone %>%
  filter(Saison == 2022) %>%
  filter(Epreuves == "Bogota") %>%
  filter(Categorie == "MenElite") 

athlete <- temps_par_zone_2 %>%
  filter(nom == "DAUDET Joris") %>%
  filter(Typologie_Epreuves == "CDM 8") %>%
  filter(Manche == "Demi-final") %>%
  select(Temps_T1, Temps_T2, Temps_T3, Temps_T4, Temps_Final)

athlete_2 <- athlete %>%
  tidyr::pivot_longer(everything(), names_to = "intermediaires", values_to = "time_athlete")

temps_intermediaires <- as.data.frame(c("Portion_1", "Portion_2", "Portion_3", "Portion_4", "Portion_5"))

temps_intermediaires <- cbind(temps_intermediaires, athlete_2)
colnames(temps_intermediaires) <- c("intermediaires", "a", "time_athlete")
temps_intermediaires <- temps_intermediaires %>%
  select(intermediaires, time_athlete)

T1 <- temps_par_zone_2 %>%
  select(Temps_T1) 
T1 <- as.numeric(T1$Temps_T1)
T1 <- T1[complete.cases(T1)]  # Garder uniquement les observations complètes
T1 <- as.data.frame(T1)
T1 <- T1 |> arrange(T1)


T2 <- temps_par_zone_2 %>%
  select(Temps_T2) 
T2 <- as.numeric(T2$Temps_T2)
T2 <- T2[complete.cases(T2)]  # Garder uniquement les observations complètes
T2 <- as.data.frame(T2)
T2 <- T2 |> arrange(T2)


T3 <- temps_par_zone_2 %>%
  select(Temps_T3) 
T3 <- as.numeric(T3$Temps_T3)
T3 <- T3[complete.cases(T3)]  # Garder uniquement les observations complètes
T3 <- as.data.frame(T3)
T3 <- T3 |> arrange(T3)


T4 <- temps_par_zone_2 %>%
  select(Temps_T4) 
T4 <- as.numeric(T4$Temps_T4)
T4 <- T4[complete.cases(T4)]  # Garder uniquement les observations complètes
T4 <- as.data.frame(T4)
T4 <- T4 |> arrange(T4)


Final <- temps_par_zone_2 %>%
  select(Temps_Final) 
Final <- as.numeric(Final$Temps_Final)
Final <- Final[complete.cases(Final)]  # Garder uniquement les observations complètes
Final <- as.data.frame(Final)
Final <- Final |> arrange(Final)


T1_1 <- T1[1,1]
T1_8 <- T1[8,1]
T1_10 <- T1[10,1]
T1_16 <- T1[16,1]
T1_32 <- T1[32,1]
T1_50 <- T1[50,1]
T1_75 <- T1[75,1]

T2_1 <- T2[1,1]
T2_8 <- T2[8,1]
T2_10 <- T2[10,1]
T2_16 <- T2[16,1]
T2_32 <- T2[32,1]
T2_50 <- T2[50,1]
T2_75 <- T2[75,1]

T3_1 <- T3[1,1]
T3_8 <- T3[8,1]
T3_10 <- T3[10,1]
T3_16 <- T3[16,1]
T3_32 <- T3[32,1]
T3_50 <- T3[50,1]
T3_75 <- T3[75,1]

T4_1 <- T4[1,1]
T4_8 <- T4[8,1]
T4_10 <- T4[10,1]
T4_16 <- T4[16,1]
T4_32 <- T4[32,1]
T4_50 <- T4[50,1]
T4_75 <- T4[75,1]

Final_1 <- Final[1,1]
Final_8 <- Final[8,1]
Final_10 <- Final[10,1]
Final_16 <- Final[16,1]
Final_32 <- Final[32,1]
Final_50 <- Final[50,1]
Final_75 <- Final[75,1]

Meilleur <- as.data.frame(c(T1_1, T2_1, T3_1, T4_1, Final_1))
top_8 <- as.data.frame(c(T1_8, T2_8, T3_8, T4_8, Final_8))
top_10 <- as.data.frame(c(T1_10, T2_10, T3_10, T4_10, Final_10))
top_16 <- as.data.frame(c(T1_16, T2_16, T3_16, T4_16, Final_16))
top_32 <- as.data.frame(c(T1_32, T2_32, T3_32, T4_32, Final_32))
top_50 <- as.data.frame(c(T1_50, T2_50, T3_50, T4_50, Final_50))
top_75 <- as.data.frame(c(T1_75, T2_75, T3_75, T4_75, Final_75))

temps_intermediaires <- cbind(temps_intermediaires, Meilleur, top_8, top_10, top_16, top_32, top_50, top_75)
colnames(temps_intermediaires) <- c("intermediaires", "time_athlete", "Meilleur", "top_8", "top_10", "top_16", "top_32", "top_50", "top_75")

ordre_type <- c("Portion_1", "Portion_2", "Portion_3", "Portion_4", "Portion_5")
temps_intermediaires$intermediaires <- factor((temps_intermediaires$intermediaires), levels = ordre_type)
temps_intermediaires$time_athlete <- as.numeric(temps_intermediaires$time_athlete)

p <- ggplot(temps_intermediaires) +
  geom_hline(aes(yintercept = top_8),
             linetype = "dashed",
             color = "black") +
  geom_hline(aes(yintercept = top_10),
             linetype = "dashed",
             color = "black") +
  geom_hline(aes(yintercept = top_16),
             linetype = "dashed",
             color = "black") +
  geom_hline(aes(yintercept = top_32),
             linetype = "dashed",
             color = "black") +
  geom_hline(aes(yintercept = top_50),
             linetype = "dashed",
             color = "black") +
  geom_hline(aes(yintercept = top_75),
             linetype = "dashed",
             color = "black") +
  geom_point(aes(x = "T3", y = Meilleur), color = "black") +
  geom_rect(aes(
    xmin = -Inf,
    xmax = Inf,
    ymin = Meilleur,
    ymax = top_8,
    alpha = 0.1
  ),
  fill = "#00FF00") +
  geom_rect(aes(
    xmin = -Inf,
    xmax = Inf,
    ymin = top_8,
    ymax = top_10,
    alpha = 0.1
  ),
  fill = "#CCFFCC") +
  geom_rect(aes(
    xmin = -Inf,
    xmax = Inf,
    ymin = top_10,
    ymax = top_16,
    alpha = 0.1
  ),
  fill = "#FFF333") +
  geom_rect(aes(
    xmin = -Inf,
    xmax = Inf,
    ymin = top_16,
    ymax = top_32,
    alpha = 0.1
  ),
  fill = "#FF9933") +
  geom_rect(aes(
    xmin = -Inf,
    xmax = Inf,
    ymin = top_32,
    ymax = top_50,
    alpha = 0.1
  ),
  fill = "#FF0000") +
  geom_rect(aes(
    xmin = -Inf,
    xmax = Inf,
    ymin = top_50,
    ymax = top_75,
    alpha = 0.1
  ),
  fill = "#660000") +
  labs(x = NULL, y = "Temps (s)") +
  ggtitle("Analyse des temps intermediaires") +
  theme_bw() +
  facet_wrap(
    ~ intermediaires,
    scales = "free_y",
    strip.position = "top",
    ncol = 5
  ) +
  xlab(NULL) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5
    )
  )
p <- p + geom_hline(aes(yintercept = time_athlete), color = "black")

