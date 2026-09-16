library(tidyr)
library(dplyr)
library(broom.mixed)
library(lmerTest)   
library(emmeans)
library(ggplot2)
library(lme4)

# importe dataset

data <- read.csv(
  "data/student_bmi.csv",
  header = TRUE,
  sep = ",",
  na.strings = c("NA", "")
)

# transforme variable en facteur
data <- data %>%
  mutate(
    ID = factor(ID),
    
    GROUP = factor(
      GROUP,
      levels = c(1, 2, 3),
      labels = c("Approche 1", "Approche 2", "Approche 3")
    ),
    
    GENDER = factor(
      GENDER,
      levels = c(0, 1),
      labels = c("Garçon", "Fille")
    ),
    
    SMOKE = factor(
      SMOKE,
      levels = c(0, 1),
      labels = c("Moins de 1 cigarette/jour",
                 "Au moins 1 cigarette/jour")
    ),
    
    LIVE = factor(
      LIVE,
      levels = c(0, 1, 2),
      labels = c(
        "Aucune activité",
        "Activité occasionnelle",
        "Au moins une fois/semaine"
      )
    )
  )


# valeur manquantes

valeur_manquantes <- data.frame(
  Variable = names(data),
  Nombre_manquant = colSums(is.na(data)),
  Pourcentage_manquant = round(colMeans(is.na(data)) * 100, 2)
)

valeur_manquantes




# Mettre les données en format LONG

data_long <- data %>%
  pivot_longer(
    cols = c(BMI_T1, BMI_T2, BMI_T3, BMI_T4, BMI_T5),
    names_to = "TIME",
    values_to = "BMI"
  ) %>%
  mutate(
    TIME = factor(
      TIME,
      levels = c("BMI_T1", "BMI_T2", "BMI_T3", "BMI_T4", "BMI_T5"),
      labels = c("Novembre", "Janvier", "Mars", "Mai", "Juillet"),
      ordered = FALSE
    )
  )

#analyse descriptive

bmi_group_time <- data_long %>%
  group_by(GROUP, TIME) %>%
  summarise(
    Nombre = sum(!is.na(BMI)),
    Moyenne = mean(BMI, na.rm = TRUE),
    Ecart_type = sd(BMI, na.rm = TRUE),
    Erreur_standard = Ecart_type / sqrt(Nombre),
    IC_inf = Moyenne - 1.96 * Erreur_standard,
    IC_sup = Moyenne + 1.96 * Erreur_standard,
    .groups = "drop"
  )

bmi_group_time

ggplot(
  bmi_group_time,
  aes(
    x = TIME,
    y = Moyenne,
    group = GROUP,
    linetype = GROUP,
    shape = GROUP
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Évolution moyenne du BMI selon l'approche",
    x = "Temps",
    y = "BMI moyen",
    linetype = "Groupe",
    shape = "Groupe"
  ) +
  theme_minimal()


data_long_all <- data %>%
  pivot_longer(
    cols = c(BMIb, BMI_T1, BMI_T2, BMI_T3, BMI_T4, BMI_T5),
    names_to = "TIME",
    values_to = "BMI"
  ) %>%
  mutate(
    time = case_when(
      TIME == "BMIb"   ~ 0,
      TIME == "BMI_T1" ~ 1,
      TIME == "BMI_T2" ~ 2,
      TIME == "BMI_T3" ~ 3,
      TIME == "BMI_T4" ~ 4,
      TIME == "BMI_T5" ~ 5
    )
  )
ggplot(
  data_long_all,
  aes(x = time, y = BMI, group = ID)
) +
  geom_line(
    alpha = 0.08,
    linewidth = 0.3,
    colour = "black",
    na.rm = TRUE
  ) +
  facet_wrap(~ GROUP, nrow = 1) +
  scale_x_continuous(
    breaks = 0:5,
    labels = c("Base", "Nov.", "Jan.", "Mars", "Mai", "Juil.")
  ) +
  labs(
    title = "Trajectoires individuelles du BMI selon l'approche",
    x = "Temps",
    y = "BMI"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 13),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

# Modèle de référence ajusté sur le BMI initial
m0 <- lmer(
  BMI ~ BMIb + (1 | ID),
  data = data_long,
  REML = FALSE
)

m0
summary(m0)# Ajout du temps
m1 <- lmer(
  BMI ~ BMIb + TIME + (1 | ID),
  data = data_long,
  REML = FALSE
)
m1
# Ajout du groupe
m2 <- lmer(
  BMI ~ BMIb + TIME + GROUP + (1 | ID),
  data = data_long,
  REML = FALSE
)
m2
# Interaction temps-groupe
m3 <- lmer(
  BMI ~ BMIb + TIME * GROUP + (1 | ID),
  data = data_long,
  REML = FALSE
)
m3
# Modèle complet
m4 <- lmer(
  BMI ~ BMIb +
    TIME * GROUP +
    AGE +
    GENDER +
    SMOKE +
    LIVE +
    (1 | ID),
  data = data_long,
  REML = FALSE
)
m4
# comparaison entre m1 et m2b
anova(m0, m1, m2, m3, m4)
AIC(m0, m1, m2, m3, m4)
BIC(m0, m1, m2, m3, m4)


data_long$time_num <- c(0, 1, 2, 3, 4)[match(
  data_long$TIME,
  levels(data_long$TIME)
)]



m4_slope <- lmer(
  BMI ~ BMIb +
    TIME * GROUP +
    AGE +
    GENDER +
    SMOKE +
    LIVE +
    (1 + time_num | ID),
  data = data_long,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa")
)

anova(m4, m4_slope)

AIC(m4, m4_slope)

BIC(m4, m4_slope)

summary(m4_slope)
anova(m4_slope)
VarCorr(m4_slope)

#  Marginal predicted values + IC 95%
emmip(m4_slope, GROUP ~ TIME, CIs = TRUE)




# Conditional predicted values
dat_long <- data_long %>%
  mutate(
    pred_cond = predict(
      m4_slope,
      newdata = data_long,
      re.form = NULL
    ),
    pred_marg = predict(
      m4_slope,
      newdata = data_long,
      re.form = NA
    )
  )

set.seed(1)

ids <- sample(unique(dat_long$ID), 25)

p_cond <- dat_long %>%
  filter(ID %in% ids) %>%
  ggplot(aes(x = TIME)) +
  
  geom_line(
    aes(y = BMI, group = ID),
    colour = "grey60",
    alpha = 0.5
  ) +
  
  geom_line(
    aes(y = pred_cond, group = ID),
    colour = "#0072B2",
    linewidth = 0.7
  ) +
  
  facet_wrap(~GROUP) +
  
  labs(
    title = "Trajectoires individuelles : observé (gris) vs prédiction conditionnelle (bleu)",
    x = "Temps",
    y = "BMI"
  ) +
  
  theme_minimal()

p_cond


# model diagnostic

plot(m4_slope)
qqnorm(residuals(m4_slope))
qqline(residuals(m4_slope), col = "red")


