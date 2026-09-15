############################################# Two-way ANOVA Audience Challenge

# Install packages
install.packages(c("dplyr", "ggplot2", "stats", "pharmaverseadam", "emmeans"))

# Load packages
library(dplyr)
library(ggplot2)
library(stats)
library(pharmaverseadam)
library(emmeans)

# Import data (ADVS dataset)
advs <- pharmaverseadam::advs

# Pre-process data
advs_filtered <- advs %>% 
  filter(AVISIT == "End of Treatment") %>% 
  mutate(
    AGEGR1 = AGEGR1 %>% 
      factor(levels = c("18-64", ">64")),
    TRT01P = TRT01P %>% 
      factor(levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
  )

# BMI (Body Mass Index (kg/m^2)) - AGEGR1
bmi_boxplot <- advs_filtered %>%
  filter(PARAMCD == "BMI") %>%
  ggplot(aes(x = TRT01P, y = AVAL, fill = AGEGR1)) +
  labs(
    y = "BMI"
  ) +
  geom_boxplot() +
  theme(
    text = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  ); bmi_boxplot

# Fit linear models and compute the two-way ANOVA results
advs_bmi <- advs_filtered %>% 
  filter(PARAMCD == "BMI")

lm_no_int <- stats::lm(AVAL ~ TRT01P + AGEGR1, data = advs_bmi)
stats::anova(lm_no_int)

lm_int <- stats::lm(AVAL ~ TRT01P + AGEGR1 + TRT01P:AGEGR1, data = advs_bmi)
stats::anova(lm_int)

# Least Square Means
emmeans::emmeans(lm_no_int, ~ TRT01P)

emmeans::emmeans(lm_no_int, ~ AGEGR1)

emmeans::emmeans(lm_int, ~ TRT01P : AGEGR1)

# Final EMM graph
emm <- emmeans(lm_int, ~ TRT01P : AGEGR1) %>% 
  as.data.frame()

emm_graph <- ggplot(emm, aes(x = TRT01P, y = emmean, color = AGEGR1)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1, linewidth = 0.8) +
  labs(
    y = "BMI EMM (with 95% CI)"
  ) +
  theme(
    text = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  ) ; emm_graph