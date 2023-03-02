# Load packages ----------------------------------------------------------------
library(afex)
library(emmeans)
library(pastecs)
library(reshape2)
library(tidyverse)

# Load *anterior* data --------------------------------------------------------------------

anterior_data <- read_csv("anterior_reach_data.csv")
head(anterior_data)

# Data descriptives --------------------

all_descriptives_anterior_data <- anterior_data %>%
  gather(key = "time", value = "score", pre01, pre02, pre03, post01, post02, post03) %>%
  group_by(time) %>%
  summarise(mean = mean(score),
            sd = sd(score))

# Prepare anterior data -------------------------------------------------------------------
# Convert data to long dataset and add intervention column
# Drop the "pre01" and "pre03" observation - original study only compared pre03 to post scores

anova_anterior_data <- anterior_data  %>%
  select(-c("pre01", "pre02")) %>%
  gather(key = "time", value = "score", pre03, post01, post02, post03) %>%
  mutate(intervention =  case_when(
    time %in% c("pre03") ~ "pre_fatigue",
    time %in% c("post01", "post02", "post03") ~ "post_fatigue"
  ))
head(anova_anterior_data, 3)

# Assumptions - anterior ----------------------------------------------------------------------------
## Normality test ------------------------------------------------------

stat.desc(anterior_data$pre03, basic = TRUE, norm = TRUE)
stat.desc(anterior_data$post01, basic = TRUE, norm = TRUE)
stat.desc(anterior_data$post02, basic = TRUE, norm = TRUE)
stat.desc(anterior_data$post03, basic = TRUE, norm = TRUE)

## Plots ---------------------------------------------------------------------------

### Histogram -------------------

# Prepare anterior_data
anterior_hist_dat <- anterior_data %>%
  select(pre03, post01, post02, post03)

anterior_hist_dat$id <- 1:nrow(anterior_hist_dat)
anterior_hist_dat <- melt(anterior_hist_dat, id.vars = "id")

# Plot histogram
hist <- ggplot(data = anterior_hist_dat, aes(x = value, fill = variable)) +
  geom_histogram(color = "black", fill = "white",
                 bins = 15) +
  facet_wrap( ~ variable) +
  scale_x_continuous(name = "Y balance score")
hist

### Q-Q plots -------------------

ggplot(anterior_data, aes(sample = pre03)) +
  geom_qq() +
  geom_qq_line() +
  scale_x_continuous(name = "Observed Value") +
  scale_y_continuous(name = "Expected Normal")

ggplot(anterior_data, aes(sample = post01)) +
  geom_qq() +
  geom_qq_line() +
  scale_x_continuous(name = "Observed Value") +
  scale_y_continuous(name = "Expected Normal")

ggplot(anterior_data, aes(sample = post02)) +
  geom_qq() +
  geom_qq_line() +
  scale_x_continuous(name = "Observed Value") +
  scale_y_continuous(name = "Expected Normal")

ggplot(anterior_data, aes(sample = post03)) +
  geom_qq() +
  geom_qq_line() +
  scale_x_continuous(name = "Observed Value") +
  scale_y_continuous(name = "Expected Normal")

### Boxplot -------------------

ggplot(anova_anterior_data, aes(x = time, y = score)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = .2)

# Anterior ANOVA omnibus test ----------------------------------------------------------------------------

anterior_data_afx <- afex::aov_4(
  score ~ time + (time | participant),
  data = anova_anterior_data,
  anova_table = list(correction = "GG", es = "pes")
) # using Greenhouse Geisser sphercity correction and partial eta squared
anterior_data_afx

summary(anterior_data_afx)

anterior_data_emm <- emmeans::emmeans(anterior_data_afx, ~ time, model = "multivariate")

# Anterior post hoc contrasts ----------------------------------------------------------------------------

posthocresults <- pairs(anterior_data_emm, adjust = "bon") %>%
  broom::tidy(conf.int = T)
posthocresults

# Load *posteromedial* data --------------------------------------------------------------------

posteromed_data <- read_csv("postmed_reach_data.csv")
head(posteromed_data)

# Data descriptives --------------------

all_descriptives_posteromed_data <- posteromed_data %>%
  gather(key = "time", value = "score", pre01, pre02, pre03, post01, post02, post03) %>%
  group_by(time) %>%
  summarise(mean = mean(score),
            sd = sd(score))

# Prepare posteromedial data -------------------------------------------------------------------
# Convert data to long dataset and add intervention column
# Drop the "pre01" and "pre03" observation - original study only compared pre03 to post scores

anova_posteromed_data <- posteromed_data  %>%
  select(-c("pre01", "pre02")) %>%
  gather(key = "time", value = "score", pre03, post01, post02, post03) %>%
  mutate(intervention =  case_when(
    time %in% c("pre03") ~ "pre_fatigue",
    time %in% c("post01", "post02", "post03") ~ "post_fatigue"
  ))
head(anova_posteromed_data, 3)

# Posteromedial assumptions ----------------------------------------------------------------------------
## Summary stats and normality test ------------------------------------------------------

stat.desc(posteromed_data$pre03, basic = TRUE, norm = TRUE)
stat.desc(posteromed_data$post01, basic = TRUE, norm = TRUE)
stat.desc(posteromed_data$post02, basic = TRUE, norm = TRUE)
stat.desc(posteromed_data$post03, basic = TRUE, norm = TRUE)

## Plots ---------------------------------------------------------------------------

### Histogram -------------------

# Prepare posteromed_data
hist_dat <- posteromed_data %>%
  select(pre03, post01, post02, post03)

hist_dat $id <- 1:nrow(hist_dat )
hist_dat <- melt(hist_dat,id.vars = "id")

# Plot histogram
hist <- ggplot(data = hist_dat, aes(x = value, fill = variable)) + 
  geom_histogram(color="black", fill="white",
                 bins = 15) +
  facet_wrap(~variable) +
  scale_x_continuous(name = "Y balance score")
hist

### Q-Q plots -------------------

ggplot(posteromed_data, aes(sample = pre03)) +
  geom_qq() +
  geom_qq_line() +
  scale_x_continuous(name = "Observed Value") +
  scale_y_continuous(name = "Expected Normal")

ggplot(posteromed_data, aes(sample = post01)) +
  geom_qq() +
  geom_qq_line() +
  scale_x_continuous(name = "Observed Value") +
  scale_y_continuous(name = "Expected Normal")

ggplot(posteromed_data, aes(sample = post02)) +
  geom_qq() +
  geom_qq_line() +
  scale_x_continuous(name = "Observed Value") +
  scale_y_continuous(name = "Expected Normal")

ggplot(posteromed_data, aes(sample = post03)) +
  geom_qq() +
  geom_qq_line() +
  scale_x_continuous(name = "Observed Value") +
  scale_y_continuous(name = "Expected Normal")

### Boxplot -------------------

ggplot(anova_posteromed_data, aes(x = time, y = score))+
  geom_violin(trim = FALSE)+
  geom_boxplot(width = .2)

# Posteromedial ANOVA omnibus test ----------------------------------------------------------------------------

posteromed_data_afx <- afex::aov_4(score ~ time + (time|participant), 
                                   data = anova_posteromed_data,
                                   anova_table = list(correction = "GG", es = "pes")) # using Greenhouse Geisser sphercity correction and partial eta squared
posteromed_data_afx

summary(posteromed_data_afx)

posteromed_data_emm <- emmeans::emmeans(posteromed_data_afx, ~time, model = "multivariate")

# Posteromedial post hoc contrasts ----------------------------------------------------------------------------

posthocresults <- pairs(posteromed_data_emm, adjust = "bon") %>%
  broom::tidy(conf.int = T)
posthocresults

# Load *posterolateral* data --------------------------------------------------------------------

posterolat_data <- read_csv("postlat_reach_data.csv")
head(posterolat_data)

# Data descriptives --------------------

all_descriptives_posterolat_data <- posterolat_data %>%
  gather(key = "time", value = "score", pre01, pre02, pre03, post01, post02, post03) %>%
  group_by(time) %>%
  summarise(mean = mean(score),
            sd = sd(score))

# Prepare posteromedial data -------------------------------------------------------------------
# Convert data to long dataset and add intervention column
# Drop the "pre01" and "pre03" observation - original study only compared pre03 to post scores

anova_posterolat_data <- posterolat_data  %>%
  select(-c("pre01", "pre02")) %>%
  gather(key = "time", value = "score", pre03, post01, post02, post03) %>%
  mutate(intervention =  case_when(
    time %in% c("pre03") ~ "pre_fatigue",
    time %in% c("post01", "post02", "post03") ~ "post_fatigue"
  ))
head(anova_posterolat_data, 3)

# Posterolateral assumptions ----------------------------------------------------------------------------
## Summary stats and normality test ------------------------------------------------------

stat.desc(posterolat_data$pre03, basic = TRUE, norm = TRUE)
stat.desc(posterolat_data$post01, basic = TRUE, norm = TRUE)
stat.desc(posterolat_data$post02, basic = TRUE, norm = TRUE)
stat.desc(posterolat_data$post03, basic = TRUE, norm = TRUE)

### Plots ---------------------------------------------------------------------------

### Histogram -------------------

# Prepare posterolat_data
hist_dat <- posterolat_data %>%
  select(pre03, post01, post02, post03)

hist_dat $id <- 1:nrow(hist_dat )
hist_dat <- melt(hist_dat,id.vars = "id")

# Plot histogram
hist <- ggplot(data = hist_dat, aes(x = value, fill = variable)) + 
  geom_histogram(color="black", fill="white",
                 bins = 15) +
  facet_wrap(~variable) +
  scale_x_continuous(name = "Y balance score")
hist

### Q-Q plots -------------------

ggplot(posterolat_data, aes(sample = pre03)) +
  geom_qq() +
  geom_qq_line() +
  scale_x_continuous(name = "Observed Value") +
  scale_y_continuous(name = "Expected Normal")

ggplot(posterolat_data, aes(sample = post01)) +
  geom_qq() +
  geom_qq_line() +
  scale_x_continuous(name = "Observed Value") +
  scale_y_continuous(name = "Expected Normal")

ggplot(posterolat_data, aes(sample = post02)) +
  geom_qq() +
  geom_qq_line() +
  scale_x_continuous(name = "Observed Value") +
  scale_y_continuous(name = "Expected Normal")

ggplot(posterolat_data, aes(sample = post03)) +
  geom_qq() +
  geom_qq_line() +
  scale_x_continuous(name = "Observed Value") +
  scale_y_continuous(name = "Expected Normal")

### Boxplot -------------------

ggplot(anova_posterolat_data, aes(x = time, y = score))+
  geom_violin(trim = FALSE)+
  geom_boxplot(width = .2)

# Posterolateral ANOVA omnibus test ----------------------------------------------------------------------------

posterolat_data_afx <- afex::aov_4(score ~ time + (time|participant), 
                                   data = anova_posterolat_data,
                                   anova_table = list(correction = "GG", es = "pes")) # using Greenhouse Geisser sphercity correction and partial eta squared
posterolat_data_afx

summary(posterolat_data_afx)

posterolat_data_emm <- emmeans::emmeans(posterolat_data_afx, ~time, model = "multivariate")

# Posterolateral post hoc contrasts ----------------------------------------------------------------------------

posthocresults <- pairs(posterolat_data_emm, adjust = "bon") %>%
  broom::tidy(conf.int = T)
posthocresults
