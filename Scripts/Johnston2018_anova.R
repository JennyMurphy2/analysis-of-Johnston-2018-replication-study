# Load packages 
library(afex)
library(emmeans)
library(pastecs)
library(reshape2)
library(tidyverse)
library(MOTE)
library(TOSTER)

# ANTERIOR DATA --------------------------------------------------------------------

anterior_data <- read_csv("anterior_reach_data.csv")
head(anterior_data)

## Data descriptives --------------------

all_descriptives_anterior_data <- anterior_data %>%
  gather(key = "time", value = "score", pre01, pre02, pre03, post01, post02, post03) %>%
  group_by(time) %>%
  summarise(mean = mean(score),
            sd = sd(score))

# Prepare anterior data 
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


## Plots ---------------------------------------------------------------------------

### Histogram 

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

### Q-Q plots 

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

### Boxplot 

ggplot(anova_anterior_data, aes(x = time, y = score)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = .2)

## Anterior ANOVA ----------------------------------------------------------------------------

anterior_data_afx <- afex::aov_4(
  score ~ time + (time | participant),
  data = anova_anterior_data,
  anova_table = list(correction = "GG", es = "pes")
) # using Greenhouse Geisser sphercity correction and partial eta squared
anterior_data_afx

summary(anterior_data_afx)

## Post hoc --------------

anterior_data_emm <- emmeans::emmeans(anterior_data_afx, ~ time, model = "multivariate")
anterior_data_emm

posthocresults <- pairs(anterior_data_emm, adjust = "bon") %>%
  broom::tidy(conf.int = T)
posthocresults

### Assumption checking ---------

# Normality test

shapiro.test(anterior_data_afx$lm$residuals) 

anova_anterior_data %>% 
  dplyr::group_by(time) %>% 
  rstatix::shapiro_test(score)


# POSTEROMEDIAL --------------------------------------------------------------------

posteromed_data <- read_csv("postmed_reach_data.csv")
head(posteromed_data)

## Data descriptives --------------------

all_descriptives_posteromed_data <- posteromed_data %>%
  gather(key = "time", value = "score", pre01, pre02, pre03, post01, post02, post03) %>%
  group_by(time) %>%
  summarise(mean = mean(score),
            sd = sd(score))

# Prepare posteromedial data 
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


## Plots ---------------------------------------------------------------------------

### Histogram 

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

### Q-Q plots 

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

### Boxplot 

ggplot(anova_posteromed_data, aes(x = time, y = score))+
  geom_violin(trim = FALSE)+
  geom_boxplot(width = .2)

## Posteromedial ANOVA  ----------------------------------------------------------------------------

posteromed_data_afx <- afex::aov_4(score ~ time + (time|participant), 
                                   data = anova_posteromed_data,
                                   anova_table = list(correction = "GG", es = "pes")) # using Greenhouse Geisser sphercity correction and partial eta squared
posteromed_data_afx

summary(posteromed_data_afx)


### Assumption checking ---------

# Normality test

shapiro.test(posteromed_data_afx$lm$residuals) 

anova_posteromed_data %>% 
  dplyr::group_by(time) %>% 
  rstatix::shapiro_test(score)

# POSTEROLATERAL --------------------------------------------------------------------

posterolat_data <- read_csv("postlat_reach_data.csv")
head(posterolat_data)

## Data descriptives --------------------

all_descriptives_posterolat_data <- posterolat_data %>%
  gather(key = "time", value = "score", pre01, pre02, pre03, post01, post02, post03) %>%
  group_by(time) %>%
  summarise(mean = mean(score),
            sd = sd(score))

# Prepare posteromedial data 
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

## Plots ---------------------------------------------------------------------------

### Histogram 

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

### Q-Q plots 

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

### Boxplot 

ggplot(anova_posterolat_data, aes(x = time, y = score))+
  geom_violin(trim = FALSE)+
  geom_boxplot(width = .2)

## Posterolateral ANOVA  ----------------------------------------------------------------------------

posterolat_data_afx <- afex::aov_4(score ~ time + (time|participant), 
                                   data = anova_posterolat_data,
                                   anova_table = list(correction = "GG", es = "pes")) # using Greenhouse Geisser sphercity correction and partial eta squared
posterolat_data_afx

summary(posterolat_data_afx)

### Assumption checking ---------

# Normality test

shapiro.test(posterolat_data_afx$lm$residuals) 

anova_posterolat_data %>% 
  dplyr::group_by(time) %>% 
  rstatix::shapiro_test(score)


# ANTERIOR EFFECT SIZE --------

## Calculate replication partial eta squared 

ant_pes_rep <- eta.F(dfm = anterior_data_afx$anova_table$`num Df`,
                     dfe = anterior_data_afx$anova_table$`den Df`,
                     Fvalue = anterior_data_afx$anova_table$F, a = 0.05) %>%
  as.data.frame() %>%
  select(eta, etalow, etahigh) %>%
  mutate(study_id = "Replication study")
ant_pes_rep


## Calculate original partial eta squared 

ant_pes_ori <- eta.F(dfm=3, dfe=19, Fvalue=3.818, a = 0.05) %>%
  as.data.frame() %>%
  select(eta, etalow, etahigh) %>% 
  mutate(study_id = "Original study")

# Z-TEST ------

pes_ori = ant_pes_ori$eta
pes_rep = ant_pes_rep$eta

rho_ori = 2*sqrt(pes_ori)-1
rho_rep = 2*sqrt(pes_rep)-1

rep_test <- compare_cor(r1 = rho_ori,
                        df1 = 19,
                        r2 = rho_rep,
                        df2 = 60.75)
rep_test

# POSTEROMEDIAL EFFECT SIZE --------

## Calculate replication partial eta squared

postmed_pes_rep <- eta.F(dfm = posteromed_data_afx$anova_table$`num Df`,
                     dfe = posteromed_data_afx$anova_table$`den Df`,
                     Fvalue = posteromed_data_afx$anova_table$F, a = 0.05) %>%
  as.data.frame() %>%
  select(eta, etalow, etahigh) %>%
  mutate(study_id = "Replication study")
postmed_pes_rep

## Calculate original partial eta squared

postmed_pes_ori <- eta.F(dfm=3, dfe=19, Fvalue=2.215, a = 0.05) %>%
  as.data.frame() %>%
  select(c("eta", "etalow", "etahigh")) %>% 
  mutate(study_id = c("Original study"))
postmed_pes_ori

# POSTEROLATERAL EFFECT SIZE --------
## Calculate replication partial eta squared

postlat_pes_rep <- eta.F(dfm = posterolat_data_afx$anova_table$`num Df`,
                         dfe = posterolat_data_afx$anova_table$`den Df`,
                         Fvalue = posterolat_data_afx$anova_table$F, a = 0.05) %>%
  as.data.frame() %>%
  select(eta, etalow, etahigh) %>%
  mutate(study_id = "Replication study")
postlat_pes_rep

## Calculate original  partial eta squared

postlat_pes_ori <- eta.F(dfm=3, dfe=19, Fvalue=6.503, a = 0.05) %>%
  as.data.frame() %>%
  select(c("eta", "etalow", "etahigh")) %>% 
  mutate(study_id = c("Original study"))
postlat_pes_ori

# Forest plot effect sizes ---------

## Labels for anterior reach direction effect sizes forest plot 
label_rep <- "0.148 [0.02, 0.32]"
label_ori <- "0.376 [0.00, 0.62]"

## Join rep_datasets 
plot <-
  merge(
    ant_pes_ori,
    ant_pes_rep,
    by = c("eta", "etalow", "etahigh", "study_id"),
    all = TRUE
  )

## Plot
ggplot(plot,
       aes(
         y = study_id,
         x = eta,
         xmin = etalow,
         xmax = etahigh
       )) +
  ggtitle("Partial eta squared [95% CI]") +
  geom_point() +
  geom_errorbarh(height = .1) +
  geom_vline(
    xintercept = 0,
    color = 'black',
    linetype = 'dashed',
    alpha = .4
  ) +
  theme_minimal() +
  scale_x_continuous(name = "Observed Effect Size", limits = c(-1, 2.2)) +
  scale_y_discrete(name = "") +
  annotate("text",
           x = 1.8,
           y = 2,
           label = label_rep) +
  annotate("text",
           x = 1.8,
           y = 1,
           label = label_ori) +
  theme(
    axis.line.x = element_line(color = "black"),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust = 0.94),
    panel.background = element_blank()
  )

ggsave(
  "anterior_reach_forestplot.png",
  plot = last_plot(),
  device = "png",
  width = NA,
  height = NA,
  dpi = 300,
  limitsize = TRUE,
  bg = '#ffffff'
)



