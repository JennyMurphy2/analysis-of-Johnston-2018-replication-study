library(pastecs)
library(tidyverse)
library(forcats)

# ANTERIOR --------------------------------------------------------------------

anterior_data <- read_csv("anterior_reach_data.csv")
head(anterior_data)

## Prepare anterior data 
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

# Convert categorical data to factors

anova_anterior_data <- anova_anterior_data %>%
  mutate(participant = as.factor(participant),
         time = as.factor(time),
         intervention = as.factor(intervention),
         score = as.numeric(score))

## Descriptives ------------------------------------------------------

descriptives_anterior_data <- anova_anterior_data %>%
  group_by(time, intervention) %>%
  summarise(mean = mean(score),
            sd = sd(score))

## Anterior plot ------------------------------------------------------

### Line graph

ant_plot <- descriptives_anterior_data %>%
  select(-c("intervention")) %>%
  mutate(time = fct_relevel(time, c("pre03", "post01", "post02", "post03"))) %>%
  ggplot(aes(x= time, y= mean)) + 
  geom_line(group = 1) + 
  geom_point() + 
  geom_errorbar(aes(ymin = mean - sd, ymax= mean + sd), 
                width=.2, position=position_dodge(0.05)) +
  scale_y_continuous(name = "Normalised mean anterior reach distance (%)", 
                     limits = c(45, 85)) +
  scale_x_discrete(name = "Time (minutes) to fatigue intervention") +
  theme_bw()
ant_plot

ggsave("ant_linegraph.png",
       plot = ant_plot,
       device = "png",
       width = NA,
       height = NA,
       dpi = 300,
       limitsize = TRUE
)

# POSTEROMEDIAL --------------------------------------------------------------------

posteromed_data <- read_csv("postmed_reach_data.csv")
head(posteromed_data)

## Prepare posteromed data 
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

# Convert categorical data to factors

anova_posteromed_data <- anova_posteromed_data %>%
  mutate(participant = as.factor(participant),
         time = as.factor(time),
         intervention = as.factor(intervention),
         score = as.numeric(score))

## Descriptives ------------------------------------------------------

descriptives_posteromed_data <- anova_posteromed_data %>%
  group_by(time, intervention) %>%
  summarise(mean = mean(score),
            sd = sd(score))

## Posteromedial plot ------------------------------------------------------

### Line graph 

postmed_plot <- descriptives_posteromed_data %>%
  select(-c("intervention")) %>%
  mutate(time = fct_relevel(time, c("pre03", "post01", "post02", "post03"))) %>%
  ggplot(aes(x= time, y= mean)) + 
  geom_line(group = 1) + 
  geom_point() + 
  geom_errorbar(aes(ymin = mean - sd, ymax= mean + sd), 
                width=.2, position=position_dodge(0.05)) +
  scale_y_continuous(name = "Normalised mean posteromedial reach distance (%)", 
                     limits = c(90, 125)) +
  scale_x_discrete(name = "Time (minutes) to fatigue intervention") +
  theme_bw()
postmed_plot

ggsave("postmed_linegraph.png",
       plot = postmed_plot,
       device = "png",
       width = NA,
       height = NA,
       dpi = 300,
       limitsize = TRUE
)

# POSTEROLATERAL --------------------------------------------------------------------

posterolat_data <- read_csv("postlat_reach_data.csv")
head(posterolat_data)

## Prepare posterolateral data 
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

# Convert categorical data to factors

anova_posterolat_data <- anova_posterolat_data %>%
  mutate(participant = as.factor(participant),
         time = as.factor(time),
         intervention = as.factor(intervention),
         score = as.numeric(score))

## Descriptives ------------------------------------------------------

descriptives_posterolat_data <- anova_posterolat_data %>%
  group_by(time, intervention) %>%
  summarise(mean = mean(score),
            sd = sd(score))

## Posterolateral plot ------------------------------------------------------

### Line graph

postlat_plot <- descriptives_posterolat_data %>%
  select(-c("intervention")) %>%
  mutate(time = fct_relevel(time, c("pre03", "post01", "post02", "post03"))) %>%
  ggplot(aes(x= time, y= mean)) + 
  geom_line(group = 1) + 
  geom_point() + 
  geom_errorbar(aes(ymin = mean - sd, ymax= mean + sd), 
                width=.2, position=position_dodge(0.05)) +
  scale_y_continuous(name = "Normalised mean posterolateral reach distance (%)", 
                     limits = c(80, 120)) +
  scale_x_discrete(name = "Time (minutes) to fatigue intervention") +
  theme_bw()
postlat_plot

ggsave("postlat_linegraph.png",
       plot = postlat_plot,
       device = "png",
       width = NA,
       height = NA,
       dpi = 300,
       limitsize = TRUE
)

