
library(tidyverse)
library(irr)

#Intraclass correlation coefficients (ICC 3, 1) were calculated
# across the three baseline measurements in order to determine
# the repeatability of the normalised YBT scores. Standard error of
# measurement (SEM) is an absolute index of reliability and was
# calculated in order to assess the degree of variation between the
#repeated measures. SEM was calculated using the formula: SEM = SD × √(1 − ICC)


# ANTERIOR DATA --------------------------------------------------------------------

anterior_data <- read_csv("anterior_reach_data.csv") %>%
  select(pre01, pre02, pre03)
head(anterior_data)

# Calculate the ICC 
icc_result_ant <- icc(anterior_data, model = "twoway", type = "consistency", unit = "average")

print(icc_result_ant)

# POSTEROLATERAL DATA --------------------------------------------------------------------

postlat_data <- read_csv("postlat_reach_data.csv") %>%
  select(pre01, pre02, pre03)
head(postlat_data)

# Calculate the ICC 
icc_result_pl <- icc(postlat_data, model = "twoway", type = "consistency", unit = "average")

print(icc_result_pl)

# POSTEROMEDIAL DATA --------------------------------------------------------------------

postmed_data <- read_csv("postmed_reach_data.csv") %>%
  select(pre01, pre02, pre03)
head(postmed_data)

# Calculate the ICC 
icc_result_ant <- icc(postmed_data, model = "twoway", type = "consistency", unit = "average")

print(icc_result_ant)
