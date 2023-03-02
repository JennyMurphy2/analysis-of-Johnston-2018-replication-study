library(MOTE)
library(tidyverse)
library(TOSTER)

# Anterior data --------
## Calculate replication partial eta squared using F statistic and df  -------------
#dfm = degrees of freedom for the model/IV/between
#dfe = degrees of freedom for the error/residual/within

ant_pes_rep <- eta.F(dfm=1.6, dfe=60.7, Fvalue=6.60, a = 0.05)

ant_pes_rep <- as.data.frame(ant_pes_rep) 

ant_pes_rep_df <- ant_pes_rep %>%
  select(c("eta", "etalow", "etahigh")) %>%
  mutate(study_id = c("Replication study"))

## Calculate original study ES for anterior -------------

ant_pes_ori <- eta.F(dfm=3, dfe=19, Fvalue=3.818, a = 0.05) 

ant_pes_ori <- as.data.frame(ant_pes_ori) 

ant_pes_ori_df <- ant_pes_ori %>%
  select(c("eta", "etalow", "etahigh")) %>% 
  mutate(study_id = c("Original study"))

## Analyze the Anterior Reach Effect Sizes ------

pes_ori = ant_pes_ori_df$eta
pes_rep = ant_pes_rep_df$eta

rho_ori = 2*sqrt(pes_ori)-1
rho_rep = 2*sqrt(pes_rep)-1

rep_test <- compare_cor(r1 = rho_ori,
                        df1 = 19,
                        r2 = rho_rep,
                        df2 = 60.75)
rep_test

# Posteromedial data --------
## Calculate replication partial eta squared using F statistic and df --------
#dfm = degrees of freedom for the model/IV/between
#dfe = degrees of freedom for the error/residual/within

pm_pes_rep <- eta.F(dfm=2.45, dfe=93.09, Fvalue=2.067, a = 0.05)


pm_pes_rep <- as.data.frame(pm_pes_rep) 

pm_pes_rep_df <- pm_pes_rep %>%
  select(c("eta", "etalow", "etahigh")) %>% 
  mutate(study_id = c("Replication study"))

## Calculate original study ES for posteromedial -------------

pm_pes_ori <- eta.F(dfm=3, dfe=19, Fvalue=2.215, a = 0.05)


pm_pes_ori <- as.data.frame(pm_pes_ori) 

pm_pes_ori_df <- pm_pes_ori %>%
  select(c("eta", "etalow", "etahigh")) %>% 
  mutate(study_id = c("Original study"))

# Posterolateral data --------
## Calculate replication partial eta squared using F statistic and df----------
#dfm = degrees of freedom for the model/IV/between
#dfe = degrees of freedom for the error/residual/within

pl_pes_rep <- eta.F(dfm=2.32, dfe=88.15, Fvalue=2.19, a = 0.05)


pl_pes_rep <- as.data.frame(pl_pes_rep) 

pl_pes_rep_df <- pl_pes_rep %>%
  select(c("eta", "etalow", "etahigh")) %>% 
  mutate(study_id = c("Replication study"))

## Calculate original study ES for posterolateral -------------

pl_pes_ori <- eta.F(dfm=3, dfe=19, Fvalue=6.503, a = 0.05)


pl_pes_ori <- as.data.frame(pl_pes_ori) 

pl_pes_ori_df <- pl_pes_ori %>%
  select(c("eta", "etalow", "etahigh")) %>% 
  mutate(study_id = c("Original study"))