library(MOTE)
library(tidyverse)
library(TOSTER)

# Anterior data --------
# Only one main effect to be selected as per the selection protocol - DV = anterior reach distance (Murphy et al., 2023)
## Calculate replication partial eta squared using F statistic and df  -------------
#dfm = degrees of freedom for the model/IV/between
#dfe = degrees of freedom for the error/residual/within

ant_pes_rep <- eta.F(dfm=1.6, dfe=60.7, Fvalue=6.60, a = 0.05) %>%
  as.data.frame() %>%
  select(c("eta", "etalow", "etahigh")) %>%
  mutate(study_id = c("Replication study"))

## Calculate original study ES for anterior -------------

ant_pes_ori <- eta.F(dfm=3, dfe=19, Fvalue=3.818, a = 0.05) %>%
 as.data.frame() %>%
  select(c("eta", "etalow", "etahigh")) %>% 
  mutate(study_id = c("Original study"))

## Analyze the Anterior Reach Effect Sizes ------

pes_ori = ant_pes_ori$eta
pes_rep = ant_pes_rep$eta

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

pm_pes_rep <- eta.F(dfm=2.45, dfe=93.09, Fvalue=2.067, a = 0.05) %>%
 as.data.frame() %>%
  select(c("eta", "etalow", "etahigh")) %>% 
  mutate(study_id = c("Replication study"))

## Calculate original study ES for posteromedial -------------

pm_pes_ori <- eta.F(dfm=3, dfe=19, Fvalue=2.215, a = 0.05) %>%
  as.data.frame() %>%
  select(c("eta", "etalow", "etahigh")) %>% 
  mutate(study_id = c("Original study"))

# Posterolateral data --------
## Calculate replication partial eta squared using F statistic and df----------
#dfm = degrees of freedom for the model/IV/between
#dfe = degrees of freedom for the error/residual/within

pl_pes_rep <- eta.F(dfm=2.32, dfe=88.15, Fvalue=2.19, a = 0.05) %>%
 as.data.frame() %>%
  select(c("eta", "etalow", "etahigh")) %>% 
  mutate(study_id = c("Replication study"))

## Calculate original study ES for posterolateral -------------

pl_pes_ori <- eta.F(dfm=3, dfe=19, Fvalue=6.503, a = 0.05) %>%
 as.data.frame() %>%
  select(c("eta", "etalow", "etahigh")) %>% 
  mutate(study_id = c("Original study"))


# Murphy, J., Mesquida, C., Caldwell, A.R., Earp, B.D. and Warne, J.P., 2023. Proposal of a selection protocol for replication of studies in sports and exercise science. Sports medicine, 53(1), pp.281-291.
  
  
  
  