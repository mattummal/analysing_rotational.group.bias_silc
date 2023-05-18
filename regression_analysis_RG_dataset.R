library(ggplot2) 
library(dplyr)
library(magrittr)
library(data.table)
library(MASS)
library(car)
library(texreg)
library(jtools)


setwd('C:/THESIS/SILC_dataset')

# Read data
SILC_FULL_DATASET <- read.csv("SILC_FULL_DATASET_2019-22.csv")

SILC_FULL_DATASET <- SILC_FULL_DATASET %>% 
  dplyr::select(DB010, AGE, RB090, EQ_INC20, DB075, AROPE_new, LWI_BD_new, SMD, MD5HH3, Sev_MSD, MIN60, RB030, PB190, PE040, PE041)


setnames(SILC_FULL_DATASET, c("DB010", "RB090", "EQ_INC20", "DB075", "AROPE_new", "LWI_BD_new", "SMD", "MD5HH3", "Sev_MSD", "MIN60", "RB030", "PB190", "PE040", "PE041"), 
         c("YEAR", "SEX", "EQINC", "RG", "AROPE", "LWI", "SMD", "MSD", "SMSD", "AROP", "PID", "MARITAL_STATUS", "EDUCATIONAL_STATUS", "EDUC"))

SILC_FULL_DATASET <- data.table(SILC_FULL_DATASET)
SILC_FULL_DATASET <- SILC_FULL_DATASET[, EDUC := EDUCATIONAL_STATUS]
SILC_FULL_DATASET[, EDUC:= NULL]


BE_data.19 <- SILC_FULL_DATASET %>% 
  filter(YEAR == 2019) 

BE_data.20 <- SILC_FULL_DATASET %>% 
  filter(YEAR == 2020) 

BE_data.21 <- SILC_FULL_DATASET %>% 
  filter(YEAR == 2021) 

BE_data.22 <- SILC_FULL_DATASET %>% 
  filter(YEAR == 2022)

data.2019 <- SILC_FULL_DATASET %>% 
  filter(YEAR == 2019) 

data.2020 <- SILC_FULL_DATASET %>% 
  filter(YEAR == 2020) 

data.2021 <- SILC_FULL_DATASET %>% 
  filter(YEAR == 2021) 

data.2022 <- SILC_FULL_DATASET %>% 
  filter(YEAR == 2022)



BE_data.19 %<>% mutate(RG = case_when(RG == "1" ~ "1 (2017)",
                                      RG == "2" ~ "2 (2018)",
                                      RG == "3" ~ "3 (2015)",
                                      RG == "4" ~ "4 (2016)",
                                      TRUE ~   "5 (2019)"))



BE_data.19 %<>% mutate(MARITAL_STATUS = case_when(MARITAL_STATUS == "1" ~ "Never married",
                                                  MARITAL_STATUS == "2" ~ "Married",
                                                  MARITAL_STATUS == "3" ~ "Separated",
                                                  MARITAL_STATUS == "4" ~ "Widowed",
                                                   TRUE ~   "Divorced"))




BE_data.20 %<>%  mutate(RG = case_when(RG == "1" ~ "1 (2017)",
                                       RG == "2" ~ "2 (2018)",
                                       RG == "3" ~ "3 (2015)",
                                       RG == "4" ~ "4 (2016)",
                                       RG == "5" ~ "5 (2019)",
                                            TRUE ~ "6 (2020)"))




BE_data.20 %<>% mutate(MARITAL_STATUS = case_when(MARITAL_STATUS == "1" ~ "Never married",
                                                  MARITAL_STATUS == "2" ~ "Married",
                                                  MARITAL_STATUS == "3" ~ "Separated",
                                                  MARITAL_STATUS == "4" ~ "Widowed",
                                                  TRUE ~   "Divorced"))


BE_data.21 %<>% mutate(RG = case_when(RG == "1" ~ "1 (2017)",
                                      RG == "2" ~ "2 (2018)",
                                      RG == "4" ~ "4 (2016)",
                                      RG == "5" ~ "5 (2019)",
                                      RG == "6" ~ "6 (2020)",
                                            TRUE ~ "7 (2021)"))





BE_data.21 %<>% mutate(MARITAL_STATUS = case_when(MARITAL_STATUS == "1" ~ "Never married",
                                                  MARITAL_STATUS == "2" ~ "Married",
                                                  MARITAL_STATUS == "3" ~ "Separated",
                                                  MARITAL_STATUS == "4" ~ "Widowed",
                                                  TRUE ~   "Divorced"))


BE_data.22 %<>% mutate(RG = case_when(RG == "1" ~ "1 (2017)",
                                      RG == "2" ~ "2 (2018)",
                                      RG == "5" ~ "5 (2019)",
                                      RG == "6" ~ "6 (2020)",
                                      RG == "7" ~ "7 (2021)",
                           TRUE ~ "8 (2022)"))  



BE_data.22 %<>% mutate(MARITAL_STATUS = case_when(MARITAL_STATUS == "1" ~ "Never married",
                                                  MARITAL_STATUS == "2" ~ "Married",
                                                  MARITAL_STATUS == "3" ~ "Separated",
                                                  MARITAL_STATUS == "4" ~ "Widowed",
                                                  TRUE ~   "Divorced"))



BE_data <- rbind(BE_data.19, BE_data.20, BE_data.21, BE_data.22)


################################ 2019 dataset ################################################

# Perform OLS regression on AROP


regression_arop.19 <- data.2019 %>% 
  dplyr::select(AGE, SEX, AROP, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(AROP ~ AGE + SEX + `RG (Aggregated)`, data = .)
  

BE_regression_arop.19 <- BE_data.19 %>% 
  dplyr::select(AGE, SEX, AROP) %>% 
  lm(AROP ~ AGE + SEX, data = .)

BE_regression_arop.rg.19 <- BE_data.19 %>% 
  dplyr::select(AGE, SEX, AROP, RG) %>% 
  lm(AROP ~ AGE + SEX + RG, data = .)



regression_summary_arop.19 <- summary(regression_arop.19)
BE_regression_summary_arop.19.1 <- summary(BE_regression_arop.19)
BE_regression_summary_arop.19.2 <- summary(BE_regression_arop.rg.19)


texreg(list(regression_summary_arop.19,BE_regression_summary_arop.19.1, BE_regression_summary_arop.19.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_arop.19", caption = "SILC 2019: AROP",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))





# Perform OLS regression on EQINC






BE_regression_eqinc.19 <- BE_data.19 %>% 
  dplyr::select(AGE, SEX, EQINC) %>% 
  lm(EQINC ~ AGE + SEX, data = .)



BE_regression_eqinc.rg.19 <- BE_data.19 %>% 
  dplyr::select(AGE, SEX, EQINC, RG) %>% 
  lm(EQINC ~ AGE + SEX + RG, data = .)


BE_regression_summary_eqinc.19.1 <- summary(BE_regression_eqinc.19)
BE_regression_summary_eqinc.19.2 <- summary(BE_regression_eqinc.rg.19)


regression_eqinc.19 <- data.2019 %>% 
  dplyr::select(AGE, SEX, EQINC, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(EQINC ~ AGE + SEX + `RG (Aggregated)`, data = .)

regression_summary_eqinc.19 <- summary(regression_eqinc.19)



texreg(list(regression_summary_eqinc.19,BE_regression_summary_eqinc.19.1, BE_regression_summary_eqinc.19.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_eqinc.19", caption = "SILC 2019: EQINC",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))





# Perform OLS regression on SMD

BE_regression_SMD.19 <- BE_data.19 %>% 
  dplyr::select(AGE, SEX, SMD) %>% 
  lm(SMD ~ AGE + SEX, data = .)



BE_regression_SMD.rg.19 <- BE_data.19 %>% 
  dplyr::select(AGE, SEX, SMD, RG) %>% 
  lm(SMD ~ AGE + SEX + RG, data = .)


BE_regression_summary_SMD.19.1 <- summary(BE_regression_SMD.19)
BE_regression_summary_SMD.19.2 <- summary(BE_regression_SMD.rg.19)




regression_SMD.19 <- data.2019 %>% 
  dplyr::select(AGE, SEX, SMD, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(SMD ~ AGE + SEX + `RG (Aggregated)`, data = .)

regression_summary_SMD.19 <- summary(regression_SMD.19)



texreg(list(regression_summary_SMD.19,BE_regression_summary_SMD.19.1, BE_regression_summary_SMD.19.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_SMD.19", caption = "SILC 2019: SMD",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))






# Perform OLS regression on LWI


BE_regression_lwi.19 <- BE_data.19 %>% 
  dplyr::select(AGE, SEX, LWI) %>% 
  lm(LWI ~ AGE + SEX, data = .)



BE_regression_lwi.rg.19 <- BE_data.19 %>% 
  dplyr::select(AGE, SEX, LWI, RG) %>% 
  lm(LWI ~ AGE + SEX + RG, data = .)


BE_regression_summary_lwi.19.1 <- summary(BE_regression_lwi.19)
BE_regression_summary_lwi.19.2 <- summary(BE_regression_lwi.rg.19)



regression_lwi.19 <- data.2019 %>% 
  dplyr::select(AGE, SEX, LWI, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(LWI ~ AGE + SEX + `RG (Aggregated)`, data = .)

regression_summary_lwi.19 <- summary(regression_lwi.19)

texreg(list(regression_summary_lwi.19,BE_regression_summary_lwi.19.1, BE_regression_summary_lwi.19.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_lwi.19", caption = "SILC 2019: LWI",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))






# Perform OLS regression on AROPE


BE_regression_arope.19 <- BE_data.19 %>% 
  dplyr::select(AGE, SEX, AROPE) %>% 
  lm(AROPE ~ AGE + SEX, data = .)



BE_regression_arope.rg.19 <- BE_data.19 %>% 
  dplyr::select(AGE, SEX, AROPE, RG) %>% 
  lm(AROPE ~ AGE + SEX + RG, data = .)


BE_regression_summary_arope.19.1 <- summary(BE_regression_arope.19)
BE_regression_summary_arope.19.2 <- summary(BE_regression_arope.rg.19)





regression_arope.19 <- data.2019 %>% 
  dplyr::select(AGE, SEX, AROPE, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(AROPE ~ AGE + SEX + `RG (Aggregated)`, data = .)

regression_summary_arope.19 <- summary(regression_arope.19)

texreg(list(regression_summary_arope.19,BE_regression_summary_arope.19.1, BE_regression_summary_arope.19.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_arope.19", caption = "SILC 2019: AROPE",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))




# Perform OLS regression on MSD


BE_regression_MSD.19 <- BE_data.19 %>% 
  dplyr::select(AGE, SEX, MSD) %>% 
  lm(MSD ~ AGE + SEX, data = .)



BE_regression_MSD.rg.19 <- BE_data.19 %>% 
  dplyr::select(AGE, SEX, MSD, RG) %>% 
  lm(MSD ~ AGE + SEX + RG, data = .)


BE_regression_summary_MSD.19.1 <- summary(BE_regression_MSD.19)
BE_regression_summary_MSD.19.2 <- summary(BE_regression_MSD.rg.19)

regression_MSD.19 <- data.2019 %>% 
  dplyr::select(AGE, SEX, MSD, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(MSD ~ AGE + SEX + `RG (Aggregated)`, data = .)

regression_summary_MSD.19 <- summary(regression_MSD.19)

texreg(list(regression_summary_MSD.19,BE_regression_summary_MSD.19.1, BE_regression_summary_MSD.19.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_MSD.19", caption = "SILC 2019: MSD",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))





# Perform OLS regression on SMSD


BE_regression_SMSD.19 <- BE_data.19 %>% 
  dplyr::select(AGE, SEX, SMSD) %>% 
  lm(SMSD ~ AGE + SEX, data = .)



BE_regression_SMSD.rg.19 <- BE_data.19 %>% 
  dplyr::select(AGE, SEX, SMSD, RG) %>% 
  lm(SMSD ~ AGE + SEX + RG, data = .)


BE_regression_summary_SMSD.19.1 <- summary(BE_regression_SMSD.19)
BE_regression_summary_SMSD.19.2 <- summary(BE_regression_SMSD.rg.19)

regression_SMSD.19 <- data.2019 %>% 
  dplyr::select(AGE, SEX, SMSD, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(SMSD ~ AGE + SEX + `RG (Aggregated)`, data = .)

regression_summary_SMSD.19 <- summary(regression_SMSD.19)

texreg(list(regression_summary_SMSD.19,BE_regression_summary_SMSD.19.1, BE_regression_summary_SMSD.19.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_SMSD.19", caption = "SILC 2019: SMSD",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))






################################ 2020 dataset ################################################



# Perform OLS regression on AROP


regression_arop.20 <- data.2020 %>% 
  dplyr::select(AGE, SEX, AROP, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(AROP ~ AGE + SEX + `RG (Aggregated)`, data = .)


BE_regression_arop.20 <- BE_data.20 %>% 
  dplyr::select(AGE, SEX, AROP) %>% 
  lm(AROP ~ AGE + SEX, data = .)



BE_regression_arop.rg.20 <- BE_data.20 %>% 
  dplyr::select(AGE, SEX, AROP, RG) %>% 
  lm(AROP ~ AGE + SEX + RG, data = .)


regression_summary_arop.20 <- summary(regression_arop.20)
BE_regression_summary_arop.20.1 <- summary(BE_regression_arop.20)
BE_regression_summary_arop.20.2 <- summary(BE_regression_arop.rg.20)



texreg(list(regression_summary_arop.20,BE_regression_summary_arop.20.1, BE_regression_summary_arop.20.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_arop.20", caption = "SILC 2020: AROP",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))




# Perform OLS regression on EQINC


BE_regression_eqinc.20 <- BE_data.20 %>% 
  dplyr::select(AGE, SEX, EQINC) %>% 
  lm(EQINC ~ AGE + SEX, data = .)



BE_regression_eqinc.rg.20 <- BE_data.20 %>% 
  dplyr::select(AGE, SEX, EQINC, RG) %>% 
  lm(EQINC ~ AGE + SEX + RG, data = .)


BE_regression_summary_eqinc.20.1 <- summary(BE_regression_eqinc.20)
BE_regression_summary_eqinc.20.2 <- summary(BE_regression_eqinc.rg.20)

regression_eqinc.20 <- data.2020 %>% 
  dplyr::select(AGE, SEX, EQINC, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(EQINC ~ AGE + SEX + `RG (Aggregated)`, data = .)

regression_summary_eqinc.20 <- summary(regression_eqinc.20)



texreg(list(regression_summary_eqinc.20,BE_regression_summary_eqinc.20.1, BE_regression_summary_eqinc.20.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_eqinc.20", caption = "SILC 2020: EQINC",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))






# Perform OLS regression on SMD

BE_regression_SMD.20 <- BE_data.20 %>% 
  dplyr::select(AGE, SEX, SMD) %>% 
  lm(SMD ~ AGE + SEX, data = .)



BE_regression_SMD.rg.20 <- BE_data.20 %>% 
  dplyr::select(AGE, SEX, SMD, RG) %>% 
  lm(SMD ~ AGE + SEX + RG, data = .)


BE_regression_summary_SMD.20.1 <- summary(BE_regression_SMD.20)
BE_regression_summary_SMD.20.2 <- summary(BE_regression_SMD.rg.20)


regression_SMD.20 <- data.2020 %>% 
  dplyr::select(AGE, SEX, SMD, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(SMD ~ AGE + SEX + `RG (Aggregated)`, data = .)

regression_summary_SMD.20 <- summary(regression_SMD.20)

texreg(list(regression_summary_SMD.20,BE_regression_summary_SMD.20.1, BE_regression_summary_SMD.20.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_SMD.20", caption = "SILC 2020: SMD",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))






# Perform OLS regression on LWI


BE_regression_lwi.20 <- BE_data.20 %>% 
  dplyr::select(AGE, SEX, LWI) %>% 
  lm(LWI ~ AGE + SEX, data = .)



BE_regression_lwi.rg.20 <- BE_data.20 %>% 
  dplyr::select(AGE, SEX, LWI, RG) %>% 
  lm(LWI ~ AGE + SEX + RG, data = .)


BE_regression_summary_lwi.20.1 <- summary(BE_regression_lwi.20)
BE_regression_summary_lwi.20.2 <- summary(BE_regression_lwi.rg.20)



regression_lwi.20 <- data.2020 %>% 
  dplyr::select(AGE, SEX, LWI, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(LWI ~ AGE + SEX + `RG (Aggregated)`, data = .)

regression_summary_lwi.20 <- summary(regression_lwi.20)

texreg(list(regression_summary_lwi.20,BE_regression_summary_lwi.20.1, BE_regression_summary_lwi.20.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_lwi.20", caption = "SILC 2020: LWI",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))



# Perform OLS regression on AROPE


BE_regression_arope.20 <- BE_data.20 %>% 
  dplyr::select(AGE, SEX, AROPE) %>% 
  lm(AROPE ~ AGE + SEX, data = .)



BE_regression_arope.rg.20 <- BE_data.20 %>% 
  dplyr::select(AGE, SEX, AROPE, RG) %>% 
  lm(AROPE ~ AGE + SEX + RG, data = .)


BE_regression_summary_arope.20.1 <- summary(BE_regression_arope.20)
BE_regression_summary_arope.20.2 <- summary(BE_regression_arope.rg.20)


regression_arope.20 <- data.2020 %>% 
  dplyr::select(AGE, SEX, AROPE, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(AROPE ~ AGE + SEX + `RG (Aggregated)`, data = .)

regression_summary_arope.20 <- summary(regression_arope.20)

texreg(list(regression_summary_arope.20,BE_regression_summary_arope.20.1, BE_regression_summary_arope.20.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_arope.20", caption = "SILC 2020: AROPE",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))



# Perform OLS regression on MSD


BE_regression_MSD.20 <- BE_data.20 %>% 
  dplyr::select(AGE, SEX, MSD) %>% 
  lm(MSD ~ AGE + SEX, data = .)



BE_regression_MSD.rg.20 <- BE_data.20 %>% 
  dplyr::select(AGE, SEX, MSD, RG) %>% 
  lm(MSD ~ AGE + SEX + RG, data = .)


BE_regression_summary_MSD.20.1 <- summary(BE_regression_MSD.20)
BE_regression_summary_MSD.20.2 <- summary(BE_regression_MSD.rg.20)


regression_MSD.20 <- data.2020 %>% 
  dplyr::select(AGE, SEX, MSD, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(MSD ~ AGE + SEX + `RG (Aggregated)`, data = .)

regression_summary_MSD.20 <- summary(regression_MSD.20)

texreg(list(regression_summary_MSD.20,BE_regression_summary_MSD.20.1, BE_regression_summary_MSD.20.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_MSD.20", caption = "SILC 2020: MSD",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))




# Perform OLS regression on SMSD


BE_regression_SMSD.20 <- BE_data.20 %>% 
  dplyr::select(AGE, SEX, SMSD) %>% 
  lm(SMSD ~ AGE + SEX, data = .)



BE_regression_SMSD.rg.20 <- BE_data.20 %>% 
  dplyr::select(AGE, SEX, SMSD, RG) %>% 
  lm(SMSD ~ AGE + SEX + RG, data = .)


BE_regression_summary_SMSD.20.1 <- summary(BE_regression_SMSD.20)
BE_regression_summary_SMSD.20.2 <- summary(BE_regression_SMSD.rg.20)



regression_SMSD.20 <- data.2020 %>% 
  dplyr::select(AGE, SEX, SMSD, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(SMSD ~ AGE + SEX + `RG (Aggregated)`, data = .)

regression_summary_SMSD.20 <- summary(regression_SMSD.20)

texreg(list(regression_summary_SMSD.20,BE_regression_summary_SMSD.20.1, BE_regression_summary_SMSD.20.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_SMSD.20", caption = "SILC 2020: SMSD",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))



################################ 2021 dataset ################################################



# Perform OLS regression on AROP



regression_arop.21 <- data.2021 %>% 
  dplyr::select(AGE, SEX, AROP, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(AROP ~ AGE + SEX + `RG (Aggregated)`, data = .)


BE_regression_arop.21 <- BE_data.21 %>% 
  dplyr::select(AGE, SEX, AROP) %>% 
  lm(AROP ~ AGE + SEX, data = .)



BE_regression_arop.rg.21 <- BE_data.21 %>% 
  dplyr::select(AGE, SEX, AROP, RG) %>% 
  lm(AROP ~ AGE + SEX + RG, data = .)


regression_summary_arop.21 <- summary(regression_arop.21)
BE_regression_summary_arop.21.1 <- summary(BE_regression_arop.21)
BE_regression_summary_arop.21.2 <- summary(BE_regression_arop.rg.21)



texreg(list(regression_summary_arop.21,BE_regression_summary_arop.21.1, BE_regression_summary_arop.21.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_arop.21", caption = "SILC 2021: AROP",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))



# Perform OLS regression on SMD

BE_regression_SMD.21 <- BE_data.21 %>% 
  dplyr::select(AGE, SEX, SMD) %>% 
  lm(SMD ~ AGE + SEX, data = .)



BE_regression_SMD.rg.21 <- BE_data.21 %>% 
  dplyr::select(AGE, SEX, SMD, RG) %>% 
  lm(SMD ~ AGE + SEX + RG, data = .)


BE_regression_summary_SMD.21.1 <- summary(BE_regression_SMD.21)
BE_regression_summary_SMD.21.2 <- summary(BE_regression_SMD.rg.21)


regression_SMD.21 <- data.2021 %>% 
  dplyr::select(AGE, SEX, SMD, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(SMD ~ AGE + SEX + `RG (Aggregated)`, data = .)

regression_summary_SMD.21 <- summary(regression_SMD.21)

texreg(list(regression_summary_SMD.21,BE_regression_summary_SMD.21.1, BE_regression_summary_SMD.21.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_SMD.21", caption = "SILC 2021: SMD",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))






# Perform OLS regression on EQINC


BE_regression_eqinc.21 <- BE_data.21 %>% 
  dplyr::select(AGE, SEX, EQINC) %>% 
  lm(EQINC ~ AGE + SEX, data = .)



BE_regression_eqinc.rg.21 <- BE_data.21 %>% 
  dplyr::select(AGE, SEX, EQINC, RG) %>% 
  lm(EQINC ~ AGE + SEX + RG, data = .)


BE_regression_summary_eqinc.21.1 <- summary(BE_regression_eqinc.21)
BE_regression_summary_eqinc.21.2 <- summary(BE_regression_eqinc.rg.21)


regression_eqinc.21 <- data.2021 %>% 
  dplyr::select(AGE, SEX, EQINC, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(EQINC ~ AGE + SEX + `RG (Aggregated)`, data = .)

regression_summary_eqinc.21 <- summary(regression_eqinc.21)



texreg(list(regression_summary_eqinc.21,BE_regression_summary_eqinc.21.1, BE_regression_summary_eqinc.21.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_eqinc.21", caption = "SILC 2021: EQINC",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))





# Perform OLS regression on LWI


BE_regression_lwi.21 <- BE_data.21 %>% 
  dplyr::select(AGE, SEX, LWI) %>% 
  lm(LWI ~ AGE + SEX, data = .)



BE_regression_lwi.rg.21 <- BE_data.21 %>% 
  dplyr::select(AGE, SEX, LWI, RG) %>% 
  lm(LWI ~ AGE + SEX + RG, data = .)


BE_regression_summary_lwi.21.1 <- summary(BE_regression_lwi.21)
BE_regression_summary_lwi.21.2 <- summary(BE_regression_lwi.rg.21)



regression_lwi.21 <- data.2021 %>% 
  dplyr::select(AGE, SEX, LWI, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(LWI ~ AGE + SEX + `RG (Aggregated)`, data = .)

regression_summary_lwi.21 <- summary(regression_lwi.21)

texreg(list(regression_summary_lwi.21,BE_regression_summary_lwi.21.1, BE_regression_summary_lwi.21.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_lwi.21", caption = "SILC 2021: LWI",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))





# Perform OLS regression on AROPE


BE_regression_arope.21 <- BE_data.21 %>% 
  dplyr::select(AGE, SEX, AROPE) %>% 
  lm(AROPE ~ AGE + SEX, data = .)



BE_regression_arope.rg.21 <- BE_data.21 %>% 
  dplyr::select(AGE, SEX, AROPE, RG) %>% 
  lm(AROPE ~ AGE + SEX + RG, data = .)


BE_regression_summary_arope.21.1 <- summary(BE_regression_arope.21)
BE_regression_summary_arope.21.2 <- summary(BE_regression_arope.rg.21)


regression_arope.21 <- data.2021 %>% 
  dplyr::select(AGE, SEX, AROPE, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(AROPE ~ AGE + SEX + `RG (Aggregated)`, data = .)

regression_summary_arope.21 <- summary(regression_arope.21)

texreg(list(regression_summary_arope.21,BE_regression_summary_arope.21.1, BE_regression_summary_arope.21.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_arope.21", caption = "SILC 2021: AROPE",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))



# Perform OLS regression on MSD


BE_regression_MSD.21 <- BE_data.21 %>% 
  dplyr::select(AGE, SEX, MSD) %>% 
  lm(MSD ~ AGE + SEX, data = .)



BE_regression_MSD.rg.21 <- BE_data.21 %>% 
  dplyr::select(AGE, SEX, MSD, RG) %>% 
  lm(MSD ~ AGE + SEX + RG, data = .)


BE_regression_summary_MSD.21.1 <- summary(BE_regression_MSD.21)
BE_regression_summary_MSD.21.2 <- summary(BE_regression_MSD.rg.21)


regression_MSD.21 <- data.2021 %>% 
  dplyr::select(AGE, SEX, MSD, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(MSD ~ AGE + SEX + `RG (Aggregated)`, data = .)

regression_summary_MSD.21 <- summary(regression_MSD.21)

texreg(list(regression_summary_MSD.21,BE_regression_summary_MSD.21.1, BE_regression_summary_MSD.21.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_MSD.21", caption = "SILC 2021: MSD",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))



# Perform OLS regression on SMSD


BE_regression_SMSD.21 <- BE_data.21 %>% 
  dplyr::select(AGE, SEX, SMSD) %>% 
  lm(SMSD ~ AGE + SEX, data = .)



BE_regression_SMSD.rg.21 <- BE_data.21 %>% 
  dplyr::select(AGE, SEX, SMSD, RG) %>% 
  lm(SMSD ~ AGE + SEX + RG, data = .)


BE_regression_summary_SMSD.21.1 <- summary(BE_regression_SMSD.21)
BE_regression_summary_SMSD.21.2 <- summary(BE_regression_SMSD.rg.21)

regression_SMSD.21 <- data.2021 %>% 
  dplyr::select(AGE, SEX, SMSD, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(SMSD ~ AGE + SEX + `RG (Aggregated)`, data = .)

regression_summary_SMSD.21 <- summary(regression_SMSD.21)

texreg(list(regression_summary_SMSD.21,BE_regression_summary_SMSD.21.1, BE_regression_summary_SMSD.21.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_SMSD.21", caption = "SILC 2021: SMSD",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))





################################ 2022 dataset ################################################


# Perform OLS regression on AROP




regression_arop.22 <- data.2022 %>% 
  dplyr::select(AGE, SEX, AROP, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(AROP ~ AGE + SEX + `RG (Aggregated)`, data = .)




BE_regression_arop.22 <- BE_data.22 %>% 
  dplyr::select(AGE, SEX, AROP) %>% 
  lm(AROP ~ AGE + SEX, data = .)



BE_regression_arop.rg.22 <- BE_data.22 %>% 
  dplyr::select(AGE, SEX, AROP, RG) %>% 
  lm(AROP ~ AGE + SEX + RG, data = .)


regression_summary_arop.22 <- summary(regression_arop.22)
BE_regression_summary_arop.22.1 <- summary(BE_regression_arop.22)
BE_regression_summary_arop.22.2 <- summary(BE_regression_arop.rg.22)



texreg(list(regression_summary_arop.22,BE_regression_summary_arop.22.1, BE_regression_summary_arop.22.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_arop.22", caption = "SILC 2022: AROP",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))



# Perform OLS regression on SMD

BE_regression_SMD.22 <- BE_data.22 %>% 
  dplyr::select(AGE, SEX, SMD) %>% 
  lm(SMD ~ AGE + SEX, data = .)



BE_regression_SMD.rg.22 <- BE_data.22 %>% 
  dplyr::select(AGE, SEX, SMD, RG) %>% 
  lm(SMD ~ AGE + SEX + RG, data = .)


BE_regression_summary_SMD.22.1 <- summary(BE_regression_SMD.22)
BE_regression_summary_SMD.22.2 <- summary(BE_regression_SMD.rg.22)


regression_SMD.22 <- data.2022 %>% 
  dplyr::select(AGE, SEX, SMD, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(SMD ~ AGE + SEX + `RG (Aggregated)`, data = .)

regression_summary_SMD.22 <- summary(regression_SMD.22)

texreg(list(regression_summary_SMD.22,BE_regression_summary_SMD.22.1, BE_regression_summary_SMD.22.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_SMD.22", caption = "SILC 2022: SMD",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))


# Perform OLS regression on EQINC


BE_regression_eqinc.22 <- BE_data.22 %>% 
  dplyr::select(AGE, SEX, EQINC) %>% 
  lm(EQINC ~ AGE + SEX, data = .)



BE_regression_eqinc.rg.22 <- BE_data.22 %>% 
  dplyr::select(AGE, SEX, EQINC, RG) %>% 
  lm(EQINC ~ AGE + SEX + RG, data = .)


BE_regression_summary_eqinc.22.1 <- summary(BE_regression_eqinc.22)
BE_regression_summary_eqinc.22.2 <- summary(BE_regression_eqinc.rg.22)



regression_eqinc.22 <- data.2022 %>% 
  dplyr::select(AGE, SEX, EQINC, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(EQINC ~ AGE + SEX + `RG (Aggregated)`, data = .)

regression_summary_eqinc.22 <- summary(regression_eqinc.22)



texreg(list(regression_summary_eqinc.22,BE_regression_summary_eqinc.22.1, BE_regression_summary_eqinc.22.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_eqinc.22", caption = "SILC 2022: EQINC",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))




# Perform OLS regression on LWI


BE_regression_lwi.22 <- BE_data.22 %>% 
  dplyr::select(AGE, SEX, LWI) %>% 
  lm(LWI ~ AGE + SEX, data = .)



BE_regression_lwi.rg.22 <- BE_data.22 %>% 
  dplyr::select(AGE, SEX, LWI, RG) %>% 
  lm(LWI ~ AGE + SEX + RG, data = .)


BE_regression_summary_lwi.22.1 <- summary(BE_regression_lwi.22)
BE_regression_summary_lwi.22.2 <- summary(BE_regression_lwi.rg.22)



regression_lwi.22 <- data.2022 %>% 
  dplyr::select(AGE, SEX, LWI, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(LWI ~ AGE + SEX + `RG (Aggregated)`, data = .)

regression_summary_lwi.22 <- summary(regression_lwi.22)

texreg(list(regression_summary_lwi.22,BE_regression_summary_lwi.22.1, BE_regression_summary_lwi.22.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_lwi.22", caption = "SILC 2022: LWI",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))



# Perform OLS regression on AROPE


BE_regression_arope.22 <- BE_data.22 %>% 
  dplyr::select(AGE, SEX, AROPE) %>% 
  lm(AROPE ~ AGE + SEX, data = .)



BE_regression_arope.rg.22 <- BE_data.22 %>% 
  dplyr::select(AGE, SEX, AROPE, RG) %>% 
  lm(AROPE ~ AGE + SEX + RG, data = .)


BE_regression_summary_arope.22.1 <- summary(BE_regression_arope.22)
BE_regression_summary_arope.22.2 <- summary(BE_regression_arope.rg.22)


regression_arope.22 <- data.2022 %>% 
  dplyr::select(AGE, SEX, AROPE, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(AROPE ~ AGE + SEX + `RG (Aggregated)`, data = .)

regression_summary_arope.22 <- summary(regression_arope.22)

texreg(list(regression_summary_arope.22,BE_regression_summary_arope.22.1, BE_regression_summary_arope.22.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_arope.22", caption = "SILC 2022: AROPE",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))




# Perform OLS regression on MSD


BE_regression_MSD.22 <- BE_data.22 %>% 
  dplyr::select(AGE, SEX, MSD) %>% 
  lm(MSD ~ AGE + SEX, data = .)



BE_regression_MSD.rg.22 <- BE_data.22 %>% 
  dplyr::select(AGE, SEX, MSD, RG) %>% 
  lm(MSD ~ AGE + SEX + RG, data = .)


BE_regression_summary_MSD.22.1 <- summary(BE_regression_MSD.22)
BE_regression_summary_MSD.22.2 <- summary(BE_regression_MSD.rg.22)


regression_MSD.22 <- data.2022 %>% 
  dplyr::select(AGE, SEX, MSD, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(MSD ~ AGE + SEX + `RG (Aggregated)`, data = .)

regression_summary_MSD.22 <- summary(regression_MSD.22)

texreg(list(regression_summary_MSD.22,BE_regression_summary_MSD.22.1, BE_regression_summary_MSD.22.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_MSD.22", caption = "SILC 2022: MSD",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))



# Perform OLS regression on SMSD


BE_regression_SMSD.22 <- BE_data.22 %>% 
  dplyr::select(AGE, SEX, SMSD) %>% 
  lm(SMSD ~ AGE + SEX, data = .)



BE_regression_SMSD.rg.22 <- BE_data.22 %>% 
  dplyr::select(AGE, SEX, SMSD, RG) %>% 
  lm(SMSD ~ AGE + SEX + RG, data = .)


BE_regression_summary_SMSD.22.1 <- summary(BE_regression_SMSD.22)
BE_regression_summary_SMSD.22.2 <- summary(BE_regression_SMSD.rg.22)



regression_SMSD.22 <- data.2022 %>% 
  dplyr::select(AGE, SEX, SMSD, RG) %>% 
  dplyr::rename(`RG (Aggregated)` = RG) %>% 
  lm(SMSD ~ AGE + SEX + `RG (Aggregated)`, data = .)

regression_summary_SMSD.22 <- summary(regression_SMSD.22)

texreg(list(regression_summary_SMSD.22,BE_regression_summary_SMSD.22.1, BE_regression_summary_SMSD.22.2), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "regression_SMSD.22", caption = "SILC 2022: SMSD",
       float.pos = "h", digits = 7, custom.model.names = c("Rotational Group (Aggregated)","w/o Rotational Group", "Rotational Group (Categorised)"))





##########################################################################################################################################################################


require(devtools)
require(sjPlot)
require(ggiraph)
require(ggiraphExtra)
require(plyr)

windows(width=11.9, height=9.1, title="Controlled Size")

#############  AROP #############  

glm_arop.19 <- data.2019 %>% 
  dplyr::select(AGE, SEX, AROP, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(AROP ~  AGE + SEX + MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP, data = . , family = binomial)

summary(glm_arop.19)


glm_arop.20 <- data.2020 %>% 
  dplyr::select(AGE, SEX, AROP, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(AROP ~  AGE + SEX + MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP, data = . , family = binomial)

summary(glm_arop.20)


glm_arop.21 <- data.2021 %>% 
  dplyr::select(AGE, SEX, AROP, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(AROP ~  AGE + SEX  + MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP, data = . , family = binomial)

summary(glm_arop.21)


glm_arop.22 <- data.2022 %>% 
  dplyr::select(AGE, SEX, AROP, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(AROP ~  AGE + SEX +  MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP, data = . , family = binomial)

summary(glm_arop.22)



OR_arop <- sjPlot::plot_models(glm_arop.19, glm_arop.20, glm_arop.21, glm_arop.22, digits = 5, 
                    show.values = T, show.p = T, p.shape = F,
                    dot.size = 5, spacing = 1, title = "Odds Ratio: AROP", 
                    m.labels = c("AROP 2019", "AROP 2020", "AROP 2021", "AROP 2022"))



ggsave("OR/OR_arop.pdf")


texreg(list(glm_arop.19,glm_arop.20, glm_arop.21, glm_arop.22), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "glm_arop", caption = "GLM: AROP",
       float.pos = "h", digits = 5, custom.model.names = c("SILC: 2019","SILC: 2020", "SILC: 2021", "SILC: 2022"))





#############  SMD #############  


glm_SMD.19 <- data.2019 %>% 
  dplyr::select(AGE, SEX, SMD, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(SMD ~  AGE + SEX + MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP , data = . , family = binomial)

summary(glm_SMD.19)


glm_SMD.20 <- data.2020 %>% 
  dplyr::select(AGE, SEX, SMD, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(SMD ~  AGE + SEX + MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP , data = . , family = binomial)

summary(glm_SMD.20)


glm_SMD.21 <- data.2021 %>% 
  dplyr::select(AGE, SEX, SMD, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(SMD ~  AGE + SEX + MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP , data = . , family = binomial)

summary(glm_SMD.21)


glm_SMD.22 <- data.2022 %>% 
  dplyr::select(AGE, SEX, SMD, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(SMD ~  AGE + SEX + MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP , data = . , family = binomial)

summary(glm_SMD.22)




OR_SMD <- sjPlot::plot_models(glm_SMD.19, glm_SMD.20, glm_SMD.21, glm_SMD.22, digits = 5, 
                    show.values = T, show.p = T, p.shape = F, 
                    dot.size = 5, spacing = 0.9, title = "Odds Ratio: SMD",
                    m.labels = c("SMD 2019", "SMD 2020", "SMD 2021", "SMD 2022"))

ggsave("OR/OR_SMD.pdf")



texreg(list(glm_SMD.19,glm_SMD.20, glm_SMD.21, glm_SMD.22), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "glm_SMD", caption = "GLM: SMD",
       float.pos = "h", digits = 5, custom.model.names = c("SILC: 2019","SILC: 2020", "SILC: 2021", "SILC: 2022"))




#############  LWI #############  


glm_LWI.19 <- data.2019 %>% 
  dplyr::select(AGE, SEX, LWI, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(LWI ~  AGE + SEX + MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP , data = . , family = binomial)

summary(glm_LWI.19)


glm_LWI.20 <- data.2020 %>% 
  dplyr::select(AGE, SEX, LWI, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(LWI ~  AGE + SEX + MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP , data = . , family = binomial)

summary(glm_LWI.20)


glm_LWI.21 <- data.2021 %>% 
  dplyr::select(AGE, SEX, LWI, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(LWI ~  AGE + SEX + MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP , data = . , family = binomial)

summary(glm_LWI.21)


glm_LWI.22 <- data.2022 %>% 
  dplyr::select(AGE, SEX, LWI, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(LWI ~  AGE + SEX + MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP , data = . , family = binomial)

summary(glm_LWI.22)




OR_LWI <- sjPlot::plot_models(glm_LWI.19, glm_LWI.20, glm_LWI.21, glm_LWI.22, digits = 5, 
                              show.values = T, show.p = T, p.shape = F, 
                              dot.size = 5, spacing = 0.9, title = "Odds Ratio: LWI",
                              m.labels = c("LWI 2019", "LWI 2020", "LWI 2021", "LWI 2022"))

ggsave("OR/OR_LWI.pdf")



texreg(list(glm_LWI.19,glm_LWI.20, glm_LWI.21, glm_LWI.22), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "glm_LWI", caption = "GLM: LWI",
       float.pos = "h", digits = 5, custom.model.names = c("SILC: 2019","SILC: 2020", "SILC: 2021", "SILC: 2022"))





#############  EQINC #############  


glm_EQINC.19 <- data.2019 %>% 
  dplyr::select(AGE, SEX, EQINC, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(EQINC ~  AGE + SEX + MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP , data = ., family = gaussian)

summary(glm_EQINC.19)


glm_EQINC.20 <- data.2020 %>% 
  dplyr::select(AGE, SEX, EQINC, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(EQINC ~  AGE + SEX + MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP , data = . , family = gaussian)

summary(glm_EQINC.20)


glm_EQINC.21 <- data.2021 %>% 
  dplyr::select(AGE, SEX, EQINC, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(EQINC ~  AGE + SEX + MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP , data = . , family = gaussian)

summary(glm_EQINC.21)


glm_EQINC.22 <- data.2022 %>% 
  dplyr::select(AGE, SEX, EQINC, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(EQINC ~  AGE + SEX + MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP , data = . , family = gaussian)

summary(glm_EQINC.22)



OR_EQINC <- sjPlot::plot_models(glm_EQINC.19, glm_EQINC.20, glm_EQINC.21, glm_EQINC.22, digits = 5, 
                              show.values = T, show.p = T, p.shape = F, 
                              dot.size = 5, spacing = 0.9, title = "GLM Estimates: EQINC",
                              m.labels = c("EQINC 2019", "EQINC 2020", "EQINC 2021", "EQINC 2022"))

ggsave("OR/OR_EQINC.pdf")






texreg(list(glm_EQINC.19,glm_EQINC.20, glm_EQINC.21, glm_EQINC.22), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "glm_EQINC", caption = "GLM: EQINC",
       float.pos = "h", digits = 5, custom.model.names = c("SILC: 2019","SILC: 2020", "SILC: 2021", "SILC: 2022"))





#############  AROPE #############  


glm_AROPE.19 <- data.2019 %>% 
  dplyr::select(AGE, SEX, AROPE, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(AROPE ~  AGE + SEX + MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP , data = . , family = binomial)

summary(glm_AROPE.19)


glm_AROPE.20 <- data.2020 %>% 
  dplyr::select(AGE, SEX, AROPE, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(AROPE ~  AGE + SEX + MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP , data = . , family = binomial)

summary(glm_AROPE.20)


glm_AROPE.21 <- data.2021 %>% 
  dplyr::select(AGE, SEX, AROPE, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(AROPE ~  AGE + SEX + MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP , data = . , family = binomial)

summary(glm_AROPE.21)


glm_AROPE.22 <- data.2022 %>% 
  dplyr::select(AGE, SEX, AROPE, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(AROPE ~  AGE + SEX + MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP , data = . , family = binomial)

summary(glm_AROPE.22)



OR_AROPE <- sjPlot::plot_models(glm_AROPE.19, glm_AROPE.20, glm_AROPE.21, glm_AROPE.22, digits = 5, 
                                show.values = T, show.p = T, p.shape = F, 
                                dot.size = 5, spacing = 0.9, title = "Odds Ratio: AROPE",
                                m.labels = c("AROPE 2019", "AROPE 2020", "AROPE 2021", "AROPE 2022"))

ggsave("OR/OR_AROPE.pdf")



texreg(list(glm_AROPE.19,glm_AROPE.20, glm_AROPE.21, glm_AROPE.22), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "glm_AROPE", caption = "GLM: AROPE",
       float.pos = "h", digits = 5, custom.model.names = c("SILC: 2019","SILC: 2020", "SILC: 2021", "SILC: 2022"))




#############  MSD #############  


glm_MSD.19 <- data.2019 %>% 
  dplyr::select(AGE, SEX, MSD, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(MSD ~  AGE + SEX + MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP , data = . , family = binomial)

summary(glm_MSD.19)


glm_MSD.20 <- data.2020 %>% 
  dplyr::select(AGE, SEX, MSD, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(MSD ~  AGE + SEX + MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP , data = . , family = binomial)

summary(glm_MSD.20)


glm_MSD.21 <- data.2021 %>% 
  dplyr::select(AGE, SEX, MSD, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(MSD ~  AGE + SEX + MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP , data = . , family = binomial)

summary(glm_MSD.21)


glm_MSD.22 <- data.2022 %>% 
  dplyr::select(AGE, SEX, MSD, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(MSD ~  AGE + SEX + MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP , data = . , family = binomial)

summary(glm_MSD.22)



OR_MSD <- sjPlot::plot_models(glm_MSD.19, glm_MSD.20, glm_MSD.21, glm_MSD.22, digits = 5, 
                                show.values = T, show.p = T, p.shape = F, 
                                dot.size = 5, spacing = 0.9, title = "Odds Ratio: MSD",
                                m.labels = c("MSD 2019", "MSD 2020", "MSD 2021", "MSD 2022"))

ggsave("OR/OR_MSD.pdf")


texreg(list(glm_MSD.19,glm_MSD.20, glm_MSD.21, glm_MSD.22), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "glm_MSD", caption = "GLM: MSD",
       float.pos = "h", digits = 5, custom.model.names = c("SILC: 2019","SILC: 2020", "SILC: 2021", "SILC: 2022"))






#############  SMSD #############  


glm_SMSD.19 <- data.2019 %>% 
  dplyr::select(AGE, SEX, SMSD, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(SMSD ~  AGE + SEX + MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP , data = . , family = binomial)

summary(glm_SMSD.19)


glm_SMSD.20 <- data.2020 %>% 
  dplyr::select(AGE, SEX, SMSD, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(SMSD ~  AGE + SEX + MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP , data = . , family = binomial)

summary(glm_SMSD.20)


glm_SMSD.21 <- data.2021 %>% 
  dplyr::select(AGE, SEX, SMSD, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(SMSD ~  AGE + SEX + MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP , data = . , family = binomial)

summary(glm_SMSD.21)


glm_SMSD.22 <- data.2022 %>% 
  dplyr::select(AGE, SEX, SMSD, RG, MARITAL_STATUS, EDUCATIONAL_STATUS) %>% 
  dplyr::rename(ROTATIONAL_GROUP = RG) %>% 
  glm(SMSD ~  AGE + SEX + MARITAL_STATUS + EDUCATIONAL_STATUS + ROTATIONAL_GROUP , data = . , family = binomial)

summary(glm_SMSD.22)



OR_SMSD <- sjPlot::plot_models(glm_SMSD.19, glm_SMSD.20, glm_SMSD.21, glm_SMSD.22, digits = 5, 
                              show.values = T, show.p = T, p.shape = F, 
                              dot.size = 5, spacing = 0.9, title = "Odds Ratio: SMSD",
                              m.labels = c("SMSD 2019", "SMSD 2020", "SMSD 2021", "SMSD 2022"))

ggsave("OR/OR_SMSD.pdf")




texreg(list(glm_SMSD.19,glm_SMSD.20, glm_SMSD.21, glm_SMSD.22), dcolumn = TRUE, booktabs = TRUE,
       use.packages = FALSE, label = "glm_SMSD", caption = "GLM: SMSD",
       float.pos = "h", digits = 5, custom.model.names = c("SILC: 2019","SILC: 2020", "SILC: 2021", "SILC: 2022"))











