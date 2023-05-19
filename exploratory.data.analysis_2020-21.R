library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(ggsurvey)
library(stargazer)
library(srvyr)
library(MASS)
library(RPEIF)
library(dineq)
library(boot)
library(MethodCompare)
library(psych)
library(magrittr)
library(laeken)
library(SimDesign)
library(RColorBrewer)
library(gmodels)
library(car)


setwd('C:/THESIS/SILC_dataset')


####################################### BE-SILC 2020 ##########################################

merged2020 <- read.csv("merged2020.csv")
data_2020 <- as_tibble(merged2020)
data_2020.df <- data.frame(data_2020)
data_2020.df  %<>% mutate(DB075 = case_when(DB075 == "1" ~ "1 (2017)",
                         DB075 == "2" ~ "2 (2018)",
                         DB075 == "3" ~ "3 (2015)",
                         DB075 == "4" ~ "4 (2016)",
                         DB075 == "5" ~ "5 (2019)",
                         TRUE ~ "6 (2020)"))





# linvar_arop <- read.csv("linvar_arop_20.csv")
# linvar_arop <- data.frame(linvar_arop)
# 
# 
# linvar_arop  %<>% mutate(DB075 = case_when(DB075 == "1" ~ "1 (2017)",
#                                             DB075 == "2" ~ "2 (2018)",
#                                             DB075 == "3" ~ "3 (2015)",
#                                             DB075 == "4" ~ "4 (2016)",
#                                             DB075 == "5" ~ "5 (2019)",
#                                             TRUE ~ "6 (2020)"))
# 
# 
# 
# 
# 
# linvar_min60 <- linvar_arop %>% 
#   rename(rotational_group = DB075) %>%
#   group_by(rotational_group) %>%
#   summarise(Mean = ci(LINVAR)[1], 
#               L.CI = ci(LINVAR)[2],
#               U.CI = ci(LINVAR)[3], 
#               Std.Dev = ci(LINVAR)[4]) %>%
#   as.data.frame()
# 
# linvar_min60
# 
# 
# 
# 
# plot_linvar_min60 <- ggplot(linvar_min60, aes(rotational_group, Mean)) +
#   geom_line(colour = "red", linetype = "dotted", size = .9, group = 1) +
#   geom_col(colour = "red", alpha = 0.69, fill = "black") +
#   geom_point(colour = "red", shape = "diamond", size = 5) +
#   labs(x = "Rotational Group (Year)", y = "Mean")  +
#   theme_bw()  +
#   ggtitle("AROP indicator: Linearisation estimator")+
#   theme(axis.text = element_text(face="bold"))
# 
# plot_linvar_min60
# 
# 
# 
# 
# ggsave("pdf-plot/plot_linvar_min60.pdf")
# 
# pd <- position_dodge(0.1) 
# 
# ci_plot_linvar_min60 <- ggplot(linvar_min60, aes(x=rotational_group, y=Mean, colour=rotational_group,  group=1)) + 
#   geom_errorbar(aes(ymin=L.CI, ymax=U.CI), width=.5, position=pd, size = 1) +
#   geom_line(position=pd, size = 1) +
#   geom_point(position=pd) +
#   scale_colour_brewer(palette = 5) +
#   theme(axis.text = element_text(face="bold")) +
#   theme_dark()
# ci_plot_linvar_min60
# 
# 
# ggsave("pdf-plot/ci_plot_linvar_min60.pdf")
# 
# 
# 
# 
# 
# 
# stargazer(linvar_min60, type = "latex", title = " At Risk of Poverty (min60)", summary = FALSE, digits = 10)
# 


anal_min60 <- data_2020.df %>%
  rename(rotational_group = DB075) %>%
  as_survey(weights = c(rb050)) %>%
  group_by(rotational_group) %>% 
  summarise(Mean =  survey_mean(MIN60, na.rm = T),
            N = sum(!is.na(MIN60)), 
            Std.Dev = sd(MIN60, na.rm = T),
            Min = min(MIN60, na.rm = T),
            Max = max(MIN60, na.rm = T))  %>%
  mutate(L.CI = Mean - qt(1 - (0.05 / 2), N - 1) * Mean_se,
         U.CI = Mean + qt(1 - (0.05 / 2), N - 1) * Mean_se) %>% 
  dplyr::select(-Mean_se) %>%
  arrange(rotational_group) %>%
  as.data.frame()                

anal_min60





# The errorbars overlapped, so use position_dodge to move them horizontally

pd <- position_dodge(0.1) # move them .05 to the left and right

ci_plot_arop <- ggplot(anal_min60, aes(x=rotational_group, y=Mean, colour=rotational_group,  group=1)) + 
  geom_errorbar(aes(ymin=L.CI, ymax=U.CI), width=.69, position=pd, size = 1.25) +
  geom_point(position=pd, size = 2.5) +
  theme(axis.text = element_text(face="bold")) +
  ggtitle("SILC'20: AROP")

ci_plot_arop


ggsave("pdf-plot/ci_plot_arop.pdf")


stargazer(anal_min60, type = "text", title = " At Risk of Poverty (min60)", summary = FALSE)

stargazer(anal_min60, type = "latex", title = " At Risk of Poverty (min60)", summary = FALSE)




# plot_min60 <- ggplot(anal_min60, aes(rotational_group, Mean)) +
#   geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
#   geom_col(colour = "red", alpha = 0.69, fill = "coral") +
#   geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
#   labs(x = "Rotational Group (Year)", y = "Mean")  +
#   theme_bw()  +
#   scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)")) +
#   theme(axis.text = element_text(face="bold"))
# 
# plot_min60
# 
# ggsave("pdf-plot/plot_min60.pdf")



anal_smd <- data_2020.df %>%
  rename(rotational_group = DB075) %>%
  as_survey(weights = c(rb050)) %>%
  group_by(rotational_group) %>% 
  summarise(Mean =  survey_mean(SMD, na.rm = T),
            N = sum(!is.na(SMD)), 
            Std.Dev = sd(SMD, na.rm = T),
            Min = min(SMD, na.rm = T),
            Max = max(SMD, na.rm = T))  %>%
  mutate(L.CI = Mean - qt(1 - (0.05 / 2), N - 1) * Mean_se,
         U.CI = Mean + qt(1 - (0.05 / 2), N - 1) * Mean_se) %>% 
  dplyr::select(-Mean_se) %>%
  arrange(rotational_group) %>%
  as.data.frame()    

anal_smd




ci_plot_smd <- ggplot(anal_smd, aes(x=rotational_group, y=Mean, colour=rotational_group,  group=1)) + 
  geom_errorbar(aes(ymin=L.CI, ymax=U.CI), width=.69, position=pd, size = 1.25) +
  geom_point(position=pd, size = 2.5) +
  theme(axis.text = element_text(face="bold")) +
  ggtitle("SILC'20: SMD")
ci_plot_smd


ggsave("pdf-plot/ci_plot_smd.pdf")



stargazer(anal_smd, type = "text", title = "Descriptive Statistics", summary = FALSE)

stargazer(anal_smd, type = "latex", title = "Severe Material Deprivation rate", summary = FALSE)


# plot_smd <- ggplot(anal_smd, aes(rotational_group, Mean),  color = rotational_group) +
#   geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
#   geom_col(colour = "blue", alpha = 0.69, fill = "cyan") +
#   geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
#   labs(x = "Rotational Group (Year)", y = "Mean")  +
#   theme_bw()  +
#   scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)")) +
#   theme(axis.text = element_text(face="bold"))
# 
# plot_smd
# 
# 
# 
# ggsave("pdf-plot/plot_smd.pdf")





anal_lwi <- data_2020.df %>%
  rename(rotational_group = DB075) %>%
  as_survey(weights = c(rb050)) %>%
  group_by(rotational_group) %>% 
  summarise(Mean =  survey_mean(LWI_BD_new, na.rm = T),
            N = sum(!is.na(LWI_BD_new)), 
            Std.Dev = sd(LWI_BD_new, na.rm = T),
            Min = min(LWI_BD_new, na.rm = T),
            Max = max(LWI_BD_new, na.rm = T))  %>%
  mutate(L.CI = Mean - qt(1 - (0.05 / 2), N - 1) * Mean_se,
         U.CI = Mean + qt(1 - (0.05 / 2), N - 1) * Mean_se) %>% 
  dplyr::select(-Mean_se) %>%
  arrange(rotational_group) %>%
  as.data.frame()         

anal_lwi 

 
stargazer(anal_lwi, type = "text", title = "Low Work Intensity rate", summary = FALSE)


stargazer(anal_lwi, type = "latex", title = "Low Work Intensity rate", summary = FALSE)



ci_plot_lwi <- ggplot(anal_lwi, aes(x=rotational_group, y=Mean, colour=rotational_group,  group=1)) + 
  geom_errorbar(aes(ymin=L.CI, ymax=U.CI), width=.69, position=pd, size = 1.25) +
  geom_point(position=pd, size = 2.5) +
  theme(axis.text = element_text(face="bold")) +
  ggtitle("SILC'20: LWI")
ci_plot_lwi


ggsave("pdf-plot/ci_plot_lwi.pdf")




# 
# plot_lwi <- ggplot(anal_lwi, aes(rotational_group, Mean),  color = rotational_group) +
#   geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
#   geom_col(colour = "blue", alpha = 0.69, fill = "cyan") +
#   geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
#   labs(x = "Rotational Group (Year)", y = "Mean")  +
#   theme_bw()  +
#   scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)")) +
#   theme(axis.text = element_text(face="bold"))
# 
# plot_lwi
# 
# ggsave("pdf-plot/plot_lwi.pdf")



anal_arope <- data_2020.df %>%
  rename(rotational_group = DB075) %>%
  as_survey(weights = c(rb050)) %>%
  group_by(rotational_group) %>% 
  summarise(Mean =  survey_mean(AROPE_new, na.rm = T),
            N = sum(!is.na(AROPE_new)), 
            Std.Dev = sd(AROPE_new, na.rm = T),
            Min = min(AROPE_new, na.rm = T),
            Max = max(AROPE_new, na.rm = T))  %>%
  mutate(L.CI = Mean - qt(1 - (0.05 / 2), N - 1) * Mean_se,
         U.CI = Mean + qt(1 - (0.05 / 2), N - 1) * Mean_se) %>% 
  dplyr::select(-Mean_se) %>%
  arrange(rotational_group) %>%
  as.data.frame()         

anal_arope 



ci_plot_arope <- ggplot(anal_arope, aes(x=rotational_group, y=Mean, colour=rotational_group,  group=1)) + 
  geom_errorbar(aes(ymin=L.CI, ymax=U.CI), width=.69, position=pd, size = 1.25) +
  geom_point(position=pd, size = 2.5) +
  theme(axis.text = element_text(face="bold")) +
  ggtitle("SILC'20: AROPE")
ci_plot_arope


ggsave("pdf-plot/ci_plot_arope.pdf")



stargazer(anal_arope, type = "text", title = "At Risk of Poverty Exclusion rate", summary = FALSE)

stargazer(anal_arope, type = "text", title = "At Risk of Poverty Exclusion rate")

stargazer(anal_arope, type = "latex", title = "At Risk of Poverty Exclusion rate", summary = FALSE)

# 
# plot_arope <- ggplot(anal_arope, aes(rotational_group, Mean),  color = rotational_group) +
#   geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
#   geom_col(colour = "red", alpha = 0.69, fill = "coral") +
#   geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
#   labs(x = "Rotational Group (Year)", y = "Mean")  +
#   theme_bw()  +
#   scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)")) +
#   theme(axis.text = element_text(face="bold"))
# 
# plot_arope
# 
# ggsave("pdf-plot/plot_arope.pdf")




## EQ_INC20 =  equivalised disposable income


anal_eqinc20 <- data_2020.df %>%
  rename(rotational_group = DB075) %>%
  as_survey(weights = c(rb050)) %>%
  group_by(rotational_group) %>% 
  summarise(Mean =  survey_mean(EQ_INC20, na.rm = T),
            N = n(),
            Median = median(EQ_INC20, na.rm = T),
            Std.Dev = sd(EQ_INC20, na.rm = T),
            Min = min(EQ_INC20, na.rm = T),
            Max = max(EQ_INC20, na.rm = T))  %>%
  mutate(L.CI = Mean - qt(1 - (0.05 / 2), N - 1) * Mean_se,
         U.CI = Mean + qt(1 - (0.05 / 2), N - 1) * Mean_se) %>% 
  dplyr::select(-Mean_se) %>%
  arrange(rotational_group) %>%
  as.data.frame()            

anal_eqinc20 



ci_plot_eqinc20 <- ggplot(anal_eqinc20, aes(x=rotational_group, y=Mean, colour=rotational_group,  group=1)) + 
  geom_errorbar(aes(ymin=L.CI, ymax=U.CI), width=.69, position=pd, size = 1.25) +
  geom_point(position=pd, size = 2.5) +
  theme(axis.text = element_text(face="bold")) +
  ggtitle("SILC'20: EQINC")
ci_plot_eqinc20


ggsave("pdf-plot/ci_plot_eqinc20.pdf")



stargazer(anal_eqinc20, type = "text", title = "equivalised disposable income", summary = FALSE)

stargazer(anal_eqinc20, type = "latex", title = "equivalised disposable income", summary = FALSE)

# plot_eqinc20 <- ggplot(anal_eqinc20, aes(rotational_group, Mean)) +
#   geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
#   geom_col(colour = "red", alpha = 0.69, fill = "coral") +
#   geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
#   labs(x = "Rotational Group (Year)", y = "Mean")  +
#   theme_bw()  +
#   ylim(0, 30000) +
#   scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)")) +
#   theme(axis.text = element_text(face="bold"))
# 
# plot_eqinc20
# 
# 
# ggsave("pdf-plot/plot_eqinc20.pdf")



# anal_workint <- data_2020.df %>%
#   rename(rotational_group = DB075) %>%
#   as_survey(weights = c(rb050)) %>%
#   group_by(rotational_group) %>% 
#   summarise(Mean =  survey_mean(WORK_INT, na.rm = T),
#             N = n(),
#             Std.Dev = sd(WORK_INT),
#             Min = min(WORK_INT),
#             Max = max(WORK_INT))  %>%
#   mutate(L.CI = Mean - qt(1 - (0.05 / 2), N - 1) * Mean_se,
#          U.CI = Mean + qt(1 - (0.05 / 2), N - 1) * Mean_se) %>% 
#   arrange(rotational_group) %>%
#   as.data.frame()               
# 
# anal_workint 
# 
# 
# stargazer(anal_workint, type = "text", title = "Work Intensity rate", summary = FALSE)
# 
# stargazer(anal_workint, type = "latex", title = "Work Intensity rate", summary = FALSE)
# 
# 
# plot_workint <- ggplot(anal_workint, aes(rotational_group, Mean),  color = rotational_group) +
#   geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
#   geom_col(colour = "blue", alpha = 0.69, fill = "cyan") +
#   geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
#   labs(x = "Rotational Group (Year)", y = "Mean")  +
#   theme_bw()  +
#   ylim(0, 30) +
#   scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)")) +
#   theme(axis.text = element_text(face="bold"))
# 
# plot_workint
# 
# 
# ggsave("pdf-plot/plot_workint.pdf")
# 



anal_msd <- data_2020.df %>%
  rename(rotational_group = DB075) %>%
  as_survey(weights = c(rb050)) %>%
  group_by(rotational_group) %>% 
  summarise(Mean =  survey_mean(MD5HH3, na.rm = T),
            N = sum(!is.na(MD5HH3)), 
            Std.Dev = sd(MD5HH3, na.rm = T),
            Min = min(MD5HH3, na.rm = T),
            Max = max(MD5HH3, na.rm = T))  %>%
  mutate(L.CI = Mean - qt(1 - (0.05 / 2), N - 1) * Mean_se,
         U.CI = Mean + qt(1 - (0.05 / 2), N - 1) * Mean_se) %>% 
  dplyr::select(-Mean_se) %>%
  arrange(rotational_group) %>%
  as.data.frame()               

anal_msd 



ci_plot_msd <- ggplot(anal_msd, aes(x=rotational_group, y=Mean, colour=rotational_group,  group=1)) + 
  geom_errorbar(aes(ymin=L.CI, ymax=U.CI), width=.69, position=pd, size = 1.25) +
  geom_point(position=pd, size = 2.5) +
  theme(axis.text = element_text(face="bold")) +
  ggtitle("SILC'20: MSD")
ci_plot_msd


ggsave("pdf-plot/ci_plot_msd.pdf")


stargazer(anal_msd, type = "text", title = "Material and social deprivation rate (MSD)", summary = FALSE)

stargazer(anal_msd, type = "latex", title = "Material and social deprivation rate (MSD)", summary = FALSE)



# plot_msd <- ggplot(anal_msd, aes(rotational_group, Mean),  color = rotational_group) +
#   geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
#   geom_col(colour = "blue", alpha = 0.69, fill = "cyan") +
#   geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
#   labs(x = "Rotational Group (Year)", y = "Mean")  +
#   theme_bw()  +
#   ylim(0, 0.15) +
#   scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)")) +
#   theme(axis.text = element_text(face="bold"))
# 
# plot_msd
# 
# 
# ggsave("pdf-plot/plot_msd.pdf")



## severe material and social deprivation (SMSD)

anal_sevmsd <- data_2020.df %>%
  rename(rotational_group = DB075) %>%
  as_survey(weights = c(rb050)) %>%
  group_by(rotational_group) %>% 
  summarise(Mean =  survey_mean(Sev_MSD, na.rm = T),
            N = sum(!is.na(Sev_MSD)), 
            Std.Dev = sd(Sev_MSD, na.rm = T),
            Min = min(Sev_MSD, na.rm = T),
            Max = max(Sev_MSD, na.rm = T))  %>%
  mutate(L.CI = Mean - qt(1 - (0.05 / 2), N - 1) * Mean_se,
         U.CI = Mean + qt(1 - (0.05 / 2), N - 1) * Mean_se) %>% 
  dplyr::select(-Mean_se) %>%
  arrange(rotational_group) %>%
  as.data.frame()               

anal_sevmsd



ci_plot_sevmsd <- ggplot(anal_sevmsd, aes(x=rotational_group, y=Mean, colour=rotational_group,  group=1)) + 
  geom_errorbar(aes(ymin=L.CI, ymax=U.CI), width=.69, position=pd, size = 1.25) +
  geom_point(position=pd, size = 2.5) +
  theme(axis.text = element_text(face="bold")) +
  ggtitle("SILC'20: SMSD")
ci_plot_sevmsd


ggsave("pdf-plot/ci_plot_sevmsd.pdf")


stargazer(anal_sevmsd, type = "text", title = "Severe Material and Social Deprivation (SMSD)", summary = FALSE)

stargazer(anal_sevmsd, type = "latex", title = "Severe Material and Social Deprivation (SMSD)", summary = FALSE)

# 
# plot_sevmsd <- ggplot(anal_sevmsd, aes(rotational_group, Mean),  color = rotational_group) +
#   geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
#   geom_col(colour = "blue", alpha = 0.69, fill = "cyan") +
#   geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
#   labs(x = "Rotational Group (Year)", y = "Mean")  +
#   theme_bw()  +
#   ylim(0, 0.1) +
#   scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)")) +
#   theme(axis.text = element_text(face="bold"))
# 
# plot_sevmsd
# 
# ggsave("pdf-plot/plot_sevmsd.pdf")
# 
# 
# 


GINI <- readxl::read_excel("gini.xlsx")
GINI <- data.frame(GINI)


anal_GINI <- GINI |>
  summarise(mean = mean(valeur))

anal_GINI


stargazer(GINI, type = "text", title = "GINI Index", summary = FALSE)
stargazer(GINI, type = "latex", title = "GINI Index", summary = FALSE)



rg_GINI <- readxl::read_excel("2020_RG_gini.xlsx")
rg_GINI <- data.frame(rg_GINI)



anal_GINI <- rg_GINI %>%
  summarise(Mean =  mean(value),
            Std.Dev = sd(value),
            Min = min(value),
            Max = max(value)) 
              
anal_GINI



rg_GINI  %<>% mutate(rotational_group = case_when(rotational_group == "1" ~ "1 (2017)",
                                                  rotational_group == "2" ~ "2 (2018)",
                                                  rotational_group == "3" ~ "3 (2015)",
                                                  rotational_group == "4" ~ "4 (2016)",
                                                  rotational_group == "5" ~ "5 (2019)",
                                            TRUE ~ "6 (2020)"))


stargazer(rg_GINI, type = "text", title = "GINI Index Rotation Group wise", summary = FALSE)

stargazer(rg_GINI, type = "text", title = "GINI Index Rotation Group wise")

stargazer(rg_GINI, type = "latex", title = "GINI Index Rotation Group wise", summary = FALSE)




# linvar_gini <- read.csv("linvar_gini_20.csv")
# linvar_GINI <- data.frame(linvar_gini)
# 
# 
# linvar_GINI  %<>% mutate(DB075 = case_when(DB075 == "1" ~ "1 (2017)",
#                                            DB075 == "2" ~ "2 (2018)",
#                                            DB075 == "3" ~ "3 (2015)",
#                                            DB075 == "4" ~ "4 (2016)",
#                                            DB075 == "5" ~ "5 (2019)",
#                                            TRUE ~ "6 (2020)"))
# 
# 
# 
# 
# 
# linvar_GINI <- linvar_GINI %>% 
#   rename(rotational_group = DB075) %>%
#   group_by(rotational_group) %>%
#   summarise(Mean = ci(LINVAR)[1], 
#             L.CI = ci(LINVAR)[2],
#             U.CI = ci(LINVAR)[3], 
#             Std.Dev = ci(LINVAR)[4]) %>%
#   as.data.frame()
# 
# linvar_GINI
# 
# 
# 
# stargazer(linvar_GINI, type = "latex", title = " Linearisation estimator: GINI", summary = FALSE, digits = 15)
# 
# 
# plot_linvar_GINI <-  ggplot(linvar_GINI, aes(rotational_group, Mean)) +
#   geom_line(colour = "red", linetype = "dotted", size = .9, group = 1) +
#   geom_col(colour = "red", alpha = 0.69, fill = "black") +
#   geom_point(colour = "red", shape = "diamond", size = 5) +
#   labs(x = "Rotational Group (Year)", y = "Mean")  +
#   theme_bw()  +
#   ggtitle("GINI Indicator: Linearisation Estimator") +
#   theme(axis.text = element_text(face="bold"))
# 
# plot_linvar_GINI
# 
# ggsave("pdf-plot/plot_linvar_GINI.pdf")
# 
# 
# 
# ci_plot_linvar_GINI <- ggplot(linvar_GINI, aes(x=rotational_group, y=Mean, colour=rotational_group,  group=1)) + 
#   geom_errorbar(aes(ymin=L.CI, ymax=U.CI), width=.5, position=pd, size = 1) +
#   geom_line(position=pd, size = 1) +
#   geom_point(position=pd) +
#   theme(axis.text = element_text(face="bold")) +
#   scale_colour_brewer(palette = 4) +
#   theme_dark()
# 
# ci_plot_linvar_GINI
# 
# 
# ggsave("pdf-plot/ci_plot_linvar_GINI.pdf")





plot_rg_GINI <-  ggplot(rg_GINI, aes(rotational_group, value),  color = rotational_group) +
  geom_line(colour = "purple", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "purple", alpha = 0.69, fill = "white") +
  geom_point(colour = "purple", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  ylim(0, 30) +
  scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)")) +
  theme(axis.text = element_text(face="bold"))
  
plot_rg_GINI

ggsave("pdf-plot/plot_rg_GINI.pdf")





S80S20 <- readxl::read_excel("s80s20.xlsx")
S80S20 <- data.frame(S80S20)

rg_S80S20 <- readxl::read_excel("2020_RG_s80s20.xlsx")
rg_S80S20 <- data.frame(rg_S80S20)

stargazer(rg_S80S20, type = "text", title = "S80S20 Ratio Rotation Group wise", summary = FALSE)
stargazer(rg_S80S20, type = "latex", title = "S80S20 Ratio Rotation Group wise", summary = FALSE)



anal_S80S20 <- rg_S80S20 %>%
  summarise(Mean =  mean(value),
            Std.Dev = sd(value),
            Min = min(value),
            Max = max(value)) 

anal_S80S20




rg_S80S20  %<>% mutate(rotational_group = case_when(rotational_group == "1" ~ "1 (2017)",
                                                  rotational_group == "2" ~ "2 (2018)",
                                                  rotational_group == "3" ~ "3 (2015)",
                                                  rotational_group == "4" ~ "4 (2016)",
                                                  rotational_group == "5" ~ "5 (2019)",
                                                  TRUE ~ "6 (2020)"))




plot_rg_S80S20 <-  ggplot(rg_S80S20, aes(rotational_group, value),  color = rotational_group) +
  geom_line(colour = "red", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "red", alpha = 0.69, fill = "white") +
  geom_point(colour = "red", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  ylim(0,5) +
  scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)")) +
  theme(axis.text = element_text(face="bold"))

plot_rg_S80S20

ggsave("pdf-plot/plot_rg_S80S20.pdf")






## all indicators


# eda <- data_2020.df %>%
#   as_survey(weights = c(rb050)) %>%
#   summarise(Mean =  survey_mean(Sev_MSD
# 
# , na.rm = T),
#             N = sum(!is.na(Sev_MSD
# 
# )),
#             Std.Dev = sd(Sev_MSD
# 
# , na.rm=T),
#             Min = min(Sev_MSD
# 
# ),
#             Max = max(Sev_MSD
# 
# ))                    
# 
# eda



eda_2020 <- readxl::read_excel("eda2020.xlsx")
eda_2020 <- data.frame(eda_2020)            
eda_2020            




stargazer(eda_2020, type = "text", title = "All indicators", summary = FALSE)

stargazer(eda_2020, type = "latex", title = "All indicators", summary = FALSE)




####################################### BE-SILC 2021 ##########################################


merged <- read.csv("merged2021.csv")
data_2021 <- as_tibble(merged)
data_2021.df <- data.frame(data_2021)
data_2021.df  %<>% mutate(DB075 = case_when(DB075 == "1" ~ "1 (2017)",
                                            DB075 == "2" ~ "2 (2018)",
                                            DB075 == "4" ~ "4 (2016)",
                                            DB075 == "5" ~ "5 (2019)",
                                            DB075 == "6" ~ "6 (2020)",
                                            TRUE ~ "7 (2021)"))






## all indicators silc 2021
# eda <- data_2021.df %>%
#   as_survey(weights = c(RB050)) %>%
#   summarise(Mean =  survey_mean(MIN60
# 
# 
# 
#                                 
#                                 , na.rm = T),
#             N = sum(!is.na(MIN60
# 
# 
# 
#                            
#             )),
#             Std.Dev = sd(MIN60
# 
# 
# 
#                          
#                          , na.rm=T),
#             Min = min(MIN60
# 
# , na.rm=T
# 
#                       
#             ),
#             Max = max(MIN60
# 
# , na.rm=T
# 
#                       
#             ))                    
# 
# eda
# 
# 
# 
# 





eda_2021 <- readxl::read_excel("eda2021.xlsx")
eda_2021 <- data.frame(eda_2021)            
eda_2021            

stargazer(eda_2021, type = "text", title = "All indicators", summary = FALSE)

stargazer(eda_2021, type = "latex", title = "All indicators", summary = FALSE)


ggbarcrosstabs(data_2021,DB076, MIN60 ,RB050, fill= TRUE)





anal_min60.21 <- data_2021.df %>%
  rename(rotational_group = DB075) %>%
  as_survey(weights = c(RB050)) %>%
  group_by(rotational_group) %>% 
  summarise(Mean =  survey_mean(MIN60, na.rm = T),
            N = n(),
            Std.Dev = sd(MIN60, na.rm = T),
            Min = min(MIN60, na.rm = T),
            Max = max(MIN60, na.rm = T))  %>% 
  mutate(L.CI = Mean - qt(1 - (0.05 / 2), N - 1) * Mean_se,
         U.CI = Mean + qt(1 - (0.05 / 2), N - 1) * Mean_se) %>% 
  dplyr::select(-Mean_se) %>%
  arrange(rotational_group) %>%
  as.data.frame()                

anal_min60.21


ci_plot_min60.21 <- ggplot(anal_min60.21, aes(x=rotational_group, y=Mean, colour=rotational_group,  group=1)) + 
  geom_errorbar(aes(ymin=L.CI, ymax=U.CI), width=.69, position=pd, size = 1.25) +
  geom_point(position=pd, size = 2.5) +
  theme(axis.text = element_text(face="bold")) +
  ggtitle("SILC'21: AROP") +
  scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)"))
ci_plot_min60.21


ggsave("pdf-plot/ci_plot_min60.21.pdf")


# linvar_arop.21 <- read.csv("linvar_arop_21.csv")
# linvar_arop.21 <- data.frame(linvar_arop.21)
# 
# 
# linvar_arop.21 %<>% mutate(DB075 = case_when(DB075 == "1" ~ "1 (2017)",
#                                       DB075 == "2" ~ "2 (2018)",
#                                       DB075 == "4" ~ "4 (2016)",
#                                       DB075 == "5" ~ "5 (2019)",
#                                       DB075 == "6" ~ "6 (2020)",
#                                       TRUE ~ "7 (2021)"))
# 
# 
# 
# 
# 
# linvar_min60.21 <- linvar_arop.21 %>% 
#   rename(rotational_group = DB075) %>%
#   group_by(rotational_group) %>%
#   summarise(Mean = ci(LINVAR)[1], 
#             L.CI = ci(LINVAR)[2],
#             U.CI = ci(LINVAR)[3], 
#             Std.Dev = ci(LINVAR)[4]) %>%
#   as.data.frame()
# 
# linvar_min60.21
# 
# 
# 
# 
# plot_linvar_min60.21 <- ggplot(linvar_min60.21, aes(rotational_group, Mean)) +
#   geom_line(colour = "red", linetype = "dotted", size = .9, group = 1) +
#   geom_col(colour = "red", alpha = 0.69, fill = "black") +
#   geom_point(colour = "red", shape = "diamond", size = 5) +
#   labs(x = "Rotational Group (Year)", y = "Mean")  +
#   theme_bw()  +
#   ggtitle("AROP indicator: Linearisation estimator") +
#   scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)")) +
#   theme(axis.text = element_text(face="bold"))
# 
# plot_linvar_min60.21
# 
# 
# 
# 
# ggsave("pdf-plot/plot_linvar_min60.21.pdf")
# 
# 
# 
# ci_plot_linvar_min60.21 <- ggplot(linvar_min60.21, aes(x=rotational_group, y=Mean, colour=rotational_group,  group=1)) + 
#   geom_errorbar(aes(ymin=L.CI, ymax=U.CI), width=.5, position=pd, size = 1) +
#   geom_line(position=pd, size = 1) +
#   geom_point(position=pd) +
#   scale_colour_brewer(palette = 5) +
#   theme(axis.text = element_text(face="bold")) +
#   scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)")) +
#   theme_dark()
# ci_plot_linvar_min60.21
# 
# 
# ggsave("pdf-plot/ci_plot_linvar_min60.21.pdf")
# 



stargazer(anal_min60.21, type = "text", title = " At Risk of Poverty (min60)", summary = FALSE)

stargazer(anal_min60.21, type = "latex", title = " At Risk of Poverty (min60)", summary = FALSE)
# 
# 
# plot_min60.21 <- ggplot(anal_min60.21, aes(rotational_group, Mean)) +
#   geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
#   geom_col(colour = "red", alpha = 0.69, fill = "coral") +
#   geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
#   labs(x = "Rotational Group (Year)", y = "Mean")  +
#   theme_bw()  +
#   scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)")) +
#   theme(axis.text = element_text(face="bold"))
# 
# plot_min60.21
# 
# 
# ggsave("pdf-plot/plot_min60.21.pdf")



anal_smd.21 <- data_2021.df %>%
  rename(rotational_group = DB075) %>%
  as_survey(weights = c(RB050)) %>%
  group_by(rotational_group) %>% 
  summarise(Mean =  survey_mean(SMD, na.rm = T),
            N = n(),
            Std.Dev = sd(SMD, na.rm = T),
            Min = min(SMD, na.rm = T),
            Max = max(SMD, na.rm = T))  %>%
  mutate(L.CI = Mean - qt(1 - (0.05 / 2), N - 1) * Mean_se,
         U.CI = Mean + qt(1 - (0.05 / 2), N - 1) * Mean_se) %>%
  dplyr::select(-Mean_se) %>%
  arrange(rotational_group) %>%
  as.data.frame()    

anal_smd.21



ci_plot_smd.21 <- ggplot(anal_smd.21, aes(x=rotational_group, y=Mean, colour=rotational_group,  group=1)) + 
  geom_errorbar(aes(ymin=L.CI, ymax=U.CI), width=.69, position=pd, size = 1.25) +
  geom_point(position=pd, size = 2.5) +
  theme(axis.text = element_text(face="bold")) +
  ggtitle("SILC'21: SMD") +
  scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)"))
ci_plot_smd.21


ggsave("pdf-plot/ci_plot_smd.21.pdf")


stargazer(anal_smd.21, type = "text", title = " Severe material deprivation", summary = FALSE)

stargazer(anal_smd.21, type = "latex", title = "SILC 2021: SMD", summary = FALSE)
# 
# 
# plot_smd.21 <- ggplot(anal_smd.21, aes(rotational_group, Mean)) +
#   geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
#   geom_col(colour = "red", alpha = 0.69, fill = "coral") +
#   geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
#   labs(x = "Rotational Group (Year)", y = "Mean")  +
#   theme_bw()  +
#   scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)")) +
#   theme(axis.text = element_text(face="bold"))
# 
# plot_smd.21
# 
# ggsave("pdf-plot/plot_smd.21.pdf")
# 
# 



anal_arope.21 <- data_2021.df %>%
  rename(rotational_group = DB075) %>%
  as_survey(weights = c(RB050)) %>%
  group_by(rotational_group) %>% 
  summarise(Mean =  survey_mean(AROPE_new, na.rm = T),
            N = n(),
            Std.Dev = sd(AROPE_new, na.rm = T),
            Min = min(AROPE_new, na.rm = T),
            Max = max(AROPE_new, na.rm = T))  %>%          
  mutate(L.CI = Mean - qt(1 - (0.05 / 2), N - 1) * Mean_se,
         U.CI = Mean + qt(1 - (0.05 / 2), N - 1) * Mean_se) %>%
  dplyr::select(-Mean_se) %>%
  arrange(rotational_group)   %>%
  as.data.frame()        

anal_arope.21


stargazer(anal_arope.21, type = "latex", title = "SILC 2021: AROPE", summary = FALSE)


ci_plot_arope.21 <- ggplot(anal_arope.21, aes(x=rotational_group, y=Mean, colour=rotational_group,  group=1)) + 
  geom_errorbar(aes(ymin=L.CI, ymax=U.CI), width=.69, position=pd, size = 1.25) +
  geom_point(position=pd, size = 2.5) +
  theme(axis.text = element_text(face="bold")) +
  ggtitle("SILC'21: AROPE") +
  scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)"))
ci_plot_arope.21


ggsave("pdf-plot/ci_plot_arope.21.pdf")

# 
# plot_arope.21 <- ggplot(anal_arope.21, aes(rotational_group, Mean)) +
#   geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
#   geom_col(colour = "red", alpha = 0.69, fill = "coral") +
#   geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
#   labs(x = "Rotational Group (Year)", y = "Mean")  +
#   theme_bw()  +
#   scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)")) +
#   theme(axis.text = element_text(face="bold"))
# 
# plot_arope.21
# 
# ggsave("pdf-plot/plot_arope.21.pdf")
# 
# 



anal_lwi.21 <- data_2021.df %>%
  rename(rotational_group = DB075) %>%
  as_survey(weights = c(RB050)) %>%
  group_by(rotational_group) %>% 
  summarise(Mean =  survey_mean(LWI_BD_NEW, na.rm = T),
            N = sum(!is.na(LWI_BD_NEW)), 
            Std.Dev = sd(LWI_BD_NEW, na.rm = T),
            Min = min(LWI_BD_NEW, na.rm = T),
            Max = max(LWI_BD_NEW, na.rm = T))  %>%
  mutate(L.CI = Mean - qt(1 - (0.05 / 2), N - 1) * Mean_se,
         U.CI = Mean + qt(1 - (0.05 / 2), N - 1) * Mean_se) %>%
  dplyr::select(-Mean_se) %>%
  arrange(rotational_group)    %>%
  as.data.frame()       

anal_lwi.21

stargazer(anal_lwi.21, type = "latex", title = "SILC 2021: LWI", summary = FALSE)




ci_plot_lwi.21 <- ggplot(anal_lwi.21, aes(x=rotational_group, y=Mean, colour=rotational_group,  group=1)) + 
  geom_errorbar(aes(ymin=L.CI, ymax=U.CI), width=.69, position=pd, size = 1.25) +
  geom_point(position=pd, size = 2.5) +
  theme(axis.text = element_text(face="bold")) +
  ggtitle("SILC'21: LWI") +
  scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)"))
ci_plot_lwi.21


ggsave("pdf-plot/ci_plot_lwi.21.pdf")


# 
# plot_lwi.21 <- ggplot(anal_lwi.21, aes(rotational_group, Mean)) +
#   geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
#   geom_col(colour = "red", alpha = 0.69, fill = "coral") +
#   geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
#   labs(x = "Rotational Group (Year)", y = "Mean")  +
#   theme_bw()  +
#   scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)")) +
#   theme(axis.text = element_text(face="bold"))
# 
# plot_lwi.21
# 
# ggsave("pdf-plot/plot_lwi.21.pdf")
# 
# 




anal_eqinc20.21 <- data_2021.df %>%
  rename(rotational_group = DB075) %>%
  as_survey(weights = c(RB050)) %>%
  group_by(rotational_group) %>% 
  summarise(Mean =  survey_mean(EQ_INC20, na.rm = T),
            N = n(),
            Median = median(EQ_INC20, na.rm = T),
            Std.Dev = sd(EQ_INC20, na.rm = T),
            Min = min(EQ_INC20, na.rm = T),
            Max = max(EQ_INC20, na.rm = T))  %>% 
  mutate(L.CI = Mean - qt(1 - (0.05 / 2), N - 1) * Mean_se,
         U.CI = Mean + qt(1 - (0.05 / 2), N - 1) * Mean_se) %>%
  dplyr::select(-Mean_se) %>%
  arrange(rotational_group) %>%
  as.data.frame()            

anal_eqinc20.21


stargazer(anal_eqinc20.21, type = "latex", title = "SILC 2021: EQINC", summary = FALSE)



ci_plot_eqinc20.21 <- ggplot(anal_eqinc20.21, aes(x=rotational_group, y=Mean, colour=rotational_group,  group=1)) + 
  geom_errorbar(aes(ymin=L.CI, ymax=U.CI), width=.69, position=pd, size = 1.25) +
  geom_point(position=pd, size = 2.5) +
  theme(axis.text = element_text(face="bold")) +
  ggtitle("SILC'21: EQINC") +
  scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)"))
ci_plot_eqinc20.21


ggsave("pdf-plot/ci_plot_eqinc20.21.pdf")



# plot_eqinc20.21 <- ggplot(anal_eqinc20.21, aes(rotational_group, Mean)) +
#   geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
#   geom_col(colour = "red", alpha = 0.69, fill = "coral") +
#   geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
#   labs(x = "Rotational Group (Year)", y = "Mean")  +
#   theme_bw()  +
#   scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)")) +
#   theme(axis.text = element_text(face="bold"))
# 
# plot_eqinc20.21
# 
# ggsave("pdf-plot/plot_eqinc20.21.pdf")
# 


anal_msd.21 <- data_2021.df %>%
  rename(rotational_group = DB075) %>%
  as_survey(weights = c(RB050)) %>%
  group_by(rotational_group) %>% 
  summarise(Mean =  survey_mean(MD5HH3, na.rm = T),
            N = sum(!is.na(MD5HH3)), 
            Std.Dev = sd(MD5HH3, na.rm = T),
            Min = min(MD5HH3, na.rm = T),
            Max = max(MD5HH3, na.rm = T))  %>%
  mutate(L.CI = Mean - qt(1 - (0.05 / 2), N - 1) * Mean_se,
         U.CI = Mean + qt(1 - (0.05 / 2), N - 1) * Mean_se) %>% 
  dplyr::select(-Mean_se) %>%
  arrange(rotational_group) %>%
  as.data.frame()               

anal_msd.21

stargazer(anal_msd.21, type = "latex", title = "SILC 2021: MSD", summary = FALSE)


ci_plot_msd.21 <- ggplot(anal_msd.21, aes(x=rotational_group, y=Mean, colour=rotational_group,  group=1)) + 
  geom_errorbar(aes(ymin=L.CI, ymax=U.CI), width=.69, position=pd, size = 1.25) +
  geom_point(position=pd, size = 2.5) +
  theme(axis.text = element_text(face="bold")) +
  ggtitle("SILC'21: MSD") +
  scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)"))
ci_plot_msd.21


ggsave("pdf-plot/ci_plot_msd.21.pdf")


stargazer(anal_msd, type = "text", title = "Material and social deprivation rate (MSD)", summary = FALSE)

stargazer(anal_msd, type = "latex", title = "Material and social deprivation rate (MSD)", summary = FALSE)



# plot_msd.21 <- ggplot(anal_msd.21, aes(rotational_group, Mean),  color = rotational_group) +
#   geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
#   geom_col(colour = "blue", alpha = 0.69, fill = "cyan") +
#   geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
#   labs(x = "Rotational Group (Year)", y = "Mean")  +
#   theme_bw()  +
#   ylim(0, 0.15) +
#   scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)")) +
#   theme(axis.text = element_text(face="bold"))
# 
# plot_msd.21
# 
# 
# ggsave("pdf-plot/plot_msd.21.pdf")







## severe material and social deprivation (SMSD)

anal_sevmsd.21 <- data_2021.df %>%
  rename(rotational_group = DB075) %>%
  as_survey(weights = c(RB050)) %>%
  group_by(rotational_group) %>% 
  summarise(Mean =  survey_mean(Sev_MSD, na.rm = T),
            N = sum(!is.na(Sev_MSD)), 
            Std.Dev = sd(Sev_MSD, na.rm = T),
            Min = min(Sev_MSD, na.rm = T),
            Max = max(Sev_MSD, na.rm = T))  %>%
  mutate(L.CI = Mean - qt(1 - (0.05 / 2), N - 1) * Mean_se,
         U.CI = Mean + qt(1 - (0.05 / 2), N - 1) * Mean_se) %>% 
  dplyr::select(-Mean_se) %>%
  arrange(rotational_group) %>%
  as.data.frame()               

anal_sevmsd.21

stargazer(anal_sevmsd.21, type = "latex", title = "SILC 2021: SMSD", summary = FALSE)


ci_plot_sevmsd.21 <- ggplot(anal_sevmsd.21, aes(x=rotational_group, y=Mean, colour=rotational_group,  group=1)) + 
  geom_errorbar(aes(ymin=L.CI, ymax=U.CI), width=.69, position=pd, size = 1.25) +
  geom_point(position=pd, size = 2.5) +
  theme(axis.text = element_text(face="bold")) +
  ggtitle("SILC'21: SMSD") +
  scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)"))
ci_plot_sevmsd.21


ggsave("pdf-plot/ci_plot_sevmsd.21.pdf")



stargazer(anal_sevmsd, type = "text", title = "Severe Material and Social Deprivation (SMSD)", summary = FALSE)

stargazer(anal_sevmsd, type = "latex", title = "Severe Material and Social Deprivation (SMSD)", summary = FALSE)


# plot_sevmsd.21 <- ggplot(anal_sevmsd.21, aes(rotational_group, Mean),  color = rotational_group) +
#   geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
#   geom_col(colour = "blue", alpha = 0.69, fill = "cyan") +
#   geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
#   labs(x = "Rotational Group (Year)", y = "Mean")  +
#   theme_bw()  +
#   ylim(0, 0.1) +
#   scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)")) +
#   theme(axis.text = element_text(face="bold"))
# 
# plot_sevmsd.21
# 
# ggsave("pdf-plot/plot_sevmsd.21.pdf")





rg_GINI.21 <- readxl::read_excel("2021_RG_gini.xlsx")
rg_GINI.21 <- data.frame(rg_GINI.21)





rg_GINI.21  %<>% mutate(rotational_group = case_when(rotational_group == "1" ~ "1 (2017)",
                                                     rotational_group == "2" ~ "2 (2018)",
                                                     rotational_group == "4" ~ "4 (2016)",
                                                     rotational_group == "5" ~ "5 (2019)",
                                                     rotational_group == "6" ~ "6 (2020)",
                                                     TRUE ~ "7 (2021)"))



anal_GINI.21 <- rg_GINI.21 %>%
  summarise(Mean =  mean(value),
            Std.Dev = sd(value),
            Min = min(value),
            Max = max(value))  %>%
  as.data.frame()

anal_GINI.21




stargazer(rg_GINI.21, type = "text", title = "GINI Index Rotation Group wise", summary = FALSE)

stargazer(rg_GINI.21, type = "latex", title = "GINI Index Rotation Group wise", summary = FALSE)



# 
# linvar_gini.21 <- read.csv("linvar_gini_21.csv")
# linvar_GINI.21 <- data.frame(linvar_gini.21)
# 
# 
# linvar_GINI.21  %<>% mutate(DB075 = case_when(DB075 == "1" ~ "1 (2017)",
#                                            DB075 == "2" ~ "2 (2018)",
#                                            DB075 == "4" ~ "4 (2016)",
#                                            DB075 == "5" ~ "5 (2019)",
#                                            DB075 == "6" ~ "6 (2020)",
#                                            TRUE ~ "7 (2021)"))
# 
# 
# 
# 
# 
# linvar_GINI.21 <- linvar_GINI.21 %>% 
#   rename(rotational_group = DB075) %>%
#   group_by(rotational_group) %>%
#   summarise(Mean = ci(LINVAR)[1], 
#             L.CI = ci(LINVAR)[2],
#             U.CI = ci(LINVAR)[3], 
#             Std.Dev = ci(LINVAR)[4]) %>%
#   as.data.frame()
# 
# linvar_GINI.21
# 
# 
# 
# stargazer(linvar_GINI.21, type = "latex", title = " Linearisation estimator: GINI", summary = FALSE, digits = 15)
# 
# 
# plot_linvar_GINI.21 <-  ggplot(linvar_GINI.21, aes(rotational_group, Mean)) +
#   geom_line(colour = "red", linetype = "dotted", size = .9, group = 1) +
#   geom_col(colour = "red", alpha = 0.69, fill = "black") +
#   geom_point(colour = "red", shape = "diamond", size = 5) +
#   labs(x = "Rotational Group (Year)", y = "Mean")  +
#   theme_bw()  +
#   ggtitle("GINI Indicator: Linearisation Estimator") +
#   scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)")) +
#   theme(axis.text = element_text(face="bold"))
# 
# plot_linvar_GINI.21
# 
# ggsave("pdf-plot/plot_linvar_GINI.21.pdf")
# 
# 
# 
# ci_plot_linvar_GINI.21 <- ggplot(linvar_GINI.21, aes(x=rotational_group, y=Mean, colour=rotational_group,  group=1)) + 
#   geom_errorbar(aes(ymin=L.CI, ymax=U.CI), width=.5, position=pd, size = 1) +
#   geom_line(position=pd, size = 1) +
#   geom_point(position=pd) +
#   theme(axis.text = element_text(face="bold")) +
#   scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)")) +
#   scale_colour_brewer(palette = 4) +
#   theme_dark()
# 
# ci_plot_linvar_GINI.21
# 
# 
# ggsave("pdf-plot/ci_plot_linvar_GINI.21.pdf")



plot_rg_GINI.21 <-  ggplot(rg_GINI.21, aes(rotational_group, value),  color = rotational_group) +
  geom_line(colour = "purple", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "purple", alpha = 0.69, fill = "white") +
  geom_point(colour = "purple", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  ylim(0, 30) +
  scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)")) +
  theme(axis.text = element_text(face="bold"))

plot_rg_GINI.21

ggsave("pdf-plot/plot_rg_GINI.21.pdf")





rg_S80S20.21 <- readxl::read_excel("2021_RG_s80s20.xlsx")
rg_S80S20.21 <- data.frame(rg_S80S20.21)

rg_S80S20.21  %<>% mutate(rotational_group = case_when(rotational_group == "1" ~ "1 (2017)",
                                                       rotational_group == "2" ~ "2 (2018)",
                                                       rotational_group == "4" ~ "4 (2016)",
                                                       rotational_group == "5" ~ "5 (2019)",
                                                       rotational_group == "6" ~ "6 (2020)",
                                                       TRUE ~ "7 (2021)"))



stargazer(rg_S80S20.21, type = "text", title = "S80S20 Ratio Rotation Group wise", summary = FALSE)
stargazer(rg_S80S20.21, type = "latex", title = "S80S20 Ratio Rotation Group wise", summary = FALSE)




anal_S80S20.21 <- rg_S80S20.21 %>%
  summarise(Mean =  mean(value),
            Std.Dev = sd(value),
            Min = min(value),
            Max = max(value))  %>%
  as.data.frame()

anal_S80S20.21




plot_rg_S80S20.21 <-  ggplot(rg_S80S20.21, aes(rotational_group, value),  color = rotational_group) +
  geom_line(colour = "red", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "red", alpha = 0.69, fill = "white") +
  geom_point(colour = "red", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  ylim(0,5) +
  scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)")) +
  theme(axis.text = element_text(face="bold"))

plot_rg_S80S20.21

ggsave("pdf-plot/plot_rg_S80S20.21.pdf")









stargazer(eda.df, type = "text", title = "Descriptive Statistics", summary = FALSE)
stargazer(eda.df, type = "latex", title = "Descriptive Statistics", summary = FALSE)


plot <- ggplot(eda, aes(interview_wave, Mean),  color = interview_wave) +
  geom_point(color = "firebrick", shape = "diamond", size = 2) +
  geom_line(color = "firebrick", linetype = "dotted", size = .3) +
  labs(x = "Interview Wave", y = "Mean")

plot








