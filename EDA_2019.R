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

####################################### BE-SILC 2019 ##########################################


merged2019 <- read.csv("merged2019.csv")
data_2019 <- as_tibble(merged2019)
data_2019.df <- data.frame(data_2019)
data_2019.df  %<>% mutate(DB075 = case_when(DB075 == "1" ~ "1 (2017)",
                                            DB075 == "2" ~ "2 (2018)",
                                            DB075 == "3" ~ "3 (2015)",
                                            DB075 == "4" ~ "4 (2016)",
                                            TRUE ~   "5 (2019)"))

windows(width=8.5, height=7.5, title="Controlled Size")


anal_min60.19 <- data_2019.df %>%
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

anal_min60.19



pd <- position_dodge(0.1) 


ci_plot_min60.19 <- ggplot(anal_min60.19, aes(x=rotational_group, y=Mean, colour=rotational_group,  group=1)) + 
  geom_errorbar(aes(ymin=L.CI, ymax=U.CI), width=.69, position=pd, size = 1.25) +
  geom_point(position=pd, size = 2.5) +
  scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)")) +
  theme(axis.text = element_text(face="bold")) +
  ggtitle("SILC'19: AROP")
ci_plot_min60.19


ggsave("pdf-plot/ci_plot_min60.19.pdf")


stargazer(anal_min60.19, type = "text", title = " At Risk of Poverty (min60)", summary = FALSE)

stargazer(anal_min60.19, type = "latex", title = "SILC 2019: AROP", summary = FALSE)


# plot_min60.19 <- ggplot(anal_min60.19, aes(rotational_group, Mean)) +
#   geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
#   geom_col(colour = "red", alpha = 0.69, fill = "yellow") +
#   geom_point(colour = "purple", shape = "diamond", size = 5) +
#   labs(x = "Rotational Group (Year)", y = "Mean")  +
#   theme_bw()  +
#   scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)")) +
#   theme(axis.text = element_text(face="bold"))
# 
# plot_min60.19
# 
# 
# ggsave("pdf-plot/plot_min60.19.pdf")
# 


anal_smd.19 <- data_2019.df %>%
  rename(rotational_group = DB075) %>%
  as_survey(weights = c(rb050)) %>%
  group_by(rotational_group) %>% 
  summarise(Mean =  survey_mean(SMD, na.rm = T),
            N = n(),
            Std.Dev = sd(SMD, na.rm = T),
            Min = min(SMD, na.rm = T),
            Max = max(SMD, na.rm = T))  %>%
  mutate(L.CI = Mean - qt(1 - (0.05 / 2), N - 1) * Mean_se,
         U.CI = Mean + qt(1 - (0.05 / 2), N - 1) * Mean_se) %>%
  arrange(rotational_group) %>%
  dplyr::select(-Mean_se) %>%
  as.data.frame()    

anal_smd.19



ci_plot_smd.19 <- ggplot(anal_smd.19, aes(x=rotational_group, y=Mean, colour=rotational_group,  group=1)) + 
  geom_errorbar(aes(ymin=L.CI, ymax=U.CI), width=.69, position=pd, size = 1.25) +
  geom_point(position=pd, size = 2.5) +
  scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)")) +
  theme(axis.text = element_text(face="bold")) +
  ggtitle("SILC'19: SMD")
ci_plot_smd.19


ggsave("pdf-plot/ci_plot_smd.19.pdf")


stargazer(anal_smd.19, type = "text", title = " Severe material deprivation", summary = FALSE)
stargazer(anal_smd.19, type = "latex", title = "SILC 2019: SMD", summary = FALSE)

# 
# plot_smd.19 <- ggplot(anal_smd.19, aes(rotational_group, Mean)) +
#   geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
#   geom_col(colour = "red", alpha = 0.69, fill = "yellow") +
#   geom_point(colour = "purple", shape = "diamond", size = 5) +
#   labs(x = "Rotational Group (Year)", y = "Mean")  +
#   theme_bw()  +
#   scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)")) +
#   theme(axis.text = element_text(face="bold"))
# 
# plot_smd.19
# 
# ggsave("pdf-plot/plot_smd.19.pdf")





anal_arope.19 <- data_2019.df %>%
  rename(rotational_group = DB075) %>%
  as_survey(weights = c(rb050)) %>%
  group_by(rotational_group) %>% 
  summarise(Mean =  survey_mean(AROPE_new, na.rm = T),
            N = n(),
            Std.Dev = sd(AROPE_new, na.rm = T),
            Min = min(AROPE_new, na.rm = T),
            Max = max(AROPE_new, na.rm = T))  %>%          
  mutate(L.CI = Mean - qt(1 - (0.05 / 2), N - 1) * Mean_se,
         U.CI = Mean + qt(1 - (0.05 / 2), N - 1) * Mean_se) %>%
  arrange(rotational_group)   %>%
  dplyr::select(-Mean_se) %>%
  as.data.frame()        

anal_arope.19



stargazer(anal_arope.19, type = "latex", title = "SILC 2019: AROPE", summary = FALSE)


ci_plot_arope.19 <- ggplot(anal_arope.19, aes(x=rotational_group, y=Mean, colour=rotational_group,  group=1)) + 
  geom_errorbar(aes(ymin=L.CI, ymax=U.CI), width=.69, position=pd, size = 1.25) +
  geom_point(position=pd, size = 2.5) +
  scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)")) +
  theme(axis.text = element_text(face="bold")) +
  ggtitle("SILC'19: AROPE")
ci_plot_arope.19


ggsave("pdf-plot/ci_plot_arope.19.pdf")

# 
# plot_arope.19 <- ggplot(anal_arope.19, aes(rotational_group, Mean)) +
#   geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
#   geom_col(colour = "red", alpha = 0.69, fill = "yellow") +
#   geom_point(colour = "purple", shape = "diamond", size = 5) +
#   labs(x = "Rotational Group (Year)", y = "Mean")  +
#   theme_bw()  +
#   scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)")) +
#   theme(axis.text = element_text(face="bold"))
# 
# plot_arope.19
# 
# ggsave("pdf-plot/plot_arope.19.pdf")
# 


anal_lwi.19 <- data_2019.df %>%
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
  arrange(rotational_group)    %>%
  dplyr::select(-Mean_se) %>%
  as.data.frame()       

anal_lwi.19


stargazer(anal_lwi.19, type = "latex", title = "SILC 2019: LWI", summary = FALSE)



ci_plot_lwi.19 <- ggplot(anal_lwi.19, aes(x=rotational_group, y=Mean, colour=rotational_group,  group=1)) + 
  geom_errorbar(aes(ymin=L.CI, ymax=U.CI), width=.69, position=pd, size = 1.25) +
  geom_point(position=pd, size = 2.5) +
  scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)")) +
  theme(axis.text = element_text(face="bold")) +
  ggtitle("SILC'19: LWI")
ci_plot_lwi.19


ggsave("pdf-plot/ci_plot_lwi.19.pdf")


# 
# plot_lwi.19 <- ggplot(anal_lwi.19, aes(rotational_group, Mean)) +
#   geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
#   geom_col(colour = "red", alpha = 0.69, fill = "yellow") +
#   geom_point(colour = "purple", shape = "diamond", size = 5) +
#   labs(x = "Rotational Group (Year)", y = "Mean")  +
#   theme_bw()  +
#   scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)")) +
#   theme(axis.text = element_text(face="bold"))
# 
# plot_lwi.19
# 
# ggsave("pdf-plot/plot_lwi.19.pdf")
# 
# 
# 
# 


anal_eqinc20.19 <- data_2019.df %>%
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
  arrange(rotational_group) %>%
  dplyr::select(-Mean_se) %>%
  as.data.frame()            

anal_eqinc20.19


stargazer(anal_eqinc20.19, type = "latex", title = "SILC 2019: LWI", summary = FALSE)



ci_plot_eqinc20.19 <- ggplot(anal_eqinc20.19, aes(x=rotational_group, y=Mean, colour=rotational_group,  group=1)) + 
  geom_errorbar(aes(ymin=L.CI, ymax=U.CI), width=.69, position=pd, size = 1.25) +
  geom_point(position=pd, size = 2.5) +
  scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)")) +
  theme(axis.text = element_text(face="bold")) +
  ggtitle("SILC'19: EQINC")
ci_plot_eqinc20.19


ggsave("pdf-plot/ci_plot_eqinc20.19.pdf")


# 
# plot_eqinc20.19 <- ggplot(anal_eqinc20.19, aes(rotational_group, Mean)) +
#   geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
#   geom_col(colour = "red", alpha = 0.69, fill = "yellow") +
#   geom_point(colour = "purple", shape = "diamond", size = 5) +
#   labs(x = "Rotational Group (Year)", y = "Mean")  +
#   theme_bw()  +
#   scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)")) +
#   theme(axis.text = element_text(face="bold"))
# 
# plot_eqinc20.19
# 
# ggsave("pdf-plot/plot_eqinc20.19.pdf")
# 


anal_msd.19 <- data_2019.df %>%
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
  arrange(rotational_group) %>%
  dplyr::select(-Mean_se) %>%
  as.data.frame()               

anal_msd.19


stargazer(anal_msd.19, type = "latex", title = "SILC 2019: MSD", summary = FALSE)



ci_plot_msd.19 <- ggplot(anal_msd.19, aes(x=rotational_group, y=Mean, colour=rotational_group,  group=1)) + 
  geom_errorbar(aes(ymin=L.CI, ymax=U.CI), width=.69, position=pd, size = 1.25) +
  geom_point(position=pd, size = 2.5) +
  scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)")) +
  theme(axis.text = element_text(face="bold")) +
  ggtitle("SILC'19: MSD")
ci_plot_msd.19


ggsave("pdf-plot/ci_plot_msd.19.pdf")


stargazer(anal_msd.19, type = "text", title = "Material and social deprivation rate (MSD)", summary = FALSE)

stargazer(anal_msd.19, type = "latex", title = "Material and social deprivation rate (MSD)", summary = FALSE)


# 
# plot_msd.19 <- ggplot(anal_msd.19, aes(rotational_group, Mean),  color = rotational_group) +
#   geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
#   geom_col(colour = "red", alpha = 0.69, fill = "yellow") +
#   geom_point(colour = "purple", shape = "diamond", size = 5) +
#   labs(x = "Rotational Group (Year)", y = "Mean")  +
#   theme_bw()  +
#   ylim(0, 0.15) +
#   scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)")) +
#   theme(axis.text = element_text(face="bold"))
# 
# plot_msd.19
# 
# 
# ggsave("pdf-plot/plot_msd.19.pdf")




## severe material and social deprivation (SMSD)


anal_sevmsd.19 <- data_2019.df %>%
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
  arrange(rotational_group) %>%
  dplyr::select(-Mean_se) %>%
  as.data.frame()               

anal_sevmsd.19

stargazer(anal_sevmsd.19, type = "latex", title = "SILC 2019: SMSD", summary = FALSE)


ci_plot_sevmsd.19 <- ggplot(anal_sevmsd.19, aes(x=rotational_group, y=Mean, colour=rotational_group,  group=1)) + 
  geom_errorbar(aes(ymin=L.CI, ymax=U.CI), width=.69, position=pd, size = 1.25) +
  geom_point(position=pd, size = 2.5) +
  scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)")) +
  theme(axis.text = element_text(face="bold")) +
  ggtitle("SILC'19: SMSD")
ci_plot_sevmsd.19


ggsave("pdf-plot/ci_plot_sevmsd.19.pdf")



stargazer(anal_sevmsd.19, type = "text", title = "Severe Material and Social Deprivation (SMSD)", summary = FALSE)

stargazer(anal_sevmsd.19, type = "latex", title = "Severe Material and Social Deprivation (SMSD)", summary = FALSE)

# 
# plot_sevmsd.19 <- ggplot(anal_sevmsd.19, aes(rotational_group, Mean),  color = rotational_group) +
#   geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
#   geom_col(colour = "red", alpha = 0.69, fill = "yellow") +
#   geom_point(colour = "purple", shape = "diamond", size = 5) +
#   labs(x = "Rotational Group (Year)", y = "Mean")  +
#   theme_bw()  +
#   ylim(0, 0.1) +
#   scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)")) +
#   theme(axis.text = element_text(face="bold"))
# 
# plot_sevmsd.19
# 
# ggsave("pdf-plot/plot_sevmsd.19.pdf")



rg_GINI.19 <- readxl::read_excel("2019_RG_gini.xlsx")
rg_GINI.19 <- data.frame(rg_GINI.19)


rg_GINI.19  %<>% mutate(rotational_group = case_when(rotational_group == "1" ~ "1 (2017)",
                                                     rotational_group == "2" ~ "2 (2018)",
                                                     rotational_group == "3" ~ "3 (2015)",
                                                     rotational_group == "4" ~ "4 (2016)",
                                                     TRUE ~   "5 (2019)"))
anal_GINI.19 <- rg_GINI.19 %>%
  summarise(Mean =  mean(value),
            Std.Dev = sd(value),
            Min = min(value),
            Max = max(value))  %>%
  as.data.frame()

anal_GINI.19




stargazer(rg_GINI.19, type = "text", title = "GINI Index Rotation Group wise", summary = FALSE)

stargazer(rg_GINI.19, type = "latex", title = "GINI Index Rotation Group wise", summary = FALSE)







plot_rg_GINI.19 <-  ggplot(rg_GINI.19, aes(rotational_group, value),  color = rotational_group) +
  geom_line(colour = "purple", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "purple", alpha = 0.69, fill = "white") +
  geom_point(colour = "purple", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  ylim(0, 30) +
  scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)")) +
  theme(axis.text = element_text(face="bold"))

plot_rg_GINI.19

ggsave("pdf-plot/plot_rg_GINI.19.pdf")





rg_S80S20.19 <- readxl::read_excel("2019_RG_s80s20.xlsx")
rg_S80S20.19 <- data.frame(rg_S80S20.19)

rg_S80S20.19  %<>% mutate(rotational_group = case_when(rotational_group == "1" ~ "1 (2017)",
                                                       rotational_group == "2" ~ "2 (2018)",
                                                       rotational_group == "3" ~ "3 (2015)",
                                                       rotational_group == "4" ~ "4 (2016)",
                                                       TRUE ~   "5 (2019)"))




stargazer(rg_S80S20.19, type = "text", title = "S80S20 Ratio Rotation Group wise", summary = FALSE)
stargazer(rg_S80S20.19, type = "latex", title = "S80S20 Ratio Rotation Group wise", summary = FALSE)


anal_S80S20.19 <- rg_S80S20.19 %>%
  summarise(Mean =  mean(value),
            Std.Dev = sd(value),
            Min = min(value),
            Max = max(value))  %>%
  as.data.frame()

anal_S80S20.19




plot_rg_S80S20.19 <-  ggplot(rg_S80S20.19, aes(rotational_group, value),  color = rotational_group) +
  geom_line(colour = "red", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "red", alpha = 0.69, fill = "white") +
  geom_point(colour = "red", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  ylim(0,5) +
  scale_x_discrete(limit = c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)")) +
  theme(axis.text = element_text(face="bold"))

plot_rg_S80S20.19

ggsave("pdf-plot/plot_rg_S80S20.19.pdf")







eda2019 <- data_2019.df %>%
  as_survey(weights = c(rb050)) %>%
  summarise(Mean =  survey_mean(MIN60


, na.rm = T),
            N = sum(!is.na(MIN60

)),
            Std.Dev = sd(MIN60

, na.rm=T
),
            Min = min(MIN60


),
            Max = max(MIN60

                      

)) 
eda2019






stargazer(eda2019, type = "text", title = "Descriptive Statistics", summary = FALSE)
stargazer(eda2019, type = "latex", title = "Descriptive Statistics", summary = FALSE)


plot.eda2019 <- ggplot(eda2019, aes(rotational_group, Mean),  color = rotational_group) +
  geom_point(color = "firebrick", shape = "diamond", size = 2) +
  geom_line(color = "firebrick", linetype = "dotted", size = .3,  group = 1) +
  labs(x = "Rotational Group (Year)", y = "Mean")

plot.eda2019









