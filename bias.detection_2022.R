
windows(width=8.5, height=7.5, title="Controlled Size")


setwd('C:/THESIS/SILC_dataset')


## all indicators 2020

eda_2022 <- readxl::read_excel("eda2022.xlsx")
eda_2022 <- data.frame(eda_2022)            
eda_2022      

stargazer(eda_2022, type = "latex", title = "Descriptive statistics for the indicators: SILC'22", summary = FALSE)



## RGB AROP

gb_arop.22 <- (anal_min60.22[2]/eda_2022$Mean[1]) * 100

gb_arop.22$rotational_group <- c("1 (2017)", "2 (2018)", "5 (2019)", "6 (2020)", "7 (2021)", "8 (2022)")
gb_arop.22


stargazer(gb_arop.22, type = "latex", title = " At Risk of Poverty (min60)", summary = FALSE)

gb_plot_arop.22 <- ggplot(gb_arop.22, aes(rotational_group, Mean)) +
  geom_line(colour = "darkgreen", linetype = "dotted", group = 1) +
  geom_col(colour = "darkgreen", alpha = 0.69, fill = "red") +
  geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'22 Rotational Group Bias: AROP")+
  theme(axis.text = element_text(face="bold"))

gb_plot_arop.22


ggsave("pdf-plot/gb_plot_arop.22.pdf")





## RGB SMD


gb_smd.22 <- (anal_smd.22[2]/eda_2022$Mean[2]) * 100

gb_smd.22$rotational_group <- c("1 (2017)", "2 (2018)", "5 (2019)", "6 (2020)", "7 (2021)", "8 (2022)")

stargazer(gb_smd.22, type = "latex", title = "Rotation group bias SMD", summary = FALSE)



gb_plot_smd.22 <- ggplot(gb_smd.22, aes(rotational_group, Mean)) +
  geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "darkgreen", alpha = 0.69, fill = "red") +
  geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'22 Rotational Group Bias: SMD")+
  theme(axis.text = element_text(face="bold"))

gb_plot_smd.22

ggsave("pdf-plot/gb_plot_smd.22.pdf")


## RGB LWI


gb_lwi.22 <- (anal_lwi.22[2]/eda_2022$Mean[3]) * 100

gb_lwi.22$rotational_group <- c("1 (2017)", "2 (2018)", "5 (2019)", "6 (2020)", "7 (2021)", "8 (2022)")

stargazer(gb_lwi.22, type = "latex", title = "Rotation group bias LWI", summary = FALSE)


gb_plot_lwi.22 <- ggplot(gb_lwi.22, aes(rotational_group, Mean)) +
  geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "darkgreen", alpha = 0.69, fill = "red") +
  geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'22 Rotational Group Bias: LWI")+
  theme(axis.text = element_text(face="bold"))

gb_plot_lwi.22


ggsave("pdf-plot/gb_plot_lwi.22.pdf")




## RGB EQINC20


gb_eqinc.22 <- (anal_eqinc20.22[2]/eda_2022$Mean[4]) * 100

gb_eqinc.22$rotational_group <- c("1 (2017)", "2 (2018)", "5 (2019)", "6 (2020)", "7 (2021)", "8 (2022)")

stargazer(gb_eqinc.22, type = "latex", title = "Rotation group bias EQ_INC20", summary = FALSE)


gb_plot_eqinc.22 <- ggplot(gb_eqinc.22, aes(rotational_group, Mean)) +
  geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "darkgreen", alpha = 0.69, fill = "red") +
  geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'22 Rotational Group Bias: EQ_INC20")+
  theme(axis.text = element_text(face="bold"))

gb_plot_eqinc.22


ggsave("pdf-plot/gb_plot_eqinc.22.pdf")






## RGB AROPE


gb_arope.22 <- (anal_arope.22[2]/eda_2022$Mean[5]) * 100

gb_arope.22$rotational_group <- c("1 (2017)", "2 (2018)", "5 (2019)", "6 (2020)", "7 (2021)", "8 (2022)")

stargazer(gb_arope.22, type = "latex", title = "SILC'22 Rotation group bias AROPE", summary = FALSE)


gb_plot_arope.22 <- ggplot(gb_arope.22, aes(rotational_group, Mean)) +
  geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "darkgreen", alpha = 0.69, fill = "red") +
  geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  ylim(0,120) +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'22 Rotational Group Bias: AROPE")+
  theme(axis.text = element_text(face="bold"))

gb_plot_arope.22


ggsave("pdf-plot/gb_plot_arope.22.pdf")





## RGB MSD


gb_msd.22 <- (anal_msd.22[2]/eda_2022$Mean[6]) * 100

gb_msd.22$rotational_group <- c("1 (2017)", "2 (2018)", "5 (2019)", "6 (2020)", "7 (2021)", "8 (2022)")

stargazer(gb_msd.22, type = "latex", title = "SILC'22 Rotation group bias MSD", summary = FALSE)


gb_plot_msd.22 <- ggplot(gb_msd.22, aes(rotational_group, Mean)) +
  geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "darkgreen", alpha = 0.69, fill = "red") +
  geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'22 Rotational Group Bias: MSD")+
  theme(axis.text = element_text(face="bold"))

gb_plot_msd.22


ggsave("pdf-plot/gb_plot_msd.22.pdf")



## RGB SMSD


gb_sevmsd.22 <- (anal_sevmsd.22[2]/eda_2022$Mean[7]) * 100

gb_sevmsd.22$rotational_group <- c("1 (2017)", "2 (2018)", "5 (2019)", "6 (2020)", "7 (2021)", "8 (2022)")

stargazer(gb_sevmsd.22, type = "latex", title = "SILC'22 Rotation group bias SMSD", summary = FALSE)


gb_plot_sevmsd.22 <- ggplot(gb_sevmsd.22, aes(rotational_group, Mean)) +
  geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "darkgreen", alpha = 0.69, fill = "red") +
  geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'22 Rotational Group Bias: SMSD")+
  theme(axis.text = element_text(face="bold"))

gb_plot_sevmsd.22


ggsave("pdf-plot/gb_plot_sevmsd.22.pdf")




## RGB GINI

gb_gini.22 <- (rg_GINI.22$value/anal_GINI.22$Mean[1]) * 100
gb_gini.22 <- as.data.frame(gb_gini.22)




gb_gini.22$rotational_group <-  c("1 (2017)", "2 (2018)", "5 (2019)", "6 (2020)", "7 (2021)", "8 (2022)")



stargazer(gb_gini.22, type = "latex", title = "Rotation group bias GINI", summary = FALSE)


gb_plot_gini.22 <- ggplot(gb_gini.22, aes(rotational_group, gb_gini.22)) +
  geom_line(colour = "blue", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "blue", alpha = 0.69, fill = "red") +
  geom_point(colour = "blue", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "green", size=1.25) +
  ggtitle("SILC'22 Rotational Group Bias: GINI")+
  theme(axis.text = element_text(face="bold"))

gb_plot_gini.22


ggsave("pdf-plot/gb_plot_gini.22.pdf")



## RGB S80S20

gb_s80s20.22 <- (rg_S80S20.22$value/anal_S80S20.22$Mean[1]) * 100
gb_s80s20.22 <- as.data.frame(gb_s80s20.22)



gb_s80s20.22$rotational_group <-  c("1 (2017)", "2 (2018)", "5 (2019)", "6 (2020)", "7 (2021)", "8 (2022)")


stargazer(gb_s80s20.22, type = "latex", title = "Rotation group bias S80S20", summary = FALSE)


gb_plot_s80s20.22 <- ggplot(gb_s80s20.22, aes(rotational_group, gb_s80s20.22)) +
  geom_line(colour = "blue", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "blue", alpha = 0.69, fill = "red") +
  geom_point(colour = "blue", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "green", size=1.25) +
  ggtitle("SILC'22 Rotational Group Bias: S80S20")+
  theme(axis.text = element_text(face="bold"))

gb_plot_s80s20.22


ggsave("pdf-plot/gb_plot_s80s20.22.pdf")




