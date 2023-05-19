


setwd('C:/THESIS/SILC_dataset')
windows(width=8.5, height=7.5, title="Controlled Size")

## all indicators 2019

eda_2019 <- readxl::read_excel("eda2019.xlsx")
eda_2019 <- data.frame(eda_2019)            
eda_2019      



stargazer(eda_2019, type = "latex", title = "Descriptive statistics for the indicators: SILC'19", summary = FALSE)



## RGB AROP

gb_arop.19 <- (anal_min60.19[2]/eda_2019$Mean[1]) * 100

gb_arop.19$rotational_group <- c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)")
gb_arop.19


stargazer(gb_arop.19, type = "latex", title = " At Risk of Poverty (min60)", summary = FALSE)

gb_plot_arop.19 <- ggplot(gb_arop.19, aes(rotational_group, Mean)) +
  geom_line(colour = "darkgreen", linetype = "dotted", group = 1) +
  geom_col(colour = "darkgreen", alpha = 0.69, fill = "violet") +
  geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'19 Rotational Group Bias: AROP")+
  theme(axis.text = element_text(face="bold"))

gb_plot_arop.19


ggsave("pdf-plot/gb_plot_arop.19.pdf")





## RGB SMD


gb_smd.19 <- (anal_smd.19[2]/eda_2019$Mean[2]) * 100

gb_smd.19$rotational_group <- c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)")

stargazer(gb_smd.19, type = "latex", title = "Rotation group bias SMD", summary = FALSE)



gb_plot_smd.19 <- ggplot(gb_smd.19, aes(rotational_group, Mean)) +
  geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "darkgreen", alpha = 0.69, fill = "violet") +
  geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'19 Rotational Group Bias: SMD")+
  theme(axis.text = element_text(face="bold"))

gb_plot_smd.19


ggsave("pdf-plot/gb_plot_smd.19.pdf")



## RGB LWI


gb_lwi.19 <- (anal_lwi.19[2]/eda_2019$Mean[3]) * 100

gb_lwi.19$rotational_group <- c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)")

stargazer(gb_lwi.19, type = "latex", title = "Rotation group bias LWI", summary = FALSE)


gb_plot_lwi.19 <- ggplot(gb_lwi.19, aes(rotational_group, Mean)) +
  geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "darkgreen", alpha = 0.69, fill = "violet") +
  geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'19 Rotational Group Bias: LWI")+
  theme(axis.text = element_text(face="bold"))

gb_plot_lwi.19


ggsave("pdf-plot/gb_plot_lwi.19.pdf")



## RGB EQINC20


gb_eqinc.19 <- (anal_eqinc20.19[2]/eda_2019$Mean[4]) * 100

gb_eqinc.19$rotational_group <- c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)")

stargazer(gb_eqinc.19, type = "latex", title = "Rotation group bias EQ_INC20", summary = FALSE)


gb_plot_eqinc.19 <- ggplot(gb_eqinc.19, aes(rotational_group, Mean)) +
  geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "darkgreen", alpha = 0.69, fill = "violet") +
  geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'19 Rotational Group Bias: EQ_INC20")+
  theme(axis.text = element_text(face="bold"))

gb_plot_eqinc.19


ggsave("pdf-plot/gb_plot_eqinc.19.pdf")






## RGB AROPE


gb_arope.19 <- (anal_arope.19[2]/eda_2019$Mean[6]) * 100

gb_arope.19$rotational_group <- c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)")

stargazer(gb_arope.19, type = "latex", title = "SILC'19 Rotation group bias AROPE", summary = FALSE)


gb_plot_arope.19 <- ggplot(gb_arope.19, aes(rotational_group, Mean)) +
  geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "darkgreen", alpha = 0.69, fill = "violet") +
  geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  ylim(0,120) +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'19 Rotational Group Bias: AROPE")+
  theme(axis.text = element_text(face="bold"))

gb_plot_arope.19


ggsave("pdf-plot/gb_plot_arope.19.pdf")





## RGB MSD


gb_msd.19 <- (anal_msd.19[2]/eda_2019$Mean[7]) * 100

gb_msd.19$rotational_group <- c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)")

stargazer(gb_msd.19, type = "latex", title = "SILC'19 Rotation group bias MSD", summary = FALSE)


gb_plot_msd.19 <- ggplot(gb_msd.19, aes(rotational_group, Mean)) +
  geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "darkgreen", alpha = 0.69, fill = "violet") +
  geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'19 Rotational Group Bias: MSD")+
  theme(axis.text = element_text(face="bold"))

gb_plot_msd.19


ggsave("pdf-plot/gb_plot_msd.19.pdf")



## RGB SMSD


gb_sevmsd.19 <- (anal_sevmsd.19[2]/eda_2019$Mean[8]) * 100

gb_sevmsd.19$rotational_group <- c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)")

stargazer(gb_sevmsd.19, type = "latex", title = "SILC'19 Rotation group bias SMSD", summary = FALSE)


gb_plot_sevmsd.19 <- ggplot(gb_sevmsd.19, aes(rotational_group, Mean)) +
  geom_line(colour = "darkgreen", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "darkgreen", alpha = 0.69, fill = "violet") +
  geom_point(colour = "darkgreen", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'19 Rotational Group Bias: SMSD")+
  theme(axis.text = element_text(face="bold"))

gb_plot_sevmsd.19


ggsave("pdf-plot/gb_plot_sevmsd.19.pdf")




## RGB GINI

gb_gini.19 <- (rg_GINI.19$value/anal_GINI.19$Mean[1]) * 100
gb_gini.19 <- as.data.frame(gb_gini.19)




gb_gini.19$rotational_group <-  c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)")



stargazer(gb_gini.19, type = "latex", title = "Rotation group bias GINI", summary = FALSE)


gb_plot_gini.19 <- ggplot(gb_gini.19, aes(rotational_group, gb_gini.19)) +
  geom_line(colour = "blue", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "blue", alpha = 0.69, fill = "violet") +
  geom_point(colour = "blue", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "green", size=1.25) +
  ggtitle("SILC'19 Rotational Group Bias: GINI")+
  theme(axis.text = element_text(face="bold"))

gb_plot_gini.19


ggsave("pdf-plot/gb_plot_gini.19.pdf")



## RGB S80S20

gb_s80s20.19 <- (rg_S80S20.19$value/anal_S80S20.19$Mean[1]) * 100
gb_s80s20.19 <- as.data.frame(gb_s80s20.19)



gb_s80s20.19$rotational_group <-  c("1 (2017)", "2 (2018)", "3 (2015)", "4 (2016)", "5 (2019)")


stargazer(gb_s80s20.19, type = "latex", title = "Rotation group bias S80S20", summary = FALSE)


gb_plot_s80s20.19 <- ggplot(gb_s80s20.19, aes(rotational_group, gb_s80s20.19)) +
  geom_line(colour = "blue", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "blue", alpha = 0.69, fill = "violet") +
  geom_point(colour = "blue", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "green", size=1.25) +
  ggtitle("SILC'19 Rotational Group Bias: S80S20")+
  theme(axis.text = element_text(face="bold"))

gb_plot_s80s20.19


ggsave("pdf-plot/gb_plot_s80s20.19.pdf")




