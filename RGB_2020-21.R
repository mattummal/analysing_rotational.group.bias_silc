


setwd('C:/THESIS/SILC_dataset')

## all indicators 2020

eda_2020 <- readxl::read_excel("eda2020.xlsx")
eda_2020 <- data.frame(eda_2020)            
eda_2020      


## RGB AROP

gb_arop <- (anal_min60[2]/eda_2020$Mean[1]) * 100

gb_arop$rotational_group <- c("1 (2017)", "2 (2018)",  "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)")
gb_arop




stargazer(gb_arop, type = "latex", title = " At Risk of Poverty (min60)", summary = FALSE)

gb_plot_arop <- ggplot(gb_arop, aes(rotational_group, Mean)) +
  geom_line(colour = "red", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "red", alpha = 0.69, fill = "black") +
  geom_point(colour = "red", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'20 Rotational Group Bias: AROP")+
  theme(axis.text = element_text(face="bold"))

gb_plot_arop



ggsave("pdf-plot/gb_plot_arop.pdf")



## RGB SMD


gb_smd <- (anal_smd[2]/eda_2020$Mean[2]) * 100

gb_smd$rotational_group <- c("1 (2017)", "2 (2018)",  "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)")

stargazer(gb_smd, type = "latex", title = "Rotation group bias SMD", summary = FALSE)



gb_plot_smd <- ggplot(gb_smd, aes(rotational_group, Mean)) +
  geom_line(colour = "red", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "red", alpha = 0.69, fill = "black") +
  geom_point(colour = "red", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'20 Rotational Group Bias: SMD")+
  theme(axis.text = element_text(face="bold"))

gb_plot_smd


ggsave("pdf-plot/gb_plot_smd.pdf")




## RGB LWI


gb_lwi <- (anal_lwi[2]/eda_2020$Mean[3]) * 100

gb_lwi$rotational_group <- c("1 (2017)", "2 (2018)",  "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)")

stargazer(gb_lwi, type = "latex", title = "Rotation group bias LWI", summary = FALSE)


gb_plot_lwi <- ggplot(gb_lwi, aes(rotational_group, Mean)) +
  geom_line(colour = "red", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "red", alpha = 0.69, fill = "black") +
  geom_point(colour = "red", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'20 Rotational Group Bias: LWI")+
  theme(axis.text = element_text(face="bold"))

gb_plot_lwi


ggsave("pdf-plot/gb_plot_lwi.pdf")



## RGB eqinc20 


gb_eqinc20 <- (anal_eqinc20[2]/eda_2020$Mean[4]) * 100

gb_eqinc20$rotational_group <- c("1 (2017)", "2 (2018)",  "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)")

stargazer(gb_eqinc20, type = "latex", title = "Rotation group bias EQ_INC20", summary = FALSE)


gb_plot_eqinc20 <- ggplot(gb_eqinc20, aes(rotational_group, Mean)) +
  geom_line(colour = "red", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "red", alpha = 0.69, fill = "black") +
  geom_point(colour = "red", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'20 Rotational Group Bias: EQ_INC20")+
  theme(axis.text = element_text(face="bold"))

gb_plot_eqinc20 


ggsave("pdf-plot/gb_plot_eqinc20 .pdf")





## RGB AROPE


gb_arope <- (anal_arope[2]/eda_2020$Mean[5]) * 100

gb_arope$rotational_group <- c("1 (2017)", "2 (2018)",  "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)")

stargazer(gb_arope, type = "latex", title = "Rotation group bias AROPE", summary = FALSE)


gb_plot_arope <- ggplot(gb_arope, aes(rotational_group, Mean)) +
  geom_line(colour = "red", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "red", alpha = 0.69, fill = "black") +
  geom_point(colour = "red", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'20 Rotational Group Bias: AROPE")+
  theme(axis.text = element_text(face="bold"))

gb_plot_arope


ggsave("pdf-plot/gb_plot_arope.pdf")





## RGB MSD


gb_msd <- (anal_msd[2]/eda_2020$Mean[6]) * 100

gb_msd$rotational_group <- c("1 (2017)", "2 (2018)",  "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)")

stargazer(gb_msd, type = "latex", title = "Rotation group bias MSD", summary = FALSE)


gb_plot_msd <- ggplot(gb_msd, aes(rotational_group, Mean)) +
  geom_line(colour = "red", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "red", alpha = 0.69, fill = "black") +
  geom_point(colour = "red", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'20 Rotational Group Bias: MSD")+
  theme(axis.text = element_text(face="bold"))

gb_plot_msd


ggsave("pdf-plot/gb_plot_msd.pdf")





## RGB SMSD


gb_sevmsd <- (anal_sevmsd[2]/eda_2020$Mean[7]) * 100

gb_sevmsd$rotational_group <- c("1 (2017)", "2 (2018)",  "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)")

stargazer(gb_sevmsd, type = "latex", title = "Rotation group bias SMSD", summary = FALSE)


gb_plot_sevmsd <- ggplot(gb_sevmsd, aes(rotational_group, Mean)) +
  geom_line(colour = "red", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "red", alpha = 0.69, fill = "black") +
  geom_point(colour = "red", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'20 Rotational Group Bias: SMSD")+
  theme(axis.text = element_text(face="bold"))

gb_plot_sevmsd


ggsave("pdf-plot/gb_plot_sevmsd.pdf")





## RGB GINI


gb_gini <- (rg_GINI$value/anal_GINI$Mean[1]) * 100
gb_gini <- as.data.frame(gb_gini)



gb_gini$rotational_group <- c("1 (2017)", "2 (2018)",  "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)")



stargazer(gb_gini, type = "latex", title = "Rotation group bias GINI", summary = FALSE)


gb_plot_gini <- ggplot(gb_gini, aes(rotational_group, gb_gini)) +
  geom_line(colour = "blue", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "blue", alpha = 0.69, fill = "black") +
  geom_point(colour = "blue", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'20 Rotational Group Bias: GINI")+
  theme(axis.text = element_text(face="bold"))

gb_plot_gini


ggsave("pdf-plot/gb_plot_gini.pdf")



## RGB S80S20


gb_s80s20 <- (rg_S80S20$value/anal_S80S20$Mean[1]) * 100
gb_s80s20 <- as.data.frame(gb_s80s20)



gb_s80s20$rotational_group <- c("1 (2017)", "2 (2018)",  "3 (2015)", "4 (2016)", "5 (2019)", "6 (2020)")



stargazer(gb_s80s20, type = "latex", title = "Rotation group bias S80S20", summary = FALSE)


gb_plot_s80s20 <- ggplot(gb_s80s20, aes(rotational_group, gb_s80s20)) +
  geom_line(colour = "blue", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "blue", alpha = 0.69, fill = "black") +
  geom_point(colour = "blue", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  ylim(0,120) +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'20 Rotational Group Bias: S80S20")+
  theme(axis.text = element_text(face="bold"))

gb_plot_s80s20


ggsave("pdf-plot/gb_plot_s80s20.pdf")


#################################################################################################################


# ## all indicators 2021

eda_2021 <- readxl::read_excel("eda2021.xlsx")
eda_2021 <- data.frame(eda_2021)            
eda_2021       


## RGB AROP

gb_arop.21 <- (anal_min60.21[2]/eda_2021$Mean[1]) * 100

gb_arop.21$rotational_group <- c("1 (2017)", "2 (2018)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)")
gb_arop.21


stargazer(gb_arop.21, type = "latex", title = " At Risk of Poverty (min60)", summary = FALSE)

gb_plot_arop.21 <- ggplot(gb_arop.21, aes(rotational_group, Mean)) +
  geom_line(colour = "darkblue", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "darkblue", alpha = 0.69, fill = "pink") +
  geom_point(colour = "darkblue", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'21 Rotational Group Bias: AROP")+
  theme(axis.text = element_text(face="bold"))

gb_plot_arop.21


ggsave("pdf-plot/gb_plot_arop.21.pdf")





## RGB SMD


gb_smd.21 <- (anal_smd.21[2]/eda_2021$Mean[2]) * 100

gb_smd.21$rotational_group <- c("1 (2017)", "2 (2018)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)")

stargazer(gb_smd.21, type = "latex", title = "Rotation group bias SMD", summary = FALSE)



gb_plot_smd.21 <- ggplot(gb_smd.21, aes(rotational_group, Mean)) +
  geom_line(colour = "darkblue", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "darkblue", alpha = 0.69, fill = "pink") +
  geom_point(colour = "darkblue", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'21 Rotational Group Bias: SMD")+
  theme(axis.text = element_text(face="bold"))

gb_plot_smd.21


ggsave("pdf-plot/gb_plot_smd.21.pdf")



## RGB LWI


gb_lwi.21 <- (anal_lwi.21[2]/eda_2021$Mean[3]) * 100

gb_lwi.21$rotational_group <- c("1 (2017)", "2 (2018)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)")

stargazer(gb_lwi.21, type = "latex", title = "Rotation group bias LWI", summary = FALSE)


gb_plot_lwi.21 <- ggplot(gb_lwi.21, aes(rotational_group, Mean)) +
  geom_line(colour = "darkblue", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "darkblue", alpha = 0.69, fill = "pink") +
  geom_point(colour = "darkblue", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'21 Rotational Group Bias: LWI")+
  theme(axis.text = element_text(face="bold"))

gb_plot_lwi.21


ggsave("pdf-plot/gb_plot_lwi.21.pdf")




## RGB EQINC20


gb_eqinc.21 <- (anal_eqinc20.21[2]/eda_2021$Mean[4]) * 100

gb_eqinc.21$rotational_group <- c("1 (2017)", "2 (2018)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)")

stargazer(gb_eqinc.21, type = "latex", title = "Rotation group bias EQ_INC20", summary = FALSE)


gb_plot_eqinc.21 <- ggplot(gb_eqinc.21, aes(rotational_group, Mean)) +
  geom_line(colour = "darkblue", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "darkblue", alpha = 0.69, fill = "pink") +
  geom_point(colour = "darkblue", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'21 Rotational Group Bias: EQ_INC20")+
  theme(axis.text = element_text(face="bold"))

gb_plot_eqinc.21


ggsave("pdf-plot/gb_plot_eqinc.21.pdf")






## RGB AROPE


gb_arope.21 <- (anal_arope.21[2]/eda_2021$Mean[5]) * 100

gb_arope.21$rotational_group <- c("1 (2017)", "2 (2018)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)")

stargazer(gb_arope.21, type = "latex", title = "SILC'21 Rotation group bias AROPE", summary = FALSE)


gb_plot_arope.21 <- ggplot(gb_arope.21, aes(rotational_group, Mean)) +
  geom_line(colour = "darkblue", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "darkblue", alpha = 0.69, fill = "pink") +
  geom_point(colour = "darkblue", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  ylim(0,120) +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'21 Rotational Group Bias: AROPE")+
  theme(axis.text = element_text(face="bold"))

gb_plot_arope.21


ggsave("pdf-plot/gb_plot_arope.21.pdf")





## RGB MSD


gb_msd.21 <- (anal_msd.21[2]/eda_2021$Mean[6]) * 100

gb_msd.21$rotational_group <- c("1 (2017)", "2 (2018)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)")

stargazer(gb_msd.21, type = "latex", title = "SILC'21 Rotation group bias MSD", summary = FALSE)


gb_plot_msd.21 <- ggplot(gb_msd.21, aes(rotational_group, Mean)) +
  geom_line(colour = "darkblue", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "darkblue", alpha = 0.69, fill = "pink") +
  geom_point(colour = "darkblue", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'21 Rotational Group Bias: MSD")+
  theme(axis.text = element_text(face="bold"))

gb_plot_msd.21


ggsave("pdf-plot/gb_plot_msd.21.pdf")



## RGB SMSD


gb_sevmsd.21 <- (anal_sevmsd.21[2]/eda_2021$Mean[7]) * 100

gb_sevmsd.21$rotational_group <- c("1 (2017)", "2 (2018)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)")

stargazer(gb_sevmsd.21, type = "latex", title = "SILC'21 Rotation group bias SMSD", summary = FALSE)


gb_plot_sevmsd.21 <- ggplot(gb_sevmsd.21, aes(rotational_group, Mean)) +
  geom_line(colour = "darkblue", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "darkblue", alpha = 0.69, fill = "pink") +
  geom_point(colour = "darkblue", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'21 Rotational Group Bias: SMSD")+
  theme(axis.text = element_text(face="bold"))

gb_plot_sevmsd.21


ggsave("pdf-plot/gb_plot_sevmsd.21.pdf")




## RGB GINI

gb_gini.21 <- (rg_GINI.21$value/anal_GINI.21$Mean[1]) * 100
gb_gini.21 <- as.data.frame(gb_gini.21)




gb_gini.21$rotational_group <-  c("1 (2017)", "2 (2018)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)")



stargazer(gb_gini.21, type = "latex", title = "Rotation group bias GINI", summary = FALSE)


gb_plot_gini.21 <- ggplot(gb_gini.21, aes(rotational_group, gb_gini.21)) +
  geom_line(colour = "blue", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "blue", alpha = 0.69, fill = "pink") +
  geom_point(colour = "blue", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'21 Rotational Group Bias: GINI")+
  theme(axis.text = element_text(face="bold"))

gb_plot_gini.21


ggsave("pdf-plot/gb_plot_gini.21.pdf")



## RGB S80S20

gb_s80s20.21 <- (rg_S80S20.21$value/anal_S80S20.21$Mean[1]) * 100
gb_s80s20.21 <- as.data.frame(gb_s80s20.21)



gb_s80s20.21$rotational_group <-  c("1 (2017)", "2 (2018)", "4 (2016)", "5 (2019)", "6 (2020)", "7 (2021)")


stargazer(gb_s80s20.21, type = "latex", title = "Rotation group bias S80S20", summary = FALSE)


gb_plot_s80s20.21 <- ggplot(gb_s80s20.21, aes(rotational_group, gb_s80s20.21)) +
  geom_line(colour = "blue", linetype = "dotted", size = .9, group = 1) +
  geom_col(colour = "blue", alpha = 0.69, fill = "pink") +
  geom_point(colour = "blue", shape = "diamond", size = 5) +
  labs(x = "Rotational Group (Year)", y = "Mean")  +
  theme_bw()  +
  geom_hline(yintercept=100, linetype="dashed", color = "blue", size=1.25) +
  ggtitle("SILC'21 Rotational Group Bias: S80S20")+
  theme(axis.text = element_text(face="bold"))

gb_plot_s80s20.21


ggsave("pdf-plot/gb_plot_s80s20.21.pdf")

































