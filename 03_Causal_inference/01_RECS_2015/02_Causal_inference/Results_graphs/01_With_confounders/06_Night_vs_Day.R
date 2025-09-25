library(ggplot2)
library(ggpmisc)
library(readxl)
library(plotly)
library(dplyr)


setwd("C:/Users/HUBEL/GDrive/08_THESIS/Causal_infer/01. RECS/Modelling/02_DAG/02.Without_outliers/04_Analysis_v2/02_Causal_inference/01_All_variables/results_v2/01_With_confounders")
data <- read_excel('With_confounders.xlsx', sheet = "Night_day_IAT")



my_vector <- seq(from = 0, to = 500, by = 0.5)

data$EUI = abs(data$EUI)


bins = cut(data$EUI, breaks=,my_vector,include.lowest = TRUE, right = FALSE,  ordered_result = TRUE)

data$`EUI range` = bins

data$Variable<-factor(data$Variable, levels = c('0 to -1', '0 to 1'))




data$`EUI change:`<-factor(data$`EUI change:`, levels = c('Decrease','No change','Increase'))

data$`Scenario:`<-factor(data$`Scenario:`, levels = c('Scenario 1','Scenario 2'))


View(data)


my_labels <- c('From  0 to -1', 'From 0 to 1')


jpeg("06_Night_day_IAT.jpeg", units="cm", width=22, height=15, res=300)
ggplot(data, aes(x = Variable, y =EUI,
                      # size = `EUI range`,
                      color = `Scenario:`,
                 fill=`Scenario:`,
                 shape=`EUI change:`
                 ))+
  geom_point(alpha = 0.7, size = 5)+
  scale_shape_manual(values = c(25, 21))+
  scale_fill_manual(values=c('purple','#FFCC00'))+
  scale_color_manual(values=c('purple','#FFCC00'))+
  ylab(bquote('Average change in EUI (kWh/'~m^2~')'))+
  xlab("\nNighttime air temperature")+
  scale_x_discrete(labels= my_labels)+
  # scale_y_continuous(limits = c(0,6), breaks= seq(0,6,by=1)) +
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 15), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=14), legend.title = element_text(size=15), legend.position = "bottom")
dev.off()

