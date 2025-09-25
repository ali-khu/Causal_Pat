library(ggplot2)
library(ggpmisc)
library(readxl)
library(plotly)
library(dplyr)
library(ggpubr)


setwd("C:/Users/HUBEL/GDrive/08_THESIS/Causal_infer/01. RECS/Modelling/02_DAG/02.Without_outliers/04_Analysis_v2/02_Causal_inference/01_All_variables/results_v2/01_With_confounders")
data <- read_excel('With_confounders.xlsx', sheet = "Other_b_char")



my_vector <- seq(from = 0, to = 500, by = 0.5)

data$EUI = abs(data$EUI)


bins = cut(data$EUI, breaks=,my_vector,include.lowest = TRUE, right = FALSE,  ordered_result = TRUE)

data$`EUI range` = bins




data$Variable<-factor(data$Variable, levels = c('No. of windows', 'Year built', 'Total No. of rooms',
                                                'Total floor area', 'Total cooled area', 
                                                'Latent heat infiltration'))



data$`EUI change:`<-factor(data$`EUI change:`, levels = c('Decrease','No change','Increase'))

View(data)


data_1 <- data[1, c(1,2,3,4)]

data_2 <- data[2, c(1,2,3,4)]

data_3 <- data[3, c(1,2,3,4)]

data_4 <- data[4, c(1,2,3,4)]

data_5 <- data[5, c(1,2,3,4)]

data_6 <- data[6, c(1,2,3,4)]



my_labels_1 <- c('No. of windows')

my_labels_2 <- c('Year built')

my_labels_3 <- c('Total No. of rooms')

my_labels_4 <- c('Total floor area')

my_labels_5 <- c('Total cooled area')

my_labels_6 <- c('Latent heat infiltration')




jpeg("09_other_b_char.jpeg", units="cm", width=40, height=30, res=300)
p1 <- ggplot(data_1, aes(x = Variable, y =EUI,
                      # size = `EUI range`,
                      color = `EUI change:`,
                 fill=`EUI change:`,
                 shape=`EUI change:`
                 ))+
  geom_point(alpha = 0.7, size=5)+
  scale_shape_manual(values = c(25))+
  scale_fill_manual(values=c('blue'))+
  scale_color_manual(values=c('blue'))+
  ylab(bquote('Change in EUI (kWh/'~m^2~'/3 windows)'))+
  xlab("Other building characteristics")+
  scale_x_discrete(labels= my_labels_1)+
  scale_y_continuous(limits = c(0,5), breaks= seq(0,5,by=1)) +
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 16), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=15), legend.title = element_text(size=15),  legend.position = "bottom")

p2 <- ggplot(data_2, aes(x = Variable, y =EUI,
                         # size = `EUI range`,
                         color = `EUI change:`,
                         fill=`EUI change:`,
                         shape=`EUI change:`
))+
  geom_point(alpha = 0.7, size=5)+
  scale_shape_manual(values = c(25))+
  scale_fill_manual(values=c('blue'))+
  scale_color_manual(values=c('blue'))+
  ylab(bquote('Change in EUI (kWh/'~m^2~'/decade)'))+
  xlab("Other building characteristics")+
  scale_x_discrete(labels= my_labels_2)+
  scale_y_continuous(limits = c(0,5), breaks= seq(0,5,by=1)) +
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 16), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=15), legend.title = element_text(size=15),  legend.position = "bottom")

p3 <- ggplot(data_3, aes(x = Variable, y =EUI,
                         # size = `EUI range`,
                         color = `EUI change:`,
                         fill=`EUI change:`,
                         shape=`EUI change:`
))+
  geom_point(alpha = 0.7, size=5)+
  scale_shape_manual(values = c(25))+
  scale_fill_manual(values=c('blue'))+
  scale_color_manual(values=c('blue'))+
  ylab(bquote('Change in EUI (kWh/'~m^2~'/2 rooms)'))+
  xlab("Other building characteristics")+
  scale_x_discrete(labels= my_labels_3)+
  scale_y_continuous(limits = c(0,5), breaks= seq(0,5,by=1)) +
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 16), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=15), legend.title = element_text(size=15),  legend.position = "bottom")


p4 <- ggplot(data_4, aes(x = Variable, y =EUI,
                         size = `EUI range`,
                         # color = `EUI change:`,
                         fill=`EUI change:`,
                         shape=`EUI change:`
))+
  geom_point(alpha = 0.7, size=5)+
  scale_shape_manual(values = c(25))+
  scale_fill_manual(values=c('blue'))+
  scale_color_manual(values=c('blue'))+
  ylab(bquote('Change in EUI (kWh/'~m^2~')'))+
  xlab("Other building characteristics")+
  scale_x_discrete(labels= my_labels_4)+
  scale_y_continuous(limits = c(0,5), breaks= seq(0,5,by=1)) +
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 16), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=15), legend.title = element_text(size=15),  legend.position = "bottom")

p5 <- ggplot(data_5, aes(x = Variable, y =EUI,
                         # size = `EUI range`,
                         color = `EUI change:`,
                         fill=`EUI change:`,
                         shape=`EUI change:`
))+
  geom_point(alpha = 0.7, size=5)+
  scale_shape_manual(values = c(25))+
  scale_fill_manual(values=c('blue'))+
  scale_color_manual(values=c('blue'))+
  ylab(bquote('Change in EUI (kWh/'~m^2~')'))+
  xlab("Other building characteristics")+
  scale_x_discrete(labels= my_labels_5)+
  scale_y_continuous(limits = c(0,5), breaks= seq(0,5,by=1)) +
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 16), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=15), legend.title = element_text(size=15),  legend.position = "bottom")

p6 <- ggplot(data_6, aes(x = Variable, y =EUI,
                         # size = `EUI range`,
                         color = `EUI change:`,
                         fill=`EUI change:`,
                         shape=`EUI change:`
))+
  geom_point(alpha = 0.7, size=5)+
  scale_shape_manual(values = c(24))+
  scale_fill_manual(values=c('brown'))+
  scale_color_manual(values=c('brown'))+
  ylab(bquote('Change in EUI (kWh/'~m^2~')'))+
  xlab("Other building characteristics")+
  scale_x_discrete(labels= my_labels_6)+
  scale_y_continuous(limits = c(0,5), breaks= seq(0,5,by=1)) +
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 16), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=15), legend.title = element_text(size=15),  legend.position = "bottom")

ggarrange(p1, p2, p3, p4, p5, p6 , labels = c("A", "B", "C", "D", "E", "F"),ncol = 3, nrow = 2)

dev.off()
