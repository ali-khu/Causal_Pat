library(ggplot2)
library(ggpmisc)
library(readxl)
library(plotly)
library(dplyr)
library(ggpubr)


setwd("C:/Users/HUBEL/GDrive/08_THESIS/Causal_infer/02_Kor_resi/05_Causal_infer/Results/01_with_confounders")
data <- read_excel('With_confounders.xlsx', sheet = "other_b_char")



my_vector <- seq(from = 0, to = 500, by = 0.5)

data$EUI = abs(data$EUI)


bins = cut(data$EUI, breaks=,my_vector,include.lowest = TRUE, right = FALSE,  ordered_result = TRUE)

data$`EUI range` = bins




data$Variable<-factor(data$Variable, levels = c('No. of outer walls', 'Residential floor area', 'No. of heated rooms',
                                                'No. of heated bathrooms', 'No. of external wall windows'))




data$`EUI change:`<-factor(data$`EUI change:`, levels = c('Decrease','No change','Increase'))



View(data)



data_1 <- data[1, c(1,2,3,4)]

data_2 <- data[2, c(1,2,3,4)]

data_3 <- data[3, c(1,2,3,4)]

data_4 <- data[4, c(1,2,3,4)]

data_5 <- data[5, c(1,2,3,4)]



my_labels_1 <- c('No. of\nexternal walls')

my_labels_2 <- c('Residential\nfloor area')

my_labels_3 <- c('No. of\nheated rooms')

my_labels_4 <- c('No. of heated\nbathrooms')

my_labels_5 <- c('No. of external\nwall windows')



jpeg("05_other_b_char_2.jpeg", units="cm", width=25, height=30, res=300)
p1<-  ggplot(data_1, aes(x = Variable, y =EUI,
                      # size = `EUI range`,
                      color = `EUI change:`,
                 fill=`EUI change:`,
                 shape=`EUI change:`
                 ))+
  geom_point(alpha = 0.7, size = 5)+
  scale_shape_manual(values = c(24))+
  scale_fill_manual(values=c('brown'))+
  scale_color_manual(values=c('brown'))+
  ylab(bquote('Change in EUI (kWh/'~m^2~'/ext.wall)'))+
  xlab("")+
  scale_x_discrete(labels= my_labels_1)+
  scale_y_continuous(limits = c(0,42), breaks= seq(0,42,by=6)) +
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 16), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=15), legend.title = element_text(size=15), legend.position = "bottom")


p2<-  ggplot(data_2, aes(x = Variable, y =EUI,
                         # size = `EUI range`,
                         color = `EUI change:`,
                         fill=`EUI change:`,
                         shape=`EUI change:`
    ))+
  geom_point(alpha = 0.7, size = 5)+
  scale_shape_manual(values = c(25))+
  scale_fill_manual(values=c('blue'))+
  scale_color_manual(values=c('blue'))+
  ylab(bquote('Change in EUI (kWh/'~m^2~'/'~m^2~' of res.floor)'))+
  xlab("")+
  scale_x_discrete(labels= my_labels_2)+
  scale_y_continuous(limits = c(0,42), breaks= seq(0,42,by=6)) +
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 16), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=15), legend.title = element_text(size=15), legend.position = "bottom")



p3<-  ggplot(data_3, aes(x = Variable, y =EUI,
                         # size = `EUI range`,
                         color = `EUI change:`,
                         fill=`EUI change:`,
                         shape=`EUI change:`
))+
  geom_point(alpha = 0.7, size = 5)+
  scale_shape_manual(values = c(25))+
  scale_fill_manual(values=c('blue'))+
  scale_color_manual(values=c('blue'))+
  ylab(bquote('Change in EUI (kWh/'~m^2~'/heated room)'))+
  xlab("")+
  scale_x_discrete(labels= my_labels_3)+
  scale_y_continuous(limits = c(0,42), breaks= seq(0,42,by=6)) +
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 16), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=15), legend.title = element_text(size=15), legend.position = "bottom")



p4<-  ggplot(data_4, aes(x = Variable, y =EUI,
                         # size = `EUI range`,
                         color = `EUI change:`,
                         fill=`EUI change:`,
                         shape=`EUI change:`
))+
  geom_point(alpha = 0.7, size = 5)+
  scale_shape_manual(values = c(25))+
  scale_fill_manual(values=c('blue'))+
  scale_color_manual(values=c('blue'))+
  ylab(bquote('Change in EUI (kWh/'~m^2~'/heated bathroom)'))+
  xlab("")+
  scale_x_discrete(labels= my_labels_4)+
  scale_y_continuous(limits = c(0,42), breaks= seq(0,42,by=6)) +
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 16), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=15), legend.title = element_text(size=15), legend.position = "bottom")

p5<-  ggplot(data_5, aes(x = Variable, y =EUI,
                         # size = `EUI range`,
                         color = `EUI change:`,
                         fill=`EUI change:`,
                         shape=`EUI change:`
))+
  geom_point(alpha = 0.7, size = 5)+
  scale_shape_manual(values = c(25))+
  scale_fill_manual(values=c('blue'))+
  scale_color_manual(values=c('blue'))+
  ylab(bquote('Change in EUI (kWh/'~m^2~'/ext.wall window)'))+
  xlab("")+
  scale_x_discrete(labels= my_labels_5)+
  scale_y_continuous(limits = c(0,42), breaks= seq(0,42,by=6)) +
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 16), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=15), legend.title = element_text(size=15), legend.position = "bottom")


ggarrange(p1, p2, p3, p4, p5 , labels = c("A", "B", "C", "D", "E"),ncol = 3, nrow = 2)

dev.off()
