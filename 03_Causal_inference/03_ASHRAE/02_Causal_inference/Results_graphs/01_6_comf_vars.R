library(ggplot2)
library(ggpmisc)
library(readxl)
library(plotly)
library(dplyr)
library(ggpubr)


setwd("C:/Users/HUBEL/GDrive/08_THESIS/Causal_infer/03. Therm_comf/03_Analysis/03_causal_inference/Results/01_with_confounders")
data <- read_excel('With_confounders.xlsx', sheet = "6_comf_var")


my_vector <- seq(from = 0, to = 500, by = 0.5)

data$EUI = abs(data$EUI)


bins = cut(data$EUI, breaks=,my_vector,include.lowest = TRUE, right = FALSE,  ordered_result = TRUE)

data$`EUI range` = bins




data$Variable<-factor(data$Variable, levels = c('ta', 'tr', 'vel','rh', 'met', "clo", "t_out"))




data$`EUI change:`<-factor(data$`EUI change:`, levels = c('Decrease','No change','Increase'))



View(data)



data_1 <- data[c(1,2), c(1,2,3,4)] # ta tr t_out

data_2 <- data[3, c(1,2,3,4)] # vel

data_3 <- data[4, c(1,2,3,4)] # rh

data_4 <- data[5, c(1,2,3,4)] # met

data_5 <- data[6, c(1,2,3,4)] # clo



my_labels_1 <- c('Air\ntemperature', 'Radiant\ntemperature')

my_labels_2 <- c('Air\nvelocity')

my_labels_3 <- c('Relative\nhumidity')

my_labels_4 <- c('Metabolic\nrate')

my_labels_5 <- c('Clothing\ninsulation level')



jpeg("01_ta.jpeg", units="cm", width=18, height=15, res=300)
ggplot(data_1, aes(x = Variable, y =EUI,
                      # size = `EUI range`,
                      color = `EUI change:`,
                 fill=`EUI change:`,
                 shape=`EUI change:`
                 ))+
  geom_point(alpha = 0.7, size = 5)+
  scale_shape_manual(values = c(24))+
  scale_fill_manual(values=c('black'))+
  scale_color_manual(values=c('black'))+
  ylab(bquote('Thermal sensation change/2K'))+
  xlab("wwwwww")+
  scale_x_discrete(labels= my_labels_1)+
  scale_y_continuous(limits = c(0,0.5), breaks= seq(0,0.5,by=0.1)) +
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 16), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=15), legend.title = element_text(size=15))

dev.off()


jpeg("01_vel.jpeg", units="cm", width=10, height=15, res=300)
ggplot(data_2, aes(x = Variable, y =EUI,
                         # size = `EUI range`,
                         color = `EUI change:`,
                         fill=`EUI change:`,
                         shape=`EUI change:`
    ))+
  geom_point(alpha = 0.7, size = 5)+
  scale_shape_manual(values = c(24))+
  scale_fill_manual(values=c('black'))+
  scale_color_manual(values=c('black'))+
  ylab(bquote('Thermal sensation change/0.5 m/s)'))+
  xlab("Environmental factors")+
  scale_x_discrete(labels= my_labels_2)+
  scale_y_continuous(limits = c(0,0.5), breaks= seq(0,10.54,by=0.1)) +
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 16), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=15), legend.title = element_text(size=15))

dev.off()


jpeg("01_rh.jpeg", units="cm", width=10, height=15, res=300)
ggplot(data_3, aes(x = Variable, y =EUI,
                         # size = `EUI range`,
                         color = `EUI change:`,
                         fill=`EUI change:`,
                         shape=`EUI change:`
))+
  geom_point(alpha = 0.7, size = 5)+
  scale_shape_manual(values = c(21))+
  scale_fill_manual(values=c('black'))+
  scale_color_manual(values=c('black'))+
  ylab(bquote('Thermal sensation change/5%)'))+
  xlab("Other building characteristics")+
  scale_x_discrete(labels= my_labels_3)+
  scale_y_continuous(limits = c(0,0.5), breaks= seq(0,0.5,by=0.1)) +
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 16), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=15), legend.title = element_text(size=15))
dev.off()


jpeg("01_met.jpeg", units="cm", width=10, height=15, res=300)
ggplot(data_4, aes(x = Variable, y =EUI,
                         # size = `EUI range`,
                         color = `EUI change:`,
                         fill=`EUI change:`,
                         shape=`EUI change:`
))+
  geom_point(alpha = 0.7, size = 5)+
  scale_shape_manual(values = c(24))+
  scale_fill_manual(values=c('black'))+
  scale_color_manual(values=c('black'))+
  ylab(bquote('Thermal sensation change/0.5 met)'))+
  xlab("Other building characteristics")+
  scale_x_discrete(labels= my_labels_4)+
  scale_y_continuous(limits = c(0,0.5), breaks= seq(0,0.5,by=0.1)) +
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 16), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=15), legend.title = element_text(size=15))
dev.off()


jpeg("01_clo.jpeg", units="cm", width=10, height=15, res=300)
ggplot(data_5, aes(x = Variable, y =EUI,
                         # size = `EUI range`,
                         color = `EUI change:`,
                         fill=`EUI change:`,
                         shape=`EUI change:`
))+
  geom_point(alpha = 0.7, size = 5)+
  scale_shape_manual(values = c(25))+
  scale_fill_manual(values=c('black'))+
  scale_color_manual(values=c('black'))+
  ylab(bquote('Thermal sensation change/0.5 clo)'))+
  xlab("Other building characteristics")+
  scale_x_discrete(labels= my_labels_5)+
  scale_y_continuous(limits = c(0,0.5), breaks= seq(0,0.5,by=0.1)) +
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 16), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=15), legend.title = element_text(size=15))

dev.off()

