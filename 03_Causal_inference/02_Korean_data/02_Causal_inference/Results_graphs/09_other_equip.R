library(ggplot2)
library(ggpmisc)
library(readxl)
library(plotly)
library(dplyr)


setwd("C:/Users/HUBEL/GDrive/08_THESIS/Causal_infer/02_Kor_resi/05_Causal_infer/Results/01_with_confounders")
data <- read_excel('With_confounders.xlsx', sheet = "other_equip")



my_vector <- seq(from = 0, to = 500, by = 0.5)

data$EUI = abs(data$EUI)


bins = cut(data$EUI, breaks=,my_vector,include.lowest = TRUE, right = FALSE,  ordered_result = TRUE)

data$`EUI range` = bins


data$Variable<-factor(data$Variable, levels = c('Kerosene boiler heating efficiency', 'Gas boiler heating efficiency'))

data$`EUI change:`<-factor(data$`EUI change:`, levels = c('Decrease','No change','Increase'))






my_labels_2 <- c('Kerosene boiler\nheating efficiency', 'Gas boiler\nheating efficiency')


View(data)



jpeg("09_other_equip_v2.jpeg", units="cm", width=15, height=15, res=300)
ggplot(data, aes(x = Variable, y =EUI,
                      # size = `EUI range`,
                      color = `EUI change:`,
                 fill=`EUI change:`,
                 shape=`EUI change:`
                 ))+
  geom_point(alpha = 0.7, size=5)+
  scale_shape_manual(values = c(25, 24))+
  scale_fill_manual(values=c('blue','brown'))+
  scale_color_manual(values=c('blue', 'brown'))+
  ylab(bquote('Change in EUI (kWh/'~m^2~')'))+
  xlab("\nEquipment efficiency")+
  scale_x_discrete(labels= my_labels_2)+
  scale_y_continuous(limits = c(0,35), breaks= seq(0,35,by=5)) +
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 14), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=14), legend.title = element_text(size=15), legend.position = "bottom")


dev.off()
