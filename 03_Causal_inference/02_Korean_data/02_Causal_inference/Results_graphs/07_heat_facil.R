library(ggplot2)
library(ggpmisc)
library(readxl)
library(plotly)
library(dplyr)


setwd("C:/Users/HUBEL/GDrive/08_THESIS/Causal_infer/02_Kor_resi/05_Causal_infer/Results/01_with_confounders")
data <- read_excel('With_confounders.xlsx', sheet = "heat_facil")



my_vector <- seq(from = 0, to = 500, by = 0.5)

data$EUI = abs(data$EUI)


bins = cut(data$EUI, breaks=,my_vector,include.lowest = TRUE, right = FALSE,  ordered_result = TRUE)

data$`EUI range` = bins




data$Variable<-factor(data$Variable, levels = c('1 to 3', '1 to 5', '1 to 8',
                                                '3 to 5',  '3 to 8',
                                                '5 to 8'))




data$`EUI change:`<-factor(data$`EUI change:`, levels = c('Decrease','No change','Increase'))

View(data)


my_labels <- c('From 1\nto 3', 'From 1\nto 5', 'From 1\nto 8',
                'From 3\nto 5',  'From 3\nto 8', 'From 5\nto 8')

jpeg("07_heat_facil.jpeg", units="cm", width=20, height=15, res=300)
ggplot(data, aes(x = Variable, y =EUI,
                      # size = `EUI range`,
                      color = `EUI change:`,
                 fill=`EUI change:`,
                 shape=`EUI change:`
                 ))+
  geom_point(alpha = 0.7, size=5)+
  scale_shape_manual(values = c(25, 21, 24))+
  scale_fill_manual(values=c('blue','green','brown'))+
  scale_color_manual(values=c('blue','green','brown'))+
  ylab(bquote('\nAverage change in EUI (kWh/'~m^2~')'))+
  xlab("\nMain heating facility")+
  scale_x_discrete(labels= my_labels)+
  # scale_x_discrete(labels= my_labels)+
  scale_y_continuous(limits = c(0,8), breaks= seq(0,8,by=2)) +
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 14), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=14), legend.title = element_text(size=15), legend.position = "bottom")
dev.off()
