library(ggplot2)
library(ggpmisc)
library(readxl)
library(plotly)
library(dplyr)


setwd("C:/Users/HUBEL/GDrive/08_THESIS/Causal_infer/02_Kor_resi/05_Causal_infer/Results/01_with_confounders")
data <- read_excel('With_confounders.xlsx', sheet = "Housing_type")



my_vector <- seq(from = 0, to = 500, by = 0.5)

data$EUI = abs(data$EUI)


bins = cut(data$EUI, breaks=,my_vector,include.lowest = TRUE, right = FALSE,  ordered_result = TRUE)

data$`EUI range` = bins




data$Variable<-factor(data$Variable, levels = c('General detached houses to Row houses', 'General detached houses to Apartments', 
                                                'General detached houses to Officetels', 'Multi-family houses to Apartments'))




data$`EUI change:`<-factor(data$`EUI change:`, levels = c('Decrease','No change','Increase'))


View(data)

my_labels <- c('From General detached \nhouse to Row houses', 'From General detached\nhouses to Apartments', 
               'From General detached\nhouses to Officetels', 'From Multi-household\nhousing to Apartments')


jpeg("03_Housing_type.jpeg", units="cm", width=30, height=15, res=300)
ggplot(data, aes(x = Variable, y =EUI,
                      # size = `EUI range`,
                      color = `EUI change:`,
                 fill=`EUI change:`,
                 shape=`EUI change:`
                 ))+
  geom_point(alpha = 0.7, size=5)+
  scale_shape_manual(values = c(25))+
  scale_fill_manual(values=c('blue'))+
  scale_color_manual(values=c('blue'))+
  ylab(bquote('Average change in EUI (kWh/'~m^2~')'))+
  xlab("\nHousing type")+
  scale_x_discrete(labels= my_labels)+
  # scale_y_continuous(limits = c(0,20), breaks= seq(0,20,by=2)) +
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 14), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=14), legend.title = element_text(size=15), legend.position = "bottom")
dev.off()
