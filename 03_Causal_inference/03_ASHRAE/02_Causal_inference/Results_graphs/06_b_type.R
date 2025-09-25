library(ggplot2)
library(ggpmisc)
library(readxl)
library(plotly)
library(dplyr)
library(ggpubr)


setwd("C:/Users/HUBEL/GDrive/08_THESIS/Causal_infer/03. Therm_comf/03_Analysis/03_causal_inference/Results/01_with_confounders")
data <- read_excel('With_confounders.xlsx', sheet = "b_type")


my_vector <- seq(from = 0, to = 500, by = 0.5)

data$EUI = abs(data$EUI)


bins = cut(data$EUI, breaks=,my_vector,include.lowest = TRUE, right = FALSE,  ordered_result = TRUE)

data$`EUI range` = bins




data$Variable<-factor(data$Variable, levels = c('From 1 to 2', 'From 1 to 3', 'From 1 to 5',
                                                'From 2 to 3', 'From 2 to 4', 'From 2 to 5'))




data$`EUI change:`<-factor(data$`EUI change:`, levels = c('Decrease','No change','Increase'))



View(data)





my_labels <- c('From 1\nto 2', 'From 1\nto 3', 'From 1\nto 5',
               'From 2\nto 3', 'From 2\nto 4', 'From 2\nto 5')




jpeg("06_b_type.jpeg", units="cm", width=15, height=15, res=300)
ggplot(data, aes(x = Variable, y =EUI,
                      # size = `EUI range`,
                      color = `EUI change:`,
                 fill=`EUI change:`,
                 shape=`EUI change:`
                 ))+
  geom_point(alpha = 0.7, size = 5)+
  scale_shape_manual(values = c(21, 24))+
  scale_fill_manual(values=c('black','black'))+
  scale_color_manual(values=c('black','black'))+
  ylab(bquote('Thermal sensation change'))+
  xlab("\nCooling type")+
  scale_x_discrete(labels= my_labels)+
  # scale_y_continuous(limits = c(0,0.4), breaks= seq(0,0.4,by=0.1)) +
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 16), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=15), legend.title = element_blank(), legend.position = "bottom")

dev.off()

