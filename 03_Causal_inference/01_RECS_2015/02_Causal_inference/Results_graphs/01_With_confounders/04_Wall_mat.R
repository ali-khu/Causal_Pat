library(ggplot2)
library(ggpmisc)
library(readxl)
library(plotly)
library(dplyr)


setwd("C:/Users/HUBEL/GDrive/08_THESIS/Causal_infer/01. RECS/Modelling/02_DAG/02.Without_outliers/04_Analysis_v2/02_Causal_inference/01_All_variables/results_v2/01_With_confounders")
data <- read_excel('With_confounders.xlsx', sheet = "Wall_mat")



my_vector <- seq(from = 0, to = 500, by = 0.5)

data$EUI = abs(data$EUI)


bins = cut(data$EUI, breaks=,my_vector,include.lowest = TRUE, right = FALSE,  ordered_result = TRUE)

data$`EUI range` = bins

data$Variable<-factor(data$Variable, levels = c('1 to 2', '1 to 3', '1 to 4', '1 to 5', '1 to 6', '1 to 7', '1 to 8', '1 to 9',
                                                '2 to 3', '2 to 4', '2 to 5', '2 to 6', '2 to 7', '2 to 8', '2 to 9',
                                                '3 to 4', '3 to 5', '3 to 6', '3 to 7', '3 to 8', '3 to 9',
                                                '4 to 5', '4 to 6', '4 to 7', '4 to 8', '4 to 9',
                                                '5 to 6', '5 to 7', '5 to 8', '5 to 9',
                                                '6 to 7', '6 to 8', '7 to 8', '8 to 9'))




data$`EUI change:`<-factor(data$`EUI change:`, levels = c('Decrease','No change','Increase'))


View(data)


my_labels <- c('From  1\nto 2', 'From 1\nto 3', 'From 1\nto 7', 'From 2\nto 3', 'From 2\nto 7', 
               'From 3\nto 4', 'From 3\nto 5', 'From 3\nto 6', 'From 3\nto 7',
               'From 4\nto 5','From 4\nto 7')


jpeg("04_Major_wall.jpeg", units="cm", width=25, height=15, res=300)
ggplot(data, aes(x = Variable, y =EUI,
                      # size = `EUI range`,
                      color = `EUI change:`,
                 fill=`EUI change:`,
                 shape=`EUI change:`
                 ))+
  geom_point(alpha = 0.7, size = 5)+
  scale_shape_manual(values = c(25, 24))+
  scale_fill_manual(values=c('blue','brown'))+
  scale_color_manual(values=c('blue','brown'))+
  ylab(bquote('Average change in EUI (kWh/'~m^2~')'))+
  xlab("\nMajor wall material")+
  scale_x_discrete(labels= my_labels)+
  scale_y_continuous(limits = c(0,18), breaks= seq(0,18,by=3)) +
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 15), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=14), legend.title = element_text(size=15), legend.position = "bottom")
dev.off()
