library(ggplot2)
library(ggpmisc)
library(readxl)
library(plotly)
library(dplyr)


setwd("C:/Users/HUBEL/GDrive/08_THESIS/Causal_infer/02_Kor_resi/05_Causal_infer/Results/01_with_confounders")
data <- read_excel('With_confounders.xlsx', sheet = "income_source")



my_vector <- seq(from = 0, to = 500, by = 0.5)

data$EUI = abs(data$EUI)


bins = cut(data$EUI, breaks=,my_vector,include.lowest = TRUE, right = FALSE,  ordered_result = TRUE)

data$`EUI range` = bins




data$Variable<-factor(data$Variable, levels = c('1 to 2', '1 to 3', '1 to 4', '1 to 5', '1 to 6', '1 to 7', '1 to 8',
                                                '2 to 3', '2 to 4', '2 to 5', '2 to 6', '2 to 7', '2 to 8',
                                                '3 to 4', '3 to 5', '3 to 6', '3 to 7', '3 to 8',
                                                '4 to 5', '4 to 6', '4 to 7', '4 to 8',
                                                '5 to 6', '5 to 7', '5 to 8',
                                                '6 to 7', '6 to 8', '7 to 8'))




data$`EUI change:`<-factor(data$`EUI change:`, levels = c('Decrease','No change','Increase'))


View(data)


my_labels <- c('From 1 to 6', 'From 2 to 6', 'From 3 to 6', 'From 5 to 6')


jpeg("11_income_source.jpeg", units="cm", width=25, height=15, res=300)
ggplot(data, aes(x = Variable, y =EUI,
                      # size = `EUI range:`,
                      color = `EUI change:`,
                 fill=`EUI change:`,
                 shape=`EUI change:`
                 ))+
  geom_point(alpha = 0.7, size=5)+
  scale_shape_manual(values = c(24))+
  scale_fill_manual(values=c('brown'))+
  scale_color_manual(values=c('brown'))+
  ylab(bquote('Average change in EUI (kWh/'~m^2~')'))+
  xlab("\nMain income source")+
  scale_x_discrete(labels= my_labels)+
  # scale_y_continuous(limits = c(0,350), breaks= seq(0,350,by=50)) +
  # scale_x_discrete(guide = guide_axis(n.dodge=2))+
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 14), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=14), legend.title = element_text(size=15),  legend.position = "bottom")
dev.off()
