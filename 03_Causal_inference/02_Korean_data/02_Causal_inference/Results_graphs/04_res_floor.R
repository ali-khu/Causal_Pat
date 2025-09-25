library(ggplot2)
library(ggpmisc)
library(readxl)
library(plotly)
library(dplyr)


setwd("C:/Users/HUBEL/GDrive/08_THESIS/Causal_infer/02_Kor_resi/05_Causal_infer/Results/01_with_confounders")
data <- read_excel('With_confounders.xlsx', sheet = "Res_floor")



my_vector <- seq(from = 0, to = 500, by = 0.5)

data$EUI = abs(data$EUI)


bins = cut(data$EUI, breaks=,my_vector,include.lowest = TRUE, right = FALSE,  ordered_result = TRUE)

data$`EUI range` = bins




data$Variable<-factor(data$Variable, levels = c('1st or lower to 2nd', '1st or lower to 3rd~5th', '1st or lower to 6th~10th',
                                                '1st or lower to 11th and above', '2nd to 3rd~5th', '2nd to 6th~10th',
                                                '2nd to 11th and above', '3rd~5th to 6th~10th', '3rd~5th to 11th and above', '6th~10th to 11th and above'))




data$`EUI change:`<-factor(data$`EUI change:`, levels = c('Decrease','No change','Increase'))



View(data)

my_labels <- c('From\n1st\nto 2nd', 'From\n1st\nto 3rd~5th', 'From\n1st\nto 6th~10th',
                'From\n1st\nto 11th +', 'From\n2nd to\n3rd~5th', 'From\n2nd to\n6th~10th',
                'From\n2nd to\n11th +', 'From\n3rd~5th\nto 6th~10th', 'From\n3rd~5th to\n11th +', 'From\n6th~10th to\n11th +')


jpeg("04_res_floor.jpeg", units="cm", width=30, height=15, res=300)
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
  xlab("\nResidential floor")+
  scale_x_discrete(labels= my_labels)+
  scale_y_continuous(limits = c(0,70), breaks= seq(0,70,by=10)) +
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 14), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=14), legend.title = element_text(size=15), legend.position = "bottom")
dev.off()
