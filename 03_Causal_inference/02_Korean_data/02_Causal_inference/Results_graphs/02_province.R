library(ggplot2)
library(ggpmisc)
library(readxl)
library(plotly)
library(dplyr)


setwd("C:/Users/HUBEL/GDrive/08_THESIS/Causal_infer/02_Kor_resi/05_Causal_infer/Results/01_with_confounders")
data <- read_excel('With_confounders.xlsx', sheet = "Province")



my_vector <- seq(from = 0, to = 500, by = 0.5)

data$EUI = abs(data$EUI)


bins = cut(data$EUI, breaks=,my_vector,include.lowest = TRUE, right = FALSE,  ordered_result = TRUE)

data$`EUI range` = bins




data$Variable<-factor(data$Variable, levels = c('Seoul to Gwangju', 'Seoul to Daejeon', 'Seoul to Gangwon', 'Seoul to Jeonbuk',
                                                'Seoul to Jeonnam','Seoul to Jeju'))




data$`EUI change:`<-factor(data$`EUI change:`, levels = c('Decrease','No change','Increase'))

# data$Group<-factor(data$Group, levels = c('Climate','Building','Occupant','Equipment', 'Policy'))
# data$`EUI range`<-factor(data$EUI)

View(data)


my_labels <- c('From Seoul to\nGwangju', 'From Seoul to\nDaejeon', 'From Seoul to\nGangwon', 'From Seoul to\nJeonbuk',
               'From Seoul to\nJeonnam','From Seoul to\nJeju')


jpeg("02_province.jpeg", units="cm", width=30, height=15, res=300)
ggplot(data, aes(x = Variable, y =EUI,
                      # size = `EUI range`,
                      color = `EUI change:`,
                 fill=`EUI change:`,
                 shape=`EUI change:`
                 ))+
  geom_point(alpha = 0.7, size = 5)+
  scale_shape_manual(values = c(25, 21, 24))+
  scale_fill_manual(values=c('blue','green','brown'))+
  scale_color_manual(values=c('blue','green','brown'))+
  ylab(bquote('Average change in EUI (kWh/'~m^2~')'))+
  xlab("\nProvincial climates")+
  scale_x_discrete(labels= my_labels)+
  # scale_y_continuous(limits = c(0,20), breaks= seq(0,20,by=2)) +
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 15), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=14), legend.title = element_text(size=15), legend.position = "bottom")
dev.off()
