library(ggplot2)
library(ggpmisc)
library(readxl)
library(plotly)
library(dplyr)
library(ggpubr)


setwd("C:/Users/HUBEL/GDrive/08_THESIS/Causal_infer/02_Kor_resi/05_Causal_infer/Results/01_with_confounders")
data <- read_excel('With_confounders.xlsx', sheet = "Compl_year")



my_vector <- seq(from = 0, to = 500, by = 0.5)

data$EUI = abs(data$EUI)


bins = cut(data$EUI, breaks=,my_vector,include.lowest = TRUE, right = FALSE,  ordered_result = TRUE)

data$`EUI range` = bins




data$Variable<-factor(data$Variable, levels = c('Before 1969 to 1980-1989','Before 1969 to 1990-1999', 'Before 1969 to After 2010',
                                                '1970-1979 to 1980-1989', '1970-1979 to 1990-1999', '1970-1979 to 2000-2009',
                                                '1970-1979 to After 2010', '1980-1989 to 1990-1999', '1980-1989 to 2000-2009',
                                                '1980-1989 to After 2010'))




data$`EUI change:`<-factor(data$`EUI change:`, levels = c('Decrease','No change','Increase'))


data_1 <- data[1:3, c(1,2,3,4)]

data_2 <- data[4:7, c(1,2,3,4)]

data_3 <- data[8:10, c(1,2,3,4)]

View(data_3)


my_labels_1 <- c('From\nBefore 1969\nto 1980-1989','From\nBefore 1969\nto 1990-1999', 'From\nBefore 1969\nto After 2010')

my_labels_2 <- c('From\n1970-1979\nto 1980-1989', 'From\n1970-1979\nto 1990-1999', 'From\n1970-1979\nto 2000-2009',
                 'From\n1970-1979\nto After 2010')

my_labels_3 <- c('From\n1980-1989\nto 1990-1999', 'From\n1980-1989\nto 2000-2009',
                 'From\n1980-1989\nto After 2010')


jpeg("06_compl_year_2.jpeg", units="cm", width=40, height=30, res=300)
p1 <- ggplot(data_1, aes(x = Variable, y =EUI,
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
  xlab("\nCompletion year")+
  scale_x_discrete(labels= my_labels_1)+
  scale_y_continuous(limits = c(10,80), breaks= seq(10,80,by=10)) +
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 16), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=15), legend.title = element_text(size=15), legend.position = "bottom")
  

  p2 <- ggplot(data_2, aes(x = Variable, y =EUI,
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
    xlab("\nCompletion year")+
    scale_x_discrete(labels= my_labels_2)+
    scale_y_continuous(limits = c(10,80), breaks= seq(10,80,by=10)) +
    # coord_flip()+
    theme_bw()+
    theme(axis.text = element_text(colour = "black", size = 16), axis.title = element_text(size=16))+
    theme(legend.text=element_text(size=15) , legend.title = element_text(size=15), legend.position = "bottom")
  
  
  p3 <- ggplot(data_3, aes(x = Variable, y =EUI,
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
    xlab("\nCompletion year")+
    scale_x_discrete(labels= my_labels_3)+
    scale_y_continuous(limits = c(10,80), breaks= seq(10,80,by=10)) +
    # coord_flip()+
    theme_bw()+
    theme(axis.text = element_text(colour = "black", size = 16), axis.title = element_text(size=16))+
    theme(legend.text=element_text(size=15), legend.title = element_text(size=15), legend.position = "bottom")


  
  
  ggarrange(p1, p2, p3 , labels = c("A", "B", "C"),ncol = 2, nrow = 2)
  

dev.off()
