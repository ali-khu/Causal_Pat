library(ggplot2)
library(ggpmisc)
library(readxl)
library(plotly)
library(dplyr)


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




data$`EUI change`<-factor(data$`EUI change`, levels = c('Decrease','No change','Increase'))

# data$Group<-factor(data$Group, levels = c('Climate','Building','Occupant','Equipment', 'Policy'))
# data$`EUI range`<-factor(data$EUI)

View(data)

my_labels <- c('From\nBefore 1969\nto\n1980-1989','FromBefore 1969\nto 1990-1999', 'Before 1969\nto After 2010',
                '1970-1979\nto 1980-1989', '1970-1979\nto 1990-1999', '1970-1979\nto 2000-2009',
                '1970-1979\nto After 2010', '1980-1989\nto 1990-1999', '1980-1989\nto 2000-2009',
                '1980-1989\nto After 2010')


jpeg("06_compl_year.jpeg", units="cm", width=40, height=15, res=300)
ggplot(data, aes(x = Variable, y =EUI,
                      size = `EUI range`,
                      color = `EUI change`,
                 fill=`EUI change`,
                 shape=`EUI change`
                 ))+
  geom_point(alpha = 0.7)+
  scale_shape_manual(values = c(25, 24))+
  scale_fill_manual(values=c('blue','brown'))+
  scale_color_manual(values=c('blue', 'brown'))+
  ylab(bquote('Change in EUI (kWh/'~m^2~')'))+
  xlab("Completion year")+
  scale_x_discrete(labels= my_labels)+
  # scale_y_continuous(limits = c(0,14), breaks= seq(0,14,by=2)) +
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 14), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=12))
dev.off()
