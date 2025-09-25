library(ggplot2)
library(ggpmisc)
library(readxl)
library(plotly)
library(dplyr)


setwd("C:/Users/HUBEL/GDrive/08_THESIS/Causal_infer/02_Kor_resi/05_Causal_infer/Results/01_with_confounders")
data <- read_excel('With_confounders.xlsx', sheet = "other_b_char")



my_vector <- seq(from = 0, to = 500, by = 0.5)

data$EUI = abs(data$EUI)


bins = cut(data$EUI, breaks=,my_vector,include.lowest = TRUE, right = FALSE,  ordered_result = TRUE)

data$`EUI range` = bins




data$Variable<-factor(data$Variable, levels = c('No. of outer walls', 'Residential floor area', 'No. of heated rooms',
                                                'No. of heated bathrooms', 'No. of external wall windows'))




data$`EUI change`<-factor(data$`EUI change`, levels = c('Decrease','No change','Increase'))

# data$Group<-factor(data$Group, levels = c('Climate','Building','Occupant','Equipment', 'Policy'))
# data$`EUI range`<-factor(data$EUI)

View(data)

my_labels <- c('No. of\nouter walls', 'Residential\nfloor area', 'No. of\nheated rooms',
               'No. of heated\nbathrooms', 'No. of external\nwall windows')


jpeg("05_other_b_char.jpeg", units="cm", width=25, height=15, res=300)
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
  xlab("Other building characteristics")+
  scale_x_discrete(labels= my_labels)+
  scale_y_continuous(limits = c(0,14), breaks= seq(0,14,by=2)) +
  # coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 14), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=12))
dev.off()
