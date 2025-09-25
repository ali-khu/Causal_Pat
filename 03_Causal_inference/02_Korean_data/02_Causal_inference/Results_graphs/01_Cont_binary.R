library(ggplot2)
library(ggpmisc)
library(readxl)
library(plotly)
library(dplyr)


# setwd("/Users/heal/GDrive/08_THESIS/Causal_infer/02_Kor_resi/05_Causal_infer/Results/01_with_confounders")

setwd("C:/Users/HUBEL/GDrive/08_THESIS/Causal_infer/02_Kor_resi/05_Causal_infer/Results/01_with_confounders")
data <- read_excel('With_confounders.xlsx', sheet = "cont_bin")



data$EUI = abs(data$EUI)

bins = cut(data$EUI, breaks=c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8,
                              8.5, 9, 9.5, 10, 10.5, 11, 11.5, 12, 12.5, 13, 13.5, 14, 14.5, 15,
                              15.5, 16, 16.5, 17, 17.5, 18, 18.5, 19, 19.5, 20, 20.5, 21, 21.5,
                              22, 22.5, 23, 23.5, 24, 24.5, 25, 25.5, 26, 26.5, 27, 27.5, 28,
                              28.5, 29, 29.5, 30, 30.5, 31, 31.5, 32),
                                  include.lowest = TRUE, right = FALSE,  ordered_result = TRUE)
data$`EUI range` = bins




data$Variable<-factor(data$Variable, levels = c("Average monthly income", "Head of household education level",
                                                "Head of household age", "No. of household members",
                                                "Average monthly heating costs", "Gas boiler heating efficiency",
                                                "Gas boiler efficiency rating", "Kerosene boiler heating efficiency",
                                                "Auxilary heating used", "No. of external wall windows",
                                                "No. of heated bathrooms", "No. of heated rooms",
                                                "Residential floor area", "No. of outer walls","HDD18.5"
                                                ))




data$`EUI change`<-factor(data$`EUI change`, levels = c('Decrease','No change','Increase'))

data$Group<-factor(data$Group, levels = c('Climate','Building','Occupant','Equipment', 'Policy'))
# data$`EUI range`<-factor(data$EUI)



jpeg("01_Cont_binary.jpeg", units="cm", width=25, height=17, res=300)
ggplot(data, aes(x = Variable, y =EUI,
                      size = `EUI range`,
                      color = Group,
                 fill=Group,
                 shape=`EUI change`
                 ))+
  geom_point(alpha = 0.7)+
  scale_shape_manual(values = c(25, 21, 24))+
  scale_fill_manual(values=c('#14AAF5','violet', 'brown', 'green', 'orange'))+
  scale_color_manual(values=c('#14AAF5','violet', 'brown', 'green', 'orange'))+
  ylab(bquote('Energy Use Intensity (kWh/'~m^2~')'))+
  scale_y_continuous(limits = c(0,33), breaks= seq(0,33,by=3)) +
  coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(colour = "black", size = 14), axis.title = element_text(size=16))+
  theme(legend.text=element_text(size=12))
dev.off()
