#Tanners Lake MSC Plots:
setwd("C:/Users/mwers/Dropbox/Matthew Wersebe- OU work/Projects/Tanners Lake Data")
#Lake Profiles:

library(tidyverse)
library(ggpubr)

Profile <- read_csv("Tanners_Depth_Profile_July2019.csv", col_names = T)

#temp
temp<- ggplot(Profile, aes(x=Profile$DEP,y=Profile$TEMP))+
  geom_line(color="black", size=1, linetype=2)+
  geom_point()+
  scale_x_reverse()+
  scale_y_continuous(position = "right")+
  coord_flip()+
  xlab("Depth (m)")+
  ylab("Temperature (°C)")+
  geom_vline(xintercept=2.93, linetype="dashed", color = "blue")+
  geom_vline(xintercept=8.9, linetype="dashed", color = "red")
temp

#SPC 
spc<- ggplot(Profile, aes(x=Profile$DEP,y=Profile$`SPC-uS/cm`))+
  geom_line(color="black", size=1, linetype=2)+
  geom_point()+
  scale_x_reverse()+
  scale_y_continuous(position = "right", limits = c(800, 2050))+
  coord_flip()+
  xlab("Depth (m)")+
  ylab("Specific Condutance (??S/cm)")+
  geom_vline(xintercept=2.93, linetype="dashed", color = "blue")+
  geom_vline(xintercept=8.9, linetype="dashed", color = "red")
spc

#DO 
DO<- ggplot(Profile, aes(x=Profile$DEP,y=Profile$`DO-%`))+
  geom_line(color="black", size=1, linetype=2)+
  geom_point()+
  scale_x_reverse()+
  scale_y_continuous(position = "right")+
  coord_flip()+
  xlab("Depth (m)")+
  ylab("Dissolved Oxygen (%)")+
  geom_vline(xintercept=2.93, linetype="dashed", color = "blue")+
  geom_vline(xintercept=8.9, linetype="dashed", color = "red")
DO

ggarrange(temp, DO, spc, ncol = 3, nrow = 1, labels=c("A)","B)","C)"))

# To DO: (1-20-21)
# Stack the three profiles together. done in PPT.
# Make a map of tanners with Inset to show location. 
# GAM outputs. Done in PPT.

#Core Characteristics Profile

Character <- read_csv("Tanners_Core_Characteristics.csv", col_names = T)

head(Character)

#%-Organic 
Organic  <- ggplot(Character, aes(x=Character$Depth,y=Character$`%-Organic`))+
  geom_line(color="black", size=1, linetype=1)+
  geom_point()+
  scale_x_reverse()+
  scale_y_continuous(position = "right")+
  coord_flip()
Organic

#%-CaCO3
CaCO3 <- ggplot(Character, aes(x=Character$Depth,y=Character$`%-CaCO3`))+
  geom_line(color="black", size=1, linetype=1)+
  geom_point()+
  scale_x_reverse()+
  scale_y_continuous(position = "right")+
  coord_flip()
CaCO3

#%-Inorg
Inorg <- ggplot(Character, aes(x=Character$Depth,y=Character$`%-Inorg`))+
  geom_line(color="black", size=1, linetype=1)+
  geom_point()+
  scale_x_reverse()+
  scale_y_continuous(position = "right")+
  coord_flip()
Inorg 

#Chloride
chl<-ggplot(Character, aes(x=Character$`Year(Appox)`,y=Character$Chloride_top))+
  geom_line(color="black", size=1, linetype=1)+
  geom_point()+
  scale_y_continuous(position = "left")+
  ylab("Chloride Concentration (mg/L)")+
  xlab("Year (Approximate)")
chl  

#Stacked Percent Barplot:

#%-Puli 
Puli <-ggplot(Character, aes(x=Character$`Year(Appox)`,y=Character$`%-Puli`))+
  geom_line(color="black", size=1, linetype=1)+
  geom_point()+
  scale_y_continuous(position = "right")+
  coord_flip()
Puli 

#%-Mend
Mend <-ggplot(Character, aes(x=Character$`Year(Appox)`,y=Character$`%-Mend`))+
  geom_line(color="black", size=1, linetype=1)+
  geom_point()+
  scale_y_continuous(position = "right")+
  coord_flip()
Mend

#%-Small
Small <-ggplot(Character, aes(x=Character$`Year(Appox)`,y=Character$`%-Small`))+
  geom_line(color="black", size=1, linetype=1)+
  geom_point()+
  scale_y_continuous(position = "right")+
  coord_flip()
Small 

#Stacked Barplot of Percent Abundance

Character %>% pivot_longer(cols = c(Pulicaria, Mendotae, Small), names_to = "Species", values_to = "Proportion") -> stacked_data

stacked_data$Species <- factor(stacked_data$Species, levels = c("Mendotae", "Small", "Pulicaria"))

str(stacked_data)
tail(stacked_data)

percent_stack <- ggplot(stacked_data, aes(fill=Species, y=Proportion, x=Depth)) + 
  geom_bar(position="fill", stat="identity")+
  coord_flip()+
  scale_x_reverse()+
  scale_fill_manual(values = c("grey40", "darkgrey", "black"))+
  xlab("Depth (cm)")+
  ylab("Proportion of recovered ephippia")
  

#Plots of stacked Raw Flux across Depths:

GAMData<- read_csv("Tanners_ephippia_gmm.csv", col_names = TRUE, na = c("", "na"))
head(GAMData)

GAMData %>% pivot_longer(cols = c(Pulicaria, Mendotae, Small), names_to = "Species", values_to = "Flux") -> stacked_flux_data

stacked_flux_data$Species <- factor(stacked_flux_data$Species, levels = c("Mendotae", "Small", "Pulicaria"))

stacked_flux_data[120:135,]

flux_stack <- ggplot(stacked_flux_data, aes(fill=Species, y=Flux, x=Strata)) + 
  geom_bar(position="stack", stat="identity")+
  coord_flip()+
  scale_x_reverse()+
  scale_fill_manual(values = c("grey40", "darkgrey", "black"))+
  xlab("Depth (cm)")+
  ylab("Flux (# ephippia/g sediment/yr)")

ggarrange(percent_stack, flux_stack, labels= c("A)", "B)"), ncol = 2, nrow = 1, common.legend = T)

#Size distro Violin Plot

Ephippia.size <- read_csv("Tanners_EphippiaSize_mw.csv", col_names = T, na = "NA")

Ephippia.size$Depth_top_of_interval <-as.factor(Ephippia.size$Depth_top_of_interval)

vio <- ggplot(Ephippia.size, aes(x=Depth_top_of_interval, y=Ephippia_dorsal_length))+ 
  geom_violin()+
  stat_summary(fun.y=mean, geom = "point", shape=23, size= 2)+
  coord_flip()+
  scale_x_discrete(limits = rev)+
  ylab("Ephippia Dorsal Length (mm)")+
  xlab("Top of Interval Depth (cm)")+
  stat_smooth(method = 'lm')
vio 

vio2 <- ggplot(Ephippia.size, aes(x=Depth_top_of_interval, y=Ephippia_dorsal_length))+
  geom_point()+
  ylab("Ephippia Dorsal Length (mm)")+
  xlab("Top of Interval Depth (cm)")+
  stat_smooth(method = 'lm')
vio2

library(ggpubr)
library(gridExtra)
library(cowplot)

ggarrange(vio, vio2, labels= c("A)", "B)"), ncol = 2, nrow = 1)



#Supplemental Figure S2:

core_character_2015 <-read_csv("Tanners_core_characteristics_2015.csv", col_names = T)

head(core_character_2015)

#%-Organic 
Organic_2015  <- ggplot(core_character_2015, aes(x=core_character_2015$Depth,y=core_character_2015$`%-Organic`))+
  geom_line(color="black", size=1, linetype=1)+
  geom_point()+
  scale_x_reverse()+
  scale_y_continuous(position = "right")+
  coord_flip()
Organic_2015

#%-CaCO3
CaCO3_2015 <- ggplot(core_character_2015, aes(x=core_character_2015$Depth,y=core_character_2015$`%-CaCO3`))+
  geom_line(color="black", size=1, linetype=1)+
  geom_point()+
  scale_x_reverse()+
  scale_y_continuous(position = "right")+
  coord_flip()
CaCO3_2015

#%-Inorg
Inorg_2015 <- ggplot(core_character_2015, aes(x=core_character_2015$Depth,y=core_character_2015$`%-Inorg`))+
  geom_line(color="black", size=1, linetype=1)+
  geom_point()+
  scale_x_reverse()+
  scale_y_continuous(position = "right")+
  coord_flip()
Inorg_2015

