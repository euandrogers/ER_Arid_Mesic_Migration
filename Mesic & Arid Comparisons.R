library(tidyverse)
library(dplyr)
library(rlang)
library(ggplot2)

#import mesic and arid data

Mesic <- read.csv("Mesic_Morphology.csv", stringsAsFactors=FALSE, fileEncoding="latin1")
Comparison <-read.csv("Environmental comparison of latitude shift data.csv", stringsAsFactors=FALSE, fileEncoding="latin1")
Arid <- read.csv("Arid_Morphology.csv", stringsAsFactors=FALSE, fileEncoding="latin1")

#Create scatterplot for range size vs lat shift of the mesic species

ggplot(Mesic, aes(x=Range_Size, y = lat_year_km)) + 
  geom_point(aes(color="Red")) + 
  scale_x_continuous( trans= 'log10') + 
  xlab(" Range Size (km2) [Log Scale]") + 
  ylab("Rate of Latitude Shift (km/year) [Log Scale]") +
  theme_classic()+theme(panel.grid = element_blank()) +
  ylim(-5.2, 10.5) +
  xlim(22, 2550000)

#Mesic stats
Mesic_Size_Lat_Shift <- lm(Range_Size ~ lat_year_km, data = Mesic)

summary(Mesic_Size_Lat_Shift)

#Create scatterplot for range size vs range shift of the arid species

Mesic <- mutate(Mesic, LogRangeSize = log10(Range_Size))
Arid <- mutate(Arid, LogRangeSize = log10(Range_Size))

ggplot(Arid, aes(x = Range_Size, y = lat_year_effect_km)) +
  geom_point(aes(color="Red")) + 
  scale_x_continuous( trans= 'log10') +
  xlab(" Range Size (km2) [Log Scale]") +
  ylab("Rate of Latitude Shift (km/year) [Log Scale]") +
  theme_classic() +
  theme(panel.grid = element_blank()) +
  ylim(-5.5, 11) +
  xlim(10, 2600000)

#Arid stats

Arid_Size_Lat_Shift <- lm(Range_Size ~ lat_year_effect_km,  data = Arid)

summary(Arid_Size_Lat_Shift)

mean(Arid$lat_year_effect_km)

mean(Mesic$lat_year_km)

#Create plot with combined graphs to go into main body)

Mesic<-Mesic %>% mutate(type="Mesic") 
Arid<-Arid %>% mutate(type="Arid")

Shiftc <- ggplot(data=Mesic, aes(Range_Size, lat_year_km))+
  geom_point(aes(col=type)) +
  geom_point(data = Arid, aes(Range_Size, lat_year_effect_km, col = type))+
  xlab("Mean Range Size (km2) [Log Scale]") +
  ylab("Rate of Latitude Shift (km/year)")+
  theme_classic()+
  scale_color_manual(values=c("#E69F00", "#0096FF"))+
  geom_segment(x=log10(22.185), xend=log10(410835.788), y=-0.06, yend=0.5, col = "blue", linetype="dashed") +
  geom_segment(x=log10(5243.41), xend=log10(2548541.88), y=-1, yend=2, col = "Red", linetype="dashed") +
  geom_segment(x=log10(22.185), xend=log10(2548541.88), y=-1, yend=1.5, col = "black") +
 scale_x_log10(labels = comma)

Shiftc

ggsave("Size_Shift_Comparison.pdf", plot = Shiftc, height = 6, width = 6, dpi = 300)


#Create boxplot for range size vs shift comparison between mesic and arid
  
Boxplot_Shiftc <- ggplot(data = Comparison, aes(x = Environment, y = Lat_Shift, fill = Environment)) +
  geom_boxplot(aes(fill = Environment), show.legend = FALSE) +
  labs(x="Environment", y="Mean Rate of Latitude Shift (km/year) [Log Scale]") +
  theme_bw() + theme_classic()+theme(panel.grid = element_blank()) +
  scale_fill_manual(values = c("Dark Orange", "Blue"))

Boxplot_Shiftc

ggsave("Boxplot_Shiftc", plot = Boxplot_Shiftc, height = 6, width = 6, dpi = 300)

#Test whether there is a significant difference between lat shift of mesic and arid

t.test(Arid$lat_year_effect_km, Mesic$lat_year_km, var.equal = TRUE)
