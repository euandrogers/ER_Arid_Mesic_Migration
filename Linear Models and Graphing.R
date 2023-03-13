---
title: "Linear models and graphing Final Data"
output: html_document:
df_print: paged
editor_options:
  chunk_output_type: inline

setwd("~/Desktop/Honours/Historic data")
getwd()

#packages
install.packages("rlang")
install.packages("dplyr", verbose=T)
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("broom")
library(broom)
install.packages("ggplot2")
library(ggplot2)
install.packages("gridExtra")
library("gridExtra")
install.packages("pkg")
library (pkg)
install.packages("lme4")
library(lme4)
install.packages("emmeans")
library(emmeans)

#import dataset
Arid_Species <- read.csv("Final_Arid.csv")
Mesic_Species <- read.csv("Final_Mesic.csv")

#quick look at dataset
summary(Arid_Species)

#Check max and min latitude and longitude for species
max(Arid_Species$decimalLatitude)
min(Arid_Species$decimalLatitude)
max(Arid_Species$decimalLongitude)
min(Arid_Species$decimalLongitude)

#turn latitude in absolute
Arid_Species <- Arid_Species %>% mutate(decimalLatitude=abs(decimalLatitude))
Mesic_Species <- Arid_Species %>% mutate(decimalLatitude=abs(decimalLatitude))

hist(Arid_Species$decimalLatitude)

#modelling lat per year for each species

library(lme4);library(nlme) 

fitsLat <- lmList(decimalLatitude ~ year | Species, data=Arid_Species)
fitsLat

summary(fitsLat)
names(summary(fitsLat))

#Generating model for latitude
species_lat_lm <- as.data.frame(summary(fitsLat)$coefficients) %>% 
  rename(p.year="Pr(>|t|).year",
         sd.year=`Std. Error.year`) %>% #just for easier names
  tibble::rownames_to_column("spp") %>% 
  select(spp, Estimate.year, #we care only about slope
         sd.year,
         p.year)

#Aluta maisonneuvei

Arid_Species %>% filter(Species=="Aluta maisonneuvei") %>% 
  ggplot(aes(year, decimalLatitude))+
  geom_point()

#generating model for longitude
fitsLong <- lmList(decimalLongitude ~ year | Species, data=Arid_Species)
fitsLong

summary(fitsLong)

print(fitsLong)

Species_long_lm <- as.data.frame(summary(fitsLong)$coefficients)%>% 
  rename(p.year="Pr(>|t|).year",
         sd.year=`Std. Error.year`) %>% 
  tibble::rownames_to_column("spp") %>% 
  select(spp, Estimate.year,
         sd.year,
         p.year)

####
spp = unique(Arid_Species$Species)


#Plotting and exporting latitude graph
spp_lat_plots = list()

for(spp_lat_ in spp) {
  spp_lat_plots[[spp_lat_]] = ggplot(Arid_Species %>%
    filter( Species == spp_lat_), 
    aes(year, decimalLatitude)) + 
    geom_point() + 
    ggtitle(spp_lat_) + 
    ylab("absolute lat") + 
    xlab("year") + 
    print(spp_lat_plots[[spp_lat_]])
  ggsave(spp_lat_plots[[spp_lat_]], 
         file=paste0("plot_lat_", spp_lat_,".png"), 
         width = 20, height = 15, units = "cm", dpi=300)
}

#Plotting and exporting longitude graph
spp_long_plots = list()

for(spp_long_ in spp) {
  spp_long_plots[[spp_long_]] =  ggplot(Arid_Species %>%
    filter(Species == spp_long_), 
    aes(year, decimalLongitude)) + 
    geom_point() + 
    ggtitle(spp_long_) + 
    ylab("absolute long") + 
    xlab("year") + 
    print(spp_long_plots[[spp_long_]])
  ggsave(spp_long_plots[[spp_long_]], 
         file=paste0("plot_long_", spp_long_,".png"), 
         width = 20, height = 15, units = "cm", dpi=300)
}

#Combine plots into a grid for illustration in appendix

directional_lat <- grid.arrange(grobs = (spp_lat_plots), ncol = 2, top = "Directional Shifts in Latitude Across Time")
print(directional_lat)

directional_long <- grid.arrange(grobs =(spp_long_plots), ncol = 2, top = "Directional Shifts in Longitude Across Time")
print(directional_long)

#Create plot for year vs latitude for all arid species

Latitude_allsp_plot <- ggplot(data = Arid_Species, mapping = aes(x = year, y = decimalLatitude, group = Species))

Latitude_allsp_plot + geom_point(aes(color=Species)) + geom_smooth(method=lm, se=FALSE, aes(color=Species)) + theme(legend.position = "bottom")

#Create plot for year vs longitude for all arid species

Longitude_allsp_plot <- ggplot(data = Arid_Species, mapping = aes(x = year, y = decimalLongitude, group = Species))

Longitude_allsp_plot + geom_point(aes(color=Species)) + geom_smooth(method=lm, se=FALSE, aes(color=Species)) + theme(legend.position = "bottom")

#Create species list from Final Data

SpeciesList <- Final_Arid %>%
  group_by(Species) %>%
  summarise(count=n())

#Turning Species List into csv for future metaanalysis for sample size (count)

write.csv(SpeciesList, "Species list and count.csv")

#running linear regressions on ALL SPECIES for Latitude vs Year

Latlm <- Final_Arid %>%
  group_by(Species) %>%
  do(model=lm(decimalLatitude ~ year,.))

summary(Latlm)

emmeans(Latlm, pairwise ~ decimalLatitude)

Latlmcoefs <- as.data.frame(tidy(Latlm, model))

Latlm <- lm(decimalLatitude ~ year, data = Final_Arid)
summary(Latlm)

for(spplm in spp) {
  Lat_lm = Latlm ~ decima %>%
  filter( Species == spp_lat_), 
  aes(year, decimalLatitude)) + 
    geom_point() + 
    ggtitle(spp_lat_) + 
    ylab("absolute lat") + 
    xlab("year") + 
    print(spp_lat_plots[[spp_lat_]])

ggsave(spp_lat_plots[[spp_lat_]], 
         file=paste0("plot_lat_", spp_lat_,".png"), 
         width = 20, height = 15, units = "cm", dpi=300)