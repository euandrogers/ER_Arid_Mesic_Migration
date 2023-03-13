#load relevant packages
library(tidyverse)
library(dplyr)
library(rlang)
library(ggplot2)
library(metafor)
library(emmeans)
library(writexl)

#import data

Arid_Morphology <- read.csv("Arid_Morphology.csv", stringsAsFactors=FALSE, fileEncoding="latin1")

Mesic_Morphology <- read.csv("Mesic_Morphology.csv", stringsAsFactors=FALSE, fileEncoding="latin1")

Dispersal_Combined <- read.csv("dispersal_traits_combined.csv", stringsAsFactors=FALSE, fileEncoding="latin1")

#Log translate then transform height, weight and range shift data (arid)

Arid_Morphology <- mutate(Arid_Morphology, LogHeight = log10(Maximum_Height_In_Meters))
                     
Arid_Morphology <- mutate(Arid_Morphology, LogWeight = log10(Mean_Seed_Weight_In_mg))

Arid_Morphology <- mutate(Arid_Morphology, LogRangeShift = log10(range_year_effect_km))

#Log translate then transform height, weight and range shift data (mesic)

Mesic_Morphology <- mutate(Mesic_Morphology, LogHeight = log10(Maximum_Height_In_Meters))

Mesic_Morphology <- mutate(Mesic_Morphology, LogWeight = log10(Mean_Seed_Weight_In_mg))

Mesic_Morphology <- mutate(Mesic_Morphology, LogRangeShift = log10(range_year_effect_km))

#Create scatterplot of height vs range shift (arid)

Arid_Height_Shift <- ggplot(Arid_Morphology, aes(x=Maximum_Height_In_Meters, y = range_year_effect_km)) + 
  geom_point(colour = "dark orange") + 
  scale_x_continuous(trans= 'log10', limits = c(0.1, 40)) + 
  scale_y_continuous(trans= 'log10') + 
  xlab("Maximum Height of Species (m) [Log Scale]") + 
  ylab("Mean Rate of Range Shift (km/year) [Log Scale]") +
  theme_classic() +
  geom_smooth(method=lm, col = "black", se = FALSE)+
  theme(panel.grid = element_blank()) #Height vs Lat shift

Arid_Height_Shift

ggsave("Arid_Height_Shift.pdf", plot = Arid_Height_Shift, height = 6, width = 6, dpi = 300)

#Create scatterplot of height vs range shift (mesic)

Mesic_Height_Shift <- ggplot(Mesic_Morphology, aes(x=Maximum_Height_In_Meters, y = range_year_effect_km)) + 
  geom_point(colour = "blue") + 
  scale_x_continuous(trans= 'log10', limits = c(0.1, 40)) + 
  scale_y_continuous(trans= 'log10') + 
  xlab("Maximum Height of Species (m) [Log Scale]") + 
  ylab("Mean Rate of Range Shift (km/year) [Log Scale]") +
  theme_classic() +
  theme(panel.grid = element_blank()) #Height vs Lat shift

Mesic_Height_Shift

ggsave("Mesic_Height_Shift.pdf", plot = Mesic_Height_Shift, height = 6, width = 6, dpi = 300)

#Create scatterplot of seed weight vs range (arid)

Arid_Weight_Shift <- ggplot(Arid_Morphology, aes(x=Mean_Seed_Weight_In_mg, y = range_year_effect_km)) + 
  geom_point(colour = "dark orange") + 
  scale_x_continuous(trans= 'log10', limits = c(0.02, 145)) + 
  scale_y_continuous(trans= 'log10')+ 
  xlab(" Mean Seed Weight (mg) [Log Scale]") + 
  ylab("Mean Rate of Range Shift (km/year) [Log Scale]") +
  theme_classic()+theme(panel.grid = element_blank()) #Weight vs Range shift

Arid_Weight_Shift

ggsave("Arid_Weight_Shift.pdf", plot = Arid_Weight_Shift, height = 6, width = 6, dpi = 300)

#Create scatterplot of seed weight vs range (mesic)

Mesic_Weight_Shift <- ggplot(Mesic_Morphology, aes(x=Mean_Seed_Weight_In_mg, y = range_year_effect_km)) + 
  geom_point(colour = "Blue") + 
  scale_x_continuous(trans= 'log10', limits = c(0.02, 145)) + 
  scale_y_continuous(trans= 'log10')+ 
  xlab(" Mean Seed Weight (mg) [Log Scale]") + 
  ylab("Mean Rate of Range Shift (km/year) [Log Scale]") +
  theme_classic()+theme(panel.grid = element_blank()) #Weight vs Range shift

Mesic_Weight_Shift

ggsave("Mesic_Weight_Shift.pdf", plot = Mesic_Weight_Shift, height = 6, width = 6, dpi = 300)

#Determine significance and p-values for height vs shift data

Height_Range_Arid <- lm(LogHeight ~ LogRangeShift, data = Arid_Morphology)

summary(Height_Range_Arid)

Height_Range_Mesic <- lm(LogHeight ~ LogRangeShift, data = Mesic_Morphology)

summary(Height_Range_Mesic)

#Determine significance and p-values for weight vs shift data

Weight_Range_Arid <- lm(LogWeight ~ LogRangeShift, data = Arid_Morphology)

summary(Weight_Range_Arid)

Weight_Range_Mesic <- lm(LogWeight ~ LogRangeShift, data = Mesic_Morphology)

summary(Weight_Range_Mesic)

#Plot of dispersal syndrome vs range shift

Arid_syndrome <- ggplot(Arid_Morphology, aes(x = Dispersal_syndrome, y = RangeShift_Km_Per_Year)) + 
  scale_y_continuous(trans= 'log10') + 
  geom_boxplot(fill = "lightblue", colour = "darkblue") + 
  xlab ("Disperal Appendage") + 
  ylab( "Mean Rate of Range Shift (km/year) [Log Scale]") + 
  theme_classic() + 
  theme(panel.grid = element_blank())

Arid Syndrome

#Combined relationship between dispersal appendages and range shift

dt <- aov(Range_Shift ~ Appendage, data = Dispersal_Combined)

summary(dt)

TukeyHSD(dt, conf.level=.95)

#Violin plot : dispersal appendage vs range shift (arid)

violin_trait_arid <- Arid_Morphology %>%
  ggplot(aes(x = RangeShift_Km_Per_Year, y = Dispersal_appendage)) +
  scale_x_continuous(trans= 'log10') + 
  geom_violin(aes(fill = Dispersal_appendage), show.legend = FALSE) +
  labs(x="Mean Rate of Range Shift (km/year) [Log Scale]", y="Appendage Type") +
  theme_bw() + 
  stat_summary(fun = "mean",
               geom = "point",
               color = "Black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Oranges")

violin_trait_arid

ggsave("violin_trait_arid.pdf", plot = violin_trait_arid, height = 6, width = 6, dpi = 300)

#Violin plot : dispersal appendage vs range shift (mesic)

violin_trait_mesic <- Mesic_Morphology %>%
  ggplot(aes(x = range_year_effect_km, y = Dispersal_appendage)) +
  scale_x_continuous(trans= 'log10') + 
  geom_violin(aes(fill = Dispersal_appendage), show.legend = FALSE) +
  labs(x="Mean Rate of Range Shift (km/year) [Log Scale]", y="Appendage Type") +
  theme_bw() +
  stat_summary(fun = "mean",
               geom = "point",
               color = "Black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette = "Blues")

ggsave("violin_trait_mesic.pdf", plot = violin_trait_mesic, height = 6, width = 6, dpi = 300)

#Peform One-Way ANOVA for arid species

Arid_Morphology$Dispersal_appendage<- as.factor(Arid_Morphology$Dispersal_appendage)

# create plot with 2 boxplots for each (sig vs non sig shift)
ggplot(data=Arid_Morphology, aes(Dispersal_appendage, range_year_effect_km))+geom_boxplot()

Appendage_anov_arid <- aov(range_year_effect_km ~ Dispersal_appendage, data = Arid_Morphology)
summary(Appendage_anov_arid)

t.test(Arid_Morphology$range_year_effect_km, Arid_Morphology$Dispersal_appendage, paired = TRUE)

Appendage_anov_mesic <- aov(range_year_effect_km ~ Dispersal_appendage, data = Mesic_Morphology)
summary(Appendage_anov_mesic)


Appendage_anov_arid1 <- lm(Range_Shift ~ Appendage, data = dispersal_traits_combined)

summary(Appendage_anov_arid1)

TukeyHSD(Appendage_anov_arid, conf.level=.95)

TukeyHSD(Appendage_anov_arid1, conf.level=.95)

#Peform One-Way ANOVA for mesic species

Appendage_anov_mesic <- aov(range_year_effect_km ~ Dispersal_appendage, data = Mesic_Morphology)
summary(Appendage_anov_mesic)

pairwise.t.test(Mesic_Morphology$range_year_effect_km, Mesic_Morphology$Dispersal_appendage)

TukeyHSD(Appendage_anov_mesic, conf.level=.95)

#Extract dispersal syndrome, appendage, plant height and seed mass from AusTraits

remotes::install_github("traitecoevo/austraits", dependencies = TRUE, upgrade = "ask")

library(austraits) 

austraits <- load_austraits(version = "3.0.2", path = "data/austraits")

  #Extract dispersal syndrome for species

dispersal_syndrome <- extract_trait(austraits, "dispersal_syndrome")

write_xlsx(dispersal_syndrome_species, "/Users/Euan/Desktop/Honours/Historic data/dispersal_syndrome_species.csv")

  #Extract dispersal appendage for each species

dispersal_appendage <- extract_trait(austraits, "dispersal_appendage")

dispersal_appendage_species <- dispersal_appendage$traits

write_xlsx(dispersal_appendage_species, "/Users/Euan/Desktop/Honours/Historic data/dispersal_appendage_species.csv")

  #Extract plant height for species

plant_height <- extract_trait(austraits, "plant_height")

plant_height_species <- plant_height$traits

write_xlsx(plant_height_species, "/Users/Euan/Desktop/Honours/Historic data/plant_height_species.csv")

  #Extract seed weight for species

seed_mass <- extract_trait(austraits, "seed_mass")

seed_mass_species <- seed_mass$traits

write_xlsx(seed_mass_species, "/Users/Euan/Desktop/Honours/Historic data/seed_mass_species.csv")