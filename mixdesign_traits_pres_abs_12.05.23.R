library(Rmisc)
library(tidyverse)
library(viridis)
library(lemon)
library(lme4)
library(MASS)
library(car)
library(nlme)
library(dplyr)
library(DHARMa)
library(optimx)
library(visreg)
library(tidyverse)
library(performance)
library(lmerTest)
library(emmeans)
library(ggeffects)
library(glmmTMB)
library(ggplot2)
library(gplots)
library(writexl)

#prep tables again as there were issues with zeros not complete!
#table with floral area by species for all FFAR species
allyears_Flarea_bySpecies<-read.csv("~/Desktop/02.06.2021 Postdoc/Postdoc/Minnesota/Methods/Analysis/csv files/Plant and bee traits/allyears_Flarea_bySpecies.csv")

allyears_pos_Flarea_bySpecies<-filter(allyears_Flarea_bySpecies, species_flarea>0)
write.csv(allyears_pos_Flarea_bySpecies, "~/Desktop/02.06.2021 Postdoc/Postdoc/Minnesota/Methods/Analysis/csv files/Plant and bee traits/allyears_pos_Flarea_bySpecies.csv")
library("writexl")
write_xlsx(allyears_pos_Flarea_bySpecies, "~/Desktop/02.06.2021 Postdoc/Postdoc/Minnesota/Methods/Analysis/csv files/Plant and bee traits/allyears_pos_Flarea_bySpecies.xlsx")
allyears_pos_Flarea_bySpecies <- read_excel("~/Desktop/02.06.2021 Postdoc/Postdoc/Minnesota/Methods/Analysis/csv files/Plant and bee traits/allyears_pos_Flarea_bySpecies.xlsx")

Flarea<-allyears_pos_Flarea_bySpecies
Flarea<- Flarea%>% 
  mutate(block_plot = paste(block, plot_letter, plot_number, sep= ","))
Flarea<- Flarea%>% 
  mutate(block_plot_round = paste(block_plot, round, sep= ","))
Flarea<- Flarea%>% 
  mutate(block_plot_round_year = paste(block_plot_round, year, sep= ","))
Flarea<- Flarea%>% 
  mutate(species_block_plot_round_year = paste(species_long, block_plot_round_year,  sep= ","))

Flarea_species_pos_allyears<-Flarea
write.csv(Flarea_species_pos_allyears, "~/Desktop/02.06.2021 Postdoc/Postdoc/Minnesota/Methods/Analysis/csv files/Plant and bee traits/Flarea_species_pos_allyears.csv")


#now add the all species sown 
mix_comp_treat_SR_Y <- read_excel("~/Desktop/02.06.2021 Postdoc/Postdoc/Minnesota/Methods/Analysis/csv files/Veg survey/mix_comp_treat_SR_Y.xlsx")
mixes<-mix_comp_treat_SR_Y
#make some combo variable:
mixes<- mixes%>% 
  mutate(block_plot = paste(Block, plot_letter, plot_number, sep= ","))
mixes<- mixes%>% 
  mutate(block_plot_round = paste(block_plot, round, sep= ","))
mixes<- mixes%>% 
  mutate(block_plot_round_year = paste(block_plot_round, year, sep= ","))
mixes<- mixes%>% 
  mutate(species_block_plot_round_year = paste(species_long, block_plot_round_year,  sep= ","))

mixes_comp_treat_SR_Y_allCombos<-mixes
write.csv(mixes_comp_treat_SR_Y_allCombos, "~/Desktop/02.06.2021 Postdoc/Postdoc/Minnesota/Methods/Analysis/csv files/Plant and bee traits/mixes_comp_treat_SR_Y_allCombos.csv")
#add all the combos 

Flarea_species_pos_allyears<-read.csv( "~/Desktop/02.06.2021 Postdoc/Postdoc/Minnesota/Methods/Analysis/csv files/Plant and bee traits/Flarea_species_pos_allyears.csv")
mixes_comp_treat_SR_Y_allCombos<-read.csv("~/Desktop/02.06.2021 Postdoc/Postdoc/Minnesota/Methods/Analysis/csv files/Plant and bee traits/mixes_comp_treat_SR_Y_allCombos.csv")
#filter out monoplots:
mixes<-mixes_comp_treat_SR_Y_allCombos
#filter out the monoplots:
mixes<-filter(mixes, forb_richness !="1")

Flarea<-Flarea_species_pos_allyears
Flarea1<-dplyr::select(Flarea, species_block_plot_round_year, species_flarea)

Flarea_treat<-dplyr::full_join(Flarea1, mixes, by = "species_block_plot_round_year")
Flarea_treat<-filter(Flarea_treat, species_long !="NA")
#filter out those not sown in the plots (in floral area table but not matching to a sown mix in the plot)
Flarea_treat<-filter(Flarea_treat, species_long !="NA")
write.csv(Flarea_treat, "~/Desktop/02.06.2021 Postdoc/Postdoc/Minnesota/Methods/Analysis/csv files/Plant and bee traits/Flarea_treat.csv")
Flarea_treat<-read.csv( "~/Desktop/02.06.2021 Postdoc/Postdoc/Minnesota/Methods/Analysis/csv files/Plant and bee traits/Flarea_treat.csv")


#now add the traits:
write.csv(Maggie_Paiges_traits, "~/Desktop/02.06.2021 Postdoc/Postdoc/Minnesota/Methods/Analysis/csv files/Plant and bee traits/Maggie_Paiges_traits.csv")

Maggie_Paiges_traits<- read.csv("~/Desktop/02.06.2021 Postdoc/Postdoc/Minnesota/Methods/Analysis/csv files/Plant and bee traits/Maggie_Paiges_traits.csv")
traits <- subset (Maggie_Paiges_traits, select = -1)
Flarea_treat_traits<-dplyr::full_join(Flarea_treat, traits, by = "species_long") 

#that's the table to use for the models
#make NAs zero
Flarea_treat_traits$species_flarea[is.na(Flarea_treat_traits$species_flarea)] <- "0"
write.csv(Flarea_treat_traits, "~/Desktop/02.06.2021 Postdoc/Postdoc/Minnesota/Methods/Analysis/csv files/Plant and bee traits/Flarea_treat_traits.csv")


####start here with models
#models:
Flarea_treat_traits<-read.csv("~/Desktop/02.06.2021 Postdoc/Postdoc/Minnesota/Methods/Analysis/csv files/Plant and bee traits/Flarea_treat_traits.csv")

#make factors:
Flarea_treat_traits$forb_grass<-as.factor(Flarea_treat_traits$forb_grass)
Flarea_treat_traits$forb_richness<-as.factor(Flarea_treat_traits$forb_richness)
Flarea_treat_traits$density<-as.factor(Flarea_treat_traits$density)
Flarea_treat_traits$year<-as.factor(Flarea_treat_traits$year)

Flarea_treat_traits<-rename(Flarea_treat_traits, mix = plant_rich)
Flarea_treat_traits$mix<-as.factor(Flarea_treat_traits$mix)
Flarea_treat_traits$block_plot<-as.factor(Flarea_treat_traits$block_plot)
Flarea_treat_traits$round<-as.factor(Flarea_treat_traits$round)

#Hurdle model:
#do presence-absence first
#dublicate column:
Flarea_treat_traits$pres_abs = Flarea_treat_traits$species_flarea

Flarea_treat_traits$pres_abs<- ifelse(Flarea_treat_traits$pres_abs > "0", "1", "0")
Flarea_treat_traits$pres_abs<-as.factor(Flarea_treat_traits$pres_abs)
table(Flarea_treat_traits$pres_abs)

##nbiomial model
#first with height
#model with all seed mix design parameters:
H1<-glmmTMB(pres_abs ~
              height_m+
              forb_grass+
              density+
              forb_richness+
              year+
              height_m*forb_grass+
              height_m*density+
              height_m*forb_richness+
              height_m*year+
              (1|mix)+
              (1|block_plot),
            family="binomial",
            data=Flarea_treat_traits)
#convergence problem

#remove density:
H1a<-glmmTMB(pres_abs ~
              height_m+
              forb_grass+
              forb_richness+
              year+
              height_m*forb_grass+
              height_m*forb_richness+
              height_m*year+
              (1|mix)+
              (1|block_plot),
            family="binomial",
            data=Flarea_treat_traits)

simulationOutput <- simulateResiduals(fittedModel = H1a, plot = F)
plot(simulationOutput)

car::Anova(H1a, type = "III")
#

#####
#diaspore mass
D1<-glmmTMB(pres_abs ~
               diaspore_mass_mg+
               forb_grass+
               forb_richness+
               year+
              diaspore_mass_mg*forb_grass+
              diaspore_mass_mg*forb_richness+
              diaspore_mass_mg*year+
               (1|mix)+
               (1|block_plot),
             family="binomial",
             data=Flarea_treat_traits)

simulationOutput <- simulateResiduals(fittedModel = D1, plot = F)
plot(simulationOutput)

car::Anova(D1, type = "III")
#


#leaf area:not data for all plants yet so use selection
##nbiomial model
Flarea_treat_traits_LA<-filter(Flarea_treat_traits,Leaf_area_cm2 !="NA")


L1a<-glmmTMB(pres_abs ~
              Leaf_area_cm2+
              forb_grass+
              forb_richness+
              year+
              Leaf_area_cm2*forb_grass+
              Leaf_area_cm2*forb_richness+
              Leaf_area_cm2*year+
              (1|mix)+
              (1|block_plot),
            family="binomial",
            data=Flarea_treat_traits_LA)

simulationOutput <- simulateResiduals(fittedModel = L1a, plot = F)
plot(simulationOutput)

car::Anova(L1a, type = "III")



