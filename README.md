# species-in-seed-mixes
Species establishment in seed mixes FFAR project


library(ggplot2)
library(dplyr)

#Pie charts proportions flowering species per year
#dataframe with y=proportions flowering, group=flower_nonflower to be distinguished
#use csv file "prop_years"

#
prop_years$prop_est <- as.numeric(as.integer(prop_years$prop_est ))
prop_years$year <- as.factor(as.integer(prop_years$year ))

#partition by year
prop_2019 <- filter(prop_years, year == "2019")
#2019
ggplot(prop_2019, aes(x = "", y = prop_est, fill = flower_nonflower)) +
  geom_bar(width=1, stat="identity")+
  coord_polar("y", start=0)+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"))+
  theme_void()+
    guides(fill = guide_legend(title = "Proportions flowering-non-flowering species 2019"))
##

#2020
prop_2020 <- filter(prop_years, year == "2020")

ggplot(prop_2020, aes(x = "", y = prop_est, fill = flower_nonflower)) +
  geom_bar(width=1, stat="identity")+
  coord_polar("y", start=0)+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"))+
  theme_void()+
  guides(fill = guide_legend(title = "Proportions flowering-non-flowering species 2020"))

#2021
prop_2021 <- filter(prop_years, year == "2021")

ggplot(prop_2021, aes(x = "", y = prop_est, fill = flower_nonflower)) +
  geom_bar(width=1, stat="identity")+
  coord_polar("y", start=0)+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"))+
  theme_void()+
  guides(fill = guide_legend(title = "Proportions flowering-non-flowering species 2021"))

#2022
prop_2022 <- filter(prop_years, year == "2022")

ggplot(prop_2021, aes(x = "", y = prop_est, fill = flower_nonflower)) +
  geom_bar(width=1, stat="identity")+
  coord_polar("y", start=0)+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"))+
  theme_void()+
  guides(fill = guide_legend(title = "Proportions flowering-non-flowering species 2022"))
  
  
  #Line plots proportion plots in which species is flowering
  
#use csv file Prop_plot_bloom

Prop_plot_bloom$year <- as.factor(as.integer(Prop_plot_bloom$year))
Prop_plot_bloom$month <- as.factor(as.character(Prop_plot_bloom$month))
achmil <- filter(Prop_plot_bloom, species == "achmil")

col_palette1 <- c( "goldenrod3","green4")

col_palette1 <- c( "peru", "steelblue1", "plum")
Prop_plot_bloom$year <- factor(Prop_plot_bloom$year,levels=c("2020", "2021", "2022"))

ggplot(achmil, aes(x=month, y=prop_plot_bloom, group=year, color=year)) +
  geom_line(size=1.5)+
  geom_point(size=3)+
  scale_colour_manual(values= c("goldenrod3", "steelblue1", "plum"))+
  theme_classic()
