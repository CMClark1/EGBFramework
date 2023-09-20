####################
# 5Z Cod Survey Timing
# Caira Clark & Jessie McIntyre
# 16 August 2023
####################

#Questions
#Has the total number of vessels fishing changed over time?
#Has the size (tonnage class) of vessels fishing changed over time?
#Has the gear used by each tonnage class (fixed vs. mobile) changed over time?
#Has the catch composition of different sizes of vessels changed over time?
#Has the catch composition of different gears (fixed vs. mobile) changed over time?
#Do any of these changes coincide?

#Setup
require(tidyr)
require(dplyr)
require(ggplot2)
require(viridis)
require(here)

#Part 1 - How has the number of fishing vessels in 4V, 4X5Y, and 5Z changed over time? How has the number of vessels fishing in each tonnage class changed over time? Has the size (tonnage class) of vessels fishing changed over time? 

summary_a<-all6722_1 %>% 
  group_by(YEAR_OF_ACTIVITY,TONNAGE_CLASS_CODE,NAFO_DIVISION_CODE) %>% 
  distinct(CFV_NUMBER,.keep_all = T)

#print(summary_a %>% group_by(NAFO_DIVISION_CODE, YEAR_OF_ACTIVITY, TONNAGE_CLASS_CODE) %>%
       #filter(TONNAGE_CLASS_CODE==6 | TONNAGE_CLASS_CODE==7) %>%
       #summarise(n=n()), n=55)

#Remove the filter if you want tonnage class 0 included
plota1 <- ggplot(summary_a %>% filter(TONNAGE_CLASS_CODE != "0"),aes(YEAR_OF_ACTIVITY, fill=TONNAGE_CLASS_CODE))+
  geom_bar()+
  facet_grid(NAFO_DIVISION_CODE~., scales="free") +
  theme_light() +
  xlab("Year") +
  ylab("Number of Vessels") +
  labs(fill = "Tonnage Class") +
  scale_fill_viridis_d(option="turbo")
plota1

ggsave(here("figures/Fishery_Canada_NoVesselsperTonnageClass.png"), width=10, height=7, units="in")

#Remove the filter if you want tonnage class 0 included
plota2 <- ggplot(summary_a %>% filter(TONNAGE_CLASS_CODE != "0"),aes(YEAR_OF_ACTIVITY, fill=TONNAGE_CLASS_CODE))+
  geom_bar(position = "fill", width = 1)+
  facet_grid(~NAFO_DIVISION_CODE) +
  theme_light() +
  xlab("Year") +
  ylab("Number of Vessels") +
  labs(fill = "Tonnage Class") +
  scale_fill_viridis_d(option="turbo")
plota2

ggsave(here("figures/Fishery_Canada_PropVesselsperTonnageClass.png"), width=10, height=5, units="in")

#Part 2. Has the gear used by each tonnage class (fixed vs. mobile) changed over time?

plotb1 <- ggplot(summary_a %>% filter(TONNAGE_CLASS_CODE != "0"),aes(YEAR_OF_ACTIVITY, fill=TONNAGE_CLASS_CODE))+
  geom_bar()+
  facet_grid(~GEAR_GROUP) +
  theme_light() +
  xlab("Year") +
  ylab("Number of Vessels") +
  labs(fill = "Tonnage Class") +
  scale_fill_viridis_d(option="turbo")
plotb1

ggsave(here("figures/Fishery_Canada_NoVesselsFixedvsMobile.png"), width=10, height=5, units="in")

plotb2 <- ggplot(summary_a %>% filter(TONNAGE_CLASS_CODE != "0"),aes(YEAR_OF_ACTIVITY, fill=GEAR_GROUP))+
  geom_bar(position = "fill", width = 1)+
  facet_grid(TONNAGE_CLASS_CODE~.) +
  theme_light() +
  xlab("Year") +
  ylab("Proportion of Vessels") +
  labs(fill = "Gear Group") +
  scale_fill_viridis_d(end=0.8, option="turbo")
plotb2

ggsave(here("figures/Fishery_Canada_PropVesselsFixedvsMobile_byTonnageClass.png"), width=10, height=7, units="in")

plotb3 <- ggplot(summary_a %>% filter(TONNAGE_CLASS_CODE != "0"),aes(YEAR_OF_ACTIVITY, fill=GEAR_GROUP))+
  geom_bar(position = "fill", width = 1)+
  facet_grid(TONNAGE_CLASS_CODE~NAFO_DIVISION_CODE) +
  theme_light() +
  xlab("Year") +
  ylab("Proportion of Vessels") +
  labs(fill = "Gear Group") +
  scale_fill_viridis_d(end=0.8, option="turbo")
plotb3

ggsave(here("figures/Fishery_Canada_PropVesselsFixedvsMobile_byNAFOArea.png"), width=10, height=7, units="in")


#Part 3. Has the catch composition of different sizes of vessels changed over time? Has the catch composition of different gears (fixed vs. mobile) changed over time?

plotc1<-ggplot(summary_a %>% filter(TONNAGE_CLASS_CODE != "0"),aes(YEAR_OF_ACTIVITY,LIVE_WT,fill=SPECIES_DESC))+
  geom_col(position = "fill", width = 1)+
  facet_grid(TONNAGE_CLASS_CODE~., scales="free")+
  theme_light()+
  xlab("Year")+
  ylab("Proportion of Landings") +
  labs(fill = "Species")+
  scale_fill_viridis_d(option="turbo")
plotc1

ggsave(here("figures/Fishery_Canada_PropLandingsbySpecies_byTonnageClass.png"), width=10, height=7, units="in")

plotc2<-ggplot(summary_a %>% filter(TONNAGE_CLASS_CODE != "0"),aes(YEAR_OF_ACTIVITY,LIVE_WT,fill=SPECIES_DESC))+
  geom_col(position = "fill", width=1)+
  facet_grid(TONNAGE_CLASS_CODE~NAFO_DIVISION_CODE)+
  theme_light()+
  xlab("Year")+
  ylab("Proportion of Landings")+
  labs(fill = "Species") +
  scale_fill_viridis_d(option="turbo")
plotc2

ggsave(here("figures/Fishery_Canada_PropLandingsbySpecies_byTonnageClassandNAFOArea.png"), width=10, height=7, units="in")

plotc3<-ggplot(summary_a %>% filter(TONNAGE_CLASS_CODE != "0"),aes(YEAR_OF_ACTIVITY,LIVE_WT,fill=SPECIES_DESC))+
  geom_col(position = "fill", width=1)+
  facet_grid(TONNAGE_CLASS_CODE~GEAR_GROUP)+
  theme_light()+
  xlab("Year")+
  ylab("Proportion of Landings")+
  labs(fill = "Species") +
  scale_fill_viridis_d(option="turbo")
plotc3

ggsave(here("figures/Fishery_Canada_PropLandingsbySpecies_byTonnageClassandGearGroup.png"), width=10, height=7, units="in")

