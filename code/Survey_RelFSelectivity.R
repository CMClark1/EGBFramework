#############################
# 5Z Cod
# RelF and Selectivity
# September 2023
# Author: Caira Clark
#############################

require(ggplot2);require(dplyr);require(tidyr);require(here);require(ggpubr);require(viridis)

#Load the fishery CAA file
CAA_fishery <- read.csv(here("data/Survey CAA/fishery_combined_caa.csv"))
colnames(CAA_fishery)[1] <- "Year"

#Select columns and tidy data into long format
CAA_fishery2 <- CAA_fishery %>%
  tidyr::pivot_longer(!Year, names_to = "Age", values_to = "Abundance") 

#Finish tidying
CAA_fishery2$Age <- sub("a", "", CAA_fishery2$Age)
CAA_fishery2$Age <- as.numeric(CAA_fishery2$Age)
CAA_fishery2$Abundance <- CAA_fishery2$Abundance*1000

##DFO Spring Survey EGB + GB ----------------

#Load the DFO CAA files
CAA_dfoEGB <- read.csv(here("data/Survey CAA/dfo_survey_caa.csv"))
colnames(CAA_dfoEGB)[1] <- "Year"
CAA_dfoEGB$Survey <- "DFO EGB"

CAA_dfoWGB <- read.csv(here('data/Survey CAA/dfo_survey_caa_WGB.csv'))
colnames(CAA_dfoWGB)[1] <- "Year"
CAA_dfoWGB$Survey <- "DFO WGB"

CAA_dfo <- rbind(CAA_dfoEGB, CAA_dfoWGB)

#Select columns and tidy data into long format
CAA_dfo2 <- CAA_dfo %>%
  pivot_longer(!c(Survey, Year), names_to = "Age", values_to = "Abundance") %>%
  pivot_wider(c(Year, Age), names_from="Survey", values_from = "Abundance")
CAA_dfo2[is.na(CAA_dfo2)] <- 0
CAA_dfo2$'DFO GB' <- CAA_dfo2$`DFO EGB`+CAA_dfo2$`DFO WGB`
CAA_dfo2 <- CAA_dfo2 %>% pivot_longer(!c(Year, Age), names_to="Survey", values_to="Abundance") %>% select(Survey, Year, Age, Abundance)

#Finish tidying
CAA_dfo2$Age <- sub("a", "", CAA_dfo2$Age)
CAA_dfo2$Age[which(CAA_dfo2$Age == "10+")] = 10
CAA_dfo2$Age <- as.numeric(CAA_dfo2$Age)

#NMFS EGB ------------- 

#Load the NMFS CAA files and add 10+ groups
CAA_nmfsspr <- read.csv(here("data/Survey CAA/nmfsspr_survey_caa.csv"))
CAA_nmfsspr$SURVEY <- "NMFS SPRING EGB"

CAA_nmfsfall <- read.csv(here("data/Survey CAA/nmfsfall_survey_caa.csv")) #this stranal file should have the conversion factor selected.
CAA_nmfsfall$SURVEY <- "NMFS FALL EGB"

CAA_nmfsEGB <- rbind(CAA_nmfsspr, CAA_nmfsfall)

#Load the NMFS conversion file and calculate spring conversion factors
nmfsconv <- read.csv(here("data/Survey CAA/nmfs_conversionfactors.csv"))
nmfsconv$SpringConversion <- nmfsconv$SpringDoorEffect*nmfsconv$SpringVesselEffect
nmfsconv$FallConversion <- nmfsconv$FallDoorEffect*nmfsconv$FallVesselEffect

#Pivot dataframes so that they can be joined easily, and join them
CAA_nmfsEGB <- CAA_nmfsEGB %>% pivot_longer(!c(SURVEY, Year), names_to="Age", values_to="AbundanceUnconv")
nmfsconv <- nmfsconv %>% select(Year, SpringConversion, FallConversion) %>% pivot_longer(!c(Year), names_to="Season", values_to="Conversion") %>% mutate(SURVEY=case_when(Season=="SpringConversion" ~ "NMFS SPRING EGB", Season=="FallConversion" ~ "NMFS FALL EGB"))
CAA_nmfsEGB <- left_join(CAA_nmfsEGB, nmfsconv)
CAA_nmfsEGB$Abundance <- CAA_nmfsEGB$AbundanceUnconv*CAA_nmfsEGB$Conversion

#Finish tidying
CAA_nmfsEGB[is.na(CAA_nmfsEGB)] <- 0
CAA_nmfsEGB$Age <- sub("a", "", CAA_nmfsEGB$Age)
CAA_nmfsEGB$Age <- as.numeric(CAA_nmfsEGB$Age)
colnames(CAA_nmfsEGB)[2] <- "Survey"

CAA_nmfsEGB <- CAA_nmfsEGB %>% select(Survey, Year, Age, Abundance)

#Join all the survey data together
CAA_surveys <- rbind(CAA_dfo2, CAA_nmfsEGB)

#Join fishery CAA to survey CAA

CAA_all <- CAA_surveys %>% 
  mutate('SurveyAbundance' = Abundance) %>% 
  select(-Abundance) %>% group_by(Year, Survey, Age) %>% 
  left_join(CAA_fishery2%>%mutate('FisheryAbundance' = Abundance) %>% 
              select(-Abundance))
CAA_all[is.na(CAA_all) | CAA_all == Inf | CAA_all == -Inf] <- NA

#Calculate Relative F
CAA_all$RelF <- CAA_all$FisheryAbundance/CAA_all$SurveyAbundance
CAA_all[is.na(CAA_all) | CAA_all == Inf | CAA_all == -Inf] <- NA

CAA_all <- CAA_all %>% mutate(DECADE=case_when(
  Year <= 1979            ~ "1970-1979",
  Year > 1979 & Year <= 1989 ~ "1980-1989",
  Year > 1989 & Year <= 1999 ~ "1990-1999",
  Year > 1999 & Year <= 2009 ~ "2000-2009",
  Year > 2009 & Year <= 2019 ~ "2010-2019",
  Year > 2019  ~ "2020+")) %>%
  filter(!is.na(RelF)) %>%
  group_by(Survey, DECADE, Age) %>%
  summarise(meanRelF=mean(RelF))

relf_dgoegb <- ggplot(CAA_all%>%filter(Survey=="DFO EGB")) +
  geom_line(aes(x=Age, y=meanRelF)) +
  facet_grid(DECADE~., scales="free")+
  theme_bw()+
  scale_colour_viridis() +
  ggtitle("DFO EGB")

relf_dfowgb <- ggplot(CAA_all%>%filter(Survey=="DFO WGB")) +
  geom_line(aes(x=Age, y=meanRelF)) +
  facet_grid(DECADE~., scales="free")+
  theme_bw()+
  scale_colour_viridis() +
  ggtitle("DFO WGB")

relf_dfogb <- ggplot(CAA_all%>%filter(Survey=="DFO GB")) +
  geom_line(aes(x=Age, y=meanRelF)) +
  facet_grid(DECADE~., scales="free")+
  theme_bw()+
  scale_colour_viridis() +
  ggtitle("DFO GB")

relf_nmfsfallegb<- ggplot(CAA_all%>%filter(Survey=="NMFS FALL EGB")) +
  geom_line(aes(x=Age, y=meanRelF)) +
  facet_grid(DECADE~., scales="free")+
  theme_bw()+
  scale_colour_viridis() +
  ggtitle("NMFS FALL EGB")

relf_nmfsspregb<- ggplot(CAA_all%>%filter(Survey=="NMFS SPRING EGB")) +
  geom_line(aes(x=Age, y=meanRelF)) +
  facet_grid(DECADE~., scales="free")+
  theme_bw()+
  scale_colour_viridis() +
  ggtitle("NMFS SPRING EGB")

relF <- ggarrange(relf_dgoegb + rremove("ylab") + rremove("xlab"), relf_dfowgb + rremove("ylab") + rremove("xlab"), relf_dfogb + rremove("ylab") + rremove("xlab"), relf_nmfsspregb + rremove("ylab") + rremove("xlab"), relf_nmfsfallegb + rremove("ylab") + rremove("xlab"), nrow=1, ncol=5, common.legend = TRUE, legend="right")

require(grid)
annotate_figure(relF, left = textGrob("RelF", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Year", gp = gpar(cex = 1.3)))

ggsave(here("figures/Survey_RelativeF.png"), width=10, height=8, units="in")


#Calculate selectivity

maxab <- CAA_surveys %>% group_by(Survey, Year) %>% summarise(max=max(Abundance))
CAA_surveys2 <- left_join(CAA_surveys, maxab)
CAA_surveys2[is.na(CAA_surveys2) | CAA_surveys2 == Inf | CAA_surveys2 == -Inf | CAA_surveys2 == 0] <- NA

CAA_surveys2$Selectivity <- CAA_surveys2$Abundance/CAA_surveys2$max
CAA_surveys2[is.na(CAA_surveys2) | CAA_surveys2 == Inf | CAA_surveys2 == -Inf] <- NA
  
CAA_surveys2 <- CAA_surveys2 %>% mutate(DECADE=case_when(
  Year <= 1979            ~ "1970-1979",
  Year > 1979 & Year <= 1989 ~ "1980-1989",
  Year > 1989 & Year <= 1999 ~ "1990-1999",
  Year > 1999 & Year <= 2009 ~ "2000-2009",
  Year > 2009 & Year <= 2019 ~ "2010-2019",
  Year > 2019  ~ "2020+")) %>%
  filter(!is.na(Selectivity)) %>%
  group_by(Survey, DECADE, Age) %>%
  summarise(meanSelect=mean(Selectivity))

ggplot(CAA_surveys2) +
  #geom_smooth(aes(x=Age, y=Selectivity, colour=Year, group=DECADE)) +
  geom_line(aes(x=Age, y=meanSelect)) +
  facet_grid(DECADE~factor(Survey, levels=c('DFO EGB','DFO GB','DFO WGB','NMFS SPRING EGB','NMFS FALL EGB')), scales="free_y")+
  theme_bw()+
  scale_colour_viridis()

ggsave(here("figures/Survey_Selectivity.png"), width=10, height=8, units="in")
