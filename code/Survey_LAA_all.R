#############################
# 5Z Cod
# Survey Catch at Age
# July 1, 2021
# Author: Caira Clark
#############################

require(ggplot2)
require(dplyr)
require(tidyr)
require(here)
require(ggpubr)
require(data.table)
require(viridis)

##DFO Spring Survey EGB + GB ----------------

#Import data

setwd("~/LocalRespository/EGBFramework")

myfiles <- list.files(path="~/LocalRespository/EGBFramework/data/LAA", pattern="*.csv", full.names=TRUE) 

n <- length(myfiles)
datalist <- vector(mode="list", length=n)
for(i in 1:n) {
  cat("importing file", i, ":", myfiles[i], "\n")
  datalist[[i]] <- read.csv(myfiles[i])
}
names(datalist) <- myfiles 

all_data <- data.table::rbindlist(datalist, idcol=TRUE)

#Format and summarize data by year

colnames(all_data)[1] <- "filename"
colnames(all_data)[2] <- "length"
all_data$length <- as.numeric(all_data$length)
colnames(all_data)[19] <- "X16"

library(stringr)
all_data$Survey <- str_sub(all_data$filename,-12,-10)
all_data$Year <- str_sub(all_data$filename,-8,-5)

tidy_GB <- all_data %>% 
  select(c(2:21)) %>% 
  pivot_longer(!c(Survey, Year, length), names_to="age", values_to="number") %>% 
  filter(!is.na(number)) %>%
  group_by(Year, length, age) %>%
  summarise(tot=sum(number)) %>%
  mutate(age=substr(age, 2, 3), Survey="DFO GB")

tidy_EGB <- all_data %>%
  select(c(2:21)) %>%
  pivot_longer(!c(Survey, Year, length), names_to="age", values_to="number") %>% 
  filter(!is.na(number)) %>%
  group_by(Survey, Year, length, age) %>%
  summarise(tot=sum(number)) %>%
  mutate(age=substr(age, 2, 3)) %>% 
  filter(Survey=="EGB") %>%
  mutate(Survey="DFO EGB")

tidy <- full_join(tidy_GB, tidy_EGB)

tidy$age <- as.numeric(tidy$age)

#Calculate LAA for each year

tidy$calc <- tidy$length * tidy$tot
LAA_dfo <- tidy %>% group_by(Survey, Year, age) %>% summarise(laa=(sum(calc)/sum(tot))) %>% pivot_wider(c(Survey, Year), names_from = age, values_from = laa)
LAA_dfo[is.na(LAA_dfo)] <- 0

#Calculate 10+ group
LAA_dfo$'10+' <- LAA_dfo$`10` + LAA_dfo$`11` + LAA_dfo$`12` + LAA_dfo$`13` + LAA_dfo$`14` + LAA_dfo$`15` + LAA_dfo$`16`

#Select columns and tidy data into long format
LAA_dfo2 <- LAA_dfo %>% dplyr::select(Year, Survey, `0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `10+`) %>%
  pivot_longer(!c(Survey, Year), names_to = "Age", values_to = "Length")

#Finish tidying
LAA_dfo2$Age[which(LAA_dfo2$Age == "10+")] = 10
LAA_dfo2$Age <- as.numeric(LAA_dfo2$Age)

#Take out zeros for the plots
LAA_dfo2[LAA_dfo2 == 0] <- NA
LAA_dfo2$Year <- as.integer(LAA_dfo2$Year)

#NMFS EGB ------------- 

setwd("~/LocalRespository/EGBFramework")

LAA_nmfsspr <- read.csv(here("~/LocalRespository/EGBFramework/data/Survey CAA/nmfsspr_survey_laa.csv"))

LAA_nmfsspr$'a10+' <- LAA_nmfsspr$a10 + LAA_nmfsspr$a11 + LAA_nmfsspr$a12 + LAA_nmfsspr$a13 + LAA_nmfsspr$a14 + LAA_nmfsspr$a15 + LAA_nmfsspr$a16

#Select columns and tidy data into long format
LAA_nmfsspr2 <- LAA_nmfsspr %>% dplyr::select(Year, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, 'a10+') %>%
  pivot_longer(!c(Year), names_to = "Age", values_to = "Length")

#Finish tidying
LAA_nmfsspr2$Age <- sub("a", "", LAA_nmfsspr2$Age)
LAA_nmfsspr2$Age[which(LAA_nmfsspr2$Age == "10+")] = 10
LAA_nmfsspr2$Age <- as.numeric(LAA_nmfsspr2$Age)


LAA_nmfsfall <- read.csv(here("~/LocalRespository/EGBFramework/data/Survey CAA/nmfsfall_survey_LAA.csv"))

LAA_nmfsfall$'a10+' <- LAA_nmfsfall$a10 + LAA_nmfsfall$a11 + LAA_nmfsfall$a12 + LAA_nmfsfall$a13 + LAA_nmfsfall$a14 + LAA_nmfsfall$a15 + LAA_nmfsfall$a16

#Select columns and tidy data into long format
LAA_nmfsfall2 <- LAA_nmfsfall %>% dplyr::select(Year, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, 'a10+') %>%
  pivot_longer(!c(Year), names_to = "Age", values_to = "Length")

#Finish tidying
LAA_nmfsfall2$Age <- sub("a", "", LAA_nmfsfall2$Age)
LAA_nmfsfall2$Age[which(LAA_nmfsfall2$Age == "10+")] = 10
LAA_nmfsfall2$Age <- as.numeric(LAA_nmfsfall2$Age)

LAA_nmfsspr2$Survey <- "NMFS Spring EGB"
LAA_nmfsfall2$Survey <- "NMFS Fall EGB"

LAA_nmfs <- rbind(LAA_nmfsspr2, LAA_nmfsfall2)

#Take out zeros for the plots
LAA_nmfs[LAA_nmfs == 0] <- NA

#Plot everything but NMFS GB

LAA <- rbind(LAA_dfo2, LAA_nmfs)

#Plot ages 2-7, faceted by age
LAAplot <- ggplot(subset(LAA, Age %in% c(2:7)), aes(Year, Length, colour=Survey)) +
  geom_line() +
  geom_point() + 
  scale_color_viridis_d(option="turbo", end=0.8) +
  facet_wrap(.~Age, scales="free") +
  ylab("Length (cm)")+
  theme_bw() +
  ggtitle("Length at Age") +
  xlim(1970, 2023)
LAAplot

ggsave(here("figures/Survey_LAA.png"), width=10, height=7, units="in")
