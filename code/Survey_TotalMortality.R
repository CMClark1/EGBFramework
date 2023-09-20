#############################
# 5Z Cod
# Survey Total Mortality Z
# July 1, 2021
# Author: Caira Clark / Irene Andrushchenko / Chris Legault
#############################

require(ggplot2)
require(tidyr)
require(dplyr)
require(here)

##Total Mortality (Z45 and Z678) - originally from Survey Mortality 2021.xlsx ------------------------------

###DFO Survey----------------

CAA <- read.csv(here("data/Survey CAA/dfo_survey_caa.csv"))

#Calculate Z45
for (i in 1:nrow(CAA)) {
  CAA$Z45[i] <- log((CAA$a4[i] + CAA$a5[i]) / (CAA$a5[i+1] + CAA$a6[i+1]))
}

#Calculate Z678
for (i in 1:nrow(CAA)) {
  CAA$Z678[i] <- log((CAA$a6[i] + CAA$a7[i] + CAA$a8[i]) / (CAA$a7[i+1] + CAA$a8[i+1] + CAA$a9[i+1]))
}

#LOESS Smoothers for Z45 and Z678

loess_Z45<-loess(Z45~Year, CAA, span=0.35, iterations=3, surface="direct", family="symmetric", degree=1) 
loess_Z678<-loess(Z678~Year, CAA, span=0.35, iterations=3, surface="direct", family="symmetric", degree=1) 
loess_Z45 <- as.data.frame(loess_Z45$fitted)
zero <- NA
loess_Z45 <- rbind(loess_Z45, zero)
loess_Z678 <- as.data.frame(loess_Z678$fitted)
loess_Z678 <- rbind(loess_Z678, zero)
loess <- cbind(loess_Z45, loess_Z678)
colnames(loess)[1] <- "Z45smooth"
colnames(loess)[2] <- "Z678smooth"

CAA_loess <- cbind(CAA%>%filter(!Year%in%(c(2022, 2023))), loess)
CAA_loess <- CAA_loess %>% dplyr::select(Year, Z45, Z678, Z45smooth, Z678smooth) %>%
  pivot_longer(!Year, names_to = "Source", values_to = "Z")

dfo <- ggplot() +
  geom_point(data=subset(CAA_loess, Source %in% c("Z45", "Z678")), aes(x=Year, y=Z, color = Source)) +
  geom_line(data=subset(CAA_loess, Source %in% c("Z45smooth", "Z678smooth")), aes(x=Year, y=Z, group=Source, color = Source)) +
  theme_bw() +
  scale_color_manual(values = c("#F8766D", "#F8766D", "#00BFC4", "#00BFC4")) +
  ggtitle("DFO Spring")

##NMFS Spring Survey---------------------------

CAA_nmfsspr <- read.csv(here("data/Survey CAA/nmfsspr_survey_caa.csv"))

#Load the NMFS conversion file and calculate spring conversion factors
nmfsconv <- read.csv(here("data/Survey CAA/nmfs_conversionfactors.csv"))
nmfsconv$SpringConversion <- nmfsconv$SpringDoorEffect*nmfsconv$SpringVesselEffect
sprconv <- cbind(nmfsconv$Year, nmfsconv$SpringConversion, nmfsconv$SpringConversion, nmfsconv$SpringConversion, nmfsconv$SpringConversion, nmfsconv$SpringConversion,nmfsconv$SpringConversion, nmfsconv$SpringConversion, nmfsconv$SpringConversion, nmfsconv$SpringConversion, nmfsconv$SpringConversion, nmfsconv$SpringConversion, nmfsconv$SpringConversion, nmfsconv$SpringConversion, nmfsconv$SpringConversion, nmfsconv$SpringConversion, nmfsconv$SpringConversion, nmfsconv$SpringConversion) #There's probably a better way of doing this

#Format the two tables so that they have the same row names and columns
head(CAA_nmfsspr)
sprconv <- as.data.frame(sprconv)
colnames(sprconv) <- c("Year", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "a13", "a14", "a15", "a16")
head(sprconv)

#Pivot dataframes so that they can be joined easily, and join them
CAA_nmfsspr <- CAA_nmfsspr %>% pivot_longer(!Year, names_to="Age", values_to="Value")
sprconv <- sprconv %>% pivot_longer(!Year, names_to="Age", values_to="cValue")
CAA_nmfsspr$cValue <- CAA_nmfsspr$Value*sprconv$cValue

#Pivot converted data back into wide format
CAA_nmfsspr <- CAA_nmfsspr %>% dplyr::select(Year, Age, cValue) %>% pivot_wider(Year, names_from = Age, values_from = cValue)
CAA <- CAA_nmfsspr
CAA[is.na(CAA)] <- 0

#Calculate Z45
for (i in 1:nrow(CAA)) {
  tryCatch(CAA$Z45[i] <- log((CAA$a4[i] + CAA$a5[i]) / (CAA$a5[i+1] + CAA$a6[i+1])))
}

#Calculate Z678
for (i in 1:nrow(CAA)) {
  tryCatch(CAA$Z678[i] <- log((CAA$a6[i] + CAA$a7[i] + CAA$a8[i]) / (CAA$a7[i+1] + CAA$a8[i+1] + CAA$a9[i+1])))
}

#LOESS Smoothers for Z45 and Z678

loess_Z45<-loess(Z45~Year, subset(CAA, !is.infinite(Z45)), span=0.35, iterations=3, surface="direct", family="symmetric", degree=1) 

loess_Z678<-loess(Z678~Year, subset(CAA, !is.infinite(Z678)), span=0.35, iterations=3, surface="direct", family="symmetric", degree=1) 
loess_Z45 <- as.data.frame(loess_Z45$fitted)
zero <- NA
loess_Z45 <- rbind(loess_Z45, zero)
loess_Z678 <- as.data.frame(loess_Z678$fitted)
loess_Z45$Year<-subset(CAA, !is.infinite(Z45))$Year
loess_Z678 <- rbind(loess_Z678, zero)
loess_Z678$Year<-subset(CAA, !is.infinite(Z678))$Year
loess<-merge(loess_Z45, loess_Z678, all=TRUE)
#loess <- cbind(loess_Z45, loess_Z678)
colnames(loess)[2] <- "Z45smooth"
colnames(loess)[3] <- "Z678smooth"

CAA<-as.data.frame(CAA)
CAA$Z45<-with(CAA, ifelse(is.infinite(Z45), NA, Z45))
CAA$Z678<-with(CAA, ifelse(is.infinite(Z678), NA, Z678))

CAA_loess<-merge(CAA,loess, all=TRUE)
#CAA_loess <- cbind(CAA, loess)
CAA_loess <- CAA_loess %>% dplyr::select(Year, Z45, Z678, Z45smooth, Z678smooth) %>%
  pivot_longer(!Year, names_to = "Source", values_to = "Z")

CAA_loess<-subset(CAA_loess, Year>1977)

nmfsspring <- ggplot() +
  geom_point(data=subset(CAA_loess, Source %in% c("Z45", "Z678")), aes(x=Year, y=Z, color = Source)) +
  geom_line(data=subset(CAA_loess, Source %in% c("Z45smooth", "Z678smooth")), aes(x=Year, y=Z, group=Source, color = Source)) +
  theme_bw() +
  scale_color_manual(values = c("#F8766D", "#F8766D", "#00BFC4", "#00BFC4")) +
  ggtitle("NMFS Spring")


##NMFS Fall Survey--------------------------- #Hasn't been included in previous documents....

CAA_nmfsfall <- read.csv(here("data/Survey CAA/nmfsfall_survey_caa.csv"))
#Load the NMFS conversion file and calculate spring conversion factors
nmfsconv <- read.csv(here("data/Survey CAA/nmfs_conversionfactors.csv"))
nmfsconv$FallConversion <- nmfsconv$FallDoorEffect*nmfsconv$FallVesselEffect
fallconv <- cbind(nmfsconv$Year, nmfsconv$FallConversion, nmfsconv$FallConversion, nmfsconv$FallConversion, nmfsconv$FallConversion, nmfsconv$FallConversion,nmfsconv$FallConversion, nmfsconv$FallConversion, nmfsconv$FallConversion, nmfsconv$FallConversion, nmfsconv$FallConversion, nmfsconv$FallConversion, nmfsconv$FallConversion, nmfsconv$FallConversion, nmfsconv$FallConversion, nmfsconv$FallConversion, nmfsconv$FallConversion, nmfsconv$FallConversion) #There's probably a better way of doing this

#Format the two tables so that they have the same row names and columns
head(CAA_nmfsfall)
fallconv <- as.data.frame(fallconv)
colnames(fallconv) <- c("Year", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "a13", "a14", "a15", "a16")
head(fallconv)

#Pivot dataframes so that they can be joined easily, and join them
CAA_nmfsfall <- CAA_nmfsfall %>% pivot_longer(!Year, names_to="Age", values_to="Value")
fallconv <- fallconv %>% pivot_longer(!Year, names_to="Age", values_to="cValue")
CAA_nmfsfall$cValue <- CAA_nmfsfall$Value*fallconv$cValue

#Pivot converted data back into wide format
CAA_nmfsfall <- CAA_nmfsfall %>% dplyr::select(Year, Age, cValue) %>% pivot_wider(Year, names_from = Age, values_from = cValue)
CAA <- CAA_nmfsfall
CAA[is.na(CAA)] <- 0

#Calculate Z45
CAA$Z45 <- 0
for (i in 1:nrow(CAA)) {
  tryCatch(CAA$Z45[i] <- log((CAA$a4[i] + CAA$a5[i]) / (CAA$a5[i+1] + CAA$a6[i+1])))
}

#Calculate Z678
CAA$Z678 <- 0
for (i in 1:nrow(CAA)) {
  tryCatch(CAA$Z678[i] <- log((CAA$a6[i] + CAA$a7[i] + CAA$a8[i]) / (CAA$a7[i+1] + CAA$a8[i+1] + CAA$a9[i+1])))
}

#LOESS Smoothers for Z45 and Z678

loess_Z45<-loess(Z45~Year, subset(CAA, !is.infinite(Z45)), span=0.35, iterations=3, surface="direct", family="symmetric", degree=1) 

loess_Z678<-loess(Z678~Year, subset(CAA, !is.infinite(Z678)), span=0.35, iterations=3, surface="direct", family="symmetric", degree=1)

loess_Z45 <- as.data.frame(loess_Z45$fitted)
zero <- NA
loess_Z45 <- rbind(loess_Z45, zero)
loess_Z678 <- as.data.frame(loess_Z678$fitted)
loess_Z45$Year<-subset(CAA, !is.infinite(Z45)&!is.nan(Z45))$Year ###Loess can handle NaNs but not Inf. If you have have a mixture of those, make sure you check this line!
loess_Z678 <- rbind(loess_Z678, zero)
loess_Z678$Year<-subset(CAA, !is.infinite(Z678)&!is.nan(Z678))$Year #Check this
loess<-merge(loess_Z45, loess_Z678, all=TRUE)
#loess <- cbind(loess_Z45, loess_Z678)
colnames(loess)[2] <- "Z45smooth"
colnames(loess)[3] <- "Z678smooth"

CAA_loess<-merge(CAA,loess, all=TRUE)
#CAA_loess <- cbind(CAA, loess)
CAA_loess <- CAA_loess %>% dplyr::select(Year, Z45, Z678, Z45smooth, Z678smooth) %>%
  pivot_longer(!Year, names_to = "Source", values_to = "Z")

nmfsfall <- ggplot() +
  geom_point(data=subset(CAA_loess, Source %in% c("Z45", "Z678")), aes(x=Year, y=Z, color = Source)) +
  geom_line(data=subset(CAA_loess, Source %in% c("Z45smooth", "Z678smooth")), aes(x=Year, y=Z, group=Source, color = Source)) +
  theme_bw() +
  scale_color_manual(values = c("#F8766D", "#F8766D", "#00BFC4", "#00BFC4")) +
  ggtitle("NMFS Fall")

ggarrange(dfo, nmfsspring, nmfsfall, ncol=1, nrow=3, common.legend = TRUE, legend="bottom")

ggsave(here("figures/Survey_TotalMortality_EGB.png"), width=10, height=12, units="in")


##Part 1. LOESS Smoothers for Z45 and Z678 (DFO Survey) -------------

#Loess Z45 and Z678

#Copy this onto your clipboard (replace with new data each year)
Year	Z45	Z678
1986	1.402878426	0.902698307
1987	-0.019147044	0.264737137
1988	0.865707082	0.393444942
1989	-0.921954165	-1.11298541
1990	1.11860266	1.40481447
1991	0.834262654	1.30659615
1992	-0.065936984	0.353411312
1993	0.409511779	0.827691053
1994	0.916205766	1.508827615
1995	-0.658348478	-0.859572781
1996	1.361744398	2.686062714
1997	1.539902493	1.194127099
1998	-0.181743149	0.343431887
1999	-0.727751136	-0.491716934
2000	1.25273915	0.864007236
2001	-0.034466291	0.52763939
2002	0.967420526	1.860678494
2003	0.574363177	0.921821962
2004	-1.168796098	-0.537172475
2005	1.257090724	1.143321375
2006	0.865568569	1.547530888
2007	-0.060690385	0.667870413
2008	0.071474376	2.039016993
2009	0.112651516	0.429807868
2010	1.315473554	2.594856405
2011	1.862938976	2.703410926
2012	0.330596812	1.07885281
2013	2.488006154	1.673237039
2014	0.277883078	2.109727652
2015	0.903574936	1.635312278
2016	0.018806857	-0.118389367
2017	1.284119872	2.07064521
2018	1.378752839	3.043560418
2019	0.931352454	2.434269079
2020	1.883608868	0.78843573

index<-read.table(file = "clipboard", sep = "\t", header=TRUE)

#Read in LOESS functions (these are by Irene Andrushchenko)
loess_Z45<-loess(Z45~Year, index, span=0.35, iterations=3, surface="direct", family="symmetric", degree=1) 

loess_Z678<-loess(Z678~Year, index, span=0.35, iterations=3, surface="direct", family="symmetric", degree=1) 

#These are the data that is pasted back into the Survey Mortality 2021.xlsx spreadsheet

loess_Z45$fitted

loess_Z678$fitted





