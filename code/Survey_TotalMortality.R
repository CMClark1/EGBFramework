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
require(ggpubr)

##Total Mortality (Z45 and Z678) - originally from Survey Mortality 2021.xlsx ------------------------------

###DFO Survey----------------

#CAA <- read.csv(here("data/Survey CAA/dfo_survey_caa.csv"))
CAA <- read.csv("S:/Science/Population Ecology/Georges Bank/GBcod/y2023/Framework/Survey/R Project Files/data/Survey CAA/dfo_survey_caa.csv")
CAA <- CAA %>% select(Year, a4, a5, a6, a7, a8, a9)

#Calculate Z45
for (i in 1:nrow(CAA)) {
  CAA$Z45[i] <- log((CAA$a4[i] + CAA$a5[i]) / (CAA$a5[i+1] + CAA$a6[i+1]))
}

#Calculate Z678
for (i in 1:nrow(CAA)) {
  CAA$Z678[i] <- log((CAA$a6[i] + CAA$a7[i] + CAA$a8[i]) / (CAA$a7[i+1] + CAA$a8[i+1] + CAA$a9[i+1]))
}

#LOESS Smoothers for Z45 and Z678

loess_Z45<-loess(Z45~Year, subset(CAA, !is.infinite(Z45)), span=0.35, iterations=3, surface="direct", family="symmetric", degree=1) 
loess_Z678<-loess(Z678~Year, subset(CAA, !is.infinite(Z678)), span=0.35, iterations=3, surface="direct", family="symmetric", degree=1) 
loess_Z45 <- cbind(loess_Z45[["x"]], as.data.frame(loess_Z45$fitted))
loess_Z678 <- cbind(loess_Z678[["x"]], as.data.frame(loess_Z678$fitted))
loess <- merge(loess_Z45, loess_Z678, all=TRUE)
colnames(loess)[2] <- "Z45smooth"
colnames(loess)[3] <- "Z678smooth"

CAA_loess <- merge(CAA, loess, all=TRUE) 
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
CAA_nmfsspr <- read.csv("S:/Science/Population Ecology/Georges Bank/GBcod/y2023/Framework/Survey/R Project Files/data/Survey CAA/nmfsspr_survey_caa.csv")

#Pivot dataframes so that they can be joined easily, and join them
CAA_nmfsspr <- CAA_nmfsspr %>% pivot_longer(!Year, names_to="Age", values_to="Value")
#sprconv <- sprconv %>% pivot_longer(!Year, names_to="Age", values_to="cValue")
#CAA_nmfsspr$cValue <- CAA_nmfsspr$Value*sprconv$cValue #Hashed out because the numbers coming out ot Stranal are already converted
CAA_nmfsspr$cValue <- CAA_nmfsspr$Value 

#Pivot converted data back into wide format
CAA_nmfsspr <- CAA_nmfsspr %>% dplyr::select(Year, Age, cValue) %>% pivot_wider(Year, names_from = Age, values_from = cValue)
CAA <- CAA_nmfsspr
#CAA[is.na(CAA)] <- 0

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
loess_Z45 <- cbind(loess_Z45[["x"]], as.data.frame(loess_Z45$fitted))
loess_Z678 <- cbind(loess_Z678[["x"]], as.data.frame(loess_Z678$fitted))
loess <- merge(loess_Z45, loess_Z678, all=TRUE)
colnames(loess)[2] <- "Z45smooth"
colnames(loess)[3] <- "Z678smooth"

CAA_loess<-merge(CAA,loess, all=TRUE)
#CAA_loess <- cbind(CAA, loess)
CAA_loess <- CAA_loess %>% dplyr::select(Year, Z45, Z678, Z45smooth, Z678smooth) %>%
  pivot_longer(!Year, names_to = "Source", values_to = "Z")

CAA_loess<-subset(CAA_loess, Year>1969)

nmfsspring <- ggplot() +
  geom_point(data=subset(CAA_loess, Source %in% c("Z45", "Z678")), aes(x=Year, y=Z, color = Source)) +
  geom_line(data=subset(CAA_loess, Source %in% c("Z45smooth", "Z678smooth")), aes(x=Year, y=Z, group=Source, color = Source)) +
  theme_bw() +
  scale_color_manual(values = c("#F8766D", "#F8766D", "#00BFC4", "#00BFC4")) +
  ggtitle("NMFS Spring")


##NMFS Fall Survey--------------------------- #Hasn't been included in previous documents....

CAA_nmfsfall <- read.csv(here("data/Survey CAA/nmfsfall_survey_caa.csv"))
CAA_nmfsfall<- read.csv("S:/Science/Population Ecology/Georges Bank/GBcod/y2023/Framework/Survey/R Project Files/data/Survey CAA/nmfsfall_survey_caa.csv")

#Pivot dataframes so that they can be joined easily, and join them
CAA_nmfsfall <- CAA_nmfsfall %>% pivot_longer(!Year, names_to="Age", values_to="Value")
#fallconv <- fallconv %>% pivot_longer(!Year, names_to="Age", values_to="cValue")
CAA_nmfsfall$cValue <- CAA_nmfsfall$Value

#Pivot converted data back into wide format
CAA_nmfsfall <- CAA_nmfsfall %>% dplyr::select(Year, Age, cValue) %>% pivot_wider(Year, names_from = Age, values_from = cValue)
CAA <- CAA_nmfsfall
#CAA[is.na(CAA)] <- 0

#Calculate Z45
for (i in 1:nrow(CAA)) {
  tryCatch(CAA$Z45[i] <- log((CAA$a4[i] + CAA$a5[i]) / (CAA$a5[i+1] + CAA$a6[i+1])))
}

#Calculate Z123
for (i in 1:nrow(CAA)) {
  tryCatch(CAA$Z123[i] <- log((CAA$a1[i] + CAA$a2[i] + CAA$a3[i]) / (CAA$a2[i+1] + CAA$a3[i+1] + CAA$a4[i+1])))
}

#LOESS Smoothers for Z45 and Z678

loess_Z45<-loess(Z45~Year, subset(CAA, !is.infinite(Z45)), span=0.35, iterations=3, surface="direct", family="symmetric", degree=1) 
loess_Z123<-loess(Z123~Year, subset(CAA, !is.infinite(Z123)), span=0.35, iterations=3, surface="direct", family="symmetric", degree=1) 
loess_Z45 <- cbind(loess_Z45[["x"]], as.data.frame(loess_Z45$fitted))
loess_Z123 <- cbind(loess_Z123[["x"]], as.data.frame(loess_Z123$fitted))
loess <- merge(loess_Z45, loess_Z123, all=TRUE)
colnames(loess)[2] <- "Z45smooth"
colnames(loess)[3] <- "Z123smooth"

CAA_loess<-merge(CAA,loess, all=TRUE)
#CAA_loess <- cbind(CAA, loess)
CAA_loess <- CAA_loess %>% dplyr::select(Year, Z45, Z123, Z45smooth, Z123smooth) %>%
  pivot_longer(!Year, names_to = "Source", values_to = "Z")

CAA_loess<-subset(CAA_loess, Year>1969)

nmfsfall <- ggplot() +
  geom_point(data=subset(CAA_loess, Source %in% c("Z45","Z123")), aes(x=Year, y=Z, color = Source)) +
  geom_line(data=subset(CAA_loess, Source %in% c("Z45smooth",'Z123smooth')), aes(x=Year, y=Z, group=Source, color = Source)) +
  theme_bw() +
  scale_color_manual(values = c("#F8766D", "#F8766D", "black", "black")) +
  ggtitle("NMFS Fall")

ggarrange(dfo, nmfsspring, nmfsfall, ncol=1, nrow=3, legend="right")

ggsave(here("figures/Survey_TotalMortality_EGB.png"), width=10, height=12, units="in")
