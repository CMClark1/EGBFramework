####################
# Groundfish Fleet Data Loading
# Irene Andrushchenko
# 1 July 2023
####################

setwd("S:/Science/Population Ecology/Georges Bank/GBcod/y2023/Framework/Fishery")

require(tidyverse)
require(dplyr)
require(ROracle)
require(gdata)

#Run ComlandPullandPrep.R OR
comland_6701<-readRDS(file = here("data/comland_6701.rds"))
#Run MarfisPullAndPrep.R OR
marfis_0222<-readRDS(file = here("data/marfis_0222.rds"))

names(comland_6701)[which(colnames(comland_6701)=="Species_Sought_Clean_New_Desc")]<-'GEAR_DIRECT'
names(comland_6701)[which(colnames(comland_6701)=="Species_Sought_Clean_New_Desc.1")]<-'DominantSpecies'
names(comland_6701)

#merging tonnage class info and 
channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=oracle.username, password=oracle.password, oracle.dsn)  
catcher<-ROracle::dbGetQuery(channel, "select * from COMLAND.catchers_1986_2001")
names(catcher)
catchers<-catcher %>% 
  dplyr::select(CFV_NUMBER,TONNAGE_CLASS_CODE,TONNAGE) %>% 
  distinct(CFV_NUMBER,.keep_all = T)
#CFV #
test<-comland_6701
test<-test #%>% 
#filter(YEAR_OF_ACTIVITY==2001)

test1<-left_join(test,catchers, by="CFV_NUMBER")

##marfis
tonnage<-ROracle::dbGetQuery(channel, "select * from marfissci.VW_vessels")
names(tonnage)
tonnage<-tonnage %>% 
  dplyr::rename("VR_NUMBER_FISHING"="VR_NUMBER") %>% 
  dplyr::select(VR_NUMBER_FISHING, TONNAGE_CLASS,GROSS_TONNAGE) %>% 
  distinct(VR_NUMBER_FISHING,.keep_all = T)

testm<-marfis_0222 #%>% 
#filter(year==2010)

testm1<-left_join(testm,tonnage, by="VR_NUMBER_FISHING")

#now need to make the column titles match to join up
names(test1)
names(testm1)

names(testm1)<-names(test1)
all<-rbind(test1, testm1)
all6722<-all

###connect later once it is refined
#names(marfis_0222)<-names(comland_6701)
#all6722<-rbind(comland_6701, marfis_0222)
#all6722$GEAR_DIRECT<-all6722$Species_Sought_Clean_New

#Narrowing it down to relevant areas
table(all6722$NAFO_DIVISION_CODE)
table(is.na(all6722$NAFO_DIVISION_CODE))
all6722$NAFO_DIVISION_CODE_Original<-all6722$NAFO_DIVISION_CODE
all6722<-subset(all6722, NAFO_DIVISION_CODE%in%c('4W','4X','4Xmn','4XU','5Y','5Z','5ZE'))
all6722$NAFO_DIVISION_CODE<-with(all6722, ifelse(NAFO_DIVISION_CODE%in%c('4XU','4Xmn'), '4X', NAFO_DIVISION_CODE))
all6722$NAFO_DIVISION_CODE<-with(all6722, ifelse(NAFO_DIVISION_CODE%in%c('4X','5Y'), '4X5Y', NAFO_DIVISION_CODE))
all6722$NAFO_DIVISION_CODE<-with(all6722, ifelse(NAFO_DIVISION_CODE%in%c('5ZE'), '5Z', NAFO_DIVISION_CODE))

#Species_Sought (GEAR)
table(all6722$GEAR_DIRECT)
xtabs(LIVE_WT~GEAR_DIRECT, all6722)
all6722$GEAR_DIRECT_Original<-all6722$GEAR_DIRECT
all6722$GEAR_DIRECT<-with(all6722, ifelse(GEAR_DIRECT%in%c(" Lumpfish"," Red hake"," Tilefish",' Turbot','BUTTER FISH/DOLLARFISH','GROUNDFISH, UNSPECIFIED','GREENLAND HALIBUT/TURBOT','RED HAKE','TILEFISH','LUMPFISH','DGX','FLO','FLW','SKA','WIT','USK'), 'Other Groundfish', GEAR_DIRECT))
all6722$GEAR_DIRECT<-with(all6722, ifelse(GEAR_DIRECT%in%c('ARGENTINE','ALEWIVES/GASPEREAU','FINS, FISH UNSPECIFIED ','HAGFISH (SLIME EEL)','HERRING','LIVERS, UNSPECIFIED','MACKEREL','OTHER FIN FISH UNSPECIFIED','SHAD','SHARK, BLUE','SHARK, MAKO','SHARK, PORBEAGLE/MACKEREL','SHARK, UNSPECIFIED','FINS, FISH UNSPECIFIED','SQUID, ILLEX','SQUID, UNSPECIFIED','STURGEON','SQUID, RED; LOLLIGO','TUNA, ALBACORE','ALB','ALE','ANG','ARG','DOL','FSK','GHL','GRO','HAG','HER','LUM','MAC','RNG','SBAR','SFL','SHA','SHB','SHM','SHP','SHX','SQR','SQU','SQW','STU','SWIN','TIL','ULV'), 'Non Groundfish', GEAR_DIRECT))
all6722$GEAR_DIRECT<-with(all6722, ifelse(GEAR_DIRECT%in%c(' Catfish','CAT'), 'CATFISH', GEAR_DIRECT))
all6722$GEAR_DIRECT<-with(all6722, ifelse(GEAR_DIRECT%in%c(' Cod'), 'COD', GEAR_DIRECT))
all6722$GEAR_DIRECT<-with(all6722, ifelse(GEAR_DIRECT%in%c(' Cusk'), 'CUSK', GEAR_DIRECT))
all6722$GEAR_DIRECT<-with(all6722, ifelse(GEAR_DIRECT%in%c(' Greysole'), 'GREYSOLE/WITCH', GEAR_DIRECT))
all6722$GEAR_DIRECT<-with(all6722, ifelse(GEAR_DIRECT%in%c(' Haddock','HAD'), 'HADDOCK', GEAR_DIRECT))
all6722$GEAR_DIRECT<-with(all6722, ifelse(GEAR_DIRECT%in%c(' Halibut','HAL'), 'HALIBUT', GEAR_DIRECT))
all6722$GEAR_DIRECT<-with(all6722, ifelse(GEAR_DIRECT%in%c(' Monkfish'), 'MONKFISH', GEAR_DIRECT))
all6722$GEAR_DIRECT<-with(all6722, ifelse(GEAR_DIRECT%in%c(' Plaice','PLA'), 'AMERICAN PLAICE', GEAR_DIRECT))
all6722$GEAR_DIRECT<-with(all6722, ifelse(GEAR_DIRECT%in%c(' Pollock','POL','POK'), 'POLLOCK', GEAR_DIRECT))
all6722$GEAR_DIRECT<-with(all6722, ifelse(GEAR_DIRECT%in%c(' Redfish','RED'), 'REDFISH', GEAR_DIRECT))
all6722$GEAR_DIRECT<-with(all6722, ifelse(GEAR_DIRECT%in%c(' Sculpin','SCU'), 'SCULPIN', GEAR_DIRECT))
all6722$GEAR_DIRECT<-with(all6722, ifelse(GEAR_DIRECT%in%c(' Silver hake','SLH','HKS'), 'SILVER HAKE', GEAR_DIRECT))
all6722$GEAR_DIRECT<-with(all6722, ifelse(GEAR_DIRECT%in%c(' Unspecified flounder','FLOUNDER, UNSPECIFIED'), 'FLOUNDER, UNSP', GEAR_DIRECT))
all6722$GEAR_DIRECT<-with(all6722, ifelse(GEAR_DIRECT%in%c('WOLFFISH, STRIPED','WOLFFISH, UNSPECIFIED'), 'WOLFISH', GEAR_DIRECT))
all6722$GEAR_DIRECT<-with(all6722, ifelse(GEAR_DIRECT%in%c(' White hake','WHITE HAKE','HKW'), 'WHITE HAKE', GEAR_DIRECT))
all6722$GEAR_DIRECT<-with(all6722, ifelse(GEAR_DIRECT%in%c(' Winter flounder','WIN','FLW'), 'WINTER FLOUNDER', GEAR_DIRECT))
all6722$GEAR_DIRECT<-with(all6722, ifelse(GEAR_DIRECT%in%c(' Yellowtail','YEL'), 'YELLOWTAIL', GEAR_DIRECT))
all6722$GEAR_DIRECT<-with(all6722, ifelse(GEAR_DIRECT%in%c(' Unspecified groundfish'), 'Other Groundfish', GEAR_DIRECT))
all6722$GEAR_DIRECT<-with(all6722, ifelse(GEAR_DIRECT%in%c('ROUNDNOSE GRENADIER',' Roundnose grenadier','AMERICAN PLAICE','CATFISH','FLOUNDER, UNSP','GREYSOLE/WITCH','SCULPIN','SKATE','SKATE, BARNDOOR','SKATE, WINTER','SUMMER FLOUNDER','WOLFISH','HKR','DOGFISH','MONKFISH'), 'Other Groundfish', GEAR_DIRECT))
round(xtabs(LIVE_WT~GEAR_DIRECT, all6722)/sum(all6722$LIVE_WT)*100)

#Cleaning up Gear
all6722$GEAR_TYPE_NEW_Original<-all6722$GEAR_TYPE_NEW
all6722$GEAR_TYPE_NEW<-with(all6722, ifelse(GEAR_TYPE_NEW%in%c('GILLNET','GN'), 'GILLNET', GEAR_TYPE_NEW))
all6722$GEAR_TYPE_NEW<-with(all6722, ifelse(GEAR_TYPE_NEW%in%c('Seine','SEINE'), 'SEINE', GEAR_TYPE_NEW))
all6722$GEAR_GROUP<-with(all6722, ifelse(GEAR_TYPE_NEW%in%c('GILLNET','HL','LL'), 'FIXED', GEAR_TYPE_NEW))
all6722$GEAR_GROUP<-with(all6722, ifelse(GEAR_TYPE_NEW%in%c('OTB','OTM','SEINE'), 'MOBILE', GEAR_GROUP))
all6722$GEAR_GROUP<-with(all6722, ifelse(GEAR_TYPE_NEW%in%c('OTHER','UNK'), 'OTHER/UNK', GEAR_GROUP))
all6722$GEAR_GROUP<-with(all6722, ifelse(GEAR_TYPE_NEW%in%c('OTHER','UNK'), 'FIXED', GEAR_GROUP))

#Grouping into likely categories
all6722$GEAR_DIRECT<-with(all6722, ifelse(GEAR_GROUP=='MOBILE'&GEAR_DIRECT%in%c('COD','HADDOCK','POLLOCK')&YEAR_OF_ACTIVITY<2019, 'CHP', GEAR_DIRECT))
all6722$GEAR_DIRECT<-with(all6722, ifelse(GEAR_GROUP=='MOBILE'&GEAR_DIRECT%in%c('COD','HADDOCK','POLLOCK')&YEAR_OF_ACTIVITY>2018&NAFO_DIVISION_CODE=='5Z', 'CHP', GEAR_DIRECT))
all6722$GEAR_DIRECT<-with(all6722, ifelse(GEAR_GROUP=='MOBILE'&GEAR_DIRECT%in%c('HADDOCK','POLLOCK')&YEAR_OF_ACTIVITY>2018&NAFO_DIVISION_CODE=='4X5Y', 'HP', GEAR_DIRECT))
all6722$GEAR_DIRECT<-with(all6722, ifelse(GEAR_GROUP=='FIXED'&GEAR_DIRECT%in%c('COD','HADDOCK','POLLOCK','HALIBUT'), 'CHPHAL', GEAR_DIRECT))

#Species_Landed
all6722$SPECIES_DESC_Original<-all6722$SPECIES_DESC
all6722$SPECIES_DESC<-with(all6722, ifelse(SPECIES_DESC%in%c('ALB','ALE','ANG','ARG','HER',' Albacore tuna',' Alewife',' Argentine',' Bluefin tuna',' Exploratory scallop',' Fish fins',' Herring',' Jonah crab',' Lobster',' Mackerel',' Mussel',' Rock crab',' Roe',' Scallop',' Shad',' Shark, blue',' Shark, hammerhead',' Shark, mackerel',' Shark, porbeagle',' Smelt',' Squid',' Striped bass',' Sturgeon',' Swordfish',' Tongues, cheeks',' Unspecified crab',' Unspecified fin fish',' Unspecified liver',' Unspecified mollusc',' Unspecified pelagic',' Unspecified shark',' Unspecified tuna',' Whale, minke',' Winkle',' Yellowfin tuna','263','626','956','ALEWIVES/GASPEREAU','ARGENTINE','EEL','FINS, FISH UNSPECIFIED','HAGFISH (SLIME EEL)','HERRING','LIVERS, UNSPECIFIED','MACKEREL','MAHI MAHI (DOLPHIN)','OTHER FIN FISH UNSPECIFIED','PELAGIC, UNSPECIFIED','ROES','SCALLOP, SEA','SHAD','SHARK, BLUE','SHARK, MAKO','SHARK, PORBEAGLE/MACKEREL','SHARK, THRESHER','SHARK, UNSPECIFIED','SMELTS','SNAKEBLENY/ROCK EEL','SQUID, ILLEX','SQUID, RED; LOLLIGO','SQUID, UNSPECIFIED','STRIPED BASS','STURGEON','SWORDFISH','TONGUE, SOUNDS AND CHEEKS','TUNA, ALBACORE','BUTTER FISH/DOLLARFISH','NON GROUNDFISH','ALB','ALE','ANG','ARG','DOL','ELA','FSK','GHL','GRO','HER','LUM','MAC','MAH','PEL','REE','RNG','ROE','SARC','SBAR','SCX','SFL','SHA','SHB','SHM','SHP','SHX','SLIT','SMR','SQU','SQR','SQW','STB','STU','SWO','TIL','ULV','USK'), 'Non Groundfish', SPECIES_DESC))
all6722$SPECIES_DESC<-toupper(all6722$SPECIES_DESC)
all6722$SPECIES_DESC<-with(all6722, ifelse(SPECIES_DESC%in%c('SKATE, ARCTIC','SKATE, BARNDOOR','SKATE, LITTLE','SKATE, WINTER'), 'SKATE', SPECIES_DESC))
all6722$SPECIES_DESC<-with(all6722, ifelse(SPECIES_DESC%in%c('CAT',' CATFISH',' CATFISH, SPOTTED','CATFISH'), 'CATFISH', SPECIES_DESC))
all6722$SPECIES_DESC<-with(all6722, ifelse(SPECIES_DESC%in%c(' COD',' COD LIVER'), 'COD', SPECIES_DESC))
all6722$SPECIES_DESC<-with(all6722, ifelse(SPECIES_DESC%in%c(' CUSK'), 'CUSK', SPECIES_DESC))
all6722$SPECIES_DESC<-with(all6722, ifelse(SPECIES_DESC%in%c(' DOGFISH','DOG'), 'DOGFISH', SPECIES_DESC))
all6722$SPECIES_DESC<-with(all6722, ifelse(SPECIES_DESC%in%c(' GREYSOLE'), 'GREYSOLE/WITCH', SPECIES_DESC))
all6722$SPECIES_DESC<-with(all6722, ifelse(SPECIES_DESC%in%c(' HADDOCK','HAD'), 'HADDOCK', SPECIES_DESC))
all6722$SPECIES_DESC<-with(all6722, ifelse(SPECIES_DESC%in%c(' HALIBUT','HAL','HAG'), 'HALIBUT', SPECIES_DESC))
all6722$SPECIES_DESC<-with(all6722, ifelse(SPECIES_DESC%in%c(' LUMPFISH'), 'LUMPFISH', SPECIES_DESC))
all6722$SPECIES_DESC<-with(all6722, ifelse(SPECIES_DESC%in%c(' MONKFISH'), 'MONKFISH', SPECIES_DESC))
all6722$SPECIES_DESC<-with(all6722, ifelse(SPECIES_DESC%in%c(' PLAICE','PLA'), 'AMERICAN PLAICE', SPECIES_DESC))
all6722$SPECIES_DESC<-with(all6722, ifelse(SPECIES_DESC%in%c(' POLLOCK','POL','POK'), 'POLLOCK', SPECIES_DESC))
all6722$SPECIES_DESC<-with(all6722, ifelse(SPECIES_DESC%in%c(' RED HAKE'), 'RED HAKE', SPECIES_DESC))
all6722$SPECIES_DESC<-with(all6722, ifelse(SPECIES_DESC%in%c(' REDFISH','RED'), 'REDFISH', SPECIES_DESC))
all6722$SPECIES_DESC<-with(all6722, ifelse(SPECIES_DESC%in%c(' ROUNDNOSE GRENADIER'), 'ROUNDNOSE GRENADIER', SPECIES_DESC))
all6722$SPECIES_DESC<-with(all6722, ifelse(SPECIES_DESC%in%c(' SCULPIN','SCU'), 'SCULPIN', SPECIES_DESC))
all6722$SPECIES_DESC<-with(all6722, ifelse(SPECIES_DESC%in%c(' SILVER HAKE','SLH','HKS'), 'SILVER HAKE', SPECIES_DESC))
all6722$SPECIES_DESC<-with(all6722, ifelse(SPECIES_DESC%in%c(' SKATE'), 'SKATE', SPECIES_DESC))
all6722$SPECIES_DESC<-with(all6722, ifelse(SPECIES_DESC%in%c(' TILEFISH'), 'TILEFISH', SPECIES_DESC))
all6722$SPECIES_DESC<-with(all6722, ifelse(SPECIES_DESC%in%c(' WHITE HAKE','WHI','HKW'), 'WHITE HAKE', SPECIES_DESC))
all6722$SPECIES_DESC<-with(all6722, ifelse(SPECIES_DESC%in%c(' WINTER FLOUNDER','WIN','FLW'), 'WINTER FLOUNDER', SPECIES_DESC))
all6722$SPECIES_DESC<-with(all6722, ifelse(SPECIES_DESC%in%c(' YELLOWTAIL','YEL'), 'YELLOWTAIL', SPECIES_DESC))
all6722$SPECIES_DESC<-with(all6722, ifelse(SPECIES_DESC%in%c(' TURBOT','GREENLAND HALIBUT/TURBOT'), 'HALIBUT', SPECIES_DESC))
all6722$SPECIES_DESC<-with(all6722, ifelse(SPECIES_DESC%in%c(' UNSPECIFIED FLOUNDER'), 'FLOUNDER, UNSPECIFIED', SPECIES_DESC))
all6722$SPECIES_DESC<-with(all6722, ifelse(SPECIES_DESC%in%c(' TOMCOD',' UNSPECIFIED GROUNDFISH','160','161','DOGFISH','GROUNDFISH, UNSPECIFIED','LUMPFISH','MONKFISH','RED HAKE','ROUNDNOSE GRENADIER','SCULPIN','SKATE','SUMMER FLOUNDER','TILEFISH','WHITEFISH','WOLFFISH, NORTHERN','WOLFFISH, STRIPED','WOLFFISH, UNSPECIFIED','AMERICAN PLAICE','CATFISH','FLOUNDER, UNSPECIFIED','GREYSOLE/WITCH','DGX','FLO','WIT','HKR','SKA','SWIN','OTHER GROUNDFISH'), 'Other Groundfish', SPECIES_DESC))
round(xtabs(LIVE_WT~SPECIES_DESC, all6722)/sum(all6722$LIVE_WT)*100)

#Cleaning up NAFO units
all6722$NAFO<-toupper(all6722$NAFO)
table(all6722$NAFO)

#Sorting out YEars
all6722<-subset(all6722, YEAR_OF_ACTIVITY<2023)

#Sorting out Months
all6722$MONTH<-as.numeric(format(all6722$LAND_DATE, "%m"))

#Excluding HAGFISH
all6722<-subset(all6722, Species_Sought_Clean_New!="HAGFISH (SLIME EEL)")

keep(all6722, sure=TRUE)

#Generating Quota Year for 5Z
all6722$MONTH<-as.numeric(with(all6722, substr(LAND_DATE,6,7)))
all6722$QuotaYear<-with(all6722, ifelse(NAFO_DIVISION_CODE%in%c('4X5Y','4W')&MONTH%in%c(1,2,3), YEAR_OF_ACTIVITY-1, YEAR_OF_ACTIVITY))

#Impossible record. Changing month.
all6722$MONTH<-with(all6722, ifelse(NAFO_DIVISION_CODE=='5Z'&YEAR_OF_ACTIVITY>2000&MONTH==4, MONTH+1, MONTH)) #Impossible record. Changing it to May. 
all6722$MONTH<-with(all6722, ifelse(NAFO_DIVISION_CODE=='5Z'&YEAR_OF_ACTIVITY>2000&YEAR_OF_ACTIVITY<2018&MONTH==5, MONTH+1, MONTH))
names(all6722)
all6722_1<-all6722 %>% 
  drop_na(TONNAGE,GEAR_GROUP) #%>% 
#filter(SPECIES_CODE==100)

all6722_1$TONNAGE<-as.factor(all6722_1$TONNAGE)
unique(all6722_1$TONNAGE_CLASS_CODE)
unique(all6722_1$GEAR_TYPE_NEW)
names(all6722_1)
unique(all6722_1$Species_Desc_NGF)
unique(all6722_1$SPECIES_DESC_Original)
unique(all6722_1$SPECIES_DESC)
unique(all6722_1$NAFO_DIVISION_CODE)

