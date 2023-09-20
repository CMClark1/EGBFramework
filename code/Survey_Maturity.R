####################
# 5Z Cod Survey Maturity at Length and Age
# Melanie Barrett & Caira Clark
# 16 August 2023
####################

require(ggplot2)
require(ROracle)
require(sqldf)
require(dplyr)
require(ggpubr)
require(here)

channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=oracle.username, password=oracle.password, oracle.dsn) 

#Pull data - DFO Survey EGB

mat_dfo<-dbGetQuery(channel, "select b.year,b.season,c.strat,a.spec,a.fshno,a.FLEN,a.FWT,a.FSEX,a.FMAT,a.age,c.sdate,c.time,c.mission from groundfish.gsdet a, groundfish.gsmissions b, groundfish.gsinf c where A.SPEC=10 and b.season='SPRING' and c.strat in  ('5Z1','5Z2','5Z3','5Z4') and c.area in ('523','524') and b.year>('1985') and a.mission=b.mission and a.mission=c.mission and a.setno=c.setno")

mat_dfo<-subset(mat_dfo, !is.na(FMAT))
mat_dfo$MONTH<-as.numeric(with(mat_dfo, substr(SDATE, 6,7))) #Plucks month out of date
mat_dfo$DAY<-as.numeric(with(mat_dfo, substr(SDATE, 9,10))) #Plucks day out of date

#Pull data - NMFS Spring Survey EGB
mat_nmfs<-dbGetQuery(channel, "SELECT B.Year, A.INDWT, A.LENGTH, A.AGE, A.MATURITY, C.EST_MONTH, C.EST_DAY
                                  FROM USNEFSC.USS_MSTR_CRUISE B INNER JOIN (USNEFSC.USS_STATION C INNER JOIN USNEFSC.USS_DETAIL A ON (C.CRUISE6 = A.CRUISE6) AND (C.STRATUM = A.STRATUM) AND (C.TOW = A.TOW) AND (C.STATION = A.STATION)) ON B.CRUISE6 = A.CRUISE6
                                  WHERE (((A.LENGTH) Is Not Null) AND ((B.SEASON)='SPRING') AND ((A.SVSPP)='073') AND ((B.PURPOSE_CODE)='10') AND ((C.AREA) In ('551','552','561','562')) AND ((C.STRATUM) In ('01160','01170','01180','01190','01200','01210','01220')))
                                  ORDER BY B.Year, C.CRUISE6
                                  ")
#mat_nmfs<-read.csv(here("data/USmaturity.csv"))
mat_nmfs<-subset(mat_nmfs, !is.na(MATURITY))

mat_dfo$mature<-with(mat_dfo, ifelse(FMAT%in%c(3,4,5,6,7,8), 'MAT','IMMAT')) #For Canada, there are two ripening stages (2-3); 3 is considered mature, 2 is not. For US, there is only one 'developing', so put it in as mature.
mat_nmfs$mature<-with(mat_nmfs, ifelse(MATURITY%in%c('D','R','U','S','T'), 'MAT','IMMAT')) #See note above.

mat_dfo<-subset(mat_dfo, select=c('YEAR','FLEN','AGE','mature','MONTH','DAY'))
mat_nmfs<-subset(mat_nmfs, select=c('YEAR','LENGTH','AGE','mature','EST_MONTH','EST_DAY'))
names(mat_nmfs)<-names(mat_dfo)
mat_dfo$SOURCE<-'DFO EGB'
mat_nmfs$SOURCE<-'NMFS EGB'

#DFO whole GB data pull and formatting

mat_dfogb<-dbGetQuery(channel, "select b.year,b.season,c.strat,a.spec,a.fshno,a.FLEN,a.FWT,a.FSEX,a.FMAT,a.age,c.sdate,c.time,c.mission from groundfish.gsdet a, groundfish.gsmissions b, groundfish.gsinf c where A.SPEC=10 and b.season='SPRING' and c.strat in  ('5Z1','5Z2','5Z3','5Z4', '5Z5', '5Z6', '5Z7', '5Z8', '5Z9') and b.year>('1985') and c.area in ('522','523','524','525') and a.mission=b.mission and a.mission=c.mission and a.setno=c.setno")

mat_dfogb<-subset(mat_dfogb, !is.na(FMAT))
mat_dfogb$MONTH<-as.numeric(with(mat_dfogb, substr(SDATE, 6,7))) #Plucks month out of date
mat_dfogb$DAY<-as.numeric(with(mat_dfogb, substr(SDATE, 9,10))) #Plucks day out of date

mat_dfogb$mature<-with(mat_dfogb, ifelse(FMAT%in%c(3,4,5,6,7,8), 'MAT','IMMAT')) #For Canada, there are two ripening stages (2-3); 3 is considered mature, 2 is not. For US, there is only one 'developing', so put it in as mature.

mat_dfogb<-subset(mat_dfogb, select=c('YEAR','FLEN','AGE','mature','MONTH','DAY'))
mat_dfogb$SOURCE<-'DFO GB'

#Bind DFO and NMFS maturity data together
mat<-rbind(mat_dfo, mat_dfogb, mat_nmfs)
mat$PERIOD<-with(mat, ifelse(YEAR<1997, 'Pre-1997',YEAR))
mat$PERIOD<-with(mat, ifelse(YEAR>1996&YEAR<2003, '1997-2002',PERIOD))
mat$PERIOD<-with(mat, ifelse(YEAR>2002, '2003+',PERIOD))
mat$PERIOD<-with(mat, factor(PERIOD, levels=c('Pre-1997','1997-2002','2003+')))
unique(mat$mature)   

#####
mat$FISH<-1
temp<-aggregate(data=subset(mat, mature=="MAT"), FISH~FLEN+YEAR, FUN="sum")
temp2<-aggregate(data=mat, FISH~FLEN+YEAR, FUN="sum")
names(temp2)<-c('FLEN','YEAR','TOTAL')
temp<-merge(temp, temp2, all=TRUE)
temp$PropMat<-with(temp, FISH/TOTAL)

ggplot(temp, aes(FLEN, PropMat))+
  geom_point(aes(col=YEAR))+
  facet_wrap(~YEAR)+
  geom_vline(xintercept=45)

head(mat)
### Change the maturity column from a letter to 0 or 1. 0=immature, 1= mature

for(i in 1:nrow(mat))
{
  if(mat$mature[i]=="IMMAT")
  {
    mat$maturity[i]=0
  } else {
    mat$maturity[i]=1
  }
}

### Maturity at length
### calculating the logistic regression, if concerned with interaction of decade (slope not the same then use * to put as interaction term)

mat <- mat %>% mutate(decade = case_when(
  YEAR < 1980 ~ "1970s",
  YEAR < 1990 & YEAR > 1979 ~ "1980s",
  YEAR < 2000 & YEAR > 1989 ~ "1990s",
  YEAR < 2010 & YEAR > 1999 ~ "2000s",
  YEAR < 2020 & YEAR > 2000 ~ "2010s",
  YEAR < 2030 & YEAR > 2010 ~ "2020s"
))

bilogresdecade <- glm(maturity ~ FLEN * as.factor(decade), data = mat, family = "binomial")
summary(bilogresdecade)

lengthatmaturity <- ggplot(mat, aes(x=FLEN, y=maturity, colour=as.factor(decade))) + 
  #geom_point() + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  facet_wrap(SOURCE~.) +
  theme_bw()+
  ylab("Proportion at Maturity")+
  xlab("Length (cm)") +
  scale_color_viridis_d(name="Decade", option="turbo")

### calculating the logistic regression for maturity at age, if concerned with interaction of decade (slope not the same then use * to put as interaction term)

bilogresagedecade <- glm(maturity ~ AGE * as.factor(decade), data = mat, family = "binomial")
summary(bilogresagedecade)

ageatmaturity <- ggplot(mat, aes(x=AGE, y=maturity, colour=as.factor(decade))) + 
  #geom_point() + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  facet_wrap(SOURCE~.) +
  theme_bw()+
  ylab("Proportion at Maturity")+
  xlab("Age (years)") +
  scale_color_viridis_d(name="Decade", option="turbo")

library(ggpubr)
ggarrange(lengthatmaturity, ageatmaturity, ncol=1, nrow=2)

library(here)
ggsave(here("figures/Survey_Maturity.png"), width=10, height=10, units="in")


#WHOLE GEORGES BANK NMFS 

nmfs2 <- read.csv(here("data/NMFS.GB/maturity_ogive_GBK.csv"))

nmfs2 <- nmfs2 %>% mutate(decade = case_when(
  YEAR < 1980 ~ "1970s",
  YEAR < 1990 & YEAR > 1979 ~ "1980s",
  YEAR < 2000 & YEAR > 1989 ~ "1990s",
  YEAR < 2010 & YEAR > 1999 ~ "2000s",
  YEAR < 2020 & YEAR > 2000 ~ "2010s",
  YEAR < 2030 & YEAR > 2010 ~ "2020s"
)) %>% pivot_longer(!c(YEAR, AGE, decade), names_to="CALCULATION", values_to="PROP")

ggplot(data=nmfs2) + 
  geom_point(aes(x=AGE, y=PROP, colour=as.factor(decade))) +
  geom_smooth(aes(x=AGE, y=PROP, colour=as.factor(decade))) +
  theme_bw()+
  ylab("Proportion at Maturity")+
  xlab("Age (years)") +
  scale_color_viridis_d(name="Decade", option="turbo") +
  facet_grid(decade~CALCULATION)

ggsave(here("figures/Survey_Maturity_NMFS_GB.png"), width=8.5, height=11, units="in")
