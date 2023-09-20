#############################
# 5Z Cod
# Survey Condition - Fultons vs LeCren
# September 2023
# Author: Caira Clark
#############################

library(ROracle)
library(ggplot2)
library(plyr)
library(dplyr)
library(here)
library(ggpubr)
library(here)


channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=oracle.username, password=oracle.password, oracle.dsn) 

#A and B values from the file Survey_LengthWeightRelationships using method from Noble and Clark 2019

##DFO Spring-----------------------------

#Load condition data from all survey years
springDFO <- ROracle::dbGetQuery(channel, paste("select c.year, a.fsex, a.fmat, b.strat, (100*(a.FWT)/(POWER(a.FLEN,3)))FultK, ((a.FWT)/((0.007292124*(POWER(a.FLEN, 3.058687 )))))LeCren from 
                                                groundfish.gsdet a, 
                                                groundfish.gsinf b, 
                                                groundfish.gsmissions c where 
                                                a.spec=10 and 
                                                c.season='SPRING' and 
                                                b.strat in ('5Z1','5Z2','5Z3','5Z4','5Z5','5Z6','5Z7','5Z8','5Z9') and 
                                                a.mission=b.mission and 
                                                a.setno=b.setno and 
                                                a.mission=c.mission"))

springDFO <- springDFO[springDFO$FSEX>0 & springDFO$FMAT>5 & springDFO$YEAR>1980,]
springDFO <- na.omit(springDFO)

#Calculate average fish condition post spawn
ps <- springDFO %>% 
  group_by(YEAR) %>%
  summarize(FultonK=mean(FULTK), 
            fSD=sd(FULTK), 
            fCV=(sd(FULTK)/mean(FULTK)*100),
            LeCren=mean(LECREN), 
            lSD=sd(LECREN), 
            lCV=(sd(LECREN)/mean(LECREN)*100))
mFK <- mean(ps$FultonK) ##This is the avgFultK value 
mLC <- mean(ps$LeCren) #This is the avgLeCren value

sprDFO <- ddply(springDFO,. (YEAR, FSEX), summarize, FultonK=mean(FULTK), fSD=sd(FULTK), fCV=(sd(FULTK)/mean(FULTK)*100), LeCren=mean(LECREN), lSD=sd(LECREN), lCV=(sd(LECREN)/mean(LECREN)*100), avgFultK=mFK, avgLeCren=mLC)
sprDFO$SEX <- with(sprDFO, ifelse(FSEX==1,'Male','Female'))

dfo_fulton <- ggplot(sprDFO, aes(YEAR, FultonK, col=SEX)) + 
  geom_point(size=2) + 
  geom_line(size=1) + 
  #geom_errorbar(aes(ymin=FultonK-fSD, ymax=FultonK+fSD), width=.2, position=position_dodge(.9)) +
  #facet_wrap(~source, ncol=1)+ 
  scale_color_manual(values = c('red','blue')) + 
  theme_bw() + 
  geom_line(aes(YEAR,avgFultK),linetype=2,size=1,col='black')+ylab('Fultons K Condition') +
  ggtitle("DFO") +
  ylim(0.8, 1.2)

dfo_lecren <- ggplot(sprDFO, aes(YEAR, LeCren, col=SEX)) + 
  geom_point(size=2) + 
  geom_line(size=1) + 
  #geom_errorbar(aes(ymin=LeCren-lSD, ymax=LeCren+lSD), width=.2, position=position_dodge(.9)) +
  #facet_wrap(~source, ncol=1)+ 
  scale_color_manual(values = c('red','blue')) + 
  theme_bw() + 
  geom_line(aes(YEAR,avgLeCren),linetype=2,size=1,col='black')+ylab('LeCren Condition') +
  ylim(0.8, 1.2)

dfo <- ggarrange(dfo_fulton, dfo_lecren, nrow=2, ncol=1, legend = "bottom", common.legend=TRUE)
dfo

#NMFS Surveys (WHOLE BANK)

#Calculate Fulton's K and LeCren

fallNMFS <- ROracle::dbGetQuery(channel, paste("select b.season ID, a.svspp SPEC, a.length FLEN, (a.indwt*1000) FWT, a.sex FSEX, b.year, c.stratum STRAT
                                from usnefsc.uss_detail a, 
                                usnefsc.uss_mstr_cruise b, 
                                usnefsc.uss_station c, 
                                usnefsc.nmfs5zjm d
                                where a.indwt is not null
                                and a.maturity in ('S', 'T', 'I')
                                and b.season in ('FALL')
                                and a.svspp='073'
                                and a.sex in ('1', '2')
                                and b.purpose_code='10'
                                and c.stratum in ('00130', '00140', '00150', '01160', '01170', '01180', '01190','01200','01210')
                                and c.cruise6=a.cruise6
                                and c.stratum=a.stratum
                                and c.tow=a.tow
                                and c.station=a.station
                                and d.strat=c.stratum
                                and b.cruise6=a.cruise6"))

fallNMFS$FULTK <- (100*fallNMFS$FWT)/(fallNMFS$FLEN^3)
fallNMFS$LECREN <- fallNMFS$FWT/(0.006250619*(fallNMFS$FLEN^3.115492))

fallNMFS <- na.omit(fallNMFS)

#Calculate average fish condition post spawn
ps <- fallNMFS%>% 
  group_by(YEAR) %>%
  summarize(FultonK=mean(FULTK), 
            fSD=sd(FULTK), 
            fCV=(sd(FULTK)/mean(FULTK)*100),
            LeCren=mean(LECREN), 
            lSD=sd(LECREN), 
            lCV=(sd(LECREN)/mean(LECREN)*100))
mFK <- mean(ps$FultonK) ##This is the avgFultK value 
mLC <- mean(ps$LeCren) #This is the avgLeCren value

fNMFS <- ddply(fallNMFS,. (YEAR, FSEX), summarize, FultonK=mean(FULTK), fSD=sd(FULTK), fCV=(sd(FULTK)/mean(FULTK)*100), LeCren=mean(LECREN), lSD=sd(LECREN), lCV=(sd(LECREN)/mean(LECREN)*100), avgFultK=mFK, avgLeCren=mLC)
fNMFS$SEX <- with(fNMFS, ifelse(FSEX==1,'Male','Female'))
fNMFS$YEAR <- as.numeric(fNMFS$YEAR)

fnmfs_fulton <- ggplot(fNMFS, aes(YEAR, FultonK, col=SEX)) + 
  geom_point(size=2) + 
  geom_line(size=1) + 
  #geom_errorbar(aes(ymin=FultonK-fSD, ymax=FultonK+fSD), width=.2, position=position_dodge(.9)) +
  #facet_wrap(~source, ncol=1)+ 
  scale_color_manual(values = c('red','blue')) + 
  theme_bw() + 
  geom_line(aes(YEAR,avgFultK),linetype=2,size=1,col='black')+ylab('Fultons K Condition') +
  ggtitle("NMFS Fall") +
  ylim(0.8, 1.2)

fnmfs_lecren <- ggplot(fNMFS, aes(YEAR, LeCren, col=SEX)) + 
  geom_point(size=2) + 
  geom_line(size=1) + 
  #geom_errorbar(aes(ymin=LeCren-lSD, ymax=LeCren+lSD), width=.2, position=position_dodge(.9)) +
  #facet_wrap(~source, ncol=1)+ 
  scale_color_manual(values = c('red','blue')) + 
  theme_bw() + 
  geom_line(aes(YEAR,avgLeCren),linetype=2,size=1,col='black')+ylab('LeCren Condition') +
  ylim(0.8, 1.2)

nmfsfall <- ggarrange(fnmfs_fulton, fnmfs_lecren, nrow=2, ncol=1, legend = "bottom", common.legend=TRUE)


sprNMFS <- ROracle::dbGetQuery(channel, paste("select b.season ID, a.svspp SPEC, a.length FLEN, (a.indwt*1000) FWT, a.sex FSEX, b.year, c.stratum STRAT
                                from usnefsc.uss_detail a, 
                                usnefsc.uss_mstr_cruise b, 
                                usnefsc.uss_station c, 
                                usnefsc.nmfs5zjm d
                                where a.indwt is not null
                                and a.maturity in ('S', 'T', 'I')
                                and b.season in ('SPRING')
                                and a.svspp='073'
                                and a.sex in ('1', '2')
                                and b.purpose_code='10'
                                and c.stratum in ('00130', '00140', '00150', '01160', '01170', '01180', '01190','01200','01210')
                                and c.cruise6=a.cruise6
                                and c.stratum=a.stratum
                                and c.tow=a.tow
                                and c.station=a.station
                                and d.strat=c.stratum
                                and b.cruise6=a.cruise6"))

sprNMFS$FULTK <- (100*sprNMFS$FWT)/(sprNMFS$FLEN^3)
sprNMFS$LECREN <- sprNMFS$FWT/(0.008389590*(sprNMFS$FLEN^3.020194))

sprNMFS <- na.omit(sprNMFS)

#Calculate average fish condition post spawn
ps <- sprNMFS%>% 
  group_by(YEAR) %>%
  summarize(FultonK=mean(FULTK), 
            fSD=sd(FULTK), 
            fCV=(sd(FULTK)/mean(FULTK)*100),
            LeCren=mean(LECREN), 
            lSD=sd(LECREN), 
            lCV=(sd(LECREN)/mean(LECREN)*100))
mFK <- mean(ps$FultonK) ##This is the avgFultK value 
mLC <- mean(ps$LeCren) #This is the avgLeCren value

sNMFS <- ddply(sprNMFS,. (YEAR, FSEX), summarize, FultonK=mean(FULTK), fSD=sd(FULTK), fCV=(sd(FULTK)/mean(FULTK)*100), LeCren=mean(LECREN), lSD=sd(LECREN), lCV=(sd(LECREN)/mean(LECREN)*100), avgFultK=mFK, avgLeCren=mLC)
sNMFS$SEX <- with(sNMFS, ifelse(FSEX==1,'Male','Female'))
sNMFS$YEAR <- as.numeric(sNMFS$YEAR)

snmfs_fulton <- ggplot(sNMFS, aes(YEAR, FultonK, col=SEX)) + 
  geom_point(size=2) + 
  geom_line(size=1) + 
  #geom_errorbar(aes(ymin=FultonK-fSD, ymax=FultonK+fSD), width=.2, position=position_dodge(.9)) +
  #facet_wrap(~source, ncol=1)+ 
  scale_color_manual(values = c('red','blue')) + 
  theme_bw() + 
  geom_line(aes(YEAR,avgFultK),linetype=2,size=1,col='black')+ylab('Fultons K Condition') +
  ggtitle("NMFS Spring")+
  ylim(0.8, 1.2)

snmfs_lecren <- ggplot(sNMFS, aes(YEAR, LeCren, col=SEX)) + 
  geom_point(size=2) + 
  geom_line(size=1) + 
  #geom_errorbar(aes(ymin=LeCren-lSD, ymax=LeCren+lSD), width=.2, position=position_dodge(.9)) +
  #facet_wrap(~source, ncol=1)+ 
  scale_color_manual(values = c('red','blue')) + 
  theme_bw() + 
  geom_line(aes(YEAR,avgLeCren),linetype=2,size=1,col='black')+
  ylab('LeCren Condition') +
  ylim(0.8, 1.2)

nmfsspring <- ggarrange(snmfs_fulton, snmfs_lecren, nrow=2, ncol=1, legend = "bottom", common.legend=TRUE)

#All plots combined

ggarrange(dfo, nmfsfall, nmfsspring, ncol=3, nrow=1)

ggsave(here("figures/Survey_Condition.png"), width=15, height=10, units="in")

#Data from all surveys combined in one plot

sprDFO$SURVEY <- "DFO"
fNMFS$SURVEY <- "NMFS Fall"
sNMFS$SURVEY <- "NMFS Spring"

allsurveys <- rbind(sprDFO, fNMFS, sNMFS)
allsurveys <- allsurveys %>% select(YEAR, SEX, FultonK, LeCren, SURVEY) %>% pivot_longer(!c(YEAR, SEX, SURVEY), names_to="MEASURE", values_to = "VALUE")
allsurveys$IDENTIFIER <- paste(allsurveys$SEX, allsurveys$MEASURE, sep="-")

allsurveys <- allsurveys %>% mutate(MEAN=case_when(SURVEY=="DFO" & MEASURE=="FultonK" ~ mean(sprDFO$avgFultK),
                                     SURVEY=="DFO" & MEASURE=="LeCren" ~ mean(sprDFO$avgLeCren),
                                     SURVEY=="NMFS Spring" & MEASURE=="FultonK" ~ mean(sNMFS$avgFultK),
                                     SURVEY=="NMFS Spring" & MEASURE=="LeCren" ~ mean(sNMFS$avgLeCren),
                                     SURVEY=="NMFS Fall" & MEASURE=="FultonK" ~ mean(fNMFS$avgFultK),
                                     SURVEY=="NMFS Fall" & MEASURE=="LeCren" ~ mean(fNMFS$avgLeCren)))


ggplot(allsurveys, aes(YEAR, VALUE, col=IDENTIFIER)) + 
  geom_point(size=2) + 
  geom_line(size=1) + 
  #geom_errorbar(aes(ymin=FultonK-fSD, ymax=FultonK+fSD), width=.2, position=position_dodge(.9)) +
  facet_wrap(~SURVEY, ncol=1, scales="free")+ 
  scale_color_viridis_d(option="turbo", end=0.8)+
  theme_bw() + 
  geom_line(aes(YEAR,MEAN, group=MEASURE, linetype=MEASURE), size=1,col='black') +
  ggtitle("Condition")+
  ylim(0.8, 1.2)

ggsave(here("figures/Survey_Condition.png"), width=10, height=7, units="in")

