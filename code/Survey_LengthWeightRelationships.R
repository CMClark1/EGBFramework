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
library(tidyr)


channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=oracle.username, password=oracle.password, oracle.dsn) 

##DFO Spring-----------------------------

#Calculate As and Bs for length weight relationship using the method from Noble and Clark 2019

springDFO <- ROracle::dbGetQuery(channel, paste("select c.season ID, a.spec, a.flen, a.fwt, a.fsex, c.year, b.strat from 
                                                groundfish.gsdet a, 
                                                groundfish.gsinf b, 
                                                groundfish.gsmissions c where 
                                                a.spec=10 and 
                                                c.season='SPRING' and 
                                                c.year between ('2000') and ('2020') and
                                                b.strat in ('5Z1','5Z2','5Z3','5Z4','5Z5','5Z6','5Z7','5Z8','5Z9') and 
                                                a.mission=b.mission and 
                                                a.setno=b.setno and 
                                                a.mission=c.mission"))

springDFO$log.weight <- log(springDFO$FWT)
springDFO$log.length <- log(springDFO$FLEN)
springDFO$ID <- factor(springDFO$ID)
springDFO[is.na(springDFO) | springDFO=="Inf" | springDFO=="-Inf"] = NA

seplines2 <- ddply(springDFO, "SPEC",
                   function(mydf){
                     nsamp <- nrow(mydf)
                     fit <- lm( log.weight ~ log.length, data=mydf)
                     summary<-summary(fit)
                     mycoef1 <- summary$coefficients[1,1]
                     intercept<-exp(mycoef1)
                     se1 <- summary$coefficients[1,2]
                     if (length(summary$coefficients)<5){
                       mycoef2=NA
                       se2=NA
                     } else {
                       mycoef2 <- summary$coefficients[2,1]
                       se2<-summary$coefficients[2,2]
                     }
                     rsq <-summary$r.squared
                     rse<-summary$sigma
                     
                     myres <- c(nsamp, mycoef1,intercept,mycoef2, se1,se2, rsq,rse)
                     names(myres) <- c("Nsamp","ln.intercept","intercept","slope", "se.intercept", "se.slope", "r_square", "resid_error")
                     return(myres)
                   })
output.all<-seplines2
output.all$survey<-"DFO"
DFOoutput <- output.all

#FALL NMFS As and Bs

fallNMFS <- ROracle::dbGetQuery(channel, paste("select b.season ID, a.svspp SPEC, a.length FLEN, (a.indwt*1000) FWT, a.sex FSEX, b.year, c.stratum STRAT
                                from usnefsc.uss_detail a, 
                                usnefsc.uss_mstr_cruise b, 
                                usnefsc.uss_station c, 
                                usnefsc.nmfs5zjm d
                                where a.indwt is not null
                                and b.season in ('FALL')
                                and a.svspp='073'
                                and b.purpose_code='10'
                                and b.year between ('2000') and ('2020')
                                and c.stratum in ('00130', '00140', '00150', '01160', '01170', '01180', '01190','01200','01210')
                                and c.cruise6=a.cruise6
                                and c.stratum=a.stratum
                                and c.tow=a.tow
                                and c.station=a.station
                                and d.strat=c.stratum
                                and b.cruise6=a.cruise6"))

fallNMFS$log.weight <- log(fallNMFS$FWT)
fallNMFS$log.length <- log(fallNMFS$FLEN)
fallNMFS$ID <- factor(fallNMFS$ID)
fallNMFS[is.na(fallNMFS) | fallNMFS=="Inf" | fallNMFS=="-Inf"] = NA

seplines2 <- ddply(fallNMFS, "SPEC",
                   function(mydf){
                     nsamp <- nrow(mydf)
                     fit <- lm( log.weight ~ log.length, data=mydf)
                     summary<-summary(fit)
                     mycoef1 <- summary$coefficients[1,1]
                     intercept<-exp(mycoef1)
                     se1 <- summary$coefficients[1,2]
                     if (length(summary$coefficients)<5){
                       mycoef2=NA
                       se2=NA
                     } else {
                       mycoef2 <- summary$coefficients[2,1]
                       se2<-summary$coefficients[2,2]
                     }
                     rsq <-summary$r.squared
                     rse<-summary$sigma
                     
                     myres <- c(nsamp, mycoef1,intercept,mycoef2, se1,se2, rsq,rse)
                     names(myres) <- c("Nsamp","ln.intercept","intercept","slope", "se.intercept", "se.slope", "r_square", "resid_error")
                     return(myres)
                   })
output.all<-seplines2
output.all$survey<-"fallNMFS" 
fallNMFSoutput <- output.all

#Spring Survey As and Bs

sprNMFS <- ROracle::dbGetQuery(channel, paste("select b.season ID, a.svspp SPEC, a.length FLEN, (a.indwt*1000) FWT, a.sex FSEX, b.year, c.stratum STRAT
                                from usnefsc.uss_detail a, 
                                usnefsc.uss_mstr_cruise b, 
                                usnefsc.uss_station c, 
                                usnefsc.nmfs5zjm d
                                where a.indwt is not null
                                and b.season in ('SPRING')
                                and a.svspp='073'
                                and b.purpose_code='10'
                                and b.year between ('2000') and ('2020')
                                and c.stratum in ('00130', '00140', '00150', '01160', '01170', '01180', '01190','01200','01210')
                                and c.cruise6=a.cruise6
                                and c.stratum=a.stratum
                                and c.tow=a.tow
                                and c.station=a.station
                                and d.strat=c.stratum
                                and b.cruise6=a.cruise6"))

sprNMFS$log.weight <- log(sprNMFS$FWT)
sprNMFS$log.length <- log(sprNMFS$FLEN)
sprNMFS$ID <- factor(sprNMFS$ID)
sprNMFS[is.na(sprNMFS) | sprNMFS=="Inf" | sprNMFS=="-Inf"] = NA

seplines2 <- ddply(sprNMFS, "SPEC",
                   function(mydf){
                     nsamp <- nrow(mydf)
                     fit <- lm( log.weight ~ log.length, data=mydf)
                     summary<-summary(fit)
                     mycoef1 <- summary$coefficients[1,1]
                     intercept<-exp(mycoef1)
                     se1 <- summary$coefficients[1,2]
                     if (length(summary$coefficients)<5){
                       mycoef2=NA
                       se2=NA
                     } else {
                       mycoef2 <- summary$coefficients[2,1]
                       se2<-summary$coefficients[2,2]
                     }
                     rsq <-summary$r.squared
                     rse<-summary$sigma
                     
                     myres <- c(nsamp, mycoef1,intercept,mycoef2, se1,se2, rsq,rse)
                     names(myres) <- c("Nsamp","ln.intercept","intercept","slope", "se.intercept", "se.slope", "r_square", "resid_error")
                     return(myres)
                   })
output.all<-seplines2
output.all$survey<-"sprNMFS"
sprNMFSoutput <- output.all

alloutput <- rbind(DFOoutput, fallNMFSoutput, sprNMFSoutput)


#Graph length weight relationships for all surveys together

springDFO$ID <- "DFO"
fallNMFS$ID <- "NMFSFALL"
sprNMFS$ID <- "NMFSSPRING"

allsurveys <- rbind(springDFO, fallNMFS, sprNMFS)

logscale <- ggplot() +
  geom_point(data=allsurveys, aes(x=log.length, y=log.weight)) +
  geom_smooth(data=allsurveys, aes(x=log.length, y=log.weight), method='lm', colour="red") +
  facet_grid(~ID) +
  xlab("log(Length)") +
  ylab("log(Weight)") +
  theme_bw()
logscale

#Graph with data points

PREDLEN<-1:150
PRED <- data.frame(PREDLEN)
PRED$DFO <- 0.007292124*(PRED$PREDLEN^3.058687)
PRED$NMFSFALL <- 0.006250619*(PRED$PREDLEN^3.115492)
PRED$NMFSSPRING <- 0.008389590*(PRED$PREDLEN^3.020194)

PRED <- PRED%>%pivot_longer(!PREDLEN, names_to="ID", values_to ="values")

regscale <- ggplot() +
  geom_point(data=allsurveys, aes(x=FLEN, y=FWT)) +
  geom_path(data=PRED, aes(x=PREDLEN, y=values), colour="red") +
  facet_grid(~ID) +
  xlab("Length (cm)") +
  ylab("Weight(g)") +
  theme_bw()
regscale

ggarrange(regscale, logscale, ncol=1, nrow=2)

ggsave(here("figures/Survey_LengthWeightRelationship.png"), width=10, height=8, units="in")
