#############################
# 5Z Cod
# Survey Condition - LeCren
# July 1, 2021
# Author: Melanie Barrett & Caira Clark
#############################

setwd("~/LocalRespository/EGBFramework/data/Condition")

setwd("S:/Science/Population Ecology/Georges Bank/5Zjm Haddock/Y2023/TRAC 2023/haddock condition/DFO/")
od<-path<-"~/LocalRespository/EGBFramework/data/Condition/"

#this program is to estimate a, b parameters using random effect models variation by cort) and nlme(mixed effect model)
plotf="png"
library(ggplot2)
library(xlsx)
library(magrittr) #enable %>%
library(dplyr) #mutate function
library(mgcv)

#read in data
EGB<-read.csv("S:/Science/Population Ecology/Georges Bank/5Zjm Haddock/Y2023/TRAC 2023/haddock condition/DFO/update DFO.csv")
head(EGB)
EGB %<>% mutate(wt=FWT/1000,logwt=log10(wt),loglen=log10(FLEN),k=wt/FLEN^3*10^5)

nrow(EGB) #29688 data points
plot(k~year, data=EGB)
plot(k~FLEN, data=EGB)
hist(EGB$k)

#remove outliers
kdist<-EGB$k
quantile(kdist, probs=c(0.005,0.995)) #99%, quantile value at 1.332, 0.714
wl<-EGB[EGB$k<1.3246625 &EGB$k>0.7168867,] #dropped 283 points, kept 28103 points

#remove 0s in sex and mat
wl%<>%filter(FSEX!=0)%<>% filter(FMAT!=0)
nrow(wl) #kept 29270 points
head(wl)

#remove NAs
wl<-na.omit(wl)
nrow(wl) #kept 23423 points
#count total numbers of rows by year
table(wl$year) #good numbers for each year

save(wl, file= paste0(od,"wl_NA.Rdata"))
load(file=paste0(od,"wl_NA.Rdata"))

#outliers that are immature for ages 3+
wl2<-wl
wl3<-wl2[!((wl2$FMAT==1)&(wl2$AGE>3)),] 
nrow(wl3) #dropped 20 points, left 22403
#plot
plot(k~year, data=wl3)
plot(k~FLEN, data=wl3)
hist(wl3$k)

save(wl3, file= paste0(od,"wl_clean.Rdata"))
load(file=paste0(od,"wl_clean.Rdata"))

#finished data cleaning
############################
### caculate Kn
###########################
# %>% means "then."

lm1 <- lm(formula=logwt~loglen,data=wl3)
summary(lm1)  #lna=-5.171, b=3.09879
#plot(lm1)

#fitted(lm1)

#calculate kn
wlkn<-wl3 %>% mutate(predW=10^fitted(lm1),Kn=wt/predW)
head(wlkn)
write.csv(wlkn, file=paste0(od, "Kn.csv"), row.names=T)
save(wlkn, file= paste0(od,"wlkn.Rdata"))
load(file=paste0(od,"wlkn.Rdata"))  #the final clean data with Kn


###################################################
# 0	Undetermined
#1	Immature
#2	Ripening 1
#3	Ripening 2
#4	Ripe (Mature)
#5	Spawning (Running)
#6	Spent
#7	Recovering
#8	Resting
###################################################

#%>% means "then."



###########################################################
###########################################################
#         GLM, ages 1-12, fewer samples in older fish
###########################################################
###########################################################
#library(lme4)
names(wlkn)
str(wlkn)

##age cut at 10
wlkn.cut<-subset(wlkn,AGE<11)
save(wlkn.cut, file= paste0(od,"wlkn.cut.Rdata"))
load(file=paste0(od,"wlkn.cut.Rdata"))  #the final clean data with Kn


###################################################
#####mod2, linear function, year as factor
######mod2 is the best model

mod2<-gam(Kn~factor(year)+FSEX+factor(week)+s(AGE), data=wlkn.cut, select=TRUE,method="REML")
summary(mod2) #17.5%
save(mod2, file= paste0(od,"data_model.rda"), compress='xz')
load(file= paste0(od,"data_model.rda"))

#plot residuals
wlkn.cut$res2<-mod2$residuals
windows()
boxplot(res2~year, data=wlkn.cut); abline(h=0,col="red")
savePlot(paste0(od,"res mod2.", plotf), type=plotf)

nyear<-length(unique(wlkn.cut$year))
#pred: year, femal, week 9 and age 4
pdat<-data.frame(year=1987:2023, FSEX=rep(2,nyear),week=rep(9,nyear),AGE=rep(4,nyear))
mod2.p<-predict.gam(mod2,pdat,se=TRUE)
mod2.p
#save to a file
pred.mod2<-data.frame(cbind(1987:2023,mod2.p$fit,mod2.p$se.fit))
colnames(pred.mod2)=c("year","pred","se")
#round(pred.mod2, digits=4)
write.csv(pred.mod2, file=paste0(od, "pred mod2.csv"), row.names=F)
save(pred.mod2, file= paste0(od,"pred mod2.Rdata"))
load(file=paste0(od,"pred mod2.Rdata"))

windows()
plot(1987:2023,mod2.p$fit, xlab="year", ylab="Kn",type="b")#, ylim=c(0.9,1.1))
lines(1987:2023,(mod2.p$fit+2*mod2.p$se.fit), col="red", lty="dotted")
lines(1987:2023,(mod2.p$fit-2*mod2.p$se.fit), col="red", lty="dotted")
savePlot(paste0(od,"pred mod2.", plotf), type=plotf)

#######################################
#### by sex
#######################################
wlkn.M<-wlkn.cut[wlkn.cut$FSEX==1,]
head(wlkn.M)
wlkn.F<-wlkn.cut[wlkn.cut$FSEX==2,]
####linear function, year as factor
#male
wlkn.M$FMAT=as.numeric(wlkn.M$FMAT)
wlkn.M$year=as.numeric(wlkn.M$year)
wlkn.M$AGE=as.numeric(wlkn.M$AGE)
mod.M1<-gam(Kn~s(FMAT,year, k=5)+s(AGE, k=3), data=wlkn.M, select=TRUE,method="REML")
mod.M<-gam(Kn~factor(year)+factor(AGE)+factor(FMAT)+factor(week), data=wlkn.M, method="REML")
summary(mod.M) #17.5%, week is not significant, mat3 is significant
save(mod.M, file= paste0(od,"data_model_M.rda"), compress='xz')
load(file= paste0(od,"data_model_M.rda"))

#plot residuals
wlkn.M$res<-mod.M$residuals
windows()
boxplot(res~year, data=wlkn.M); abline(h=0,col="red")
savePlot(paste0(od,"res mod_M.", plotf), type=plotf)

nyear<-length(unique(wlkn.M$year))
#pred: year, mat 6(spent) and age 4, week=12
pdat<-data.frame(year=1987:2023, FMAT=rep(6,nyear),AGE=rep(4,nyear), week=rep(12,nyear))
modM.p<-predict.gam(mod.M,pdat,se=TRUE)
modM.p
#save to a file
pred.modM<-data.frame(cbind(1987:2023,modM.p$fit,modM.p$se.fit))
colnames(pred.modM)=c("year","pred","se")
#round(pred.mod2, digits=4)
write.csv(pred.modM, file=paste0(od, "pred modM.csv"), row.names=F)
save(pred.modM, file= paste0(od,"pred modM.Rdata"))
load(file=paste0(od,"pred modM.Rdata"))

windows()
plot(1987:2023,modM.p$fit, xlab="year", ylab="Kn",type="b", col="blue",ylim=c(0.85,1.05))
lines(1987:2023,(modM.p$fit+2*modM.p$se.fit), col="blue", lty="dotted")
lines(1987:2023,(modM.p$fit-2*modM.p$se.fit), col="blue", lty="dotted")
savePlot(paste0(od,"pred modM.", plotf), type=plotf)

#female
mod.F<-gam(Kn~factor(year)+factor(FMAT)+factor(AGE)+factor(week), data=wlkn.F, select=TRUE,method="REML")
summary(mod.F) #26.4%, only week 10 is not significant
save(mod.F, file= paste0(od,"data_model_F.rda"), compress='xz')
load(file= paste0(od,"data_model_F.rda"))

#plot residuals
wlkn.F$res<-mod.F$residuals
windows()
boxplot(res~year, data=wlkn.F); abline(h=0,col="red")
savePlot(paste0(od,"res mod_F.", plotf), type=plotf)

nyear<-length(unique(wlkn.F$year))
#pred: year, mat6(spent) and age 4, week 12
pdat<-data.frame(year=1987:2023, FMAT=rep(6,nyear),AGE=rep(4,nyear),week=rep(12,nyear) )
modF.p<-predict.gam(mod.F,pdat,se=TRUE)
modF.p
#save to a file
pred.modF<-data.frame(cbind(1987:2023,modF.p$fit,modF.p$se.fit))
colnames(pred.modF)=c("year","pred","se")
#round(pred.mod2, digits=4)
write.csv(pred.modF, file=paste0(od, "pred modF.csv"), row.names=F)
save(pred.modF, file= paste0(od,"pred modF.Rdata"))
load(file=paste0(od,"pred modF.Rdata"))

windows()
plot(c(1987:2023),modF.p$fit, xlab="year", ylab="Kn",type="b",col="red", ylim=c(0.80,1.15))
lines(1987:2023,(modF.p$fit+2*modF.p$se.fit), col="red", lty="dotted")
lines(1987:2023,(modF.p$fit-2*modF.p$se.fit), col="red", lty="dotted")
savePlot(paste0(od,"pred modF.", plotf), type=plotf)

windows()

modF.p_no2022 <- modF.p
modF.p_no2022$fit[36] <- NA
modF.p_no2022$se.fit[36] <- NA
modM.p_no2022 <- modM.p
modM.p_no2022$fit[36] <- NA
modM.p_no2022$se.fit[36] <- NA

plot(c(1987:2023),modF.p_no2022$fit, xlab="year", ylab="Kn",type="b",col="red", ylim=c(0.80,1.15))
lines(1987:2023,(modF.p_no2022$fit+2*modF.p_no2022$se.fit), col="red", lty="dotted")
lines(1987:2023,(modF.p_no2022$fit-2*modF.p_no2022$se.fit), col="red", lty="dotted")
points(2023,(modF.p_no2022$fit[37]-2*modF.p_no2022$se.fit[37]), col="red", pch='-')
points(2023,(modF.p_no2022$fit[37]+2*modF.p_no2022$se.fit[37]), col="red", pch='-')
lines(c(1987:2023),modM.p_no2022$fit, xlab="year", ylab="Kn",type="b",col="blue", ylim=c(0.80,1.15))
lines(1987:2023,(modM.p_no2022$fit+2*modM.p_no2022$se.fit), col="blue", lty="dotted")
lines(1987:2023,(modM.p_no2022$fit-2*modM.p_no2022$se.fit), col="blue", lty="dotted")
points(2023,(modM.p_no2022$fit[37]-2*modM.p_no2022$se.fit[37]), col="blue", pch='-')
points(2023,(modM.p_no2022$fit[37]+2*modM.p_no2022$se.fit[37]), col="blue", pch='-')
legend('topright',ncol=1, legend=c("male","female"), col=c('blue','red'), lty=c(1,1)) 


savePlot(paste0(od,"pred modMF.", plotf), type=plotf)


male<-ggplot(modF.p$fit, xlab="year", ylab="Kn",type="b",col="red", ylim=c(0.85,1.05))
lines(1987:2023,(modF.p$fit+2*modF.p$se.fit), col="red", lty="dotted")
lines(1987:2023,(modF.p$fit-2*modF.p$se.fit), col="red", lty="dotted")
savePlot(paste0(od,"pred modF.", plotf), type=plotf)
#####plot male and female together

windows()
plot(1987:2023,modF.p$fit, xlab="year", ylab="Kn",type="b", col="red")

lines(1987:2023,(modF.p$fit+2*modF.p$se.fit), col="red",lty="dotted")
lines(1987:2023,(modF.p$fit-2*modF.p$se.fit), col="red",lty="dotted")
plot(1987:2023,modM.p$fit, xlab="year", ylab="Kn",type="b", col="blue")
lines(1987:2023,(modM.p$fit+2*modM.p$se.fit), col="blue",lty="dotted")
lines(1987:2023,(modM.p$fit-2*modM.p$se.fit), col="blue",lty="dotted")
lines(1987:2023,modM.p$fit+2*modM.p$se.fit, type="b", col="blue")
legend('topright',ncol=1, legend=c("male","female"), col=c('blue','red'), lty=c(1,1)) 
savePlot(paste0(od,"pred modMF.", plotf), type=plotf)
condition<-ggplot()+ geom_point()

both<-read.csv("S:/Science/Population Ecology/Georges Bank/5Zjm Haddock/Y2023/TRAC 2023/haddock condition/DFO/condition_inputs.csv")
  plot<-ggplot(both, aes(x=year, y=pred, group=sex, color=sex)) + 
    geom_line() +
  geom_point()+ 
    g
  theme_bw()+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  scale_y_continuous(name="Abundance (thousands)") + 
  scale_x_continuous(name="Length(cm)",breaks = seq(0, 118, 5)) + 
  theme(legend.position = c(0.8,0.8))+ 
  theme(text=element_text(size=14)) + 
  theme(legend.text = element_text(size=14))+ 
  scale_fill_discrete(name="Year")+ 
  geom_line(data=TenYrAve, aes(variable, TenYrAve/1000, col=Year),linetype=2,size=1) + 
  ggtitle("DFO Spring")+ 
  guides(col="none", fill=guide_legend(title="Year"))+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()
lf_dfo
ggplot()+

windows()
plot(c(1992:2019,2021:2022),mod2.p$fit, xlab="year", ylab="Kn",type="b", ylim=c(1,1.30))
lines(c(1992:2019,2021:2022),(mod2.p$fit+2*mod2.p$se.fit), col="red", lty="dotted")
lines(c(1992:2019,2021:2022),(mod2.p$fit-2*mod2.p$se.fit), col="red", lty="dotted")
savePlot(paste0(od,"pred mod2.", plotf), type=plotf)
dev.off()
