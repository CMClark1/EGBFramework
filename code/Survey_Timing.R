####################
# 5Z Cod Survey Timing
# Caira Clark
# 16 August 2023
####################

library(ROracle)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(here)

channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=oracle.username, password=oracle.password, oracle.dsn) 

#DFO SPRING SURVEY-----------------------------------

##Set Types (Standard, Comparative, etc.)------------------------------

settype <- dbGetQuery(channel, "select a.mission, a.season, a.year, b.type from 
                    groundfish.gsmissions a, groundfish.gsinf b
                    where a.season in ('SPRING')
                    and a.year between ('1986') and ('2023')
                    and a.mission=b.mission")

settype %>% group_by(MISSION, YEAR, TYPE) %>% dplyr::summarise(n=n()) %>% arrange(YEAR) %>% pivot_wider(names_from = TYPE, values_from = n)

rm(settype)

#Survey Dates over time


dates1 <- dbGetQuery(channel, "select a.mission, a.year, c.unit,
                    to_char(b.sdate, 'yyyy-mm-dd') from 
                    groundfish.gsmissions a, groundfish.gsinf b, groundfish.gsarea2 c
                    where a.season in ('SPRING')
                    and a.year between ('1986') and ('2023')
                    and b.type in ('1')
                    and b.area=c.area
                    and a.mission=b.mission")

dates2 <- dbGetQuery(channel, "select a.mission, a.year, c.unit,
                    to_char(b.sdate, 'yyyy-mm-dd') from 
                    groundfish.gsmissions a, groundfish.gsinf b, groundfish.gsarea2 c
                    where a.season in ('SPRING')
                    and a.year in ('2022')
                    and b.type in ('5')
                    and b.area=c.area
                    and a.mission=b.mission")

dates <- rbind(dates1, dates2)

rm(dates1, dates2)

colnames(dates)[4] ="SDATE"
dates$DATE <- yday(dates$SDATE)
dates$MONTH <- as.factor(month(ymd(dates$SDATE)))

test <- dates %>% group_by(YEAR, MONTH, DATE, UNIT) %>% dplyr::summarize(n=n()) 
test$YEAR <- as.factor(test$YEAR)
test$UNIT <- substr(test$UNIT, start = 1, stop = 2)

minmax <- dates %>% group_by(YEAR) %>% 
  dplyr::summarize(start = min(DATE), end = max(DATE)) %>%
  gather(key=date_type, value=date, -YEAR)

minmax$YEAR <- as.factor(minmax$YEAR)

plot1 <- ggplot() +
  geom_hline(yintercept = 31, linetype="dotted", size=1) +
  geom_hline(yintercept = 58, linetype="dotted", size=1) +
  geom_hline(yintercept = 89, linetype="dotted", size=1) +
  geom_line(data=minmax, mapping=aes(x=YEAR, y=date), size=5, colour="grey") +
  geom_point(data=test, mapping=aes(x=YEAR, y=DATE, colour=UNIT)) +
  scale_colour_viridis_d(begin=0.4) +
  coord_flip() +
  theme_bw() +
  xlab("Survey Year")+
  ylab("Day of the Year") +
  annotate("text", x=39, y=43, label = "February") +
  annotate("text", x=39, y=72, label = "March") +
  annotate("text", x=39, y=95, label = "April") +
  annotate("text", x=40, y=95, label = " ") +
  ggtitle("DFO Spring Survey")
#xlim(1970, 2023)
plot1


#NMFS SPRING SURVEY-------------------------

#Survey Dates over time

dates <- dbGetQuery(channel, "select a.cruise6, a.est_year, a.est_month, a.begin_est_towdate, a.area from
                     usnefsc.uss_station a, usnefsc.uss_mstr_cruise b
                     where b.season in ('SPRING')
                     and b.purpose_code in ('10')
                     and a.est_year between ('1970') and ('2023')
                     and a.statype in ('1')
                     and a.shg < ('137')
                     and a.cruise6=b.cruise6
                     and a.area in ('522', '525', '561', '562', '551', '552')")

dates$UNIT <- "5Z"
colnames(dates)[4] ="SDATE"
colnames(dates)[2] ="YEAR"
colnames(dates)[3] ="MONTH"
dates$DATE <- yday(dates$SDATE)
dates$MONTH <- as.numeric(dates$MONTH)
dates$YEAR <- as.numeric(dates$YEAR)

test <- dates %>% group_by(YEAR, MONTH, DATE, UNIT) %>% dplyr::summarize(n=n()) 
test$YEAR <- as.factor(test$YEAR)

#ONLY GB MINMAX
minmax <- dates %>% group_by(YEAR) %>% 
  dplyr::summarize(start = yday(min(SDATE)), end = yday(max(SDATE))) %>%
  gather(key=date_type, value=date, -YEAR) %>%
  distinct()
minmax$YEAR <- as.factor(minmax$YEAR)

plot2 <- ggplot() +
  geom_hline(yintercept = 89, linetype="dotted", size=1) +
  geom_hline(yintercept = 129, linetype="dotted", size=1) +
  geom_line(data=minmax, mapping=aes(x=YEAR, y=date), size=4, colour="grey") +
  geom_point(data=test, mapping=aes(x=YEAR, y=DATE, colour=UNIT)) +
  scale_colour_viridis_d(begin=0.4) +
  coord_flip() +
  theme_bw() +
  xlab("Survey Year")+
  ylab("Day of the Year") +
  annotate("text", x=45, y=78, label = "March") +
  annotate("text", x=45, y=110, label = "April") +
  annotate("text", x=45, y=140, label = "May") +
  annotate("text", x=46, y=110, label = " ") +
  ggtitle("NMFS Spring Survey") +
  scale_colour_manual(values = c("#FFF700"))
plot2

#NMFS FALL SURVEY-------------------------

#Survey Dates over time

dates <- dbGetQuery(channel, "select a.cruise6, a.est_year, a.est_month, a.begin_est_towdate, a.area from
                     usnefsc.uss_station a, usnefsc.uss_mstr_cruise b
                     where b.season in ('FALL')
                     and b.purpose_code in ('10')
                     and a.est_year between ('1970') and ('2023')
                     and a.statype in ('1')
                     and a.shg < ('137')
                     and a.cruise6=b.cruise6
                     and a.area in ('522', '525', '561', '562', '551', '552')")

dates$UNIT <- "5Z"
colnames(dates)[4] ="SDATE"
colnames(dates)[2] ="YEAR"
colnames(dates)[3] ="MONTH"
dates$DATE <- yday(dates$SDATE)
dates$MONTH <- as.numeric(dates$MONTH)
dates$YEAR <- as.numeric(dates$YEAR)

test <- dates %>% group_by(YEAR, MONTH, DATE, UNIT) %>% dplyr::summarize(n=n()) 
test$YEAR <- as.factor(test$YEAR)

#ONLY GB MINMAX
minmax <- dates %>% group_by(YEAR) %>% 
  dplyr::summarize(start = yday(min(SDATE)), end = yday(max(SDATE))) %>%
  gather(key=date_type, value=date, -YEAR) %>%
  distinct()
minmax$YEAR <- as.factor(minmax$YEAR)

plot3 <- ggplot() +
  geom_hline(yintercept = 273, linetype="dotted", size=1) +
  geom_hline(yintercept = 304, linetype="dotted", size=1) +
  geom_line(data=minmax, mapping=aes(x=YEAR, y=date), size=4, colour="grey") +
  geom_point(data=test, mapping=aes(x=YEAR, y=DATE, colour=UNIT)) +
  scale_colour_viridis_d(begin=0.4) +
  coord_flip() +
  theme_bw() +
  xlab("Survey Year")+
  ylab("Day of the Year") +
  annotate("text", x=45, y=267, label = "Sept.") +
  annotate("text", x=45, y=290, label = "Oct.") +
  annotate("text", x=45, y=315, label = "Nov.") +
  annotate("text", x=46, y=310, label = " ") +
  ggtitle("NMFS Fall Survey") +
  scale_colour_manual(values = c("#FFF700"))
plot3

library(ggpubr)
ggarrange(plot1, plot2, plot3, ncol=3, nrow=1, common.legend=TRUE, legend="bottom")

ggsave(here("figures/Survey_Timing.png"), width=11, height=8.5, units="in")
