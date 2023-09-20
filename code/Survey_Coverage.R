####################
# 5Z Cod Survey Sets per Strata
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

#DFO SPRING SURVEY----------------------

data <- dbGetQuery(channel, "select a.year, b.strat, b.setno from 
                    groundfish.gsmissions a, groundfish.gsinf b
                    where a.season in ('SPRING')
                    and a.year between ('1986') and ('2023')
                    and b.strat in ('5Z1', '5Z2', '5Z3', '5Z4', '5Z5', '5Z6', '5Z7', '5Z8', '5Z9')
                    and b.type in ('1')
                    and a.mission=b.mission")

data2 <- dbGetQuery(channel, "select a.year, b.strat, b.setno from 
                    groundfish.gsmissions a, groundfish.gsinf b
                    where a.season in ('SPRING')
                    and a.year in ('2022')
                    and b.strat in ('5Z1', '5Z2', '5Z3', '5Z4', '5Z5', '5Z6', '5Z7', '5Z8', '5Z9')
                    and b.type in ('5')
                    and a.mission=b.mission")

data <- rbind(data, data2)

summary <- data %>% group_by(YEAR, STRAT) %>% tally()
names(summary)[names(summary) == 'n'] <- 'SETS'

summary <- summary %>%
  mutate(
    planned = dplyr::case_when(
      STRAT=='5Z1' ~ 10,
      STRAT=='5Z2' ~ 35,
      STRAT=='5Z3' ~ 15,
      STRAT=='5Z4' ~ 15,
      STRAT=='5Z5' ~ 5,
      STRAT=='5Z6' ~ 12,
      STRAT=='5Z7' ~ 5,
      STRAT=='5Z8' ~ 3,
      STRAT=='5Z9' ~ 4)) %>%
  mutate(status = dplyr::case_when(
    SETS>=(planned*0.8) ~ "Complete",
    SETS<(planned*0.8) ~ "Incomplete"
  ))


plot1 <- ggplot(data=summary) + 
  geom_point(aes(x=STRAT,y=YEAR, size=SETS, colour=status)) +
  coord_flip() +
  theme_bw() +
  xlab("STRATA") +
  scale_colour_viridis_d(name="STATUS", end=0.8, option="turbo")+
  ggtitle("DFO Spring Survey")
plot1

#ggsave(here("figures/Survey_DFO_SetsperStrata.png"), height=5, width=10, units="in")

#NMFS Spring Survey------------------------

data <- dbGetQuery(channel, "select a.cruise6, a.est_year, a.stratum, a.tow from
                     usnefsc.uss_station a, usnefsc.uss_mstr_cruise b
                     where b.season in ('SPRING')
                     and b.purpose_code in ('10')
                     and a.est_year between ('1970') and ('2023')
                     and a.statype in ('1')
                     and a.shg < ('137')
                     and a.cruise6=b.cruise6
                     and a.stratum in ('01130', '01140', '01150', '01160', '01170', '01180', '01190', '01200', '01210')")

colnames(data)[2] <- "YEAR"
data$STRATUM <- as.factor(data$STRATUM)
data <- data%>%distinct()

summary <- data %>% group_by(YEAR, STRATUM) %>% tally()
names(summary)[names(summary) == 'n'] <- 'SETS'
summary$YEAR <- as.numeric(summary$YEAR)
summary$STRATUM <- as.factor(substr(summary$STRATUM, start = 2, stop = 5))

summary <- summary %>%
  mutate(
    planned = dplyr::case_when(
      STRATUM=='1130' ~ 10,
      STRATUM=='1140' ~ 4,
      STRATUM=='1150' ~ 3,
      STRATUM=='1160' ~ 14,
      STRATUM=='1170' ~ 4,
      STRATUM=='1180' ~ 4,
      STRATUM=='1190' ~ 9,
      STRATUM=='1200' ~ 6,
      STRATUM=='1210' ~ 4)) %>%
  mutate(status = dplyr::case_when(
    SETS>=(planned*0.8) ~ "Complete",
    SETS<(planned*0.8) ~ "Incomplete"
  ))

plot2 <- ggplot(data=summary) + 
  geom_point(aes(x=STRATUM,y=YEAR, size=SETS, colour=status)) +
  coord_flip() +
  theme_bw() +
  xlab("STRATA") +
  scale_colour_viridis_d(name="STATUS", end=0.8, option="turbo") +
  ggtitle("NMFS Spring Survey")
plot2

#ggsave(here("figures/Survey_NMFSSpring_SetsperStrata.png"), height=5, width=10, units="in")

#NMFS Fall Survey---------------------------

data <- dbGetQuery(channel, "select a.cruise6, a.est_year, a.stratum, a.tow from
                     usnefsc.uss_station a, usnefsc.uss_mstr_cruise b
                     where b.season in ('FALL')
                     and b.purpose_code in ('10')
                     and a.est_year between ('1970') and ('2023')
                     and a.statype in ('1')
                     and a.shg < ('137')
                     and a.cruise6=b.cruise6
                     and a.stratum in ('01130', '01140', '01150', '01160', '01170', '01180', '01190', '01200', '01210')")

colnames(data)[2] <- "YEAR"
data$STRATUM <- as.factor(data$STRATUM)
data <- data %>% distinct()

summary <- data %>% group_by(YEAR, STRATUM) %>% tally()
names(summary)[names(summary) == 'n'] <- 'SETS'
summary$YEAR <- as.numeric(summary$YEAR)
summary$STRATUM <- as.factor(substr(summary$STRATUM, start = 2, stop = 5))

summary <- summary %>%
  mutate(
    planned = dplyr::case_when(
      STRATUM=='1130' ~ 10,
      STRATUM=='1140' ~ 4,
      STRATUM=='1150' ~ 3,
      STRATUM=='1160' ~ 14,
      STRATUM=='1170' ~ 4,
      STRATUM=='1180' ~ 4,
      STRATUM=='1190' ~ 9,
      STRATUM=='1200' ~ 6,
      STRATUM=='1210' ~ 4)) %>%
  mutate(status = dplyr::case_when(
    SETS>=(planned*0.8) ~ "Complete",
    SETS<(planned*0.8) ~ "Incomplete"
  ))

plot3 <- ggplot(data=summary) + 
  geom_point(aes(x=STRATUM,y=YEAR, size=SETS, colour=status)) +
  coord_flip() +
  theme_bw() +
  xlab("STRATA") +
  scale_colour_viridis_d(name="STATUS", end=0.8, option="turbo") +
  ggtitle("NMFS Fall Survey")
plot3

#ggsave(here("figures/Survey_NMFSSpring_SetsperStrata.png"), height=5, width=10, units="in")

library(ggpubr)
ggarrange(plot1, plot2, plot3, ncol=1, nrow=3, common.legend=TRUE, legend="bottom")

ggsave(here("figures/Survey_Coverage.png"), width=8.5, height=11, units="in")

#Table of sets for NMFS survey

data1 <- dbGetQuery(channel, "select a.cruise6, a.est_year, a.stratum, a.tow, b.season from
                     usnefsc.uss_station a, usnefsc.uss_mstr_cruise b
                     where b.season in ('SPRING')
                     and b.purpose_code in ('10')
                     and a.est_year between ('1970') and ('2023')
                     and a.statype in ('1')
                     and a.shg < ('137')
                     and a.cruise6=b.cruise6
                     and a.stratum in ('01130', '01140', '01150', '01160', '01170', '01180', '01190', '01200', '01210')")

data2 <- dbGetQuery(channel, "select a.cruise6, a.est_year, a.stratum, a.tow, b.season from
                     usnefsc.uss_station a, usnefsc.uss_mstr_cruise b
                     where b.season in ('FALL')
                     and b.purpose_code in ('10')
                     and a.est_year between ('1970') and ('2023')
                     and a.statype in ('1')
                     and a.shg < ('137')
                     and a.cruise6=b.cruise6
                     and a.stratum in ('01130', '01140', '01150', '01160', '01170', '01180', '01190', '01200', '01210')")

data <- rbind(data1, data2)
data <- data %>% distinct()
colnames(data)[2] <- "YEAR"
data$STRATUM <- as.factor(data$STRATUM)

summary <- data %>% group_by(YEAR, SEASON, STRATUM) %>% tally()
names(summary)[names(summary) == 'n'] <- 'SETS'
summary$YEAR <- as.numeric(summary$YEAR)
summary$STRATUM <- as.factor(substr(summary$STRATUM, start = 2, stop = 5))

nmfs_sets <- summary %>% pivot_wider(names_from = STRATUM, values_from = SETS)

write.csv(nmfs_sets, here("data/nmfs_setnumbers.csv"))
