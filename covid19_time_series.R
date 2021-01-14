#Xiaowen JI Covid-19 final-subsitute project scripts 

#install.packages("dplyr")
#install.packages("zoo")
#install.packages("astsa")
#install.packages("ggplot2")
#install.packages("xts")
library(xts)
library(zoo)
library(dplyr)
library(astsa)
library(ggplot2)
#library(tidyverse)
library(lubridate)
#CA

setwd("D:/Clinical_decision_support/covid-19 Project")
GlMobility <- read.csv('Global_Mobility_Report.csv')

#extract california_mobility data

CA_Mobi <- GlMobility %>% slice(104794:104868)

CA_Mobi <- rename(CA_Mobi,retail=retail_and_recreation_percent_change_from_baseline,
                  grocery=grocery_and_pharmacy_percent_change_from_baseline,
                  parks=parks_percent_change_from_baseline,
                  transit=transit_stations_percent_change_from_baseline,
                  workplace=workplaces_percent_change_from_baseline,
                  residence=residential_percent_change_from_baseline)

anyNA(CA_Mobi)
summary(CA_Mobi)


#compute one kind of mobility once a time/taking mobility trend toward residence as an example

tsCA_Mobi <- zoo(#CA_Mobi$retail,
                 #CA_Mobi$grocery,
                 #CA_Mobi$parks,
                 #CA_Mobi$transit,
                 #CA_Mobi$workplace,
                 CA_Mobi$residence,
                 seq(from = as.Date("2020-02-16"), 
                 to = as.Date("2020-04-30"), by = 1))


summary(tsCA_Mobi)

# Sicheng new_case data CA

setwd("D:/Clinical_decision_support/covid-19 Project")
California <- read.csv("California.csv")

CA_newconfirm <- select (California,date,new_confirmed)
tsCAnew <- zoo(CA_newconfirm$new_confirmed, seq(from = as.Date("2020-02-21"), 
               to = as.Date("2020-05-12"), by = 1))

anyNA(tsCAnew)
summary(tsCAnew)

#standalization-plot-on zoo objects of mobility_residence and CA_daily new cases
nortsCA_Mobi <- scale(tsCA_Mobi)
nortsCAnew <- scale(tsCAnew)



# comparing trend between mobility_residence and CA_daily new cases 
CAdata <-cbind(nortsCA_Mobi,nortsCAnew)
forZoo_CA1 <- fortify.zoo(CAdata,names = c("Index","Series","Value"),melt = TRUE)

s <- tibble(crazyfactor = ymd(c(forZoo_CA1$Index)), value=c(1:174)) %>%
  mutate(crazyfactor = as.factor(crazyfactor))
forZoo_CA1 <- forZoo_CA1%>% 
  # tibble(crazyfactor = ymd(c(forZoo_CA1$Index)), value=c(1:174)) %>%
  # mutate(crazyfactor = as.factor(crazyfactor)) %>%
  mutate(Series = as.character(Series)) %>% 
  mutate(Series = replace(Series, Series == 'nortsCA_Mobi', 'Residence'))%>%
  mutate(Series = replace(Series, Series == 'nortsCAnew', 'NewCases'))
ggplot(forZoo_CA1, aes(x = Index, y = Value, Group = Series)) +
  geom_line(aes(color = Series, linetype = Series,size = "qsec")) +
  # geom_point() +
  geom_vline(aes(xintercept = Index[which(levels(s$crazyfactor) == '2020-03-19')]), col = "black",linetype = "longdash") +theme_bw()


# week average 
MobiWeek <- apply.weekly(tsCA_Mobi, mean)
NewWeek <- apply.weekly (tsCAnew,mean)

nortsCA_Mobi1 <- scale(MobiWeek)
nortsCAnew1   <- scale(NewWeek)

CAWeek <-cbind(nortsCA_Mobi1,nortsCAnew1)
forZoo_CA2 <- fortify.zoo(CAWeek,names = c("Index","Series","Value"),melt = TRUE)
forZoo_CA2 <- forZoo_CA2%>% 
  mutate(Series = as.character(Series)) %>% 
  mutate(Series = replace(Series, Series == 'nortsCA_Mobi1', 'Residence'))%>%
  mutate(Series = replace(Series, Series == 'nortsCAnew1', 'NewCases'))
ggplot(forZoo_CA2,aes(x = Index, y = Value, Group = Series))+
  geom_line(aes(color = Series, linetype = Series,size = "qsec"))+ theme_bw()

#ccf (tsCA_Mobi,tsCAnew) 
ccf (tsCAnew,tsCA_Mobi) 

#lag2.plot(tsCA_Mobi,tsCAnew,14) #does not make sense
lag2.plot(tsCAnew,tsCA_Mobi,9)



# contrast with total_test data
setwd("D:/Clinical_decision_support/covid-19 Project")
CA_total_test <- read.csv("CA_total_test.csv") 
tsCAtotaltest <- zoo(CA_total_test$Total_test, seq(from = as.Date("2020-03-04"), 
                                                to = as.Date("2020-05-09"), by = 1))
anyNA(tsCAtotaltest)
summary(tsCAtotaltest)

norCAtotaltest <- scale(tsCAtotaltest)

CAdata1 <-cbind(nortsCA_Mobi,nortsCAnew,norCAtotaltest)
forZoo_CA3 <- fortify.zoo(CAdata1,names = c("Index","Series","Value"),melt = TRUE)
forZoo_CA3 <- forZoo_CA3%>% 
  mutate(Series = as.character(Series)) %>% 
  mutate(Series = replace(Series, Series == 'nortsCA_Mobi', 'residence'))%>%
  mutate(Series = replace(Series, Series == 'nortsCAnew', 'NewCases'))
ggplot(forZoo_CA3,aes(x = Index, y = Value, Group = Series))+
  geom_line(aes(color = Series, linetype = Series,size = "qsec"))+ theme_bw()


# weekly average
TotaltestWeek <- apply.weekly (tsCAtotaltest,mean)
nortotaltestweek <- scale(TotaltestWeek)

CAWeek1 <-cbind(nortsCA_Mobi1,nortsCAnew1,nortotaltestweek)
forZoo_CA4 <- fortify.zoo(CAWeek1,names = c("Index","Series","Value"),melt = TRUE)
forZoo_CA4 <- forZoo_CA4%>% 
  mutate(Series = as.character(Series)) %>% 
  mutate(Series = replace(Series, Series == 'nortsCA_Mobi1', 'residence'))%>%
  mutate(Series = replace(Series, Series == 'nortsCAnew1', 'NewCases'))
ggplot(forZoo_CA4,aes(x = Index, y = Value, Group = Series))+
  geom_line(aes(color = Series, linetype = Series,size = "qsec"))+ theme_bw()


# CA match tsCA_new_case and tsCA_mobi # before stay-at-home order
setwd("D:/Clinical_decision_support/covid-19 Project")
California <- read.csv("California.csv")

CA_newconfirm <- select (California,date,new_confirmed)
tsCAnew_match1 <- CA_newconfirm %>% slice (1:28)
tsCAnew_match1 <- zoo(tsCAnew_match1$new_confirmed, seq(from = as.Date("2020-02-21"), 
                                                         to = as.Date("2020-03-19"), by = 1))

CA_Mobi_match <-CA_Mobi%>% slice (6:33)
tsCA_Mobi_match <- zoo(#CA_Mobi$retail,
  #CA_Mobi$grocery,
  #CA_Mobi$parks,
  #CA_Mobi$transit,
  #CA_Mobi$workplace,
  CA_Mobi$residence,
  seq(from = as.Date("2020-02-21"), 
      to = as.Date("2020-03-19"), by = 1))

ccf (tsCA_Mobi_match,tsCAnew_match1) 

ccfvalues = ccf(tsCA_Mobi_match,tsCAnew_match1)
ccfvalues

lag2.plot (tsCA_Mobi_match,tsCAnew_match1,14)


# Sicheng new_case data
# CA match tsCA_new_case and tsCA_mobi # After stay-at-home order
setwd("D:/Clinical_decision_support/covid-19 Project")
California <- read.csv("California.csv")

CA_newconfirm <- select (California,date,new_confirmed)
tsCAnew_match2 <- CA_newconfirm %>% slice (29:70)
tsCAnew_match2 <- zoo(tsCAnew_match2$new_confirmed, seq(from = as.Date("2020-03-20"), 
                                                        to = as.Date("2020-04-30"), by = 1))

CA_Mobi_match2 <-CA_Mobi%>% slice (34:75)
tsCA_Mobi_match2 <- zoo(#CA_Mobi$retail,
  #CA_Mobi$grocery,
  #CA_Mobi$parks,
  #CA_Mobi$transit,
  #CA_Mobi$workplace,
  #CA_Mobi$residence,
  seq(from = as.Date("2020-03-20"), 
      to = as.Date("2020-04-30"), by = 1))

ccf (tsCA_Mobi_match2,tsCAnew_match2) 

ccfvalues = ccf(tsCA_Mobi_match2,tsCAnew_match2)
ccfvalues

lag2.plot (tsCAnew_match2,tsCA_Mobi_match2,14)





#FL

setwd("D:/Clinical_decision_support/covid-19 Project")
GlMobility <- read.csv('Global_Mobility_Report.csv')

FL_Mobi <- GlMobility %>% slice(113738:113812)

FL_Mobi <- rename(FL_Mobi,retail=retail_and_recreation_percent_change_from_baseline,
                  grocery=grocery_and_pharmacy_percent_change_from_baseline,
                  parks=parks_percent_change_from_baseline,
                  transit=transit_stations_percent_change_from_baseline,
                  workplace=workplaces_percent_change_from_baseline,
                  residence=residential_percent_change_from_baseline)
anyNA(FL_Mobi)

tsFL_Mobi <- zoo(#FL_Mobi$retail,
  #FL_Mobi$grocery,
  #FL_Mobi$parks,
  #FL_Mobi$transit,
  #FL_Mobi$workplace,
  FL_Mobi$residence,
  seq(from = as.Date("2020-02-16"), 
      to = as.Date("2020-04-30"), by = 1))

summary(tsFL_Mobi)

setwd("D:/Clinical_decision_support/covid-19 Project")
Florida <- read.csv("Florida.csv")

FL_newconfirm <- select (Florida,date,new_confirmed)
tsFLnew <- zoo(FL_newconfirm$new_confirmed, seq(from = as.Date("2020-03-08"), 
                                                      to = as.Date("2020-05-12"), by = 1))
anyNA (tsFLnew)
summary (tsFLnew)

ccf (tsFL_Mobi,tsFLnew) 

#lag2.plot(tsFLnew,tsFL_Mobi,14) does not make sense
lag2.plot(tsFLnew,tsFL_Mobi,7)

nortsFL_Mobi <- scale(tsFL_Mobi)
nortsFLnew <- scale(tsFLnew)

FLdata <-cbind(nortsFL_Mobi,nortsFLnew)
forZoo_FL1 <- fortify.zoo(FLdata,names = c("Index","Series","Value"),melt = TRUE)
s1 <- tibble(crazyfactor = ymd(c(forZoo_FL1$Index)), value=c(1:174)) %>%
  mutate(crazyfactor = as.factor(crazyfactor))
  forZoo_FL1 <- forZoo_FL1%>% 
  mutate(Series = as.character(Series)) %>% 
  mutate(Series = replace(Series, Series == 'nortsFL_Mobi',    'ResideceTrend'))%>%
  mutate(Series = replace(Series, Series == 'nortsFLnew', 'NewCases'))
ggplot(forZoo_FL1,aes(x = Index, y = Value, Group = Series))+geom_vline(aes(xintercept = Index[which(levels(s1$crazyfactor) == '2020-04-03')]), col = "black",linetype = "longdash") +
geom_line(aes(color = Series, linetype = Series))+ theme_bw()



# FL match tsFL_new_case and tsFL_mobi # before stay-at-home order Apirl 1
setwd("D:/Clinical_decision_support/covid-19 Project")
Florida <- read.csv("Florida.csv")

FL_newconfirm <- select (Florida,date,new_confirmed)
#tsFLnew_match <- FL_newconfirm %>% slice (1:24)
tsFLnew_match <- zoo(tsFLnew_match$new_confirmed, seq(from = as.Date("2020-03-08"), 
                                                        to = as.Date("2020-05-12"), by = 1))

FL_Mobi_match <-FL_Mobi%>% slice (22:44)
tsFL_Mobi_match <- zoo(#FL_Mobi$retail,
  #FL_Mobi$grocery,
  #FL_Mobi$parks,
  FL_Mobi$transit,
  #FL_Mobi$workplace,
  #FL_Mobi$residence,
  seq(from = as.Date("2020-03-08"), 
      to = as.Date("2020-03-31"), by = 1))

ccf (tsFL_Mobi_match,tsFLnew_match) 

ccfvalues = ccf(tsFL_Mobi_match,tsFLnew_match)
ccfvalues

lag2.plot (tsFL_Mobi_match,tsFLnew_match,10)

tsFLnew_match9 <- stats::lag(tsFL_Mobi_match,-9)# n=14 sample is too small
tsFL_Mobi_match9 <- stats::lag(tsFL_Mobi_match,9)

tryit = lm(tsFLnew_match9~tsFL_Mobi_match9)
summary (tryit)

acf2(residuals(tryit))
tryit2 = lm(tsFLnew_match9~tsFL_Mobi_match9)
summary (tryit2)




# Sicheng new_case data
# CA match tsCA_new_case and tsCA_mobi # After stay-at-home order
setwd("D:/Clinical_decision_support/covid-19 Project")
Florida <- read.csv("Florida.csv")

FL_newconfirm <- select (Florida,date,new_confirmed)
tsFLnew_match2 <- FL_newconfirm %>% slice (25:54)
tsFLnew_match2 <- zoo(tsFLnew_match2$new_confirmed, seq(from = as.Date("2020-04-01"), 
                                                        to = as.Date("2020-04-30"), by = 1))

FL_Mobi_match2 <-FL_Mobi%>% slice (46:75)
tsFL_Mobi_match2 <- zoo(#FL_Mobi$retail,
  #FL_Mobi$grocery,
  #FL_Mobi$parks,
  FL_Mobi$transit,
  #FL_Mobi$workplace,
  #FL_Mobi$residence,
  seq(from = as.Date("2020-04-01"), 
      to = as.Date("2020-04-30"), by = 1))

ccf (tsFL_Mobi_match2,tsFLnew_match2) 

ccfvalues = ccf(tsFL_Mobi_match2,tsFLnew_match2)
ccfvalues

lag2.plot (tsFL_Mobi_match2,tsFLnew_match2,14)