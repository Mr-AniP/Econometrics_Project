#Question-1
roadlength_districtlevel <- read.csv("C:/Users/Animesh/Downloads/quiz3.csv")
df<-roadlength_districtlevel
df[df == '.'] <- ''
df$road_sum <-as.numeric(df$road_sum)
df$districtcode <-as.numeric(df$districtcode)
df$yearcode <-as.numeric(df$yearcode)
library(dplyr)
df1<- df %>%
  mutate(ID=(df$districtcode *10000+df$yearcode))
df1<- df1 %>%
  mutate(t=(df$yearcode-2010))
df1$t <-as.numeric(df1$t)
road_lm<-lm(formula = road_sum ~ t, data = df1)
summary(road_lm)
#Question-2
df1<- df1 %>%
  mutate(DSouth=case_when(df1$state=="Andhra Pradesh" ~ 1,
                          df1$state=="Tamil Nadu" ~ 1,
                          df1$state=="Kerala" ~ 1,
                          df1$state=="Telangana" ~ 1,
                          TRUE ~ 0))
df1<- df1 %>%
  mutate(DEast=case_when(df1$state=="Assam" ~ 1,
                         df1$state=="Manipur" ~ 1,
                         df1$state=="Tripura" ~ 1,
                         df1$state=="Sikkim" ~ 1,
                         df1$state=="West Bengal" ~ 1,
                         df1$state=="Orissa" ~ 1,
                          TRUE ~ 0))
df1<- df1 %>%
  mutate(DWest=case_when(df1$state=="Gujarat" ~ 1,
                          df1$state=="Maharashtra" ~ 1,
                          df1$state=="Karnataka" ~ 1,
                          TRUE ~ 0))
df1<- df1 %>%
  mutate(DNorth=case_when(df1$state=="Jammu and Kashmir" ~ 1,
                          df1$state=="Himachal Pradesh" ~ 1,
                          df1$state=="Delhi" ~ 1,
                          df1$state=="Punjab" ~ 1,
                          df1$state=="Haryana" ~ 1,
                          df1$state=="Rajasthan" ~ 1,
                          df1$state=="Uttarakhand" ~ 1,
                          TRUE ~ 0))
df1<- df1 %>%
  mutate(DCentre=case_when(df1$state=="Madhya Pradesh" ~ 1,
                          df1$state=="Uttar Pradesh" ~ 1,
                          df1$state=="Chhattisgarh" ~ 1,
                          df1$state=="Jharkhand" ~ 1,
                          TRUE ~ 0))
df1$DCentre <-as.numeric(df1$DCentre)
df1$DNorth <-as.numeric(df1$DNorth)
df1$DWest <-as.numeric(df1$DWest)
df1$DEast <-as.numeric(df1$DEast)
df1$DSouth <-as.numeric(df1$DSouth)
#Question-3
road_lm<-lm(formula = road_sum ~ DSouth, data = df1)
summary(road_lm)
#Question-4
road_lm<-lm(formula = road_sum ~ DSouth+DNorth+DEast+DWest, data = df1)
summary(road_lm)
#Question-5
road_lm<-lm(formula = road_sum ~ t+DSouth+I(t*DSouth), data = df1)
summary(road_lm)