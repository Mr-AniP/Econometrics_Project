
rm(list=ls())
set.seed(50)
library(ggplot2)
library(rlang)
library(dplyr)
library(readxl)
library(stargazer)


#Part1

df <- read.csv("C:/Users/Animesh/Downloads/NDAP_REPORT_7063.csv")

df <- df[,-c(8,9,10)]
df <- df[!(df$Ground.water.level == ""), ]
df <- df[!is.na(df$Ground.water.level), ]

df$district_year_id <- do.call(paste, c(df[c("District.lgd.code", "Yearcode")], sep = "-"))


#PART2 

# Created a new column for district-year-id
s=0
i=0
v=0
df1=data.frame(matrix(ncol=0,nrow=0))
#df1[1,1:9]=df[1,1:9]
earlierid=""
currentid=""
flag=1
for(n in 1:nrow(df)){
  currentid=df[n,9]
  if(earlierid==currentid){
    s=s+df[n,8]
    i=i+1
  }
  else{if(flag==0){
    df1[v,1:9]=df[n-1,1:9]
    df1[v,8]=s/i
  }
    else{
      flag=0
    }
    s=df[n,8]
    i=1
    v=v+1
  }
  earlierid=currentid
}
df1[v,1:9]=df[nrow(df),1:9]
df1[v,8]=s/i
df1 <- df1 %>% mutate(State = tolower(State))
df1 <- df1 %>% mutate(District = tolower(District))


#Part3

SDP_df <- read.csv("C:/Users/Animesh/Downloads/SDP.csv")

SDP_df <- SDP_df %>% mutate(State = tolower(State))
dff <- merge(df1, SDP_df, by="State")


#Part4

gini_df <- read.csv("C:/Users/Animesh/Downloads/GINI.csv")
colnames(gini_df)[2] <- "District"
gini_df <- gini_df %>% mutate(District = tolower(District))
merged_df <- merge(dff, gini_df, by="District")


final_df <- select(merged_df, -c(ROWID, Yearcode.y, Sr...No..,))
colnames(final_df)[6] <- "Yearcode"

View(final_df)



#Part5

summary(final_df)
stargazer(final_df, type = "text", title = "Summary Statistics")

final_df$SDP <- as.numeric(final_df$SDP)
final_df <- na.omit(final_df)


hist(final_df$Ground.water.level, xlab = "Ground Water Level",
     main = "Histogram for Ground Water Level")
hist(final_df$SDP, xlab = "SDP",
     main = "Histogram for SDP")
hist(final_df$Gini, xlab = "Gini",
     main = "Histogram for Gini")


boxplot(final_df$Ground.water.level, main = "Boxplot for Ground Water Level")
boxplot(final_df$SDP, main = "Boxplot for SDP")
boxplot(final_df$Gini, main = "Boxplot for Gini")

plot(density(final_df$Ground.water.level), main = "Density plot for Ground Water Level")
plot(density(final_df$SDP), main = "Density plot for SDP")
plot(density(final_df$Gini), main = "Density plot for Gini")



#Part6

model <- lm(formula = final_df$Ground.water.level ~ final_df$SDP)
summary(model)

#Part7 (Need to confirm this)

residuals = resid(model)
plot(final_df$SDP, final_df$Ground.water.level, xlab = "SDP", ylab = "EQI",
     main = "SDP vs EQI")

plot(final_df$SDP, residuals, xlab = "SDP", ylab = "Residuals",
     main = "SDP vs Residuals")

predicted = predict(model)
plot(final_df$Ground.water.level, predicted, xlab = "True value",
     ylab = "Predicted value", main = "True Value vs Predicted Value of EQI")




#Part8

sum_residuals = sum(residuals)

hist(residuals, xlab="Residuals", main = "Histogram of model residuals")



#Part9

SDP <- final_df$SDP
SDP2 <- as.numeric(final_df$SDP)^2
SDP3 <- as.numeric(final_df$SDP)^3
gini <- final_df$Gini

model2 <- lm(formula = final_df$Ground.water.level ~ SDP + SDP2 + SDP3 + gini)
summary(model2)

# 
# ## data assignment
# census_df<- read.csv("C:/Users/Animesh/Downloads/6000_all_files/ZIP/6000/NDAP_REPORT_6000.csv")
# rainfall_df<- read.csv("C:/Users/Animesh/Downloads/7066_all_files/ZIP/7066/NDAP_REPORT_7066.csv")
# sownarea_df<- read.csv("C:/Users/Animesh/Downloads/animesh21131_1682486585042153.csv")
# turnout_df<-read.csv("C:/Users/Animesh/Downloads/Turn_Out_updated.csv")
# noofcand_df<-read.csv("C:/Users/Animesh/Downloads/State wise Candidate Data .csv")
# 
# rainfall_df<-rainfall_df[,-c(8,9,10,12,13)]
# rainfall_df[is.na(rainfall_df)] <- 0
# df1=data.frame(matrix(ncol=0,nrow=0))
# i=1
# s=0
# for(n in 1:nrow(rainfall_df)){
#   if(n%%12==0){
#     df1[i,1:8]=rainfall_df[n,1:8]
#     df1[i,8]=s
#     i=i+1
#     s=0
#   }
#   else
#   {s=s+rainfall_df[n,8]}
# }
# rainfall_df<-df1[,-c(1)]
# census_df<-census_df[,-c(7:12,14:24,26:83)]
# census_df<-census_df[,-c(9:24)]
# census_df[is.na(census_df)] <- 0
# census_df<-census_df[order(census_df$District.lgd.code),]
# s=0
# s1=0
# v=1
# df1=data.frame(matrix(ncol=0,nrow=0))
# earlierid=""
# currentid=""
# for(n in 1:nrow(census_df)){
#   currentid=census_df[n,5]
#   if(earlierid==currentid || n==1){
#     s=s+census_df[n,8]
#     s1=s1+census_df[n,7]
#   }
#   else{
#     df1[v,1:8]=census_df[n-1,1:8]
#     df1[v,7]=s1
#     df1[v,8]=(s*100/s1)
#     s=census_df[n,8]
#     s1=census_df[n,7]
#     v=v+1
#   }
#   earlierid=currentid
# }
# colnames(df1)[8] <- "literacy_rate"
# census_df<-df1[,-c(1)]
# sownarea_df[is.na(sownarea_df)] <- 0
# sownarea_df<-sownarea_df[,-c(7:13,15,16)]
# for(n in 1:nrow(sownarea_df)){
#   sownarea_df[n,6]=(sownarea_df[n,6]*100/sownarea_df[n,5])
#   sownarea_df[n,7]=(sownarea_df[n,7]*100/sownarea_df[n,5])
# }
# 
# colnames(sownarea_df)[7] <- "perNetSownArea"
# colnames(sownarea_df)[6] <- "perForestArea"
# #install.packages("tidyr")
# library("tidyr")
# sownarea_df<-separate(sownarea_df,col="Year",into=c("xy","Yearcode"),sep=",")
# sownarea_df<-sownarea_df[,-c(4,6)]
# View(noofcand_df)
# 
## for data

## for rainfall
rainfall_df<- read.csv("C:/Users/Animesh/Downloads/7066_all_files/ZIP/7066/NDAP_REPORT_7066.csv")
rainfall_df<-rainfall_df[,-c(8,9,10,12,13)]
rainfall_df[is.na(rainfall_df)] <- 0
df1=data.frame(matrix(ncol=0,nrow=0))
i=1
s=0
c=1
for(n in 1:nrow(rainfall_df)){
  if(c==6 && i%%11==0){
    df1[i,1:8]=rainfall_df[n,1:8]
    df1[i,8]=2*(s+rainfall_df[n,8])
    i=i+1
    s=0
    c=1
  }
  else
  { if(c==12){
    df1[i,1:8]=rainfall_df[n,1:8]
    df1[i,8]=s+rainfall_df[n,8]
    i=i+1
    s=0
    c=1
  }
    else{
      s=s+rainfall_df[n,8]
      c=c+1}
  }
}
rainfall_df<-df1[,-c(1)]

## for census
census_df<- read.csv("C:/Users/Animesh/Downloads/6000_all_files/ZIP/6000/NDAP_REPORT_6000.csv")
census_df<-census_df[,-c(7:12,14:24,26:83)]
census_df<-census_df[,-c(9:24)]
census_df[is.na(census_df)] <- 0
census_df<-census_df[order(census_df$District.lgd.code),]
s=0
s1=0
v=1
df1=data.frame(matrix(ncol=0,nrow=0))
earlierid=""
currentid=""
for(n in 1:nrow(census_df)){
  currentid=census_df[n,5]
  if(earlierid==currentid || n==1){
    s=s+census_df[n,8]
    s1=s1+census_df[n,7]
  }
  else{
    df1[v,1:8]=census_df[n-1,1:8]
    df1[v,7]=s1
    df1[v,8]=(s*100/s1)
    s=census_df[n,8]
    s1=census_df[n,7]
    v=v+1
  }
  earlierid=currentid
}
colnames(df1)[8] <- "literacy_rate"
census_df<-df1[,-c(1)]

## for land usage
sownarea_df<- read.csv("C:/Users/Animesh/Downloads/animesh21131_1682486585042153.csv")
sownarea_df[is.na(sownarea_df)] <- 0
sownarea_df<-sownarea_df[,-c(7:13,15,16)]
df1=data.frame(matrix(ncol=0,nrow=0))
i=1
c=1
for(n in 1:nrow(sownarea_df)){
  df1[i,1:7]=sownarea_df[n,1:7]
  df1[i,6]=(sownarea_df[n,6]*100/sownarea_df[n,5])
  df1[i,7]=(sownarea_df[n,7]*100/sownarea_df[n,5])
  c=c+1
  i=i+1
  if(c==9){
    df1[i,1:7]=df1[i-1,1:7]
    df1[i,4]="agy, 2019"
    i=i+1
    df1[i,1:7]=df1[i-1,1:7]
    df1[i,4]="agy, 2020"
    i=i+1
    df1[i,1:7]=df1[i-1,1:7]
    df1[i,4]="agy, 2021"
    i=i+1
    c=1
  }
}
colnames(df1)[7] <- "perNetSownArea"
colnames(df1)[6] <- "perForestArea"
#install.packages("tidyr")
library("tidyr")
df1<-separate(df1,col="Year",into=c("xy","Yearcode"),sep=", ")
sownarea_df<-df1[,-c(4,6)]

## for no of cand
noofcand_df<-read.csv("C:/Users/Animesh/Downloads/State wise Candidate Data .csv")
s=0
df1=data.frame(matrix(ncol=0,nrow=0))
for(n in 1:nrow(noofcand_df)){
  while(v%%12!=0){
    df1[s+v,1]=noofcand_df[n,1]
    df1[s+v,2]=2010+v%%12
    if(v%%12<4)
      df1[s+v,3]=noofcand_df[n,2]
    else{
      if(v%%12<9)
        df1[s+v,3]=noofcand_df[n,3]
      else
        df1[s+v,3]=noofcand_df[n,4]
    }
    v=v+1
  }
  s=s+v-1
  v=1
}
colnames(df1)[1] <- "State"
colnames(df1)[2] <- "year"
colnames(df1)[3] <- "candidateno"
noofcand_df<-df1

## for turnout
turnout_df<-read.csv("C:/Users/Animesh/Downloads/Turn_Out_updated.csv")
View(turnout_df)
s=0
df1=data.frame(matrix(ncol=0,nrow=0))
for(n in 1:nrow(turnout_df)){
  while(v%%12!=0){
    df1[s+v,1]=turnout_df[n,2]
    df1[s+v,2]=turnout_df[n,3]
    df1[s+v,3]=2010+v%%12
    if(v%%12<4)
      df1[s+v,4]=turnout_df[n,6]
    else{
      if(v%%12<9)
        df1[s+v,4]=turnout_df[n,5]
      else
        df1[s+v,4]=turnout_df[n,4]
    }
    v=v+1
  }
  s=s+v-1
  v=1
}
colnames(df1)[1] <- "District"
colnames(df1)[2] <- "State"
colnames(df1)[3] <- "year"
colnames(df1)[4] <- "win_margin"
View(df1)
turnout_df<-df1
