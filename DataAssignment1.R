
rm(list=ls())
set.seed(50)
library(ggplot2)
library(rlang)
library(dplyr)
library(readxl)
library(stargazer)


#Part1

df <- read.csv("D:/IIITD/Econometrics1/DataAssignment1/Groundwater Level (1)/NDAP_REPORT_7063.csv")

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

SDP_df <- read.csv("D:/IIITD/Econometrics1/DataAssignment1/SDP.csv")

SDP_df <- SDP_df %>% mutate(State = tolower(State))
dff <- merge(df1, SDP_df, by="State")


#Part4

gini_df <- read.csv("D:/IIITD/Econometrics1/DataAssignment1/GINI.csv")
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


