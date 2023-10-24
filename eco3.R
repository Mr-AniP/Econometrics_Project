rm(list=ls())
set.seed(50)
library(ggplot2)
library(rlang)
library(dplyr)


df <- read.csv("C:/Users/Animesh/Downloads/Groundwater Level/ZIP/7063/NDAP_REPORT_7063.csv")

df <- df[,-c(8,9,10)]
df <- df[!(df$Ground.water.level == ""), ]
df <- df[!is.na(df$Ground.water.level), ]


df$district_year_id <- do.call(paste, c(df[c("District.lgd.code", "Yearcode")], sep = "-"))

aggregate(df$Ground.water.level~.,df, FUN=mean)
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
df1<-df1[order(df1$State, decreasing = FALSE), ]
RBI_df <- read.csv("C:/Users/Animesh/Downloads/RBI_Data_1.csv")
RBI_df <- data.frame(RBI_df)
df1['sdp'] <- NA

pid=0
state=1
for(i in 1:dim(df1)[1]){
  nid=df1[i,3]
  nyear = df1[i,7] - 2010
  if(pid!=nid){
    state=state+1
  }
  df1[i,10] = RBI_df[nyear,state]
  pid=nid
}
View(df1)