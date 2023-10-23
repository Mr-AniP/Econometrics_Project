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
