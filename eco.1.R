#Basics
#Always use a script file (.R)
#<-
#Alt plus - for the assignment operator.
#Control plus enter for run.
2+2
#Creating a Variable:
a = 10
a
#Creating a different kind of variable:
b=1:10
b
class(b)
#Generating X:
c=1:50
c

x=c^2
x

?max

max(x)
min(x)

#Summarizing X:
summary(x)

var(x)
(var(x))^(1/2)

#Subsetting X:
y=x[x<200]
y

#Even-Odd Parts of X:
e <- subset(x,x%%2 == 0)
e
o <- subset(x,x%%2 != 0)
o

#Importing the Data:
roadlength_districtlevel <- read.csv("C:/Users/Animesh/Downloads/roadlength_districtlevel.csv")
df<-roadlength_districtlevel

#View the complete dataset:
View(df)
#What type of an object is the data?
class(df)
#To view the first few rows of data:
head(df)
#List of Columns in the data:
colnames(df)
#What are the dimensions of this data frame?
dim(df)
#Examine individual values
df[1,1]
df[1,"state"]
#Examine a specific column:
head(df$state)

#Is this data set a Balanced Panel?
unique(df$state)
length(unique(df$state))
length(unique(df$district))
length(unique(df$yearcode))

#Working with Road Length:
rdlength <- df$road_sum
#Road Length is a VECTOR.

#Histogram
hist(rdlength,
     main="District-Wise Road Length_2011-22",
     xlab="Road Length in Kilometres",
     col="darkmagenta",
     freq=TRUE
)







#Problem: Road Length is Non-Numeric. 
#Convert "." to Missing
rdlength <- replace(rdlength, rdlength=='.','')
#Does this solve the problem?

#Let's try to draw the histogram again:
hist(rdlength,
     main="District-Wise Road Length_2011-22",
     xlab="Road Length in Kilometres",
     col="darkmagenta",
     freq=TRUE
)








#Convert to Numeric:
rdlength <-as.numeric(rdlength)

#Histogram
hist(rdlength,
     main="District-Wise Road Length_2011-22",
     xlab="Road Length in Kilometres",
     col="darkmagenta",
     freq=TRUE
)
summary(rdlength)

#Curtailing the Road Length Distribtuion to Less than 1000 kilometres of road:
rd <- rdlength[rdlength<=1000]
#Histogram
hist(rd,
     main="District-Wise Road Length_2011-22",
     xlab="Road Length in Kilometres",
     col="darkmagenta",
     freq=TRUE
)

#Histogram
hist(rd,
     main="District-Wise Road Length_2011-22",
     xlab="Road Length in Kilometres",
     col="darkmagenta",
     freq=TRUE,breaks = 30
)

#Now, let's make the analysis a little richer:
#install.packages("ggplot2")
library(ggplot2)
#remove.packages("rlang") 
#install.packages("rlang")

#Convert to Missing in the Base Data:
df[df == '.'] <- ''
df$road_sum <-as.numeric(df$road_sum)
df$yearcode <-as.factor(df$yearcode)

#Histogram by Year
ggplot(df, aes(x = road_sum)) +
  geom_histogram(aes(color = yearcode), fill="white",
                 position = "identity", bins = 30)+ xlab("Road Length in Kilometres") +
  ylab("District Count")

#Year-Wise Histograms
ggplot(df, aes(x = road_sum)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(yearcode ~ .)
#Still not very readable as a graph. 

#First, curtail the distribution once again to 1000.
df2 = df[df$road_sum<1000,]

#Box Plot
box_plot <- ggplot(df2, aes(x = yearcode, y = road_sum))
# Add the geometric object box plot
box_plot +
  geom_boxplot()+ stat_summary(fun.y="mean",color="red", shape=1) + ggtitle("District-Level Total Road Length (km) for 2011-21") + labs(y = "Total Road Length (km)",x = "Year")

#N is missing:
#Jitter Plot
ggplot(df2, aes(x = yearcode, y = road_sum)) + geom_point(position = position_jitter(width = .2), alpha = .3) + ggtitle("District-Level Total Road Length (km) for 2011-21") + labs(y = "Total Road Length (km)",x = "Year")

#Adding the Trend Regression Line:
library(dplyr)
df3<- df2 %>%
  mutate(trend = case_when(yearcode==2011 ~ 1,
                           yearcode==2012 ~ 2,
                           yearcode==2013 ~ 3,
                           yearcode==2014 ~ 4,
                           yearcode==2015 ~ 5,
                           yearcode==2016 ~ 6,
                           yearcode==2017 ~ 7,
                           yearcode==2018 ~ 8,
                           TRUE ~ 11))
df3$trend <-as.numeric(df3$trend)
View(df3)
p <- ggplot(df3, aes(x = trend, y = road_sum)) + geom_smooth(method="lm")
p  
p + geom_point(position = position_jitter(width = .2), alpha = .3) + ggtitle("District-Level Total Road Length (km) for 2011-21") + labs(y = "Total Road Length (km)",x = "Year")

#Now, to do a scatter plot instead of a jitter plot:
ggplot(df3, aes(x = trend, y = road_sum)) + 
  geom_point()+
  geom_smooth(method="lm") + ggtitle("District-Level Total Road Length (km) for 2011-21") + labs(y = "Total Road Length (km)",x = "Year")

#Regression Models:
road_lm<-lm(formula = road_sum ~ trend, data = df3)
summary(road_lm)

road_log_lm<-lm(formula = road_log ~ trend, data = df3)
summary(road_log_lm)

#Rescaling using LOG: 
df3$road_log <- log(df3$road_sum)
#BoxPlot
box_plot <- ggplot(df3, aes(x = yearcode, y = road_log))
# Add the geometric object box plot
box_plot +
  geom_boxplot() + stat_summary(fun.y="mean",color="red", shape=1) + ggtitle("Log of District-Level Total Road Length (km) for 2011-21") + labs(y = "Log of Total Road Length (km)",x = "Year")

#Jitter Plot
ggplot(df3, aes(x = yearcode, y = road_log)) + geom_point(position = position_jitter(width = .2), alpha = .3) + ggtitle("District-Level Total Road Length (km) for 2011-21") + labs(y = "Total Road Length (km)",x = "Year")

#Adding the Regression Line:
p<- ggplot(df3, aes(x = trend, y = road_log)) + geom_smooth(method=lm)
p  
p + geom_point(position = position_jitter(width = .2), alpha = .3) + ggtitle("Log of District-Level Total Road Length (km) for 2011-21") + labs(y = "Log of Total Road Length (km)",x = "Year")

#Residual Plots: 
road_lm<-lm(formula = road_sum ~ trend, data = df3)
summary(road_lm)
#Predicted Values (Y Hat):
df3$yhat = 0.6474*(df3$trend)
df3$yhat = 108.0453 - df3$yhat 

#Residuals:
df3$uhat = df3$road_sum - df3$yhat

ggplot(df3, aes(x = trend, y = uhat)) + 
  geom_point(position = position_jitter(width = .2), alpha = .3) + ggtitle("Road Length (Residuals) vs. Trend") + labs(y = "Road Length Residuals",x = "Trend")

#Density:
df3<-na.omit(df3)
p2 <- ggplot(df3, aes(x=uhat)) + 
  geom_density()
p3<-p2+ geom_vline(aes(xintercept=mean(uhat)),
                   color="blue", linetype="dashed", size=1, na.rm=TRUE)
p3

