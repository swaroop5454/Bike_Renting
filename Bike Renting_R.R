########################BIKE#RENTING######################################################################

#Remove all the objects stored
rm(list=ls())

#Set#check#current working director
setwd("F:/Ed_project")
getwd()

#Install required packages
#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", 
      "e1071", "Information","MASS", "rpart", "gbm",
       "ROSE", 'sampling', 'DataCombine',"data.table",'inTrees',"reshape","dplyr","plyr")

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

#Read Bike Renting data in to R
br=read.csv("day.csv",header=T)


############## Analyze variables by visualize####################

#univariate distribution of numeric variables

#Numerical variable distribution
par(mfrow=c(4,2))
par(mar=rep(2,4))
hist(br$season)
hist(br$weathersit)
hist(br$hum)
hist(br$holiday)
hist(br$workingday)
hist(br$temp)
hist(br$atemp)
hist(br$windspeed)
hist(br$cnt)
par(mfrow=c(1,1))

# analyze the distribution of target variable 'cnt'
ggplot(br,aes(x=br$cnt,y=..density..,stat_bins=30))+
  geom_histogram(fill= "DarkSeaGreen")+xlab("cnt")+
  geom_density(colour="red")


# analyse the distrubution of independence variable 'temp'
ggplot(br,aes(x=br$temp,y=..density..,stat_bins=30))+
  geom_histogram(fill= "DarkSeaGreen")+xlab("temp")+
  geom_density(colour="red")

# analyse the distrubution of independence variable 'atemp'
ggplot(br,aes(x=br$atemp,y=..density..))+
  geom_histogram(fill= "DarkSeaGreen")+xlab("atemp")+
  geom_density(colour="red")

# analyse the distrubution of independence variable 'hum'
ggplot(br,aes(x=br$hum,y=..density..))+
  geom_histogram(fill= "DarkSeaGreen")+xlab("hum")+
  geom_density(colour="red")

# analyse the distrubution of independence variable 'windspeed'
ggplot(br,aes(x=br$windspeed,y=..density..,stat_bins=30))+
  geom_histogram(fill= "DarkSeaGreen")+xlab("windspeed")+
  geom_density(colour="red")

# analyse the distrubution of independence variable 'casual'
ggplot(br,aes(x=br$casual,y=..density..,stat_bins=30))+
  geom_histogram(fill= "DarkSeaGreen")+xlab("casual")+
  geom_density(colour="red")

##Bi-variate Analysis#

# Visualize categorical Variable 'yr' with target variable 'cnt'
ggplot(br, aes(x=as.factor(yr), y=cnt)) +
  stat_summary(fun.y="mean", geom="bar",fill="blue")+xlab("yr")

# Visualize categorical Variable 'mnth' with target variable 'cnt'
ggplot(br, aes(x=as.factor(mnth), y=cnt)) +
  stat_summary(fun.y="mean", geom="bar",fill="blue")+xlab("mnth")

# Visualize categorical Variable 'season' with target variable 'cnt'
ggplot(br, aes(x=as.factor(season), y=cnt)) +
  stat_summary(fun.y="mean", geom="bar",fill="blue")+xlab("season")

# Visualize categorical Variable 'weathersit' with target variable 'cnt'
ggplot(br, aes(x=as.factor(weathersit), y=cnt)) +
  stat_summary(fun.y="mean", geom="bar",fill="blue")+xlab("weathersit")

# Visualize categorical Variable 'holiday'
ggplot(br) +
  geom_bar(aes(x=holiday),fill="blue")
# it is showing that almost all the cycle rentals are happening on holidays

# Visualize categorical Variable 'weekday'
ggplot(br) +
  geom_bar(aes(x=weekday),fill="blue")
# it is showing counts are all most same on all weekdays

# Visualize categorical Variable 'weathersit'
ggplot(br) +
  geom_bar(aes(x=weathersit),fill="blue")
# count is more when whether is " Clear, Few clouds, Partly cloudy, Partly cloudy"

#check the relationship between 'temp' and 'atemp' variable
ggplot(br, aes(x= temp,y=atemp)) +
  geom_point()+
  geom_smooth()
#This graph is saying that very strong relationship between 'temp' and 'atemp' which leads to collinearity

#check the relationship between 'temp' and 'hum' variable
ggplot(br, aes(x= temp,y=hum)) +
  geom_point()+
  geom_smooth()
# here it is showing Humidity is increses till temparature is 0.7 and it is decreasing gradually

#check the relationship between 'temp' and 'windspeed' variable
ggplot(br, aes(x= temp,y=windspeed)) +
  geom_point()+
  geom_smooth()
# it is showing that very less negative correlation between temp and windspeed

#check the relationship between all numeric variable using pair plot
library(GGally)
ggpairs(br[,c('atemp','temp','hum','windspeed','casual','registered','cnt')])
# that above plot stating that less nagative relationship between'cnt'-'hum' and cnt-windspeed
# and there is strong positive relationship between temp and atemp

#Relationship between target variables
ggplot(br, aes(x= casual,y=cnt)) +
  geom_point()+
  geom_smooth()

ggplot(br, aes(x= registered,y=cnt)) +
  geom_point()+
  geom_smooth()

ggplot(br, aes(x= casual+registered,y=cnt)) +
  geom_point()+
  geom_smooth()



#############################DATA#EXPLORATION###############################################

str(br)

##Variable Identification

br$instant=NULL   #Removing thr Record Index variable
br$dteday=format(as.Date(br$dteday,format="%Y-%m-%d"), "%d")
br$dteday=as.factor(br$dteday)
br$season=as.factor(br$season)
br$yr=as.factor(br$yr)
br$mnth=as.factor(br$mnth)
br$holiday=as.factor(br$holiday)
br$weekday=as.factor(br$weekday)
br$workingday=as.factor(br$workingday)
br$weathersit=as.factor(br$weathersit)
#br=subset(br,select = -c(casual,registered))

###################Missing#Value#Analysis##################################
missing_value=data.frame(apply(br,2,function(x){sum(is.na(x))}))
missing_value$column=row.names(missing_value)
names(missing_value)[1]="missing Val"
row.names(missing_value)=NULL
missing_value=missing_value[,c(2,1)]
print(missing_value)          #There are no missing values

##################Outlier#Analysis########################################
#Already all numeric variable are in normalize form so , no need to analysing Outliers
#here the six numerics variables are present out of six four variables are in normalize form.
# temp,atem,hum,windspread are in normalize form no need for outlier treatment.

# BoxPlots - Distribution and Outlier Check
numeric_index = sapply(br,is.numeric)     #selecting only numeric
numeric_data = br[,numeric_index]
cnames = colnames(numeric_data)


for (i in 1:length(cnames))
{
assign(paste0("gn",i),ggplot(data = br, aes_string(x = "cnt", y =cnames[i])) +
  stat_boxplot(geom = "errorbar", width = 0.5)+
         geom_boxplot(outlier.colour="red",fill="blue"))
}


gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn3,gn4,ncol=2)
gridExtra::grid.arrange(gn5,gn6,ncol=2)

# detect outliers in 'hum','windspeed' and 'casual' variables
#no need for outlier treatment
#####Outlier treatment#####
#for(i in cnames){
#  print(i)
# val = br[,i][br[,i] %in% boxplot.stats(br[,i])$out]
#  print(length(val))
#br[,i][br[,i]%in%val]=NA
#}

##Impute with KNN
#br=knnImputation(br,k=3)

#################Feature#Selection#or#dimension#reduction##########

## Correlation Plot
corrgram(br[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

cor(br[cnames])

library(RColorBrewer)
library(corrplot)
corrplot(cor(br[cnames]), method="number")


## Dimension Reduction####
br = subset(br,select = -c(atemp))              #there is high coleration b/w 'temp' and 'atemp'.
br=subset(br,select = -c(casual,registered))    #collinearity with 'cnt'



#############################MODEL DEVELOPMENT#########################################

rmExcept("br")
set.seed(121)
train_index = sample(1:nrow(br), 0.8 * nrow(br))
train = br[train_index,]
test = br[-train_index,]


###########Decision tree regression #################
fit = rpart(cnt ~ ., data = train, method = "anova")
predictions_DT = predict(fit, test[,-12])

print(fit)
par(cex= .9)
plot(fit)
text(fit)



#############Random Forest Model##########################
RF_model = randomForest(cnt ~ ., train, importance = TRUE, ntree = 200)
predictions_RF = predict(RF_model, test[,-12])
plot(RF_model)

#Extract rules metrics
treeList = RF2List(RF_model) 
exec = extractRules(treeList, train[,-12])
readableRules = presentRules(exec, colnames(train))
readableRules[1:2,]
ruleMetric = getRuleMetric(exec, train[,-12], train$cnt)
ruleMetric[1:3,]


##check Variable Importance
varimp=importance(RF_model)
varimp
# sort variable
sort_var <- names(sort(varimp[,1],decreasing =T))
# draw varimp plot
varImpPlot(RF_model,type = 1)




################Linear Regression##################################

#converting multilevel categorical variable into binary dummy variable
cnames= c("dteday","season","mnth","weekday","weathersit")
data_lr=br[,cnames]
cnt=data.frame(br$cnt)
names(cnt)[1]="cnt"
library(fastDummies)
data_lr <- fastDummies::dummy_cols(data_lr)
data_lr= subset(data_lr,select = -c(dteday,season,mnth,weekday,weathersit))
d3 = cbind(data_lr,br)
d3= subset(d3,select = -c(dteday,season,mnth,weekday,weathersit,cnt))
data_lr=cbind(d3,cnt)


##dividind data into test and train
#train_index = sample(1:nrow(data_lr), 0.8 * nrow(data_lr))
train_lr = data_lr[train_index,]
test_lr = data_lr[-train_index,]


##Linear regression model making
lm_model = lm(cnt ~., data = train_lr)
predictions_LR = predict(lm_model,test_lr[,-64])

summary(lm_model)
#plot(lm_model)


#################Model Evaluation###############

#defining MAPE function
MAPE = function(y, yhat){
  mean(abs((y - yhat)*100/y))
}

#MAPE for Decision tree regression
MAPE(test[,12], predictions_DT)

#MAPE for Random Forest Model
MAPE(test[,12], predictions_RF)

#MAPE for Linear Regression
MAPE(test[,12], predictions_LR)

#RMSE
#RMSE for Decision tree regression
RMSE(test[,12], predictions_DT)

#RMSE for Random Forest Model
RMSE(test[,12], predictions_RF)

#RMSE for Linear Regression
RMSE(test[,12], predictions_LR)

##########extacting predicted values output of all models######################

results=test
results$DT_predic_cnt=predictions_DT
results$RF_predic_cnt=predictions_RF
results$LR_predic_cnt=predictions_LR

write.csv(results, file = 'output_R .csv', row.names = FALSE, quote=FALSE)

#########################Thank#You#for#reading##################################







