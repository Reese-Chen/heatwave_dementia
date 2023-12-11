# load packages
library(lme4)
library(lmerTest)
library(dplyr)
library(optimx)
library(mvmeta)
library(meta)
library(grid)
library(forestploter)
library(forestplot)
library(arm)
library(ggplot2)
library(ggiraphExtra)
library(cowplot)
library(ggcorrplot)
library(tidyverse)
library(caret)
library(car)
library(glmnet)
library(foreign)
library(metafor)
library(Rtsne)
library(ggExtra)
library(ggsci)
library(pastecs)
library(reshape2)
library(ggsignif)
library(Metrics)

################################################################################
# load data
################################################################################

setwd("d:/heatwave and dementia/data")
data = read.table(file = "data for analysis.csv", header = T , check.names=F,
                  sep = "," , fill = TRUE , encoding = "UTF-8",quote="")
data = data[,-1]
country = unique(data$country)
data_high = data[which(data$gnigroup=='high income'),]
data_low = data[which(data$gnigroup=='mid and low income'),]

################################################################################
# form accdata and incrementdata
################################################################################

# accdata
cols = c('inci_as','hw','gdp','year','country','gnigroup','pop')
accdata = data[,colnames(data) %in% cols]
accdata$inci_as_delta = accdata$inci_as
accdata$acc3 = accdata$hw
for (i in 1992:2019){
  y = i-1990
  accdata[which(accdata$year==y),]$inci_as_delta = accdata[which(accdata$year==y),]$inci_as_delta-
    accdata[which(accdata$year==y-1),]$inci_as
  accdata[which(accdata$year==y),]$acc3 = accdata[which(accdata$year==y),]$hw+
    accdata[which(accdata$year==y-1),]$hw+accdata[which(accdata$year==y-2),]$hw
}
accdata = accdata[which(accdata$year>1),]
accdata$year = accdata$year-2

# accdata_low
accdata_low = accdata[which(accdata$gnigroup=='mid and low income'),]

# accdata_high
accdata_high = data_high[,colnames(data) %in% cols]
accdata_high$acc7 = accdata_high$hw
accdata_high$inci_as_delta = accdata_high$inci_as
for (i in 1996:2019){
  y = i-1990
  accdata_high[which(accdata_high$year==y),]$inci_as_delta = accdata_high[which(accdata_high$year==y),]$inci_as_delta-
    accdata_high[which(accdata_high$year==y-1),]$inci_as
  accdata_high[which(accdata_high$year==y),]$acc7 = accdata_high[which(accdata_high$year==y),]$hw+
    accdata_high[which(accdata_high$year==y-1),]$hw+accdata_high[which(accdata_high$year==y-2),]$hw+
    accdata_high[which(accdata_high$year==y-3),]$hw+accdata_high[which(accdata_high$year==y-4),]$hw+
    accdata_high[which(accdata_high$year==y-5),]$hw+accdata_high[which(accdata_high$year==y-6),]$hw
}
accdata_high = accdata_high[which(accdata_high$year>5),]
accdata_high$year = accdata_high$year-6

################################################################################
# predictive models to calculate attributable incidence number
################################################################################

mae = function(x,y) mean(abs(y - x))
RSq = function(x, y) {cor(x, y)^2}

# 1.high income countries-------------------------------------------------------

country_high = unique(accdata_high$country)

RMSE_result_high = matrix(0,nrow=40,ncol=5)
RSq_result_high = matrix(0,nrow=40,ncol=5)
cor_result_high = matrix(0,nrow=40,ncol=5)
MAE_result_high = matrix(0,nrow=40,ncol=5)

for (i in 1:4){
  data_train = accdata_high[which(accdata_high$year<(i-1)*6 | accdata_high$year>=i*6),]
  data_test = accdata_high[which(accdata_high$year>=(i-1)*6 & accdata_high$year<i*6),]
  
  model = lmer(inci_as~acc7*year*gdp+(1+year|country),data = data_train)
  data_test$inci_as_pred = predict(model,newdata=data_test)
  for (j in 1:40){
    data_country = data_test[which(data_test$country==country_high[j]),]
    RMSE_result_high[j,i] = rmse(data_country$inci_as,data_country$inci_as_pred) 
    RSq_result_high[j,i] = RSq(data_country$inci_as,data_country$inci_as_pred)
    cor_result_high[j,i] = cor(data_country$inci_as,data_country$inci_as_pred)
    MAE_result_high[j,i] = mae(data_country$inci_as,data_country$inci_as_pred)
  }
}
for (i in 1:40){
  RMSE_result_high[i,5] = mean(RMSE_result_high[i,1:4])
  RSq_result_high[i,5] = mean(RSq_result_high[i,1:4])
  cor_result_high[i,5] = mean(cor_result_high[i,1:4])
  MAE_result_high[i,5] = mean(MAE_result_high[i,1:4])
}

predict_result_high = cbind(RMSE_result_high[,5],RSq_result_high[,5],
                            cor_result_high[,5],MAE_result_high[,5])
rownames(predict_result_high) = country_high
colnames(predict_result_high) = c('RMSE','RSq','cor','MAE')

write.csv(predict_result_high,'4-fold CV result for prediction in high income countries.csv')

# 2.low- to middle-income countries------------------------------------------------------

country_low = unique(accdata_low$country)

RMSE_result_low = matrix(0,nrow=97,ncol=5)
RSq_result_low = matrix(0,nrow=97,ncol=5)
cor_result_low = matrix(0,nrow=97,ncol=5)
MAE_result_low = matrix(0,nrow=97,ncol=5)

for (i in 1:4){
  data_train = accdata_low[which(accdata_low$year<(i-1)*7 | accdata_low$year>=i*7),]
  data_test = accdata_low[which(accdata_low$year>=(i-1)*7 & accdata_low$year<i*7),]
  
  model = lmer(inci_as~acc3*year*gdp+(1+year|country),data = data_train)
  data_test$inci_as_pred = predict(model,newdata=data_test)
  for (j in 1:97){
    data_country = data_test[which(data_test$country==country_low[j]),]
    RMSE_result_low[j,i] = rmse(data_country$inci_as,data_country$inci_as_pred) 
    RSq_result_low[j,i] = RSq(data_country$inci_as,data_country$inci_as_pred)
    cor_result_low[j,i] = cor(data_country$inci_as,data_country$inci_as_pred)
    MAE_result_low[j,i] = mae(data_country$inci_as,data_country$inci_as_pred)
  }
}

for (i in 1:97){
  RMSE_result_low[i,5] = mean(RMSE_result_low[i,1:4])
  RSq_result_low[i,5] = mean(RSq_result_low[i,1:4])
  cor_result_low[i,5] = mean(cor_result_low[i,1:4])
  MAE_result_low[i,5] = mean(MAE_result_low[i,1:4])
}
predict_result_low = cbind(RMSE_result_low[,5],RSq_result_low[,5],
                            cor_result_low[,5],MAE_result_low[,5])
rownames(predict_result_low) = country_low
colnames(predict_result_low) = c('RMSE','RSq','cor','MAE')

write.csv(predict_result_low,'4-fold CV result for prediction in low- to middle-income countries.csv')
