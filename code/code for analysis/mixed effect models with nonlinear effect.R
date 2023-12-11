# load packages
library(lme4)
library(lmerTest)
library(dplyr)
library(grid)
library(ggplot2)
library(cowplot)
library(tidyverse)
library(foreign)


################################################################################
# load data
################################################################################

setwd("d:/heatwave and dementia/data")
data = read.table(file = "data for analysis.csv", header = T , check.names=F,
                  sep = "," , fill = TRUE , encoding = "UTF-8",quote="")
data = data[,-1]

data_high = data[which(data$gnigroup=='high income'),]
data_low = data[which(data$gnigroup=='mid and low income'),]


################################################################################
# form accdata
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
# construct nonlinear effects
################################################################################

data$hw2 = data$hw*data$hw
data$hw3 = data$hw*data$hw*data$hw

data_high$hw2 = data_high$hw*data_high$hw
data_high$hw3 = data_high$hw*data_high$hw*data_high$hw

data_low$hw2 = data_low$hw*data_low$hw
data_low$hw3 = data_low$hw*data_low$hw*data_low$hw

accdata$acc32 = accdata$acc3*accdata$acc3
accdata$acc33 = accdata$acc3*accdata$acc3*accdata$acc3

accdata_high$acc72 = accdata_high$acc7*accdata_high$acc7
accdata_high$acc73 = accdata_high$acc7*accdata_high$acc7*accdata_high$acc7

accdata_low$acc32 = accdata_low$acc3*accdata_low$acc3
accdata_low$acc33 = accdata_low$acc3*accdata_low$acc3*accdata_low$acc3


################################################################################
# mixed effect models with incidence
################################################################################

# 1. data preparation-----------------------------------------------------------

data$gdp = log(data$gdp)
data[,c(2,9,17,28:29)] = scale(data[,c(2,9,17,28:29)])

data_low$gdp = log(data_low$gdp)
data_low[,c(2,9,17,28:29)] = scale(data_low[,c(2,9,17,28:29)])

data_high$gdp = log(data_high$gdp)
data_high[,c(2,9,17,28:29)] = scale(data_high[,c(2,9,17,28:29)])

accdata$gdp = log(accdata$gdp)
accdata[,c(1:3,8:11)] = scale(accdata[,c(1:3,8:11)])

accdata_low$gdp = log(accdata_low$gdp)
accdata_low[,c(1:3,8:11)] = scale(accdata_low[,c(1:3,8:11)])

accdata_high$gdp = log(accdata_high$gdp)
accdata_high[,c(1:3,8:11)] = scale(accdata_high[,c(1:3,8:11)])

# 2. model construction---------------------------------------------------------

# 2.1 hw,hw2--------------------------------------------------------------------
model = lmer(inci_as~year*(hw+hw2)*gdp+(1+year|country),data = data,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*(hw+hw2)*gdp+(1+year|country),data = data_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*(hw+hw2)*gdp+(1+year|country),data = data_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*(acc3+acc32)*gdp+(1+year|country),data = accdata,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*(acc7+acc72)*gdp+(1+year|country),data = accdata_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*(acc3+acc32)*gdp+(1+year|country),data = accdata_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

# 2.2 hw,hw2,hw3----------------------------------------------------------------

model = lmer(inci_as~year*(hw+hw2+hw3)*gdp+(1+year|country),data = data,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*(hw+hw2+hw3)*gdp+(1+year|country),data = data_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*(hw+hw2+hw3)*gdp+(1+year|country),data = data_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*(acc3+acc32+acc33)*gdp+(1+year|country),data = accdata,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)

model = lmer(inci_as~year*(acc7+acc72+acc73)*gdp+(1+year|country),data = accdata_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)

model = lmer(inci_as~year*(acc3+acc32+acc33)*gdp+(1+year|country),data = accdata_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
