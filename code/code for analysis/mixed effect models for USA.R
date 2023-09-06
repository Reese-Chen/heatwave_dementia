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
library(Rmisc)

################################################################################
# read data
#######################################
setwd("d:/heatwave and dementia/data")
inci_as = read.table(file = "age-standardized both sex incidence rate of each state in USA.csv",header=T,
                     sep=",",fill = T,encoding="UTF-8")
hw = read.table(file = "hw_usa_state.csv", header = T , 
                sep = "," , fill = TRUE , encoding = "UTF-8")
gdp = read.table(file = "美国各州gdp时间序列.csv", header = T , 
                 sep = "," , fill = TRUE , encoding = "UTF-8")

# sort
inci_as = inci_as[order(inci_as$location_name,inci_as$year),]
gdp = gdp[order(gdp$GeoName),]
hw = hw[order(hw$state),]

# get state
state = hw$state
inci_as = inci_as[which(inci_as$location_name %in% state),]
gdp = gdp[which(gdp$GeoName %in% state),]

################################################################################
# reform data
#######################################
for (i in 1997:2019){
  if (i==1997){
    
    new_inci_as = inci_as[inci_as$year==i,]$val
    new_hw = hw[,i-1997+9]
    new_gdp = gdp[,i-1997+3]
    year = rep(i-1997,46)
    new_state = state
    
  }
  else{
    
    new_inci_as = append(new_inci_as,inci_as[inci_as$year==i,]$val)
    new_hw = append(new_hw,hw[,i-1997+9])
    new_gdp = append(new_gdp,gdp[,i-1997+3])
    year = append(year,rep(i-1997,46))
    new_state = append(new_state,state)
  }
}

# combine data
data = cbind(new_inci_as,new_hw,new_gdp,year)
data = as.data.frame(data)
data$state = new_state
data$state = as.factor(data$state)
colnames(data) = c('inci_as','hw','gdp','year','state')


################################################################################
# reform data with acc7
################################################################################

accdata = data
accdata$acc7 = accdata$hw
accdata$inci_as_delta = accdata$inci_as
for (i in 2003:2019){
  y = i-1997
  accdata[which(accdata$year==y),]$inci_as_delta = accdata[which(accdata$year==y),]$inci_as_delta-
    accdata[which(accdata$year==y-1),]$inci_as
  accdata[which(accdata$year==y),]$acc7 = accdata[which(accdata$year==y),]$hw+
    accdata[which(accdata$year==y-1),]$hw+accdata[which(accdata$year==y-2),]$hw+
    accdata[which(accdata$year==y-3),]$hw+accdata[which(accdata$year==y-4),]$hw+
    accdata[which(accdata$year==y-5),]$hw+accdata[which(accdata$year==y-6),]$hw
}
accdata = accdata[which(accdata$year>5),]
accdata$year = accdata$year-6


#########################################
# run mixed effect model with hw
#########################################

hist(data$inci_as,labels=T)
hist(data$hw,labels = T)
hist(data$gdp,labels=T)

data$gdp = log(data$gdp)
data$inci_as = scale(data$inci_as)
data$hw = scale(data$hw)
data$gdp = scale(data$gdp)

model = lmer(inci_as~year*hw*gdp+(1+year|state),data = data,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)
anova(model)
ranova(model)



################################################################################
# regression with acc7
################################################################################

hist(accdata$inci_as,labels=T)
hist(accdata$inci_as_delta,labels=T)
hist(accdata$acc7,labels=T)
hist(accdata$gdp,labels=T)

accdata$gdp = log(accdata$gdp)
accdata$acc7 = scale(accdata$acc7)
accdata$inci_as_delta = scale(accdata$inci_as_delta)
accdata$inci_as = scale(accdata$inci_as)
accdata$gdp = scale(accdata$gdp)

model = lmer(inci_as~year*acc7*gdp+(1+year|state),data = accdata,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)


