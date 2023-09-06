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
library(lsr)

library(aTSA)

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
# form accdata and incrementdata
################################################################################

# 1. form accdata---------------------------------------------------------------

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

# 2. form incrementdata---------------------------------------------------------

cols = c('inci_as','hw','gdp','year','country','gnigroup')
incrementdata = data[,colnames(data) %in% cols]
incrementdata$inci_as_delta = incrementdata$inci_as
incrementdata$lag1 = incrementdata$hw
for (i in 1991:2019){
  y = i-1990
  incrementdata[which(incrementdata$year==y),]$inci_as_delta = incrementdata[which(incrementdata$year==y),]$inci_as_delta-
    incrementdata[which(incrementdata$year==y-1),]$inci_as
}
incrementdata = incrementdata[which(incrementdata$year>0),]
incrementdata$year = incrementdata$year-1


incrementdata_high = incrementdata[which(incrementdata$gnigroup=='high income'),]
incrementdata_low = incrementdata[which(incrementdata$gnigroup=='mid and low income'),]


################################################################################
# mixed effect models with incidence
################################################################################

# 1. statistic values before z-scale--------------------------------------------

# for data
sd(data$inci_as)
sd(data$hw)

sd(data_high$inci_as)
sd(data_high$hw)

sd(data_low$inci_as)
sd(data_low$hw)

# for accdata
sd(accdata$inci_as)
sd(accdata$acc3)
sd(accdata$inci_as_delta)

sd(accdata_high$inci_as)
sd(accdata_high$acc7)
sd(accdata_high$inci_as_delta)

sd(accdata_low$inci_as)
sd(accdata_low$acc3)
sd(accdata_low$inci_as_delta)

# for incrementdata
sd(incrementdata$inci_as_delta)
sd(incrementdata$hw)

sd(incrementdata_high$inci_as_delta)
sd(incrementdata_high$hw)

sd(incrementdata_low$inci_as_delta)
sd(incrementdata_low$hw)


# 2. run basic mixed effect models in 137 countries-----------------------------

# *2.1 data preprocessing-------------------------------------------------------
hist(data$inci_all,labels=T)
hist(data$inci_as,labels=T)
hist(data$prev_all,labels=T)
hist(data$prev_as,labels=T)
hist(data$mort_all)
hist(data$mort_as)
hist(data$hw,labels = T)
hist(data$HW_count,labels=T)
hist(data$HW_days,labels=T)
hist(data$HW_degree,labels=T)
hist(data$HW_mean_length,labels=T)
hist(data$HW_mean_degree,labels=T)
hist(data$HW_max_degree,labels=T)
hist(data$gdp,labels=T)
hist(data$pop,labels=T)
hist(data$temp,labels=T)
hist(data$cw,labels=T)
hist(data$sex_ratio,labels=T)
hist(data$age_ratio,labels=T)

data$inci_all = log(data$inci_all)
data$prev_all = log(data$prev_all)
data$HW_count = log(data$HW_count+1)
data$HW_days = log(data$HW_days+1)
data$HW_degree = log(data$HW_degree+1)
data$HW_mean_length = log(data$HW_mean_length+1)
data$HW_mean_degree = log(data$HW_mean_degree+1)
data$HW_max_degree = log(data$HW_max_degree+1)
data$gdp = log(data$gdp)
data$pop = log(data$pop)
data$sex_ratio = log(data$sex_ratio)
data$age_ratio = log(data$age_ratio)

data$inci_all = scale(data$inci_all)
data$inci_as = scale(data$inci_as)
data$prev_all = scale(data$prev_all)
data$prev_as = scale(data$prev_as)
data$mort_all = scale(data$mort_all)
data$mort_as = scale(data$mort_as)
data$pop = scale(data$pop)
data$gdp = scale(data$gdp)
data$hw = scale(data$hw)
data$HW_count = scale(data$HW_count)
data$HW_days = scale(data$HW_days)
data$HW_degree = scale(data$HW_degree)
data$HW_mean_length = scale(data$HW_mean_length)
data$HW_mean_degree = scale(data$HW_mean_degree)
data$HW_max_degree = scale(data$HW_max_degree)
data$temp = scale(data$temp)
data$cw = scale(data$cw)
data$sex_ratio = scale(data$sex_ratio)
data$age_ratio = scale(data$age_ratio)

# *2.2 mixed effect model-------------------------------------------------------
# principle model
model = lmer(inci_as~year*hw*gdp+(1+year|country),data = data,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)
anova(model)
ranova(model)

# look at the different between high- and low- and middle income countries
model = lmer(inci_as~year*hw*gdp+gnigroup+gnigroup:year:hw+(1+year|country),data=data,
             control=lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

# control mort_as
model = lmer(inci_as~year*hw*gdp+mort_as+(1+year|country),data = data,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)
anova(model)
ranova(model)

# control temp
model = lmer(inci_as~year*hw*gdp+temp+(1+year|country),data = data,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)
anova(model)
ranova(model)

# control sex_ratio and age_ratio
model = lmer(inci_as~year*hw*gdp+sex_ratio+(1+year|country),data = data,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)
anova(model)
ranova(model)

model = lmer(inci_as~year*hw*gdp+age_ratio+(1+year|country),data = data,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)
anova(model)
ranova(model)

model = lmer(inci_as~year*hw*gdp+sex_ratio+age_ratio+(1+year|country),data = data,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)
anova(model)
ranova(model)

# add popweight
model = lmer(inci_as~year*hw*gdp+(1+year|country),
             data = data,
             weights = popweight,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

# control coldwave
model = lmer(inci_as~year*hw*gdp+cw+(1+year|country),data = data,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

# control gdp
model = lmer(inci_as~year*hw+gdp+(1+year|country),data = data,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

# exclude special year
model = lmer(inci_as~year*hw*gdp+(1+year|country),
             data = data[which(data$year!=7),],
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

# using prevalence
model = lmer(prev_as~year*hw*gdp+(1+year|country),data = data,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

# with other heatwave indexes
model = lmer(inci_as~year*HW_count*gdp+(1+year|country),data = data,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*HW_days*gdp+(1+year|country),data = data,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*HW_degree*gdp+(1+year|country),data = data,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*HW_mean_length*gdp+(1+year|country),data = data,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*HW_mean_degree*gdp+(1+year|country),data = data,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*HW_max_degree*gdp+(1+year|country),data = data,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

# 3. run mixed effect models in high income countries---------------------------

# *3.1 data preprocessing-------------------------------------------------------

hist(data_high$inci_all,labels=T)
hist(data_high$inci_as,labels=T)
hist(data_high$prev_all,labels=T)
hist(data_high$prev_as,labels=T)
hist(data_high$mort_all)
hist(data_high$mort_as)
hist(data_high$hw,labels = T)
hist(data_high$HW_count,labels=T)
hist(data_high$HW_days,labels=T)
hist(data_high$HW_degree,labels=T)
hist(data_high$HW_mean_length,labels=T)
hist(data_high$HW_mean_degree,labels=T)
hist(data_high$HW_max_degree,labels=T)
hist(data_high$gdp,labels=T)
hist(data_high$pop,labels=T)
hist(data_high$temp,labels=T)
hist(data_high$cw,labels=T)
hist(data_high$sex_ratio,labels=T)
hist(data_high$age_ratio,labels=T)

data_high$inci_all = log(data_high$inci_all)
data_high$prev_all = log(data_high$prev_all)
data_high$HW_count = log(data_high$HW_count+1)
data_high$HW_days = log(data_high$HW_days+1)
data_high$HW_degree = log(data_high$HW_degree+1)
data_high$HW_mean_length = log(data_high$HW_mean_length+1)
data_high$HW_mean_degree = log(data_high$HW_mean_degree+1)
data_high$HW_max_degree = log(data_high$HW_max_degree+1)
data_high$gdp = log(data_high$gdp)
data_high$pop = log(data_high$pop)
data_high$sex_ratio = log(data_high$sex_ratio)
data_high$age_ratio = log(data_high$age_ratio)

data_high$inci_all = scale(data_high$inci_all)
data_high$inci_as = scale(data_high$inci_as)
data_high$prev_all = scale(data_high$prev_all)
data_high$prev_as = scale(data_high$prev_as)
data_high$mort_all = scale(data_high$mort_all)
data_high$mort_as = scale(data_high$mort_as)
data_high$pop = scale(data_high$pop)
data_high$gdp = scale(data_high$gdp)
data_high$hw = scale(data_high$hw)
data_high$HW_count = scale(data_high$HW_count)
data_high$HW_days = scale(data_high$HW_days)
data_high$HW_degree = scale(data_high$HW_degree)
data_high$HW_mean_length = scale(data_high$HW_mean_length)
data_high$HW_mean_degree = scale(data_high$HW_mean_degree)
data_high$HW_max_degree = scale(data_high$HW_max_degree)
data_high$temp = scale(data_high$temp)
data_high$cw = scale(data_high$cw)
data_high$sex_ratio = scale(data_high$sex_ratio)
data_high$age_ratio = scale(data_high$age_ratio)

# *3.2 mixed effect models------------------------------------------------------


model = lmer(inci_as~year*hw*gdp+(1+year|country),
             data = data_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+mort_as+(1+year|country),
             data = data_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+temp+(1+year|country),
             data = data_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+sex_ratio+(1+year|country),
             data = data_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+age_ratio+(1+year|country),
             data = data_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+sex_ratio+age_ratio+(1+year|country),
             data = data_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+(1+year|country),
             data = data_high,
             weights = popweight,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+cw+(1+year|country),
             data = data_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw+gdp+(1+year|country),
             data = data_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+(1+year|country),
             data = data_high[which(data_high$year!=7),],
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(prev_as~year*hw*gdp+(1+year|country),
             data = data_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

# with other heatwave indexes
model = lmer(inci_as~year*HW_count*gdp+(1+year|country),
             data = data_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*HW_days*gdp+(1+year|country),
             data = data_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*HW_degree*gdp+(1+year|country),
             data = data_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*HW_mean_length*gdp+(1+year|country),
             data = data_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*HW_mean_degree*gdp+(1+year|country),
             data = data_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*HW_max_degree*gdp+(1+year|country),
             data = data_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

# 4. run mixed effect model in mid and low income countries---------------------

# *4.1 data preprocessing-------------------------------------------------------


hist(data_low$inci_all,labels=T)
hist(data_low$inci_as,labels=T)
hist(data_low$prev_all,labels=T)
hist(data_low$prev_as,labels=T)
hist(data_low$mort_all)
hist(data_low$mort_as)
hist(data_low$hw,labels = T)
hist(data_low$HW_count,labels=T)
hist(data_low$HW_days,labels=T)
hist(data_low$HW_degree,labels=T)
hist(data_low$HW_mean_length,labels=T)
hist(data_low$HW_mean_degree,labels=T)
hist(data_low$HW_max_degree,labels=T)
hist(data_low$gdp,labels=T)
hist(data_low$pop,labels=T)
hist(data_low$temp,labels=T)
hist(data_low$cw,labels=T)
hist(data_low$sex_ratio,labels=T)
hist(data_low$age_ratio,labels=T)

data_low$inci_all = log(data_low$inci_all)
data_low$prev_all = log(data_low$prev_all)
data_low$HW_count = log(data_low$HW_count+1)
data_low$HW_days = log(data_low$HW_days+1)
data_low$HW_degree = log(data_low$HW_degree+1)
data_low$HW_mean_length = log(data_low$HW_mean_length+1)
data_low$HW_mean_degree = log(data_low$HW_mean_degree+1)
data_low$HW_max_degree = log(data_low$HW_max_degree+1)
data_low$gdp = log(data_low$gdp)
data_low$pop = log(data_low$pop)
data_low$sex_ratio = log(data_low$sex_ratio)
data_low$age_ratio = log(data_low$age_ratio)

data_low$inci_all = scale(data_low$inci_all)
data_low$inci_as = scale(data_low$inci_as)
data_low$prev_all = scale(data_low$prev_all)
data_low$prev_as = scale(data_low$prev_as)
data_low$mort_all = scale(data_low$mort_all)
data_low$mort_as = scale(data_low$mort_as)
data_low$pop = scale(data_low$pop)
data_low$gdp = scale(data_low$gdp)
data_low$hw = scale(data_low$hw)
data_low$HW_count = scale(data_low$HW_count)
data_low$HW_days = scale(data_low$HW_days)
data_low$HW_degree = scale(data_low$HW_degree)
data_low$HW_mean_length = scale(data_low$HW_mean_length)
data_low$HW_mean_degree = scale(data_low$HW_mean_degree)
data_low$HW_max_degree = scale(data_low$HW_max_degree)
data_low$temp = scale(data_low$temp)
data_low$cw = scale(data_low$cw)
data_low$sex_ratio = scale(data_low$sex_ratio)
data_low$age_ratio = scale(data_low$age_ratio)

# *4.2 mixed effect models------------------------------------------------------

model = lmer(inci_as~year*hw*gdp+(1+year|country),
             data = data_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+mort_as+(1+year|country),
             data = data_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+temp+(1+year|country),
             data = data_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+sex_ratio+(1+year|country),
             data = data_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+age_ratio+(1+year|country),
             data = data_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+sex_ratio+age_ratio+(1+year|country),
             data = data_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+(1+year|country),
             data = data_low,
             weights = popweight,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+cw+(1+year|country),
             data = data_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw+gdp+(1+year|country),
             data = data_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+(1+year|country),
             data = data_low[which(data_low$year!=7),],
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(prev_as~year*hw*gdp+(1+year|country),
             data = data_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)


# with other heatwave indexes
model = lmer(inci_as~year*HW_count*gdp+(1+year|country),
             data = data_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*HW_days*gdp+(1+year|country),
             data = data_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*HW_degree*gdp+(1+year|country),
             data = data_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*HW_mean_length*gdp+(1+year|country),
             data = data_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*HW_mean_degree*gdp+(1+year|country),
             data = data_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*HW_max_degree*gdp+(1+year|country),
             data = data_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

################################################################################
# mixed effect models with accdata
################################################################################

# 1.  data preprocessing--------------------------------------------------------

# *1.1 all accdata--------------------------------------------------------------
hist(accdata$inci_as,labels=T)
hist(accdata$acc3,labels=T)
hist(accdata$inci_as_delta,labels=T)
hist(accdata$gdp,labels=T)

accdata$gdp = log(accdata$gdp)

accdata$inci_as = scale(accdata$inci_as)
accdata$gdp = scale(accdata$gdp)
accdata$inci_as_delta = scale(accdata$inci_as_delta)
accdata$acc3 = scale(accdata$acc3)

# *1.2 high income--------------------------------------------------------------

hist(accdata_high$inci_as,labels=T)
hist(accdata_high$acc7,labels=T)
hist(accdata_high$inci_as_delta,labels=T)
hist(accdata_high$gdp,labels=T)

accdata_high$gdp = log(accdata_high$gdp)

accdata_high$inci_as = scale(accdata_high$inci_as)
accdata_high$gdp = scale(accdata_high$gdp)
accdata_high$inci_as_delta = scale(accdata_high$inci_as_delta)
accdata_high$acc7 = scale(accdata_high$acc7)

# *1.3 mid and low income-------------------------------------------------------


hist(accdata_low$inci_as,labels=T)
hist(accdata_low$acc3,labels=T)
hist(accdata_low$inci_as_delta,labels=T)
hist(accdata_low$gdp,labels=T)

accdata_low$gdp = log(accdata_low$gdp)

accdata_low$inci_as = scale(accdata_low$inci_as)
accdata_low$gdp = scale(accdata_low$gdp)
accdata_low$inci_as_delta = scale(accdata_low$inci_as_delta)
accdata_low$acc3 = scale(accdata_low$acc3)

# 2. mixed effect models--------------------------------------------------------

# with incidence
model = lmer(inci_as~year*acc3*gdp+(1+year|country),data = accdata,
             control=lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*acc3*gdp+gnigroup+gnigroup:acc3:year+(1+year|country),data=accdata,
             control=lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*acc7*gdp+(1+year|country),
             data = accdata_high)
summary(model)
confint(model)

model = lmer(inci_as~year*acc3*gdp+(1+year|country),
             data = accdata_low)
summary(model)
confint(model)

# with incidence_delta
model = lmer(inci_as_delta~acc3*gdp+(1|country),data = accdata,
             control=lmerControl(optimizer ="Nelder_Mead"))
summary(model)
ts.diag(fit$residuals)

model = lmer(inci_as_delta~acc7*gdp+(1|country),
             data = accdata_high)
summary(model)

model = lmer(inci_as_delta~acc3*gdp+(1|country),
             data = accdata_low)
summary(model)

################################################################################
# mixed effect models with incrementdata
################################################################################

# 1.  data preprocessing--------------------------------------------------------

# *1.1 all data-----------------------------------------------------------------
hist(incrementdata$inci_as_delta,labels=T)
hist(incrementdata$hw,labels=T)
hist(incrementdata$gdp,labels=T)

incrementdata$gdp = log(incrementdata$gdp)

incrementdata$inci_as_delta = scale(incrementdata$inci_as_delta)
incrementdata$hw = scale(incrementdata$hw)
incrementdata$gdp = scale(incrementdata$gdp)

# *1.2 high income--------------------------------------------------------------

hist(incrementdata_high$inci_as_delta,labels=T)
hist(incrementdata_high$hw,labels=T)
hist(incrementdata_high$gdp,labels=T)

incrementdata_high$gdp = log(incrementdata_high$gdp)

incrementdata_high$inci_as_delta = scale(incrementdata_high$inci_as_delta)
incrementdata_high$hw = scale(incrementdata_high$hw)
incrementdata_high$gdp = scale(incrementdata_high$gdp)

# *1.3 mid and low income-------------------------------------------------------

hist(incrementdata_low$inci_as_delta,labels=T)
hist(incrementdata_low$hw,labels=T)
hist(incrementdata_low$gdp,labels=T)

incrementdata_low$gdp = log(incrementdata_low$gdp)

incrementdata_low$inci_as_delta = scale(incrementdata_low$inci_as_delta)
incrementdata_low$hw = scale(incrementdata_low$hw)
incrementdata_low$gdp = scale(incrementdata_low$gdp)

# 2. mixed effect models--------------------------------------------------------

model = lmer(inci_as_delta~hw*gdp+(1|country),data = incrementdata,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)

model = lmer(inci_as_delta~hw*gdp+(1|country),
             data = incrementdata_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)

model = lmer(inci_as_delta~hw*gdp+(1|country),
             data = incrementdata_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)

