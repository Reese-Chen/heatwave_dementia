# load packages
library(lme4)
library(lmerTest)
library(dplyr)
library(grid)
library(ggplot2)
library(cowplot)
library(tidyverse)
library(foreign)
library(ggExtra)
library(bstfun)
library(gtsummary)
library(forester)
library(readxl)

################################################################################
# load data
################################################################################

setwd("d:/heatwave and dementia/data")
data = read.table(file = "data for analysis.csv", header = T , check.names=F,
                  sep = "," , fill = TRUE , encoding = "UTF-8",quote="")
data = data[,-1]

################################################################################
# select corresponding gdp per capita 
################################################################################

gdp_per = read.table(file = "GDP per capita by country 1990-2019.csv", header = T , 
                 sep = "," , fill = TRUE , encoding = "UTF-8",quote="")

gdp_per=gdp_per[complete.cases(gdp_per),]

setdiff(x=unique(gdp_per$Country.Name), y=unique(data$country))
setdiff(x=unique(data$country), y=unique(gdp_per$Country.Name))

gdp_per$Country.Name[which(gdp_per$Country.Name=='Bahamas The')] = 'Bahamas'
gdp_per$Country.Name[which(gdp_per$Country.Name=='Congo Rep.')] = 'Congo'
gdp_per$Country.Name[which(gdp_per$Country.Name=="Cote d'Ivoire")] = "C?te d'Ivoire"
gdp_per$Country.Name[which(gdp_per$Country.Name=='Czechia')] = 'Czech Republic'
gdp_per$Country.Name[which(gdp_per$Country.Name=='Egypt Arab Rep.')] = 'Egypt'
gdp_per$Country.Name[which(gdp_per$Country.Name=='Korea Rep.')] = 'Korea'
gdp_per$Country.Name[which(gdp_per$Country.Name=='Kyrgyz Republic')] = 'Kyrgyzstan'
gdp_per$Country.Name[which(gdp_per$Country.Name=='Lao PDR')] = "Lao People's Democratic Republic"
gdp_per$Country.Name[which(gdp_per$Country.Name=='Libya')] = 'Libyan Arab Jamahiriya'
gdp_per$Country.Name[which(gdp_per$Country.Name=='Russian Federation')] = 'Russia'
gdp_per$Country.Name[which(gdp_per$Country.Name=='St. Lucia')] = 'Saint Lucia'
gdp_per$Country.Name[which(gdp_per$Country.Name=='Slovak Republic')] = 'Slovakia'
gdp_per$Country.Name[which(gdp_per$Country.Name=='Solomon Islands')] = 'Solomon Islands (the)'
gdp_per$Country.Name[which(gdp_per$Country.Name=='Turkiye')] = 'Turkey'
gdp_per$Country.Name[which(gdp_per$Country.Name=='Yemen Rep.')] = 'Yemen'

gdp_per = gdp_per[which(gdp_per$Country.Name %in% unique(data$country)),]
gdp_per = gdp_per[order(gdp_per$Country.Name),]

################################################################################
# add gdp per capita to data
################################################################################

for (i in 1990:2019){
  if (i==1990){
    new_gdp_per = gdp_per[,i-1990+2]
  }
  else{
    new_gdp_per = append(new_gdp_per,gdp_per[,i-1990+2])
  }
}

# combine the data
data = cbind(data,new_gdp_per)

data_high = data[which(data$gnigroup=='high income'),]
data_low = data[which(data$gnigroup=='mid and low income'),]


################################################################################
# form accdata and increment data
################################################################################

# accdata
cols = c('inci_as','hw','gdp','year','country','gnigroup','pop',"new_gdp_per")
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

cols = c('inci_as','hw','gdp','year','country','gnigroup',"new_gdp_per")
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
# mixed effect models
################################################################################

# 1. preparation----------------------------------------------------------------
hist(data$new_gdp_per,label=T)

data$new_gdp_per = log(data$new_gdp_per)
data[,c(2,9,28)] = scale(data[,c(2,9,28)])

data_high$new_gdp_per = log(data_high$new_gdp_per)
data_high[,c(2,9,28)] = scale(data_high[,c(2,9,28)])

data_low$new_gdp_per = log(data_low$new_gdp_per)
data_low[,c(2,9,28)] = scale(data_low[,c(2,9,28)])

accdata$new_gdp_per = log(accdata$new_gdp_per)
accdata[,c(1,2,8)] = scale(accdata[,c(1,2,8)])

accdata_high$new_gdp_per = log(accdata_high$new_gdp_per)
accdata_high[,c(1,2,8)] = scale(accdata_high[,c(1,2,8)])

accdata_low$new_gdp_per = log(accdata_low$new_gdp_per)
accdata_low[,c(1,2,8)] = scale(accdata_low[,c(1,2,8)])

# 2. mixed effect models--------------------------------------------------------
model = lmer(inci_as~year*hw*new_gdp_per+(1+year|country),data = data,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*new_gdp_per+(1+year|country),data = data_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*new_gdp_per+(1+year|country),data = data_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)
