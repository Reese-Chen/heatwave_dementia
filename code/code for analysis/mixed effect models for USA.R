# load packages
library(lme4)
library(lmerTest)
library(dplyr)

################################################################################
# read data
#######################################

rm(list=ls())
setwd("d:/heatwave and dementia/data")
inci_as = read.table(file = "age-standardized both sex incidence rate of each state in USA.csv",header=T,
                     sep=",",fill = T,encoding="UTF-8")
hw = read.table(file = "hw_usa_state.csv", header = T , 
                sep = "," , fill = TRUE , encoding = "UTF-8")
gdp = read.table(file = "美国各州gdp时间序列.csv", header = T , 
                 sep = "," , fill = TRUE , encoding = "UTF-8")

gdp_per_capita = read.table("per capita gdp by state in USA.csv",header = T,
                            sep = "," , fill = TRUE , encoding = "UTF-8")

# sort
inci_as = inci_as[order(inci_as$location_name,inci_as$year),]
gdp = gdp[order(gdp$GeoName),]
hw = hw[order(hw$state),]
gdp_per_capita = gdp_per_capita[order(gdp_per_capita$Area),]

# get state
state = hw$state
state = state[order(state)]
inci_as = inci_as[which(inci_as$location_name %in% state),]
gdp = gdp[which(gdp$GeoName %in% state),]
gdp_per_capita = gdp_per_capita[gdp_per_capita$Area %in% state,]

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
    new_gdp_per_capita = rep(NA,46)
  }
  else{
    
    new_inci_as = append(new_inci_as,inci_as[inci_as$year==i,]$val)
    new_hw = append(new_hw,hw[,i-1997+9])
    new_gdp = append(new_gdp,gdp[,i-1997+3])
    year = append(year,rep(i-1997,46))
    new_state = append(new_state,state)
    if (i>=2008 & i<=2017){
      new_gdp_per_capita = append(new_gdp_per_capita,gdp_per_capita[,i-2007])
    }
    else{
      new_gdp_per_capita = append(new_gdp_per_capita,rep(NA,46))
    }
  }
}

# combine data
data = cbind(new_inci_as,new_hw,new_gdp,new_gdp_per_capita,year)
data = as.data.frame(data)
data$state = new_state
data$state = as.factor(data$state)
colnames(data) = c('inci_as','hw','gdp','gdp_per_cap','year','state')

data[, 1:5] <- lapply(data[, 1:5], as.numeric)


################################################################################
# reform data with acc3
################################################################################

accdata = data
accdata$acc3 = accdata$hw
for (i in 1999:2019){
  y = i-1997
  accdata[which(accdata$year==y),]$acc3 = accdata[which(accdata$year==y),]$hw+
    accdata[which(accdata$year==y-1),]$hw+accdata[which(accdata$year==y-2),]$hw
}
accdata = accdata[which(accdata$year>1),]
accdata$year = accdata$year-2


#########################################
# run mixed effect model with hw
#########################################

variables_to_scale <- c("hw", "gdp","gdp_per_cap")

data_scale <- data %>%
  group_by(state) %>%
  mutate(across(all_of(variables_to_scale), 
                ~ scale(., center = TRUE, scale = TRUE))) %>%
  ungroup()

model = lmer(inci_as~year*hw*gdp_per_cap+(1+year|state),data = data_scale[!is.na(data_scale$gdp_per_cap),],
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)
anova(model)
ranova(model)

model = lm(inci_as~hw*gdp+year,data_scale[data_scale$state=="Alaska",])
summary(model)
confint(model)

################################################################################
# regression with acc3
################################################################################

variables_to_scale <- c("acc3", "gdp","gdp_per_cap")

accdata_scale <- accdata %>%
  group_by(state) %>%
  mutate(across(all_of(variables_to_scale), 
                ~ scale(., center = TRUE, scale = TRUE))) %>%
  ungroup()

model = lmer(inci_as~year*acc3*gdp_per_cap+(1+year|state),data = accdata_scale[!is.na(accdata_scale$gdp_per_cap),],
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)




