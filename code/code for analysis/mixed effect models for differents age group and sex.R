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
library(forestmodel)
library(devtools)
library(usethis)
library(readxl)
library(openxlsx)
library(forester)

################################################################################
# load data
################################################################################

# 1. read data------------------------------------------------------------------
setwd("d:/heatwave and dementia/data")
data = read.table(file = "data for analysis.csv", header = T , check.names=F,
                  sep = "," , fill = TRUE , encoding = "UTF-8",quote="")
data = data[,-1]
inci_new = read.table(file = "incidence rate of dementia grouped by age and sex.csv",
                      header=T,sep = ",",fill=TRUE,encoding = "UTF-8",quote="")

# 2. rename---------------------------------------------------------------------
inci_new$location_name[which(inci_new$location_name=='Bolivia (Plurinational State of)')] = 'Bolivia'
inci_new$location_name[which(inci_new$location_name=="Côte d'Ivoire")] = "C?te d'Ivoire"
inci_new$location_name[which(inci_new$location_name=='Iran (Islamic Republic of)')] = 'Iran'
inci_new$location_name[which(inci_new$location_name=='Republic of Korea')] = 'Korea'
inci_new$location_name[which(inci_new$location_name=='Russian Federation')] = 'Russia'
inci_new$location_name[which(inci_new$location_name=='Solomon Islands')] = 'Solomon Islands (the)'
inci_new$location_name[which(inci_new$location_name=='United Republic of Tanzania')] = 'Tanzania'
inci_new$location_name[which(inci_new$location_name=='United States of America')] = 'United States'
inci_new$location_name[which(inci_new$location_name=='Venezuela (Bolivarian Republic of)')] = 'Venezuela'
inci_new$location_name[which(inci_new$location_name=='Czechia')] = 'Czech Republic'
inci_new$location_name[which(inci_new$location_name=='Libya')] = 'Libyan Arab Jamahiriya'
inci_new$location_name[which(inci_new$location_name=='Republic of Moldova')] = 'Moldova'

# 3. select countries-----------------------------------------------------------
country = unique(data$country)
inci_new = inci_new[which(inci_new$location_name %in% country),]

# 4. sort-----------------------------------------------------------------------
inci_new = inci_new[order(inci_new$location_name,inci_new$year),]

################################################################################
# reform data
################################################################################

all_male = inci_new[which(inci_new$sex_id==1 & inci_new$age_id==22),]
all_female = inci_new[which(inci_new$sex_id==2 & inci_new$age_id==22),]
all_both = inci_new[which(inci_new$sex_id==3 & inci_new$age_id==22),]

male15_49 = inci_new[which(inci_new$sex_id==1 & inci_new$age_id==24),]
female15_49 = inci_new[which(inci_new$sex_id==2 & inci_new$age_id==24),]
both15_49 = inci_new[which(inci_new$sex_id==3 & inci_new$age_id==24),]

male50_69 = inci_new[which(inci_new$sex_id==1 & inci_new$age_id==25),]
female50_69 = inci_new[which(inci_new$sex_id==2 & inci_new$age_id==25),]
both50_69 = inci_new[which(inci_new$sex_id==3 & inci_new$age_id==25),]

male70 = inci_new[which(inci_new$sex_id==1 & inci_new$age_id==26),]
female70 = inci_new[which(inci_new$sex_id==2 & inci_new$age_id==26),]
both70 = inci_new[which(inci_new$sex_id==3 & inci_new$age_id==26),]

male85 = inci_new[which(inci_new$sex_id==1 & inci_new$age_id==160),]
female85 = inci_new[which(inci_new$sex_id==2 & inci_new$age_id==160),]
both85 = inci_new[which(inci_new$sex_id==3 & inci_new$age_id==160),]

for (i in 1990:2019){
  if (i==1990){
    new_all_both = all_both[all_both$year==i,]$val
    new_all_male =all_male[all_male$year==i,]$val
    new_all_female =all_female[all_female$year==i,]$val
    
    new_male15_49 = male15_49[male15_49$year==i,]$val
    new_female15_49 = female15_49[female15_49$year==i,]$val
    new_both15_49 = both15_49[both15_49$year==i,]$val
    
    new_male50_69 = male50_69[male50_69$year==i,]$val
    new_female50_69 = female50_69[female50_69$year==i,]$val
    new_both50_69 = both50_69[both50_69$year==i,]$val
    
    new_male70 = male70[male70$year==i,]$val
    new_female70 = female70[female70$year==i,]$val
    new_both70 = both70[both70$year==i,]$val
    
    new_male85 = male85[male85$year==i,]$val
    new_female85 = female85[female85$year==i,]$val
    new_both85 = both85[both85$year==i,]$val
    
  }
  else{
    new_all_both = append(new_all_both,all_both[all_both$year==i,]$val)
    new_all_male = append(new_all_male,all_male[all_male$year==i,]$val)
    new_all_female = append(new_all_female,all_female[all_female$year==i,]$val)
    
    new_male15_49 = append(new_male15_49,male15_49[male15_49$year==i,]$val)
    new_female15_49 = append(new_female15_49,female15_49[female15_49$year==i,]$val)
    new_both15_49 = append(new_both15_49,both15_49[both15_49$year==i,]$val)
    
    new_male50_69 = append(new_male50_69,male50_69[male50_69$year==i,]$val)
    new_female50_69 = append(new_female50_69,female50_69[female50_69$year==i,]$val)
    new_both50_69 = append(new_both50_69,both50_69[both50_69$year==i,]$val)
    
    new_male70 = append(new_male70,male70[male70$year==i,]$val)
    new_female70 = append(new_female70,female70[female70$year==i,]$val)
    new_both70 = append(new_both70,both70[both70$year==i,]$val)
    
    new_male85 = append(new_male85,male85[male85$year==i,]$val)
    new_female85 = append(new_female85,female85[female85$year==i,]$val)
    new_both85 = append(new_both85,both85[both85$year==i,]$val)
  }
}

data = cbind(data,
             new_all_both,new_all_male,new_all_female,
             new_male15_49,new_female15_49,new_both15_49,
             new_male50_69,new_female50_69,new_both50_69,
             new_male70,new_female70,new_both70,
             new_male85,new_female85,new_both85)
colnames(data)[28:42] = c('all_both','all_male','all_female',
                   'male15_49','female15_49','both15_49',
                   'male50_69','female50_69','both50_69',
                   'male70','female70','both70',
                   'male85','female85','both85')

################################################################################
# mixed effect models for 137 countries
################################################################################

# 1. reform accdata-------------------------------------------------------------

accdata = data
accdata$acc3 = accdata$hw
for (i in 1992:2019){
  y = i-1990
  accdata[which(accdata$year==y),]$acc3 = accdata[which(accdata$year==y),]$hw+
    accdata[which(accdata$year==y-1),]$hw+accdata[which(accdata$year==y-2),]$hw
}
accdata = accdata[which(accdata$year>1),]
accdata$year = accdata$year-2

# 2. prepocessing---------------------------------------------------------------

accdata$gdp = log(accdata$gdp)
accdata$all_both = log(accdata$all_both)
accdata$all_male = log(accdata$all_male)
accdata$all_female = log(accdata$all_female)
accdata[,c(1:19,21:25,28:43)] = scale(accdata[,c(1:19,21:25,28:43)])

# 2. mixed effect models--------------------------------------------------------

model = lmer(all_both~year*acc3*gdp+(1+year|country),data = accdata,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(all_male~year*acc3*gdp+(1+year|country),data = accdata,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(all_female~year*acc3*gdp+(1+year|country),data = accdata,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(both15_49~year*acc3*gdp+(1+year|country),data = accdata,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(male15_49~year*acc3*gdp+(1+year|country),data = accdata,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(female15_49~year*acc3*gdp+(1+year|country),data = accdata,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(both50_69~year*acc3*gdp+(1+year|country),data = accdata,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(male50_69~year*acc3*gdp+(1+year|country),data = accdata,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(female50_69~year*acc3*gdp+(1+year|country),data = accdata,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(both70~year*acc3*gdp+(1+year|country),data = accdata,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(male70~year*acc3*gdp+(1+year|country),data = accdata,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(female70~year*acc3*gdp+(1+year|country),data = accdata,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(both85~year*acc3*gdp+(1+year|country),data = accdata,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(male85~year*acc3*gdp+(1+year|country),data = accdata,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(female85~year*acc3*gdp+(1+year|country),data = accdata,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

################################################################################
# mixed effect models for high-income countries
################################################################################
# 1. reform accdata-------------------------------------------------------------

accdata_high = data[which(data$gnigroup=='high income'),]
accdata_high$acc7 = accdata_high$hw
for (i in 1996:2019){
  y = i-1990
  accdata_high[which(accdata_high$year==y),]$acc7 = accdata_high[which(accdata_high$year==y),]$hw+
    accdata_high[which(accdata_high$year==y-1),]$hw+accdata_high[which(accdata_high$year==y-2),]$hw+
    accdata_high[which(accdata_high$year==y-3),]$hw+accdata_high[which(accdata_high$year==y-4),]$hw+
    accdata_high[which(accdata_high$year==y-5),]$hw+accdata_high[which(accdata_high$year==y-6),]$hw
}
accdata_high = accdata_high[which(accdata_high$year>5),]
accdata_high$year = accdata_high$year-6

# 2. prepocessing---------------------------------------------------------------

accdata_high$gdp = log(accdata_high$gdp)
accdata_high$all_both = log(accdata_high$all_both)
accdata_high$all_male = log(accdata_high$all_male)
accdata_high$all_female = log(accdata_high$all_female)
accdata_high[,c(1:19,21:25,28:43)] = scale(accdata_high[,c(1:19,21:25,28:43)])

# 2. mixed effect models--------------------------------------------------------

model = lmer(all_both~year*acc7*gdp+(1+year|country),data = accdata_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(all_male~year*acc7*gdp+(1+year|country),data = accdata_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(all_female~year*acc7*gdp+(1+year|country),data = accdata_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(both15_49~year*acc7*gdp+(1+year|country),data = accdata_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(male15_49~year*acc7*gdp+(1+year|country),data = accdata_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(female15_49~year*acc7*gdp+(1+year|country),data = accdata_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(both50_69~year*acc7*gdp+(1+year|country),data = accdata_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(male50_69~year*acc7*gdp+(1+year|country),data = accdata_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(female50_69~year*acc7*gdp+(1+year|country),data = accdata_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(both70~year*acc7*gdp+(1+year|country),data = accdata_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(male70~year*acc7*gdp+(1+year|country),data = accdata_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(female70~year*acc7*gdp+(1+year|country),data = accdata_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(both85~year*acc7*gdp+(1+year|country),data = accdata_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(male85~year*acc7*gdp+(1+year|country),data = accdata_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(female85~year*acc7*gdp+(1+year|country),data = accdata_high,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)


################################################################################
# mixed effect models for low- to middle-income countries
################################################################################

# 1. reform accdata-------------------------------------------------------------

accdata = data
accdata$acc3 = accdata$hw
for (i in 1992:2019){
  y = i-1990
  accdata[which(accdata$year==y),]$acc3 = accdata[which(accdata$year==y),]$hw+
    accdata[which(accdata$year==y-1),]$hw+accdata[which(accdata$year==y-2),]$hw
}
accdata = accdata[which(accdata$year>1),]
accdata$year = accdata$year-2

accdata_low = accdata[which(accdata$gnigroup=='mid and low income'),]

# 2. prepocessing---------------------------------------------------------------

accdata_low$all_both = log(accdata_low$all_both)
accdata_low$all_male = log(accdata_low$all_male)
accdata_low$all_female = log(accdata_low$all_female)
accdata_low$gdp = log(accdata_low$gdp)

accdata_low[,c(1:19,21:25,28:43)] = scale(accdata_low[,c(1:19,21:25,28:43)])

# 3. mixed effect models--------------------------------------------------------

model = lmer(all_both~year*acc3*gdp+(1+year|country),data = accdata_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(all_male~year*acc3*gdp+(1+year|country),data = accdata_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(all_female~year*acc3*gdp+(1+year|country),data = accdata_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(both15_49~year*acc3*gdp+(1+year|country),data = accdata_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(male15_49~year*acc3*gdp+(1+year|country),data = accdata_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(female15_49~year*acc3*gdp+(1+year|country),data = accdata_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(both50_69~year*acc3*gdp+(1+year|country),data = accdata_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(male50_69~year*acc3*gdp+(1+year|country),data = accdata_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(female50_69~year*acc3*gdp+(1+year|country),data = accdata_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(both70~year*acc3*gdp+(1+year|country),data = accdata_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(male70~year*acc3*gdp+(1+year|country),data = accdata_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(female70~year*acc3*gdp+(1+year|country),data = accdata_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(both85~year*acc3*gdp+(1+year|country),data = accdata_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(male85~year*acc3*gdp+(1+year|country),data = accdata_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(female85~year*acc3*gdp+(1+year|country),data = accdata_low,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

################################################################################
# visualzation
################################################################################

# 1. high-income countries------------------------------------------------------
df_high =  read.table(file = "plot data for high income countries.csv", header = T ,
                      check.names=F,sep = "," , fill = TRUE , encoding = "UTF-8")
colnames(df_high) = c('Subgroup','Estimate','CI_low','CI_high','p')


p = forester(left_side_data = df_high[,c(1,5)],   
             estimate = df_high$Estimate,      
             ci_low = df_high$CI_low,        
             ci_high = df_high$CI_high,  
             estimate_precision = 3,
             xlim = c(-0.015, 0.015),
             xbreaks = c(-0.015, -0.0075,0.000, 0.0075, 0.015),
             dpi=300,
             display = TRUE,
             render_as='pdf'
)+
  easy_remove_x_axis()

# 2. low- to middle-income countries--------------------------------------------
df_low =  read.table(file = "plot data for low to middle income countries.csv", header = T ,
                      check.names=F,sep = "," , fill = TRUE , encoding = "UTF-8")
colnames(df_low) = c('Subgroup','Estimate','CI_low','CI_high','p')

p = forester(left_side_data = df_low[,c(1,5)],   
             estimate = df_low$Estimate,      
             ci_low = df_low$CI_low,        
             ci_high = df_low$CI_high,  
             estimate_precision = 3,
             xlim = c(-0.005, 0.005),
             xbreaks = c(-0.005,-0.0025,0.000,0.0025,0.005),
             dpi=300,
             display = TRUE,
             render_as='pdf')

