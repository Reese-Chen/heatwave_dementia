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

################################################################################
# load data
################################################################################

# 1. read data------------------------------------------------------------------
setwd("d:/heatwave and dementia/data")
inci = read.table(file = "incidence rate all age and AS both sex.csv", header = T , 
                  sep = "," , fill = TRUE , encoding = "UTF-8",quote="")
prev_all = read.table(file = "prevalence rate of dementia all age both sex.csv", header = T , 
                      sep = "," , fill = TRUE , encoding = "UTF-8",quote="")
prev_as = read.table(file = "prevalence rate of dementia as both sex.csv", header = T , 
                     sep = "," , fill = TRUE , encoding = "UTF-8",quote="")
allmort_all = read.table(file = "mortality for all cause all age.csv", header = T , 
                         sep = "," , fill = TRUE , encoding = "UTF-8",quote="")
allmort_as = read.table(file = "mortality for all cause AS.csv", header = T , 
                        sep = "," , fill = TRUE , encoding = "UTF-8",quote="")
dementiamort_all = read.table(file = "mortality for dementia all age.csv", header = T , 
                              sep = "," , fill = TRUE , encoding = "UTF-8",quote="")
dementiamort_as = read.table(file = "mortality for dementia AS.csv", header = T , 
                             sep = "," , fill = TRUE , encoding = "UTF-8",quote="")

hw = read.table(file = "population weighted hw with country.csv", header = T , 
                sep = "," , fill = TRUE , encoding = "UTF-8",quote="")
hw_count = read.table(file = "HW_count_country.csv", header = F , 
                      sep = "," , fill = TRUE , encoding = "UTF-8",quote='')
hw_days = read.table(file = "HW_days_country.csv", header = F , 
                     sep = "," , fill = TRUE , encoding = "UTF-8",quote='')
hw_degree = read.table(file = "HW_degree_country.csv", header = F , 
                       sep = "," , fill = TRUE , encoding = "UTF-8",quote='')
hw_mean_length = read.table(file = "HW_mean_length_country.csv", header = F , 
                            sep = "," , fill = TRUE , encoding = "UTF-8",quote='')
hw_mean_degree = read.table(file = "HW_mean_degree_country.csv", header = F , 
                            sep = "," , fill = TRUE , encoding = "UTF-8",quote='')
hw_max_degree = read.table(file = "HW_max_degree_country.csv", header = F , 
                           sep = "," , fill = TRUE , encoding = "UTF-8",quote='')

gdp = read.table(file = "GDP世界各国1960-2021.csv", header = T , 
                 sep = "\t" , fill = TRUE , encoding = "UTF-8",quote="")
pop = read.table(file = "population 266 countries 1990-2019.csv",header=T,
                 sep = ",", fill = TRUE, encoding = "UTF-8",quote='')
temp = read.table(file = "各国30年最热月份平均温度.csv",header=T,
                  sep = ",", fill = TRUE, encoding = "UTF-8",quote="")
GNIcata = read.table(file = "高中低收入国家划分2019.csv",header=F,
                     sep = ",", fill = TRUE, encoding = "UTF-8",quote="")
GNIcata[1,1] = 'Afghanistan'
age_ratio = read.table(file = "age dependency ratio by country 2023.csv",header=T,
                       sep = ",", fill = TRUE)
sex_ratio = read.table(file = "sex ratio by countries.csv",header=T,
                       sep = ",", fill = TRUE)
cw = read.table(file = "cw_country.csv",header=F,
                sep = ",", fill = TRUE,encoding = "UTF-8",quote='')

# 2. rename some country--------------------------------------------------------

# HW
hw$cname[which(hw$cname=='United Arab Emirates ')]='United Arab Emirates'
hw_count$V31[which(hw_count$V31=='United Arab Emirates ')]='United Arab Emirates'
hw_days$V31[which(hw_days$V31=='United Arab Emirates ')]='United Arab Emirates'
hw_degree$V31[which(hw_degree$V31=='United Arab Emirates ')]='United Arab Emirates'
hw_mean_length$V31[which(hw_mean_length$V31=='United Arab Emirates ')]='United Arab Emirates'
hw_mean_degree$V31[which(hw_mean_degree$V31=='United Arab Emirates ')]='United Arab Emirates'
hw_max_degree$V31[which(hw_max_degree$V31=='United Arab Emirates ')]='United Arab Emirates'
temp$country[which(temp$country=='United Arab Emirates ')]='United Arab Emirates'
cw$V31[which(cw$V31=='United Arab Emirates ')]='United Arab Emirates'


# GDP
gdp$Country.Name[which(gdp$Country.Name=='\"Bahamas, The\"')] = 'Bahamas'
gdp$Country.Name[which(gdp$Country.Name=='\"Congo, Rep.\"')] = 'Congo'
gdp$Country.Name[which(gdp$Country.Name=="Cote d'Ivoire")] = "C?te d'Ivoire"
gdp$Country.Name[which(gdp$Country.Name=='Czechia')] = 'Czech Republic'
gdp$Country.Name[which(gdp$Country.Name=='\"Egypt, Arab Rep.\"')] = 'Egypt'
gdp$Country.Name[which(gdp$Country.Name=='\"Iran, Islamic Rep.\"')] = 'Iran'
gdp$Country.Name[which(gdp$Country.Name=='\"Korea, Rep.\"')] = 'Korea'
gdp$Country.Name[which(gdp$Country.Name=='Kyrgyz Republic')] = 'Kyrgyzstan'
gdp$Country.Name[which(gdp$Country.Name=='Lao PDR')] = "Lao People's Democratic Republic"
gdp$Country.Name[which(gdp$Country.Name=='Libya')] = 'Libyan Arab Jamahiriya'
gdp$Country.Name[which(gdp$Country.Name=='Russian Federation')] = 'Russia'
gdp$Country.Name[which(gdp$Country.Name=='St. Lucia')] = 'Saint Lucia'
gdp$Country.Name[which(gdp$Country.Name=='Slovak Republic')] = 'Slovakia'
gdp$Country.Name[which(gdp$Country.Name=='Solomon Islands')] = 'Solomon Islands (the)'
gdp$Country.Name[which(gdp$Country.Name=='Turkiye')] = 'Turkey'
gdp$Country.Name[which(gdp$Country.Name=='\"Venezuela, RB\"')] = 'Venezuela'
gdp$Country.Name[which(gdp$Country.Name=='Vietnam')] = 'Viet Nam'
gdp$Country.Name[which(gdp$Country.Name=='\"Yemen, Rep.\"')] = 'Yemen'

#pop
pop$Country.Name[which(pop$Country.Name=="Cote d'Ivoire")] = "C?te d'Ivoire"
pop$Country.Name[which(pop$Country.Name=='Czechia')] = 'Czech Republic'
pop$Country.Name[which(pop$Country.Name=='Kyrgyz Republic')] = 'Kyrgyzstan'
pop$Country.Name[which(pop$Country.Name=='Lao PDR')] = "Lao People's Democratic Republic"
pop$Country.Name[which(pop$Country.Name=='Libya')] = 'Libyan Arab Jamahiriya'
pop$Country.Name[which(pop$Country.Name=='Russian Federation')] = 'Russia'
pop$Country.Name[which(pop$Country.Name=='St. Lucia')] = 'Saint Lucia'
pop$Country.Name[which(pop$Country.Name=='Slovak Republic')] = 'Slovakia'
pop$Country.Name[which(pop$Country.Name=='Solomon Islands')] = 'Solomon Islands (the)'
pop$Country.Name[which(pop$Country.Name=='Turkiye')] = 'Turkey'
pop$Country.Name[which(pop$Country.Name=='Vietnam')] = 'Viet Nam'

# inci
inci$location_name[which(inci$location_name=='Bolivia (Plurinational State of)')] = 'Bolivia'
inci$location_name[which(inci$location_name=="Côte d'Ivoire")] = "C?te d'Ivoire"
inci$location_name[which(inci$location_name=='Iran (Islamic Republic of)')] = 'Iran'
inci$location_name[which(inci$location_name=='Republic of Korea')] = 'Korea'
inci$location_name[which(inci$location_name=='Russian Federation')] = 'Russia'
inci$location_name[which(inci$location_name=='Solomon Islands')] = 'Solomon Islands (the)'
inci$location_name[which(inci$location_name=='United Republic of Tanzania')] = 'Tanzania'
inci$location_name[which(inci$location_name=='United States of America')] = 'United States'
inci$location_name[which(inci$location_name=='Venezuela (Bolivarian Republic of)')] = 'Venezuela'
inci$location_name[which(inci$location_name=='Czechia')] = 'Czech Republic'
inci$location_name[which(inci$location_name=='Libya')] = 'Libyan Arab Jamahiriya'
inci$location_name[which(inci$location_name=='Republic of Moldova')] = 'Moldova'

prev_as$location_name[which(prev_as$location_name=='Bolivia (Plurinational State of)')] = 'Bolivia'
prev_as$location_name[which(prev_as$location_name=="Côte d'Ivoire")] = "C?te d'Ivoire"
prev_as$location_name[which(prev_as$location_name=='Iran (Islamic Republic of)')] = 'Iran'
prev_as$location_name[which(prev_as$location_name=='Republic of Korea')] = 'Korea'
prev_as$location_name[which(prev_as$location_name=='Russian Federation')] = 'Russia'
prev_as$location_name[which(prev_as$location_name=='Solomon Islands')] = 'Solomon Islands (the)'
prev_as$location_name[which(prev_as$location_name=='United Republic of Tanzania')] = 'Tanzania'
prev_as$location_name[which(prev_as$location_name=='United States of America')] = 'United States'
prev_as$location_name[which(prev_as$location_name=='Venezuela (Bolivarian Republic of)')] = 'Venezuela'
prev_as$location_name[which(prev_as$location_name=='Czechia')] = 'Czech Republic'
prev_as$location_name[which(prev_as$location_name=='Libya')] = 'Libyan Arab Jamahiriya'
prev_as$location_name[which(prev_as$location_name=='Republic of Moldova')] = 'Moldova'

prev_all$location_name[which(prev_all$location_name=='Bolivia (Plurinational State of)')] = 'Bolivia'
prev_all$location_name[which(prev_all$location_name=="Côte d'Ivoire")] = "C?te d'Ivoire"
prev_all$location_name[which(prev_all$location_name=='Iran (Islamic Republic of)')] = 'Iran'
prev_all$location_name[which(prev_all$location_name=='Republic of Korea')] = 'Korea'
prev_all$location_name[which(prev_all$location_name=='Russian Federation')] = 'Russia'
prev_all$location_name[which(prev_all$location_name=='Solomon Islands')] = 'Solomon Islands (the)'
prev_all$location_name[which(prev_all$location_name=='United Republic of Tanzania')] = 'Tanzania'
prev_all$location_name[which(prev_all$location_name=='United States of America')] = 'United States'
prev_all$location_name[which(prev_all$location_name=='Venezuela (Bolivarian Republic of)')] = 'Venezuela'
prev_all$location_name[which(prev_all$location_name=='Czechia')] = 'Czech Republic'
prev_all$location_name[which(prev_all$location_name=='Libya')] = 'Libyan Arab Jamahiriya'
prev_all$location_name[which(prev_all$location_name=='Republic of Moldova')] = 'Moldova'

allmort_all$location_name[which(allmort_all$location_name=='Bolivia (Plurinational State of)')] = 'Bolivia'
allmort_all$location_name[which(allmort_all$location_name=="Côte d'Ivoire")] = "C?te d'Ivoire"
allmort_all$location_name[which(allmort_all$location_name=='Iran (Islamic Republic of)')] = 'Iran'
allmort_all$location_name[which(allmort_all$location_name=='Republic of Korea')] = 'Korea'
allmort_all$location_name[which(allmort_all$location_name=='Russian Federation')] = 'Russia'
allmort_all$location_name[which(allmort_all$location_name=='Solomon Islands')] = 'Solomon Islands (the)'
allmort_all$location_name[which(allmort_all$location_name=='United Republic of Tanzania')] = 'Tanzania'
allmort_all$location_name[which(allmort_all$location_name=='United States of America')] = 'United States'
allmort_all$location_name[which(allmort_all$location_name=='Venezuela (Bolivarian Republic of)')] = 'Venezuela'
allmort_all$location_name[which(allmort_all$location_name=='Czechia')] = 'Czech Republic'
allmort_all$location_name[which(allmort_all$location_name=='Libya')] = 'Libyan Arab Jamahiriya'
allmort_all$location_name[which(allmort_all$location_name=='Republic of Moldova')] = 'Moldova'

allmort_as$location_name[which(allmort_as$location_name=='Bolivia (Plurinational State of)')] = 'Bolivia'
allmort_as$location_name[which(allmort_as$location_name=="Côte d'Ivoire")] = "C?te d'Ivoire"
allmort_as$location_name[which(allmort_as$location_name=='Iran (Islamic Republic of)')] = 'Iran'
allmort_as$location_name[which(allmort_as$location_name=='Republic of Korea')] = 'Korea'
allmort_as$location_name[which(allmort_as$location_name=='Russian Federation')] = 'Russia'
allmort_as$location_name[which(allmort_as$location_name=='Solomon Islands')] = 'Solomon Islands (the)'
allmort_as$location_name[which(allmort_as$location_name=='United Republic of Tanzania')] = 'Tanzania'
allmort_as$location_name[which(allmort_as$location_name=='United States of America')] = 'United States'
allmort_as$location_name[which(allmort_as$location_name=='Venezuela (Bolivarian Republic of)')] = 'Venezuela'
allmort_as$location_name[which(allmort_as$location_name=='Czechia')] = 'Czech Republic'
allmort_as$location_name[which(allmort_as$location_name=='Libya')] = 'Libyan Arab Jamahiriya'
allmort_as$location_name[which(allmort_as$location_name=='Republic of Moldova')] = 'Moldova'

dementiamort_as$location_name[which(dementiamort_as$location_name=='Bolivia (Plurinational State of)')] = 'Bolivia'
dementiamort_as$location_name[which(dementiamort_as$location_name=="Côte d'Ivoire")] = "C?te d'Ivoire"
dementiamort_as$location_name[which(dementiamort_as$location_name=='Iran (Islamic Republic of)')] = 'Iran'
dementiamort_as$location_name[which(dementiamort_as$location_name=='Republic of Korea')] = 'Korea'
dementiamort_as$location_name[which(dementiamort_as$location_name=='Russian Federation')] = 'Russia'
dementiamort_as$location_name[which(dementiamort_as$location_name=='Solomon Islands')] = 'Solomon Islands (the)'
dementiamort_as$location_name[which(dementiamort_as$location_name=='United Republic of Tanzania')] = 'Tanzania'
dementiamort_as$location_name[which(dementiamort_as$location_name=='United States of America')] = 'United States'
dementiamort_as$location_name[which(dementiamort_as$location_name=='Venezuela (Bolivarian Republic of)')] = 'Venezuela'
dementiamort_as$location_name[which(dementiamort_as$location_name=='Czechia')] = 'Czech Republic'
dementiamort_as$location_name[which(dementiamort_as$location_name=='Libya')] = 'Libyan Arab Jamahiriya'
dementiamort_as$location_name[which(dementiamort_as$location_name=='Republic of Moldova')] = 'Moldova'

dementiamort_all$location_name[which(dementiamort_all$location_name=='Bolivia (Plurinational State of)')] = 'Bolivia'
dementiamort_all$location_name[which(dementiamort_all$location_name=="Côte d'Ivoire")] = "C?te d'Ivoire"
dementiamort_all$location_name[which(dementiamort_all$location_name=='Iran (Islamic Republic of)')] = 'Iran'
dementiamort_all$location_name[which(dementiamort_all$location_name=='Republic of Korea')] = 'Korea'
dementiamort_all$location_name[which(dementiamort_all$location_name=='Russian Federation')] = 'Russia'
dementiamort_all$location_name[which(dementiamort_all$location_name=='Solomon Islands')] = 'Solomon Islands (the)'
dementiamort_all$location_name[which(dementiamort_all$location_name=='United Republic of Tanzania')] = 'Tanzania'
dementiamort_all$location_name[which(dementiamort_all$location_name=='United States of America')] = 'United States'
dementiamort_all$location_name[which(dementiamort_all$location_name=='Venezuela (Bolivarian Republic of)')] = 'Venezuela'
dementiamort_all$location_name[which(dementiamort_all$location_name=='Czechia')] = 'Czech Republic'
dementiamort_all$location_name[which(dementiamort_all$location_name=='Libya')] = 'Libyan Arab Jamahiriya'
dementiamort_all$location_name[which(dementiamort_all$location_name=='Republic of Moldova')] = 'Moldova'

# population structure ratio
age_ratio$country[which(age_ratio$country=='Brunei')] = 'Brunei Darussalam'
age_ratio$country[which(age_ratio$country=='Syria')] = 'Syrian Arab Republic'
age_ratio$country[which(age_ratio$country=='South Korea')] = 'Korea'
age_ratio$country[which(age_ratio$country=='Solomon Islands')] = 'Solomon Islands (the)'
age_ratio$country[which(age_ratio$country=='Vietnam')] = 'Viet Nam'
age_ratio$country[which(age_ratio$country=='Laos')] = "Lao People's Democratic Republic"

sex_ratio$country[which(sex_ratio$country=='Brunei')] = 'Brunei Darussalam'
sex_ratio$country[which(sex_ratio$country=='Syria')] = 'Syrian Arab Republic'
sex_ratio$country[which(sex_ratio$country=='South Korea')] = 'Korea'
sex_ratio$country[which(sex_ratio$country=='Solomon Islands')] = 'Solomon Islands (the)'
sex_ratio$country[which(sex_ratio$country=='Vietnam')] = 'Viet Nam'
sex_ratio$country[which(sex_ratio$country=='Laos')] = "Lao People's Democratic Republic"

# 3.select data for these country-----------------------------------------------

country = hw$cname

# exclude countries with pop==0
country = country[-which(country=='Armenia')] 
country = country[-which(country=='Greenland')]

country_inci = unique(inci$location_name) # calculate intersection
country_gdp = unique(gdp$Country.Name)
setdiff(x=country, y=country_inci)
setdiff(x=country, y=country_gdp)
country = intersect(x=country,y=country_inci)
country = intersect(x=country,y=country_gdp)

# exclude countries with missing gdp data
ex_country = gdp$Country.Name[which(rowSums(is.na(gdp))>0)]
ex_country = intersect(x=country,y=ex_country)
for (i in ex_country){
  country = country[-which(country==i)]
}

inci = inci[which(inci$location_name %in% country),]
prev_all = prev_all[which(prev_all$location_name %in% country),]
prev_as = prev_as[which(prev_as$location_name %in% country),]
allmort_all = allmort_all[which(allmort_all$location_name %in% country),]
allmort_as = allmort_as[which(allmort_as$location_name %in% country),]
dementiamort_all = dementiamort_all[which(dementiamort_all$location_name %in% country),]
dementiamort_as = dementiamort_as[which(dementiamort_as$location_name %in% country),]
hw = hw[which(hw$cname %in% country),]
hw_count = hw_count[which(hw_count$V31 %in% country),]
hw_days = hw_days[which(hw_days$V31 %in% country),]
hw_degree = hw_degree[which(hw_degree$V31 %in% country),]
hw_mean_length = hw_mean_length[which(hw_mean_length$V31 %in% country),]
hw_mean_degree = hw_mean_degree[which(hw_mean_degree$V31 %in% country),]
hw_max_degree = hw_max_degree[which(hw_max_degree$V31 %in% country),]
gdp = gdp[which(gdp$Country.Name %in% country),]
pop = pop[which(pop$Country.Name %in% country),]
temp = temp[which(temp$country %in% country),]
cw = cw[which(cw$V31 %in% country),]
sex_ratio = sex_ratio[which(sex_ratio$country %in% country),]
age_ratio = age_ratio[which(age_ratio$country %in% country),]

# complete sex_ratio and age_ratio
sex_diff = setdiff(x=country, y=sex_ratio$country)
for (x in sex_diff){
  cx = rep(NA,16)
  cx[5] = x
  sex_ratio = rbind(sex_ratio,cx)
}
age_diff = setdiff(x=country, y=age_ratio$country)
for (x in age_diff){
  cx = rep(NA,18)
  cx[5] = x
  age_ratio = rbind(age_ratio,cx)
}

# complete gnigroup
setdiff(x=country,y=GNIcata$V1)
cx = c('Bahamas','H')
GNIcata = rbind(GNIcata,cx)
cx = c('Korea','H')
GNIcata = rbind(GNIcata,cx)
cx = c('Russia','H')
GNIcata = rbind(GNIcata,cx)
cx = c('Slovakia','H')
GNIcata = rbind(GNIcata,cx)

# 4. sort-----------------------------------------------------------------------
hw = hw[order(hw$cname),]
hw_count = hw_count[order(hw_count$V31),]
hw_days = hw_days[order(hw_days$V31),]
hw_degree = hw_degree[order(hw_degree$V31),]
hw_mean_degree = hw_mean_degree[order(hw_mean_degree$V31),]
hw_mean_length = hw_mean_length[order(hw_mean_length$V31),]
hw_max_degree = hw_max_degree[order(hw_max_degree$V31),]
inci = inci[order(inci$location_name,inci$year),]
prev_all = prev_all[order(prev_all$location_name,prev_all$year),]
prev_as = prev_as[order(prev_as$location_name,prev_as$year),]
allmort_all = allmort_all[order(allmort_all$location_name,allmort_all$year),]
allmort_as = allmort_as[order(allmort_as$location_name,allmort_as$year),]
dementiamort_all = dementiamort_all[order(dementiamort_all$location_name,dementiamort_all$year),]
dementiamort_as = dementiamort_as[order(dementiamort_as$location_name,dementiamort_as$year),]
temp = temp[order(temp$country),]
gdp = gdp[order(gdp$Country.Name),]
pop = pop[order(pop$Country.Name),]
cw = cw[order(cw$V31),]
sex_ratio = sex_ratio[order(sex_ratio$country),]
age_ratio = age_ratio[order(age_ratio$country),]

# seperate incidence data
inci_all = inci[which(inci$age_id==22),]
inci_as = inci[which(inci$age_id==27),]

# 5. calculate mortality in people with dementia--------------------------------
mortfordementia_all = dementiamort_all
mortfordementia_all$val = dementiamort_all$val/prev_all$val*100000
mortfordementia_as = dementiamort_as
mortfordementia_as$val = dementiamort_as$val/prev_as$val*100000

################################################################################
# reform data
################################################################################

for (i in 1990:2019){
  if (i==1990){
    
    new_inci_all = inci_all[inci_all$year==i,]$val
    new_inci_as = inci_as[inci_as$year==i,]$val
    
    new_prev_all = prev_all[prev_all$year==i,]$val
    new_prev_as = prev_as[prev_as$year==i,]$val
    
    new_allmort_all = allmort_all[allmort_all$year==i,]$val
    new_allmort_as = allmort_as[allmort_as$year==i,]$val
    new_mortfordementia_all = mortfordementia_all[mortfordementia_all$year==i,]$val
    new_mortfordementia_as = mortfordementia_as[mortfordementia_as$year==i,]$val
    
    new_hw = hw[,i-1990+2]
    new_hw_count = hw_count[,i-1990+1]
    new_hw_days = hw_days[,i-1990+1]
    new_hw_degree = hw_degree[,i-1990+1]
    new_hw_mean_length = hw_mean_length[,i-1990+1]
    new_hw_mean_degree = hw_mean_degree[,i-1990+1]
    new_hw_max_degree = hw_max_degree[,i-1990+1]
    new_cw = cw[,i-1990+1]
    
    new_gdp = gdp[,i-1990+3]
    new_temp = temp[,i-1990+2]
    new_pop = pop[,i-1990+2]
    
    year = rep(i-1990,137)
    new_country = country
    
    new_sex_ratio = sex_ratio$mPer100F
    new_age_ratio = age_ratio$adrAgedPerc
    
  }
  else{
    new_inci_all = append(new_inci_all,inci_all[inci_all$year==i,]$val)
    new_inci_as = append(new_inci_as,inci_as[inci_as$year==i,]$val)
    
    new_prev_all = append(new_prev_all,prev_all[prev_all$year==i,]$val)
    new_prev_as = append(new_prev_as,prev_as[prev_as$year==i,]$val)
    
    new_allmort_all = append(new_allmort_all,allmort_all[allmort_all$year==i,]$val)
    new_allmort_as = append(new_allmort_as,allmort_as[allmort_as$year==i,]$val)
    new_mortfordementia_all = append(new_mortfordementia_all,mortfordementia_all[mortfordementia_all$year==i,]$val)
    new_mortfordementia_as = append(new_mortfordementia_as,mortfordementia_as[mortfordementia_as$year==i,]$val)
    
    new_hw = append(new_hw,hw[,i-1990+2])
    new_hw_count = append(new_hw_count,hw_count[,i-1990+1])
    new_hw_days = append(new_hw_days,hw_days[,i-1990+1])
    new_hw_degree = append(new_hw_degree,hw_degree[,i-1990+1])
    new_hw_mean_length = append(new_hw_mean_length,hw_mean_length[,i-1990+1])
    new_hw_mean_degree = append(new_hw_mean_degree,hw_mean_degree[,i-1990+1])
    new_hw_max_degree = append(new_hw_max_degree,hw_max_degree[,i-1990+1])
    new_cw = append(new_cw,cw[,i-1990+1])
    
    new_gdp = append(new_gdp,gdp[,i-1990+3])
    new_temp = append(new_temp,temp[,i-1990+2])
    new_pop = append(new_pop,pop[,i-1990+2])
    
    year = append(year,rep(i-1990,137))
    new_country = append(new_country,country)
    
    new_sex_ratio = append(new_sex_ratio,sex_ratio$mPer100F)
    new_age_ratio = append(new_age_ratio,age_ratio$adrAgedPerc)
  }
}

# combine the data
new_pop = as.numeric(new_pop)
data = cbind(new_inci_all,new_inci_as,
             new_prev_all,new_prev_as,
             new_allmort_all,new_allmort_as,new_mortfordementia_all,new_mortfordementia_as,
             new_hw,new_hw_count,new_hw_days,new_hw_degree,new_hw_mean_length,new_hw_mean_degree,new_hw_max_degree,
             new_cw,new_gdp,new_temp,new_pop,year)

# rename the data
data = as.data.frame(data)
colnames(data) = c('inci_all','inci_as',
                   'prev_all','prev_as',
                   'allmort_all','allmort_as','mortfordementia_all','mortfordementia_as',
                   'hw','HW_count','HW_days','HW_degree','HW_mean_length','HW_mean_degree','HW_max_degree',
                   'cw','gdp','temp','pop','year')

# calculate excessed mortality and popweight
data$mort_all = data$mortfordementia_all - data$allmort_all
data$mort_as = data$mortfordementia_as - data$allmort_as
for (i in 1:30){
  data$popweight[which(data$year==i-1)] = data$pop[which(data$year==i-1)]*137/
    sum(data$pop[which(data$year==i-1)])
}


# combine sex_ratio and age_ratio
data$sex_ratio = new_sex_ratio
data$age_ratio = new_age_ratio
data$sex_ratio = as.numeric(data$sex_ratio)
data$age_ratio = as.numeric(data$age_ratio)

# add coutry
data$country = new_country
data$country = as.factor(data$country)

# add country cata
highcountry = GNIcata[which(GNIcata$V2=='H'),]$V1
midcountry = GNIcata[which(GNIcata$V2=='UM' | GNIcata$V2=='LM'),]$V1
lowcountry = GNIcata[which(GNIcata$V2=='L'),]$V1

# create country group
data$gnigroup = 'mid and low income'
data[which(data$country %in% highcountry),]$gnigroup = 'high income'
data$gnigroup = as.factor(data$gnigroup)

################################################################################
# save data
################################################################################

write.csv(data,'data for analysis.csv',quote = FALSE)

