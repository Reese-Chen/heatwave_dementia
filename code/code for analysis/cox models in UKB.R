library(survival)
library(mice)
library(dplyr)
library(ggplot2)
library(ggsignif)
library("WeightIt")
library(ggpubr)
library(lme4)
library(lmerTest)

################################################################################
# load data
################################################################################

rm(list = ls())
setwd('/public/home/yhchen/data/UKB_data_for_analysis/')
coveriate_data = read.csv('UKB_coveriates.csv')
end_event_date = read.csv('end_event_date.csv')
start_event_date = read.csv('attend_date.csv')
UKB_hw = read.csv('/public/home/yhchen/data/UKB_hw.csv')
UKB_cw = read.csv('/public/home/yhchen/data/UKB_cw.csv')
UKB_lon = read.csv('transformed longitude for UKB.csv')
UKB_lat = read.csv('transformed latitude for UKB.csv')
disease_matrix = read.csv('diseases matrix for UKB.csv')
self_report = read.csv('self_reported_illnesses_exclude.csv')

coveriate_data = coveriate_data[order(coveriate_data$eid),]
end_event_date = end_event_date[order(end_event_date$eid),]
start_event_date = start_event_date[order(start_event_date$eid),]
self_report = self_report[order(self_report$eid),]
UKB_lon = UKB_lon[order(UKB_lon$eid),]
UKB_lat = UKB_lat[order(UKB_lat$eid),]

dementia_year = disease_matrix$Alzheimer.s.disease.and.other.dementias

################################################################################
# data reformation
################################################################################

# 1. end date-------------------------------------------------------------------

extract_and_convert_to_year <- function(input_string) {
  year_part <- substr(input_string, 1, 4)
  year_as_date <- as.integer(year_part)
  return(year_as_date)
}

# define the end as loss-follow-up, death, diagnosis, up to 2019
# whichever came first
dementia_end_date = rep(2020, time = dim(end_event_date)[1])
dementia_status = rep(0,time = dim(end_event_date)[1])

death_end_date = rep(2020,time = dim(end_event_date)[1])
death_status = rep(0,time = dim(end_event_date)[1])

for (i in seq(dim(end_event_date)[1])){
  if (end_event_date[i,3]!=""){
    lose_date = extract_and_convert_to_year(end_event_date[i,3])
    if (lose_date<2020){
      dementia_end_date[i] = lose_date
      dementia_status[i] = 0
      death_end_date[i] = lose_date
      death_status[i] = 0
    }
  }
  if (end_event_date[i,4]!=""){
    death_date = extract_and_convert_to_year(end_event_date[i,4])
    if (death_date<death_end_date[i]){
      dementia_end_date[i] = death_date
      dementia_status[i] = 0
      death_end_date[i] = death_date
      death_status[i] = 1
    }
  } 
  if (dementia_year[i]>0){
    if (dementia_year[i]<dementia_end_date[i]){
      dementia_end_date[i] = dementia_year[i]
      dementia_status[i] = 1
    }
  }
}

# 2. start date-----------------------------------------------------------------

year <- function(date_string) {
  date_parts <- strsplit(date_string, "-")[[1]]
  year <- as.numeric(date_parts[1])
  return(year)
}

start_date = rep(-1, times = dim(start_event_date)[1])

for (i in seq(dim(start_event_date)[1])){
  if (start_event_date[i,3]!=""){
    start_date[i] = year(start_event_date[i,3])
  }
  if ((dementia_year[i]>0) & (start_date[i]>dementia_year[i])){
    start_date[i] = -1
  }
}


# 3. accumulated heatwave and coldwave------------------------------------------

# define the heatwave exposure as three-year accumulated hw before 2004

# hw

hw_level = UKB_hw[2:31]
hw_vector = unlist(hw_level)
quartiles = quantile(hw_vector, probs = c(0.25, 0.5, 0.75),na.rm = T)
for (i in 1:30){
  hw_level[,i] = cut(
    hw_level[,i],
    breaks = c(-Inf, quartiles[1], quartiles[3], Inf),
    labels = c("low", "medium", "high"),
    include.lowest = TRUE
  )
  hw_level[,i] = relevel(hw_level[,i], ref = 2)
}
colnames(hw_level) = paste0("hw_level", 1990:2019)

# acc3
acc3_hw = UKB_hw[,2:31]
for (i in 1992:2019){
  acc3_hw[,i-1990+1] = UKB_hw[,i-1990+2]+UKB_hw[,i-1990+1]+UKB_hw[,i-1990]
}

acc3_level = acc3_hw
acc3_vector = unlist(acc3_level)
quartiles = quantile(acc3_vector, probs = c(0.25, 0.5, 0.75),na.rm = T)
for (i in 1:30){
  acc3_level[,i] = cut(
    acc3_level[,i],
    breaks = c(-Inf, quartiles[1], quartiles[3], Inf),
    labels = c("low", "medium", "high"),
    include.lowest = TRUE
  )
  acc3_level[,i] = relevel(acc3_level[,i], ref = 2)
}
colnames(acc3_level) = paste0("acc3_level", 1990:2019)

# acc7
acc7_hw = UKB_hw[,2:31]
for (i in 1996:2019){
  acc7_hw[,i-1990+1] = UKB_hw[,i-1990+2]+UKB_hw[,i-1990+1]+UKB_hw[,i-1990]+
    UKB_hw[,i-1990-1]+UKB_hw[,i-1990-2]+UKB_hw[,i-1990-3]+UKB_hw[,i-1990-4]
}

acc7_level = acc7_hw
acc7_vector = unlist(acc7_level)
quartiles = quantile(acc7_vector, probs = c(0.25, 0.5, 0.75),na.rm = T)
for (i in 1:30){
  acc7_level[,i] = cut(
    acc7_level[,i],
    breaks = c(-Inf, quartiles[1], quartiles[3], Inf),
    labels = c("low", "medium", "high"),
    include.lowest = TRUE
  )
  acc7_level[,i] = relevel(acc7_level[,i], ref = 2)
}
colnames(acc7_level) = paste0("acc7_level", 1990:2019)


# cw
cw_level = UKB_cw[2:31]
cw_vector = unlist(cw_level)
quartiles = quantile(cw_vector, probs = c(0.25, 0.5, 0.75),na.rm = T)
for (i in 1:30){
  cw_level[,i] = cut(
    cw_level[,i],
    breaks = c(-Inf, quartiles[1], quartiles[3], Inf),
    labels = c("low", "medium", "high"),
    include.lowest = TRUE
  )
  cw_level[,i] = relevel(cw_level[,i], ref = 2)
}
colnames(cw_level) = paste0("cw_level", 1990:2019)

# 4. coveriates selection-------------------------------------------------------

#coveriates_use = coveriate_data[,c(3:5,9,13,21,22,27,28,29,30),]
#coveriates_use[coveriates_use < 0] = NA
#imputed_data = mice(coveriates_use, m = 5, method = "pmm", seed = 123)
#coveriates_use = complete(imputed_data)

coveriate_use = coveriate_data[,c(40,3,25,4,43,13,21,30,34)]
colnames(coveriate_use) = c('Age','Sex','Qualifications','Townsend','BMI',
                            'Sleep','Isolation','Smoking','Drinking')

# Sex
coveriate_use$Sex = cut(coveriate_use$Sex,breaks=c(0,0.5,1),labels=c('Female','Male'),include.lowest = TRUE)
# Age
coveriate_use$Age = cut(coveriate_use$Age,breaks=c(0,60,100),labels=c('<60','>=60'),include.lowest = TRUE)
# Townsend deprivative index
quantile(coveriate_use$Townsend, probs = c(0,0.2, 0.4, 0.6,0.8,1),na.rm=T)
coveriate_use$Townsend = cut(coveriate_use$Townsend,breaks=c(-6.26,-3.93326,1.34904,11.002),labels=c('1','2-4','5(most deprived)'),include.lowest = TRUE)
# Qualification
get_qualification_level = function(qual) {
  if (is.na(qual)) {
    return(NA)
  }
  nums = as.numeric(unlist(strsplit(gsub("\\[|\\]", "", qual), ",\\s*")))
  if (any(nums %in% c(1, 5, 6))) {
    return("higher or professional")
  } else if (any(nums %in% c(2))) {
    return("upper secondary")
  } else if (any(nums %in% c(3, 4))) {
    return("lower secondary")
  } else if (any(nums %in% c(-7, -3))) {
    return("none of the above")
  } else {
    return(NA)
  }
}
coveriate_use$Qualifications = factor(sapply(coveriate_use$Qualifications, get_qualification_level))
# Sleep duration
coveriate_use$Sleep = cut(coveriate_use$Sleep,breaks=c(0,6,8,24),labels=c('Short','Normal','Long'),include.lowest = TRUE)
coveriate_use$Sleep = relevel(coveriate_use$Sleep, ref = 2)
# Smoking
coveriate_use$Smoking[which(coveriate_use$Smoking<0)] = NA
coveriate_use$Smoking = cut(coveriate_use$Smoking,breaks=c(0,0.5,1.5,2),labels=c('Never','Previous','Current'),include.lowest = TRUE)
# Drinking
coveriate_use$Drinking[which(coveriate_use$Drinking<0)] = NA
coveriate_use$Drinking = cut(coveriate_use$Drinking,breaks=c(0,0.5,1.5,2),labels=c('Never','Previous','Current'),include.lowest = TRUE)
# Isolation
coveriate_use$Isolation[which(coveriate_use$Isolation<0)] = NA
coveriate_use$Isolation = cut(coveriate_use$Isolation,breaks=c(0,0.5,1),labels=c('No','Yes'),include.lowest = T)
# BMI
coveriate_use$BMI = cut(coveriate_use$BMI,breaks=c(0,18.5,25,30,100),labels=c('Under weight','Normal weight','Over weight','Obese'),include.lowest = T)
coveriate_use$BMI = relevel(coveriate_use$BMI, ref = 2)

# 5. location confirmation------------------------------------------------------

loc_confirm = rep(0,dim(UKB_hw)[1])

for (i in 1:dim(UKB_hw)[1]){
  for (j in 2:16){
    if (UKB_lat[i,j]!=0 & UKB_lon[i,j]!=0){
      if (UKB_lat[i,j]<49.87 | UKB_lat[i,j]>61.08 | UKB_lon[i,j] < -8.65 | UKB_lon[i,j]>1.77){
        loc_confirm[i] = 1
      }
    }
  }
}

# 6. data for analysis----------------------------------------------------------

eid = coveriate_data$eid
self_report_exclude = self_report$exclude
colnames(UKB_hw)[2:31] = paste0('hw',1990:2019)
colnames(acc3_hw) = paste0('hw3',1990:2019)
colnames(acc7_hw) = paste0('hw7',1990:2019)
colnames(UKB_cw)[2:31] = paste0('cw',1990:2019)
data = cbind(eid,start_date,dementia_end_date,dementia_status,self_report_exclude,loc_confirm,
             coveriate_use,UKB_hw[,18:31],acc3_hw[,17:30],acc7_hw[,17:30],UKB_cw[,18:31],
             hw_level,acc3_level,acc7_level,cw_level,UKB_lon[,2],UKB_lat[,2])
colnames(data)[3:4] = c('end_date','status')
colnames(data)[192:193] = c('lon','lat')
colnames(data)

data = data[which(data$self_report_exclude==0),]
data = data[which(data$start_date>0),]
data = data[which(data$end_date>=data$start_date),]
data = data[which(data$loc_confirm==0),]

################################################################################
# basic time-varying cox regression
################################################################################

for (i in 2006:2019){
  datax = data[which(data$start_date<=i & data$end_date>=i),
               c(1:15,(16+i-2006),(30+i-2006),(44+i-2006),(58+i-2006),
                 (72+i-2006),(102+i-2006),(132+i-2006),(162+i-2006))]
  dementia_status = rep(0,dim(datax)[1])
  dementia_status[which(datax$status==1 & datax$end_date==i)] = 1
  start = rep(i,dim(datax)[1])
  end = rep(i+1,dim(datax)[1])
  cox_datax = cbind(start,end,dementia_status,datax[,c(1,7:23)])
  colnames(cox_datax) = c('start','end','status','eid','Age','Sex','Qualifications',
                          'Townsend','BMI','Sleep','Isolation','Smoking','Drinking',
                          'hw','acc3','acc7','cw','hw_level','acc3_level','acc7_level','cw_level')
  cox_datax$yearstrata = (i-2006)%/%5+1
  #if (dim(cox_datax)[1]>0){
  #  W = weightit(hw~Age+Sex+Qualifications+Townsend+Sleep+Isolation+Smoking+Drinking,
  #               data = cox_datax, 
  #               method = "glm", 
  #               estimand = "ATT")
  #  cox_datax$SW = W$weights
  #  x005 = quantile(cox_datax$SW,0.05)
  #  x095 = quantile(cox_datax$SW,0.95)
  #  cox_datax = cox_datax[which(cox_datax$SW>=x005 & cox_datax$SW<=x095),]
  #}
  if (i==2006){
    cox_data = cox_datax
  }
  else{
    cox_data = rbind(cox_data,cox_datax)
  }
  
}

cox_data$hcw = cox_data$hw-cox_data$cw

res.cox = coxph(Surv(start,end,status)~hcw+
                  Age+Sex+Qualifications+Townsend,
                  #Sleep+Isolation+Smoking+Drinking+
                  #strata(yearstrata),
                cluster = cluster(eid),
                data=cox_data)
                #weights=cox_data$SW)
summary(res.cox)
cox.zph(res.cox)

################################################################################
# time-varying cox regression in specific areas
################################################################################

cw_lon = matrix(c(-1.36,-0.31,-3.44,-2.39,-0.31,0.73,-2.40,-1.35,-2.40,-1.35), 
                 nrow=5,ncol=2,byrow = TRUE)
cw_lat = matrix(c(50.99,52.11,50.99,52.11,50.99,52.11,52.11,53.23,53.23,54.35),
                 nrow=5,ncol=2,byrow = TRUE)

for (i in 2006:2019){
  datax = data[which(data$start_date<=i & data$end_date>=i),
               c(1:15,(16+i-2006),(30+i-2006),(44+i-2006),(58+i-2006),
                 (72+i-2006),(102+i-2006),(132+i-2006),(162+i-2006),192,193)]
  
  in_range = rep(FALSE, nrow(datax))
  for (j in 1:5) {
    in_lon_range = datax$lon >= cw_lon[j, 1] & datax$lon <= cw_lon[j, 2]
    in_lat_range = datax$lat >= cw_lat[j, 1] & datax$lat <= cw_lat[j, 2]
    in_range = in_range | (in_lon_range & in_lat_range)
  }
  datax = datax[in_range,]

  dementia_status = rep(0,dim(datax)[1])
  dementia_status[which(datax$status==1 & datax$end_date==i)] = 1
  start = rep(i,dim(datax)[1])
  end = rep(i+1,dim(datax)[1])
  cox_datax = cbind(start,end,dementia_status,datax[,c(1,7:23)])
  colnames(cox_datax) = c('start','end','status','eid','Age','Sex','Qualifications',
                          'Townsend','BMI','Sleep','Isolation','Smoking','Drinking',
                          'hw','acc3','acc7','cw','hw_level','acc3_level','acc7_level','cw_level')
  cox_datax$yearstrata = (i-2006)%/%5+1
  if (dim(cox_datax)[1]>0){
    W = weightit(acc7~Age+Sex+Qualifications+Townsend,
                 data = cox_datax, 
                 method = "glm", 
                 estimand = "ATT")
    cox_datax$SW = W$weights
    x005 = quantile(cox_datax$SW,0.05)
    x095 = quantile(cox_datax$SW,0.95)
    cox_datax = cox_datax[which(cox_datax$SW>=x005 & cox_datax$SW<=x095),]
  }
  if (i==2006){
    cox_data = cox_datax
  }
  else{
    cox_data = rbind(cox_data,cox_datax)
  }
  
}

res.cox = coxph(Surv(start,end,status)~hw+
                  Age+Sex+Qualifications+Townsend+
                  #Sleep+Isolation+Smoking+Drinking+
                  strata(yearstrata),
                cluster = cluster(eid),
                data=cox_data)
summary(res.cox)
cox.zph(res.cox)

################################################################################
# logistic regression
################################################################################

model = glm(status ~ hw2010+Age+Sex+Qualifications+Townsend+Sleep+Isolation+Smoking+Drinking, 
            data = data[which(data$end_date>=2010),], 
            family = binomial)
summary(model)

model = glm(status ~ hw72010+Age+Sex+Qualifications+Townsend+Sleep+Isolation+Smoking+Drinking, 
            data = data[which(data$start_date>=2010),], 
            family = binomial)
summary(model)

model = glm(status ~ cw2010+Age+Sex+Qualifications+Townsend+Sleep+Isolation+Smoking+Drinking, 
            data = data[which(data$start_date>=2010),], 
            family = binomial)
summary(model)
