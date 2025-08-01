# load packages
library(lmerTest)
library(dplyr)

################################################################################
# load data
################################################################################
rm(list=ls())
gc()
setwd("d:/heatwave and dementia/data")
data = read.table(file = "data for analysis.csv", header = T , check.names=F,
                  sep = "," , fill = TRUE , encoding = "UTF-8",quote="")
data = data[,-1]

data <- data %>%
  group_by(country) %>%  # 确保列名与数据一致（注意大小写）
  mutate(
    hw_between = mean(hw, na.rm = TRUE),      # 国家热浪平均水平
    gdp_between = mean(gdp, na.rm = TRUE)     # 国家GDP平均水平
  ) %>%
  ungroup()

data_high = data[which(data$gnigroup=='high income'),]
data_low = data[which(data$gnigroup=='mid and low income'),]

################################################################################
# form accdata 
################################################################################

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

accdata_low = accdata[which(accdata$gnigroup=='mid and low income'),]
accdata_high = accdata[which(accdata$gnigroup=='high income'),]

################################################################################
# scale
################################################################################

# data
variables_to_scale <- c("mort_all", "mort_as", "pop", "gdp", "hw", 
                        "HW_count", "HW_days", "HW_degree", "HW_mean_length",
                        "HW_mean_degree", "HW_max_degree", "temp", "cw")

data_scale <- data %>%
  group_by(country) %>%
  mutate(across(all_of(variables_to_scale), 
                ~ scale(., center = TRUE, scale = TRUE))) %>%
  ungroup()

data_scale$hw_between = scale(data_scale$hw_between)
data_scale$gdp_between = scale(data_scale$gdp_between)
data_scale$sex_ratio = scale(data_scale$sex_ratio)

data_high_scale = data_scale[which(data_scale$gnigroup=='high income'),]
data_low_scale = data_scale[which(data_scale$gnigroup=='mid and low income'),]

# accdata
variables_to_scale <- c("acc3", "gdp")
accdata_scale = accdata %>%
  group_by(country) %>%
  mutate(across(all_of(variables_to_scale), 
                ~ scale(., center = TRUE, scale = TRUE))) %>%
  ungroup()
accdata_low_scale = accdata_scale[which(data_scale$gnigroup=='mid and low income'),]
accdata_high_scale = accdata_scale[which(data_scale$gnigroup=='high income'),]


################################################################################
# mixed effect models with incidence
################################################################################

model = lmer(inci_as~year*hw*gdp+(1+year|country),data = data_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)
anova(model)
ranova(model)
performance::model_performance(model)

model_null = lmer(inci_as~year+(1+year|country),data = data_scale,
                  control = lmerControl(optimizer ="Nelder_Mead"))
anova(model,model_null)

model = lmer(inci_as~year*hw*gdp+(1+year|country),data = data_high_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+(1+year|country),data = data_low_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

# control between group effect
model = lmer(inci_as~year*hw*gdp+hw_between+gdp_between+(1+year|country),data = data_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+hw_between+gdp_between+(1+year|country),data = data_high_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+hw_between+gdp_between+(1+year|country),data = data_low_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)


# acc
model = lmer(inci_as~year*acc3*gdp+(1+year|country),data = accdata_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*acc3*gdp+(1+year|country),data = accdata_high_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*acc3*gdp+(1+year|country),data = accdata_low_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)



# control mort_as
model = lmer(inci_as~year*hw*gdp+mort_as+(1+year|country),data = data_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model,method="Wald")

model = lmer(inci_as~year*hw*gdp+mort_as+(1+year|country),data = data_high_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+mort_as+(1+year|country),data = data_low_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

# control temp
model = lmer(inci_as~year*hw*gdp+temp+(1+year|country),data = data_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+temp+(1+year|country),data = data_high_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+temp+(1+year|country),data = data_low_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

# control sex_ratio
model = lmer(inci_as~year*hw*gdp+sex_ratio+(1+year|country),data = data_scale[!is.na(data_scale$sex_ratio), ],
             control = lmerControl(optimizer ="Nelder_Mead"),na.action = na.omit)
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+sex_ratio+(1+year|country),data = data_high_scale,
             control = lmerControl(optimizer ="Nelder_Mead"),na.action = na.exclude)
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+sex_ratio+(1+year|country),data = data_low_scale,
             control = lmerControl(optimizer ="Nelder_Mead"),na.action = na.exclude)
summary(model)
confint(model)

# add popweight
model = lmer(inci_as~year*hw*gdp+(1+year|country),
             data = data_scale,
             weights = popweight,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+(1+year|country),
             data = data_high_scale,
             weights = popweight,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+(1+year|country),
             data = data_low_scale,
             weights = popweight,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

# control coldwave
model = lmer(inci_as~year*hw*gdp+cw+(1+year|country),data = data_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+cw+(1+year|country),data = data_high_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+cw+(1+year|country),data = data_low_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

# control gdp
model = lmer(inci_as~year*hw+gdp+(1+year|country),data = data_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw+gdp+(1+year|country),data = data_high_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw+gdp+(1+year|country),data = data_low_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model,method="Wald")

# exclude special year
model = lmer(inci_as~year*hw*gdp+(1+year|country),
             data = data_scale[which(data_scale$year!=7),],
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+(1+year|country),
             data = data_high_scale[which(data_high_scale$year!=7),],
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*hw*gdp+(1+year|country),
             data = data_low_scale[which(data_low_scale$year!=7),],
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

# using prevalence
model = lmer(prev_as~year*hw*gdp+(1+year|country),data = data_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(prev_as~year*hw*gdp+(1+year|country),data = data_high_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(prev_as~year*hw*gdp+(1+year|country),data = data_low_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

# HW_count
model = lmer(inci_as~year*HW_count*gdp+(1+year|country),data = data_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*HW_count*gdp+(1+year|country),data = data_high_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*HW_count*gdp+(1+year|country),data = data_low_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

# HW_days
model = lmer(inci_as~year*HW_days*gdp+(1+year|country),data = data_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*HW_days*gdp+(1+year|country),data = data_high_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*HW_days*gdp+(1+year|country),data = data_low_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

# HW_degree
model = lmer(inci_as~year*HW_degree*gdp+(1+year|country),data = data_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*HW_degree*gdp+(1+year|country),data = data_high_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*HW_degree*gdp+(1+year|country),data = data_low_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

# HW mean length
model = lmer(inci_as~year*HW_mean_length*gdp+(1+year|country),data = data_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*HW_mean_length*gdp+(1+year|country),data = data_high_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*HW_mean_length*gdp+(1+year|country),data = data_low_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

# HW mean degree
model = lmer(inci_as~year*HW_mean_degree*gdp+(1+year|country),data = data_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*HW_mean_degree*gdp+(1+year|country),data = data_high_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*HW_mean_degree*gdp+(1+year|country),data = data_low_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

# HW max degree
model = lmer(inci_as~year*HW_max_degree*gdp+(1+year|country),data = data_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*HW_max_degree*gdp+(1+year|country),data = data_high_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*HW_max_degree*gdp+(1+year|country),data = data_low_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

# hw+hw2
model = lmer(inci_as~year*(hw+I(hw^2))*gdp+(1+year|country),data = data_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*(hw+I(hw^2))*gdp+(1+year|country),data = data_high_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*(hw+I(hw^2))*gdp+(1+year|country),data = data_low_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

# hw+hw2+hw3
model = lmer(inci_as~year*(hw+I(hw^2)+I(hw^3))*gdp+(1+year|country),data = data_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*(hw+I(hw^2)+I(hw^3))*gdp+(1+year|country),data = data_high_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)

model = lmer(inci_as~year*(hw+I(hw^2)+I(hw^3))*gdp+(1+year|country),data = data_low_scale,
             control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)
confint(model)
