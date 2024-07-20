library(forester)

################################################################################
# load data
################################################################################

# 1. read data------------------------------------------------------------------
rm(list=ls())
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

data_high = data[which(data$gnigroup=='high income'),]
data_low = data[which(data$gnigroup=='mid and low income'),]

################################################################################
# mixed effect models
################################################################################

# 1. prepocessing---------------------------------------------------------------

# All 137 countries
hist(data$all_both)
hist(data$all_female)
hist(data$all_male)
hist(data$both15_49)
hist(data$male15_49)
hist(data$female15_49)
hist(data$both50_69)
hist(data$male50_69)
hist(data$female50_69)
hist(data$both70)
hist(data$male70)
hist(data$female70)
hist(data$both85)
hist(data$male85)
hist(data$female85)

data$gdp = log(data$gdp)
data$all_both = log(data$all_both)
data$all_female = log(data$all_female)
data$all_male = log(data$all_male)

data[,c(9,17,28:42)] = scale(data[,c(9,17,28:42)])

# high-income countries
hist(data_high$all_both)
hist(data_high$all_female)
hist(data_high$all_male)
hist(data_high$both15_49)
hist(data_high$male15_49)
hist(data_high$female15_49)
hist(data_high$both50_69)
hist(data_high$male50_69)
hist(data_high$female50_69)
hist(data_high$both70)
hist(data_high$male70)
hist(data_high$female70)
hist(data_high$both85)
hist(data_high$male85)
hist(data_high$female85)

data_high$gdp = log(data_high$gdp)
data_high$all_both = log(data_high$all_both)
data_high$all_female = log(data_high$all_female)
data_high$all_male = log(data_high$all_male)

data_high[,c(9,17,28:42)] = scale(data_high[,c(9,17,28:42)])

# low- to middle-income countries
hist(data_low$all_both)
hist(data_low$all_female)
hist(data_low$all_male)
hist(data_low$both15_49)
hist(data_low$male15_49)
hist(data_low$female15_49)
hist(data_low$both50_69)
hist(data_low$male50_69)
hist(data_low$female50_69)
hist(data_low$both70)
hist(data_low$male70)
hist(data_low$female70)
hist(data_low$both85)
hist(data_low$male85)
hist(data_low$female85)

data_low$gdp = log(data_low$gdp)
data_low$all_both = log(data_low$all_both)
data_low$all_female = log(data_low$all_female)
data_low$all_male = log(data_low$all_male)

data_low[,c(9,17,28:42)] = scale(data_low[,c(9,17,28:42)])

# 2. mixed effect models--------------------------------------------------------

result = matrix(0,nrow=120,ncol=16,
                dimnames = list(rep(colnames(data)[28:42],each=8),c('Variable',
                                rep(c('Beta','CI_low','CI_high','Estimate','p'),3))))
result[,1] = rep(c('(Intercept)','year','hw','gdp','year:hw','year:gdp','hw:gdp','year:hw:gdp'),15)

for (i in 1:15){
  # all 137 countries
  model = lmer(data[,i+27]~year*hw*gdp+(1+year|country),data = data,
               control = lmerControl(optimizer ="Nelder_Mead"))
  fit = summary(model)
  confi  = confint(model)
  beta = signif(fit$coefficients[,1],3)
  p = signif(fit$coefficients[,5],2)
  p[p<0.0001] = "<0.0001"
  CI_low = signif(confi[5:12,1],3)
  CI_high = signif(confi[5:12,2],3)
  result[((i-1)*8+1):(i*8),2] = fit$coefficients[,1]
  result[((i-1)*8+1):(i*8),3] = confi[5:12,1]
  result[((i-1)*8+1):(i*8),4] = confi[5:12,2]
  result[((i-1)*8+1):(i*8),5] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  result[((i-1)*8+1):(i*8),6] = p
  
  # high income countries
  model = lmer(data_high[,i+27]~year*hw*gdp+(1+year|country),data = data_high,
               control = lmerControl(optimizer ="Nelder_Mead"))
  fit = summary(model)
  confi  = confint(model)
  beta = signif(fit$coefficients[,1],3)
  p = signif(fit$coefficients[,5],2)
  p[p<0.0001] = "<0.0001"
  CI_low = signif(confi[5:12,1],3)
  CI_high = signif(confi[5:12,2],3)
  result[((i-1)*8+1):(i*8),7] = fit$coefficients[,1]
  result[((i-1)*8+1):(i*8),8] = confi[5:12,1]
  result[((i-1)*8+1):(i*8),9] = confi[5:12,2]
  result[((i-1)*8+1):(i*8),10] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  result[((i-1)*8+1):(i*8),11] = p
  
  # low to middle income countries
  model = lmer(data_low[,i+27]~year*hw*gdp+(1+year|country),data = data_low,
               control = lmerControl(optimizer ="Nelder_Mead"))
  fit = summary(model)
  confi  = confint(model)
  beta = signif(fit$coefficients[,1],3)
  p = signif(fit$coefficients[,5],2)
  p[p<0.0001] = "<0.0001"
  CI_low = signif(confi[5:12,1],3)
  CI_high = signif(confi[5:12,2],3)
  result[((i-1)*8+1):(i*8),12] = fit$coefficients[,1]
  result[((i-1)*8+1):(i*8),13] = confi[5:12,1]
  result[((i-1)*8+1):(i*8),14] = confi[5:12,2]
  result[((i-1)*8+1):(i*8),15] = paste(beta,' (',CI_low,' to ',CI_high,')',sep="")
  result[((i-1)*8+1):(i*8),16] = p
}

write.csv(result,'mixed-effect models for different age and sex group hw.csv')

result = as.data.frame(result)
result = result[which(result$Variable=='year:hw'),]
result[,c(2:4,7:9,12:14)] = apply(result[,c(2:4,7:9,12:14)],2,as.numeric)

################################################################################
# visualzation
################################################################################

df_high =  read.table(file = "plot data for high income countries.csv", header = T ,
                      check.names=F,sep = "," , fill = TRUE , encoding = "UTF-8")
colnames(df_high) = c('Subgroup','Estimate','CI_low','CI_high','p')

# 0. all countries--------------------------------------------------------------

plot_data = df_high
plot_data[2:6,2:5] = result[c(1,6,9,12,15),c(2:4,6)]
plot_data[8:12,2:5] = result[c(2,4,7,10,13),c(2:4,6)]
plot_data[14:18,2:5] = result[c(3,5,8,11,14),c(2:4,6)]

plot_data$Estimate = plot_data$Estimate*1000
plot_data$CI_low = plot_data$CI_low*1000
plot_data$CI_high = plot_data$CI_high*1000

p = forester(left_side_data = plot_data[,c(1,5)],   
             estimate = plot_data$Estimate,      
             ci_low = plot_data$CI_low,        
             ci_high = plot_data$CI_high,  
             estimate_precision = 2,
             xlim = c(-2,5),
             xbreaks = c(-2, 0, 2, 4, 5),
             dpi=300,
             display = TRUE,
             render_as='pdf',
             font_family = "sans",
             stripe_colour = "white",
             background_colour = "white",
             estimate_col_name = "Beta (10^(-3))",
)

# 1. high-income countries------------------------------------------------------

plot_data = df_high
plot_data[2:6,2:5] = result[c(1,6,9,12,15),c(7:9,11)]
plot_data[8:12,2:5] = result[c(2,4,7,10,13),c(7:9,11)]
plot_data[14:18,2:5] = result[c(3,5,8,11,14),c(7:9,11)]

plot_data$Estimate = plot_data$Estimate*1000
plot_data$CI_low = plot_data$CI_low*1000
plot_data$CI_high = plot_data$CI_high*1000

p = forester(left_side_data = plot_data[,c(1,5)],   
             estimate = plot_data$Estimate,      
             ci_low = plot_data$CI_low,        
             ci_high = plot_data$CI_high,  
             estimate_precision = 2,
             xlim = c(-3,9),
             xbreaks = c(-3, 0, 3, 6, 9),
             dpi=300,
             display = TRUE,
             render_as='pdf',
             font_family = "sans",
             stripe_colour = "white",
             background_colour = "white",
             estimate_col_name = "Beta (10^(-3))",
)

# 2. low to middle income countries---------------------------------------------

plot_data = df_high
plot_data[2:6,2:5] = result[c(1,6,9,12,15),c(12:14,16)]
plot_data[8:12,2:5] = result[c(2,4,7,10,13),c(12:14,16)]
plot_data[14:18,2:5] = result[c(3,5,8,11,14),c(12:14,16)]

plot_data$Estimate = plot_data$Estimate*1000
plot_data$CI_low = plot_data$CI_low*1000
plot_data$CI_high = plot_data$CI_high*1000

p = forester(left_side_data = plot_data[,c(1,5)],   
             estimate = plot_data$Estimate,      
             ci_low = plot_data$CI_low,        
             ci_high = plot_data$CI_high,  
             estimate_precision = 2,
             xlim = c(-2,4),
             xbreaks = c(-2, 0, 2, 4),
             dpi=300,
             display = TRUE,
             render_as='pdf',
             font_family = "sans",
             stripe_colour = "white",
             background_colour = "white",
             estimate_col_name = "Beta (10^(-3))",
)
