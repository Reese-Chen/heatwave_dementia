import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import random
from pyecharts import options as opts
from pyecharts.charts import Map

#%% 读入数据

max_data = pd.read_csv('d:\\heatwave and dementia\\data\\daily max temperature.csv',sep=',',header=None)
pop = np.loadtxt('d:\\heatwave and dementia\\data\\各坐标点2000-2020年均人口密度.csv',delimiter = ',')
pop = pop[4406:,2]
hw_cn = np.loadtxt("d:\\heatwave and dementia\\data\\hw_english_countrynames.csv",
                delimiter=',',encoding='utf_8',dtype=str)

#%% 转换年份数据

#判断润年
def nian(n):
    if ((n%4==0 and n%100!=0)or(n%400==0)):
        return 1
    else:
        return 0

#按年给每天分类
year = np.zeros(10957)
current_day = 0
for i in range(30):
    day = 365
    this_year = i+1990
    if nian(this_year):
        day = day+1
    for j in range(current_day,current_day+day):
        year[j] = i
    current_day = current_day+day

#合成每年数据
max_values = max_data.values
max_year = np.zeros((30,15456))
for i in range(30):
    for j in range(15456):
        max_year[i,j] = np.mean(max_values[year==i,j])
max_year = max_year.T

#%% 按国家整合

countryname = np.unique(hw_cn)
max_country = np.zeros((168,30))
pop_country = np.zeros(168)
for i,country in enumerate(countryname):
    for j in range(15456):
        if hw_cn[j]==country:
            pop_country[i] = pop_country[i]+pop[j]
            max_country[i,:] = max_country[i,:]+pop[j]*max_year[j,:]
    if pop_country[i]!=0:
        max_country[i,:] = max_country[i,:]/pop_country[i]
    else:
        max_country[i,:] = 0
    
#%% 保存

max_country = pd.DataFrame(data=max_country)
max_country['cname'] = countryname
max_country.to_csv('d:\\heatwave and dementia\\data\\人口加权各国年均最高温度.csv', encoding='utf_8_sig')
