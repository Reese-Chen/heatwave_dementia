import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from global_land_mask import globe

#%% 读入数据

max_data = pd.read_csv('d:\\heatwave and dementia\\data\\daily max temperature.csv',sep=',',header=None)
pop = np.loadtxt('d:\\heatwave and dementia\\data\\各坐标点2000-2020年均人口密度.csv',delimiter = ',')
pop = pop[4406:,2]

hw_cn = np.loadtxt("d:\\heatwave and dementia\\data\\hw_english_countrynames.csv",
                delimiter=',',encoding='utf_8',dtype=str)

heatwave_usa = pd.read_csv('d:\\heatwave and dementia\\data\\美国各坐标点热浪.csv',sep=',')

#%% 定义判断润年的函数

def run(n):
    if ((n%4==0 and n%100!=0)or(n%400==0)):
        return 1
    else:
        return 0

#%% 统计1990-2019每个月天数并建立前缀和数组

day_of_month0 = [31,28,31,30,31,30,31,31,30,31,30,31]
day_of_month1 = [31,29,31,30,31,30,31,31,30,31,30,31]

days_cut = np.zeros(361)

k = 0

for i in range(1990,2020):
    day_of_month = day_of_month0
    if run(i)==1:
        day_of_month = day_of_month1
    for j in range(12):
        k = k+1
        days_cut[k] = days_cut[k-1]+day_of_month[j]

days_cut = np.asarray(days_cut,dtype=int)

#%% 计算每个坐标点各月份平均最高温度

max_month = np.zeros((15456,360))
for i in range(15456):
    xmax = max_data.iloc[:,i].values
    for j in range(360):
        start = days_cut[j]
        end = days_cut[j+1]
        max_month[i,j] = np.mean(xmax[start:end])
        
#%% 计算每个坐标点30年每年最热月份的温度

max_year = np.zeros((15456,30))
for i in range(15456):
    for j in range(30):
        max_year[i,j] = np.max(max_month[i,j*12:j*12+12])
        
#%% 保存
max_year1 = pd.DataFrame(max_year)
max_year1.to_csv('d:\\heatwave and dementia\\data\\maxt_year.csv',sep=',',index=False,header=None)

#%% 获取每个坐标点对应经纬度

k = 0
for i in range(-85,86,1):
    for j in range(-179,180,1):
        is_on_land = globe.is_land(i,j)
        if (is_on_land):
            k = k+1
            new = [j,i]
            if k==1:
                location = new
            else:
                location = np.column_stack((location,new))
              
location = location[:,4406:]   

#%% 人口加权算各国30年最热月份平均温度

country = np.unique(hw_cn) #获取所有坐标点对应的全部不重复国家名称
n_country = np.shape(country)[0]
max_montht_country = np.zeros((n_country,30))
pop_country = np.zeros(n_country)

for i,cname in enumerate(country):
    for j in range(15456):
        if hw_cn[j]==cname:      
            pop_country[i] = pop_country[i]+pop[j]          
            max_montht_country[i,:] = max_montht_country[i,:]+pop[j]*max_year[j,:]
            
    if pop_country[i]!=0:
        max_montht_country[i,:] = max_montht_country[i,:]/pop_country[i]
        
max_montht_country = pd.DataFrame(max_montht_country)
max_montht_country['country'] = country

#%%

max_montht_country.to_csv('d:\\heatwave and dementia\\data\\各国30年最热月份平均温度.csv',sep=',',index=True,header=True)

#%% 人口加权算美国各州30年最热月份平均温度

#heatwave_usa = heatwave_usa.rename(columns={'Unnamed: 0': 'index'})

state = heatwave_usa.state.unique()
n_state = np.shape(state)[0]
max_montht_state = np.zeros((n_state,30))
pop_state = np.zeros(n_state)

for i,sname in enumerate(state):
    for j in range(1054):
        if heatwave_usa.iloc[j,35]==sname:
            lon = heatwave_usa.iloc[j,32]
            lat = heatwave_usa.iloc[j,33]
            for k in range(15456):
                if (location[0,k]==lon) and (location[1,k]==lat):
                    p = k
                    break
            pop_state[i] = pop_state[i]+pop[p]
            max_montht_state[i,:] = max_montht_state[i,:]+pop[p]*max_year[p,:]
    if pop_state[i]!=0:
        max_montht_state[i,:] =  max_montht_state[i,:]/pop_state[i]

max_montht_state = pd.DataFrame(max_montht_state)
max_montht_state['state'] = state

#%%

max_montht_state.to_csv('d:\\heatwave and dementia\\data\\美国各州30年最热月份平均温度.csv',sep=',',index=True,header=True)

#%%
