
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import random
from pyecharts import options as opts
from pyecharts.charts import Map

#%% 

#读数据
hw = np.loadtxt('d:\\heatwave and dementia\\data\\hw(第一列是编号).csv',delimiter = ',')
pop = np.loadtxt('d:\\heatwave and dementia\\data\\各坐标点2000-2020年均人口密度.csv',delimiter = ',')
hw = hw[:,1:]
pop = pop[4406:,2]
hw_cn = np.loadtxt("d:\\heatwave and dementia\\data\\hw_english_countrynames.csv",
                delimiter=',',encoding='utf_8',dtype=str)
countryname = pd.read_csv('d:\\heatwave and dementia\\data\\热浪国家列表1.csv',
                         sep=',',header=None)
'''
#%%

hw_country = np.unique(hw_cn)
np.savetxt('countries for hw.txt', hw_country,fmt='%s')

#%%
countryname = countryname.iloc[:,0]
countryname = np.array(countryname)
list(set(hw_country)-set(countryname))
'''
#%%
#hw_cn[hw_cn=='Taiwan']='China'

#%%

###转换年份数据

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
hw_year = np.zeros((30,15456))
for i in range(30):
    for j in range(15456):
        hw_year[i,j] = sum(hw[year==i,j])
hw_year = hw_year.T
             
#%%

### 按国家整合

countryname = np.unique(hw_cn)

hw_country = np.zeros((168,30))
pop_country = np.zeros(168)
for i,country in enumerate(countryname):
    for j in range(15456):
        if hw_cn[j]==country:
            pop_country[i] = pop_country[i]+pop[j]
            hw_country[i,:] = hw_country[i,:]+pop[j]*hw_year[j,:]
    hw_country[i,:] = hw_country[i,:]/pop_country[i]
    

#%%

np.savetxt('d:\\heatwave and dementia\\data\\167国家人口总数2010.csv',pop_country,delimiter = ',')

#%%

pop_country = pd.DataFrame(data=pop_country)
pop_country['cname'] = countryname
pop_country.to_csv('d:\\heatwave and dementia\\data\\168国家人口总数2010.csv', encoding='utf_8_sig')


#%%

### 保存
np.savetxt('d:\\heatwave and dementia\\data\\人口加权各国热浪数据.csv',hw_country,delimiter = ',')

#%%

hw_country = pd.DataFrame(data=hw_country)
hw_country['cname'] = countryname
hw_country.to_csv('d:\\heatwave and dementia\\data\\population weighted hw with country.csv', encoding='utf_8_sig')


#%%

### 绘图展示

def randomcolor():
    colorArr = ['1','2','3','4','5','6','7','8','9','A','B','C','D','E','F']
    color = ""
    for i in range(6):
        color += colorArr[random.randint(0,14)]
    return "#"+color

# draw lines and spots
year = np.arange(1990,2020)
labels = []
for i in range(161):
    if np.isnan(hw_country[i,:]).any()==False:
        plt.plot(year,hw_country[i,:],color=randomcolor(),alpha=0.8)
        labels.append(countryname[i])

# add label and legend
#plt.legend(labels,ncol=4)
plt.legend(labels,ncol=10,bbox_to_anchor=(1.05, 0), loc=3, borderaxespad=0)
plt.xlabel('year')
plt.ylabel('HWMId')
plt.title('population weighted HWMId in each country')
plt.show()

#%%

hw_country = np.loadtxt('d:\\heatwave and dementia\\data\\人口加权各国热浪数据.csv',delimiter = ',')
countryname = pd.read_csv('d:\\heatwave and dementia\\data\\热浪国家列表1.csv',
                         sep=',',header=None)
countryname = countryname.iloc[:,0]
countryname = np.array(countryname)
#%%
ynum = np.zeros(161)
for i in range(161):
    ynum[i] = sum(hw_country[i,:])
xarea = list(countryname)
ynum = list(ynum)
#%%
c = (
    Map()
    .add("population weighted HWMId", [list(z) for z in zip(xarea, ynum)], "world")
    .set_series_opts(label_opts=opts.LabelOpts(is_show=False))
    .set_global_opts(
        title_opts=opts.TitleOpts(title="distribution of population weighted HWMId"),
        visualmap_opts=opts.VisualMapOpts(min_=6096,max_=7469,is_piecewise=True),
    )
    .render("d:\\heatwave and dementia\\data\\distribution of population weighted HWMId.html")
)    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
