
#%% 取得每个陆地上的坐标点
import numpy as np
from global_land_mask import globe

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

#%% 定义函数确定坐标点对应国家信息
from geopy.geocoders import Nominatim
geolocator = Nominatim(user_agent = "geoapiExercises")

def getcountry(lat,lon): 
    lat_s = str(lat)
    lon_s = str(lon)
    loc = geolocator.reverse(lat_s+','+lon_s)
    if loc==None:
        return 'None'
    else:
        address = loc.raw['address']
        country = address.get('country','')
        return country

#%% 保存每个陆地坐标点对应国家
countryname = list()
for i in range(1614,7655):
    countryname.append(getcountry(location[1,i],location[0,i]))

#%%
import pandas as pd
test=pd.DataFrame(data=countryname)
test.to_csv('d:\\heatwave and dementia\\data\\hw_cn02.csv', encoding='utf_8_sig')
