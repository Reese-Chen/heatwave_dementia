#%%
import ee
import os
#ee.Authenticate()
if __name__ == '__main__':
    os.environ['HTTP_PROXY'] = 'http://127.0.0.1:10809'
    os.environ['HTTPS_PROXY'] = 'http://127.0.0.1:10809'
ee.Initialize()

#%%
era5_2mt = ee.ImageCollection('ECMWF/ERA5/DAILY').select('maximum_2m_air_temperature').filter(ee.Filter.date('1990-01-01', '2020-01-01'));

#%%
def getdata(lon,lat): 
    poi = ee.Geometry.Point(lon, lat)
    scale = 1000
    return era5_2mt.getRegion(poi, scale).getInfo()

import pandas as pd
# convert data to time series
def ee_array_to_df(arr, list_of_bands):
    """Transforms client-side ee.Image.getRegion array to pandas.DataFrame."""
    df = pd.DataFrame(arr)

    # Rearrange the header.
    headers = df.iloc[0]
    df = pd.DataFrame(df.values[1:], columns=headers)

    # Remove rows without data inside.
    df = df[['longitude', 'latitude', 'time', *list_of_bands]].dropna()

    # Convert the data to numeric values.
    for band in list_of_bands:
        df[band] = pd.to_numeric(df[band], errors='coerce')

    # Convert the time field into a datetime.
    df['datetime'] = pd.to_datetime(df['time'], unit='ms')

    # Keep the columns of interest.
    #df = df[['longitude', 'latitude','time','datetime',  *list_of_bands]]
    df = df[['datetime',  *list_of_bands]]

    return df

def t_modis_to_celsius(t_modis):
    """Converts MODIS LST units to degrees Celsius."""
    t_celsius =  t_modis-273
    return t_celsius

#%% 
import datetime

#Determine if it's run'n
def nian(n):
    if ((n%4==0 and n%100!=0)or(n%400==0)):
        return 1
    else:
        return 0

#dayofyear
def doy2date(year,doy):    
    month_leapyear=[31,29,31,30,31,30,31,31,30,31,30,31]
    month_notleap= [31,28,31,30,31,30,31,31,30,31,30,31]

    if year%4==0 and (year%100!=0 or year%400==0):
        for i in range(0,12):
            if doy>month_leapyear[i]:
                doy-=month_leapyear[i]
                continue
            if doy<=month_leapyear[i]:
                month=i+1
                day=doy
                break
    else:
        for i in range(0,12):
            if doy>month_notleap[i]:
                doy-=month_notleap[i]
                continue
            if doy<=month_notleap[i]:
                month=i+1
                day=doy
                break
    return month,day

#get datetime
def get_datetime(year,month,day):
    dt = str(year)+str(month)+str(day)
    date =  datetime.datetime.strptime(str(dt),'%Y%m%d')
    return date

#%%

import numpy as np

def HWMId(data,win_size):
    plusminus = win_size//2
    delta=datetime.timedelta(days=plusminus)
    T30y25p = np.zeros(365)
    T30y75p = np.zeros(365)
    for i in range(1,366,1):
        for j in range(1990,2020,1):
            month,day = doy2date(j,i)
            date = get_datetime(j,month,day)
            start = date-delta
            end = date+delta
            if j==1990:
                #valid_data = data['maximum_2m_air_temperature'].where((data['datetime']<=end)&(data['datetime']>=start)).values
                #valid_data = data['maximum_2m_air_temperature'].drop((data['datetime']>end)|(data['datetime']<start)).values
                valid_data = data[(data['datetime']<=end)&(data['datetime']>=start)]['maximum_2m_air_temperature'].values
            else:
                #valid_data = np.append(valid_data,data['maximum_2m_air_temperature'].where((data['datetime']<=end)&(data['datetime']>=start).values))
                #valid_data = np.append(valid_data,data['maximum_2m_air_temperature'].drop((data['datetime']>end)|(data['datetime']<start)).values) 
                valid_data = np.append(valid_data,data[(data['datetime']<=end)&(data['datetime']>=start)]['maximum_2m_air_temperature'].values)
        T30y25p[i-1] = np.percentile(valid_data,25)
        T30y75p[i-1] = np.percentile(valid_data,75)
    for j in range(1992,2017,4):
        i = 366
        month,day = doy2date(j,i)
        date = get_datetime(j,month,day)
        start = date-delta
        end = date+delta
        if j==1992:
            valid_data = valid_data = data[(data['datetime']<=end)&(data['datetime']>=start)]['maximum_2m_air_temperature'].values
        else:
            valid_data = np.append(valid_data,data[(data['datetime']<=end)&(data['datetime']>=start)]['maximum_2m_air_temperature'].values)
    T30y25prun = np.percentile(valid_data,25)
    T30y75prun = np.percentile(valid_data,75)
    T30y25plist = T30y25p
    T30y75plist = T30y75p
    for i in range(1991,2020,1):
        T30y25plist = np.append(T30y25plist,T30y25p)
        T30y75plist = np.append(T30y75plist,T30y75p)
        if nian(i):
            T30y25plist = np.append(T30y25plist,T30y25prun)
            T30y75plist = np.append(T30y75plist,T30y75prun)
    M = (data['maximum_2m_air_temperature'].values - T30y25plist) / (T30y75plist - T30y25plist)  
    M[M<0] = 0
    #print(T30y25plist[0:100])
    #print(M[0:100])
    return M

#%%
from geopy.geocoders import Nominatim
geolocator = Nominatim(user_agent = "geoapiExercises")

def getcountry(lon,lat):
    lon_s = str(lon)
    lat_s = str(lat)
    location = geolocator.reverse(lon_s+','+lat_s)
    address = location.raw['address']
    country = address.get('country','')
    #country_code = address.get('country_code')
    return country

#%%
#提取所有在陆地上的经纬度点
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
                
#%%
import multiprocessing

def getheatwave(n):
    lon = int(location[0,n])
    lat = int(location[1,n])
    era5_2mt_poi = getdata(lon,lat)
    era5_2mt_max = ee_array_to_df(era5_2mt_poi,['maximum_2m_air_temperature'])
    era5_2mt_max['maximum_2m_air_temperature'] = era5_2mt_max['maximum_2m_air_temperature'].apply(t_modis_to_celsius)
    return HWMId(era5_2mt_max,31)

if __name__=='__main__':
    items = [x for x in range(0,3)]
    p = multiprocessing.Pool(8)
    b = p.map(getheatwave,items)
    p.close()
    p.join()
    b = pd.DateFrame(b)
    hwmid_matrix = b
    
#%%
hwmid_matrix = None
for i in range(17833,19214):
    lon = int(location[0,i])
    lat = int(location[1,i])
    era5_2mt_poi = getdata(lon,lat)
    era5_2mt_max = ee_array_to_df(era5_2mt_poi,['maximum_2m_air_temperature'])
    era5_2mt_max['maximum_2m_air_temperature'] = era5_2mt_max['maximum_2m_air_temperature'].apply(t_modis_to_celsius)
    hwmid =  HWMId(era5_2mt_max,31) 
    if hwmid_matrix is None:
        hwmid_matrix = hwmid
    else:
        hwmid_matrix = np.c_[hwmid_matrix,hwmid]

#%%
print(hwmid_matrix.shape)
#%%
np.savetxt('heatwaves_data14.csv', hwmid_matrix, delimiter = ',')
#%%
k1 = 519
k2 = 3218+519
k3 = k2+1803
k4 = k3+478
k5 = k4+89
k6 = k5+827
k7 = k6+159
k8 = k7+713
k9 = k8+169
k10 = k9+108
k11 = k10+6110
k12 = k11+744
k13 = k12+2896
k14 = k13+1381
k15 = k14+648
print(k13,' ',k14)
