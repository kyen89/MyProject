import matplotlib.pyplot as plt
import mplleaflet
import pandas as pd
import datetime as dt
import numpy as np
#'data/C2A2_data/BinnedCsvs_d400/fb441e62df2d58994928907a91895ec62c2c42e6cd075c2700843b89.csv'
def leaflet_plot_stations(binsize, hashid):

    df = pd.read_csv('data/C2A2_data/BinSize_d{}.csv'.format(binsize))

    station_locations_by_hash = df[df['hash'] == hashid]

    lons = station_locations_by_hash['LONGITUDE'].tolist()
    lats = station_locations_by_hash['LATITUDE'].tolist()

    plt.figure(figsize=(8,8))

    plt.scatter(lons, lats, c='r', alpha=0.7, s=200)

    return mplleaflet.display()

leaflet_plot_stations(400,'fb441e62df2d58994928907a91895ec62c2c42e6cd075c2700843b89.csv')

df = pd.read_csv('data/C2A2_data/BinnedCsvs_d400/e0cb0f071810107c25704c4c7c865f31749f7e121425e29e3bb131e6.csv')
df["MD"] = pd.to_datetime(df["Date"])
df["MD"] = df["MD"].dt.strftime('%m-%d')
df = df[df["MD"]!="02-29"]
df["Data_Value"]*=.1
df.reset_index(inplace=True)

uni_day = np.sort(df["MD"].unique()).tolist()
day = [days for days in range(1,366)]
dict_day = dict(zip(uni_day, day))

def day_converter(MD):
    MD_list = MD.tolist()
    new_MD = []
    for i in MD_list:
        new_MD.append(dict_day[i])
    return new_MD

day_day = pd.Series(day_converter(df["MD"]))
df["DD"] = day_day

"""YR df"""
df["YR"] = pd.to_datetime(df["Date"])
df["YR"] = df["YR"].dt.strftime("%y")

###################################################

"""max of TMAX (orange line): period 2005-2014"""

df_max = df[df["Element"]=="TMAX"]

df_max.reset_index(inplace=True)
df_max = df_max.ix[:,1:]
df_max.columns = ["df_index","ID","Date","Element","Data-Value","MD","DD","YR"]

df_max14 = df_max[df_max["YR"]!='15']
df_max14.reset_index(inplace=True)

df_max14_x = df_max14.groupby("DD")["Data-Value"].max().index.tolist()
df_max14_y = df_max14.groupby("DD")["Data-Value"].max().tolist()


"""min of TMIN (blue line): period 2005-2014"""
df_min = df[df["Element"]=="TMIN"]

df_min.reset_index(inplace=True)
df_min = df_min.ix[:,1:]
df_min.columns = ["df_index","ID","Date","Element","Data-Value","MD","DD","YR"]

df_min14 = df_min[df_min["YR"]!='15']
df_min14.reset_index(inplace=True)

df_min14_x = df_min14.groupby("DD")["Data-Value"].min().index.tolist()
df_min14_y = df_min14.groupby("DD")["Data-Value"].min().tolist()

"""2015 MAX and MIN"""
df_2015 = df[df["YR"]=="15"]
df_2015.reset_index(inplace=True)
df_2015 = df_2015.ix[:,1:]
df_2015.columns = ["df_index","ID","Date","Element","Data-Value","MD","DD","YR"]

df_2015_day_max_x = df_2015.groupby("DD")["Data-Value"].max().index.tolist()
df_2015_day_max_y = df_2015.groupby("DD")["Data-Value"].max().tolist()

df_2015_day_min_x = df_2015.groupby("DD")["Data-Value"].min().index.tolist()
df_2015_day_min_y = df_2015.groupby("DD")["Data-Value"].min().tolist()



"""Comparison"""

def new(y_2014):
    new_y = []
    new_x = []
    if y_2014==df_max14_y:
        for i in range(len(y_2014)):
            if y_2014[i]<df_2015_day_max_y[i]:
                new_y.append(df_2015_day_max_y[i])
                new_x.append(i)
    
    elif y_2014==df_min14_y:
        for i in range(len(y_2014)):
            if y_2014[i]>df_2015_day_min_y[i]:
                new_y.append(df_2015_day_min_y[i])
                new_x.append(i)
                
    return (new_x,new_y)                                                                 

"""GRAPH"""
plt.figure(figsize=(20,10))

high_14 = plt.plot(df_max14_x,df_max14_y,'-',color="pink",label="2005-2014 Highs")
low_14 = plt.plot(df_min14_x,df_min14_y,'-',color="lightgreen",label="2005-2014 Lows")
plt.gca().fill_between(range(len(df_max14_x)),df_min14_y,df_max14_y,facecolor='grey',alpha=0.1)
high_15 = plt.scatter(new(df_max14_y)[0],new(df_max14_y)[1],c="red",s=18,label="2015 Highs")
low_15 = plt.scatter(new(df_min14_y)[0],new(df_min14_y)[1],c="green",s=18,label="2015 Lows")

plt.legend(loc=4,frameon=False, bbox_to_anchor=(0.60,0.03),prop={'size':14})

ax = plt.gca()
mn,mx = ax.get_ylim()
mn_fh=mn*1.8+32
mx_fh=mx*1.8+32
ax2=ax.twinx()
ax2.set_ylim(mn_fh,mx_fh)
ax2.tick_params(axis='both', which='both', labelsize=15)
ax.tick_params(axis='both', which='both', labelsize=15)
ax.set_title("Temperature Range in Berkeley",size=25)
ax.set_xlabel("Day of year",size=15)
ax.set_ylabel("Temperature (C)",size=15)
ax2.set_ylabel("Temperature (F)",size=15)

plt.show()
plt.clf()