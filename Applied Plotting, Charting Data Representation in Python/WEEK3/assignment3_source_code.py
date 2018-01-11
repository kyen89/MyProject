# Use the following data for this assignment:

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
%matplotlib notebook

np.random.seed(12345)

df = pd.DataFrame([np.random.normal(33500,150000,3650), 
                   np.random.normal(41000,90000,3650), 
                   np.random.normal(41000,120000,3650), 
                   np.random.normal(48000,55000,3650)], 
                  index=[1992,1993,1994,1995])

df['m'] = np.mean(df,axis=1)

std_sample = np.std(df,axis=1)
sample_size = np.shape(df)[1]-1
std_error = std_sample/((sample_size)**0.5)
df['yerr'] = std_error*1.96

ci = []
margin_error = std_error*1.96

df['lower ci'] = df['m'] - margin_error
df['upper ci'] = df['m'] + margin_error

years = ('1992','1993','1994','1995')
bars = plt.bar(df.index,df['m'],width=0.99,yerr=df['yerr'],capsize=10,color='lightgrey')
plt.xticks(df.index,years) 
plt.gca().tick_params(bottom='off')  
plt.figure()
plt.bar(df.index,df['m'],width=0.99,yerr=df['yerr'],capsize=10,color='grey')
  
ink=[]

def color_bar(y_value):
    yy = y_value
    for bar,yer in zip(bars,df['yerr']):
        ci_high = bar.get_height() + yer
        ci_low = bar.get_height() - yer
        if(yy==int(bar.get_height())):
            ink.append('beige')
        elif(yy<ci_low):
            ink.append('darkred')
        elif((yy>bar.get_height())&((ci_low<yy)&(yy<ci_high))):
            ink.append('lightblue')
        elif((yy<bar.get_height())&((ci_low<yy)&(yy<ci_high))):
            ink.append('red')
        elif(yy>ci_high):
            ink.append('blue')

    plt.bar(df.index,df['m'],width=0.99,yerr=df['yerr'],capsize=10,color=ink)
    plt.axhline(y=yy,xmin=-.1,c='k',clip_on=False)
    plt.gca().text(1995.1,yy+900,'y='+str(yy),weight='bold',size=10)
    plt.gca().tick_params(bottom='off')
    plt.xticks(df.index,years) 
    plt.title('Challenge Level: Easiest\ny-axis value of interest: ' +  str(yy))

#put any number in order to check the color of the bars   
color_bar(39975)