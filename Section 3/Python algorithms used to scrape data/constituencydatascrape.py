from bs4 import BeautifulSoup
import pandas as pd
import requests 
import os
import csv
import numpy as np

#CREATE PD DATAFRAME

os.chdir("C:\\Users\\Marwan Riach\\Dropbox\\M4R Shared Marwan Chris\\Incumbency Data")
#Extract URLS of constituencies that existed from 1997
X = pd.read_csv('UsedURLS.csv')
X = np.array(X)
X=X[:,0]

list_of_data = [[0]*43]
Years = [' 1997',' 2001',' 2005',' 2010',' 2015',' 2017',' 2019']
incumbent = ' '

for i in range(353,453):
    url = X[i]
    response=requests.get(url)
    soup=BeautifulSoup(response.text,'html.parser')
    temp_list = [0]*43
    temp_list[0] = url[30:]
    
    for j in range(len(Years)):
      table = soup.find(text='General election'+Years[j])
      if table!=None:
        table=table.parent.parent.parent
        df = pd.read_html(str(table))
        MPdata = df[0]
          
        #Extract candidate name, winning party, and first and second place vote share
        
        temp_list[6*j+1] = MPdata.iat[0,1]
        temp_list[6*j+2] = MPdata.iat[0,4]
        temp_list[6*j+3] = MPdata.iat[0,2]
        temp_list[6*j+4] = MPdata.iat[1,1]
        temp_list[6*j+5] = MPdata.iat[1,4]
        temp_list[6*j+6] = MPdata.iat[1,2]

        
    list_of_data.append(temp_list)    

df=pd.DataFrame(list_of_data)    
        
        
        
        
#df.to_csv('IncumbencyData1.csv')
        
        
        
        
        
        
        
        
        
        
        
        
        