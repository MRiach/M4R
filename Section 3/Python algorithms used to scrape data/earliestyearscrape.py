#code to extract year of creation of constituency which is exported to CSV
#and then manipulated

from selenium import webdriver
import numpy as np
import pandas as pd
import os
import csv

os.chdir("C:\\Users\\Marwan Riach\\Dropbox\\M4R Shared Marwan Chris\\Incumbency Data")
X = pd.read_csv('EnglishConURLCSV.csv',header=None)
X = np.array(X)
Y = np.zeros(533)
Y=Y.tolist()

DRIVER_PATH = "C:\\Users\\Marwan Riach\\OneDrive\\Documents\\chromedriver"
driver = webdriver.Chrome(executable_path=DRIVER_PATH)

for i in range(533):
 driver.get(X[i,2])
#driver.get('https://en.wikipedia.org/wiki/Leigh_(UK_Parliament_constituency)')
#login_form = driver.find_element_by_xpath("//table[@class='infobox vcard']/tbody/tr[8]/td")
 yearcreated = driver.find_element_by_xpath("//table[@class='infobox vcard']/tbody")
#print(login_form)
#login_form.get_attribute('innerHTML')
 yearcreated = yearcreated.get_attribute('outerHTML')
#X = X.split('<')[1][3:]
 index = yearcreated.find('Created')
 Y[i] = yearcreated[index:index+40]



with open('scraped_data', 'w',newline='') as myfile:
    wr = csv.writer(myfile, quoting=csv.QUOTE_ALL)
    wr.writerow(Y)

#https://www.guru99.com/selenium-webtable.html