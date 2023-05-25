
# -*- coding: utf-8 -*-

import numpy as np
import pandas as pd
import pickle
import csv
import time
import datetime as dt
import os
import itertools
from itertools import permutations
import array
import datetime


COUNTRY = "NL"

times = []
Costs = []
hospitals_to_csv = []

H = {}
hospitals = ["rovaniemi", "hyvinkää", "jorvi", "jyväskylä"]

# Path to the directory where this file is stored (ending with a "/")
HOME_DIR = "."
# Path were the data is stored.
INPUT_DIR = HOME_DIR + "\\data_"+COUNTRY+"\\"

def write_files(hospital):
    Cost = []
    transp_times = []
    # Reading existing data into a list
    rows = list(csv.reader(open(INPUT_DIR+hospital+"_etäisyydet.csv", "r",encoding = "utf-8"), delimiter=","))

    # Creating F_testi-file containing ID of origin, type, and random lat&lon
    with open(INPUT_DIR+"F.csv", 'w', newline='', encoding = "utf-8") as file:
        
        n = 0
        writer = csv.writer(file)
        writer.writerow(['name', 'type', 'lat', 'lon'])
        
        hospital_to_add = [hospital, "E", 1, 1]
        hospitals_to_csv.insert(0, hospital_to_add)
            
        for row in rows[1:]:  
                
            speed = 100      # km/h
            fuel_cost = 1
            dist = float(row[2]) # in km
            cost = dist * fuel_cost
            t = dist/speed #time in hours

            if n < 10:
                writer.writerow([row[0], "H", 1, 1])
                H[n] = row[0]
        
                hours = int(t//1)   # full hours
                hours_r = t%1       # remaining hours 0.XXX

                hours_to_m  = hours_r*60
                mins = int(hours_to_m//1) #full minutes
                mins_r = hours_to_m%1   # remaining minutes
                sec = int(mins_r*60)    
                
                transp_time = datetime.time(hours, mins, sec)
                transp_times.insert(n, str(transp_time))
                Cost.insert(n, cost)
                n += 1
        
        writer.writerows(hospitals_to_csv)

        H_dest = list(H.values())
        H_dest.insert(0, "name")
        transp_times.insert(0, hospital)
        Cost.insert(0, hospital)

        Costs.insert(0, Cost)
        times.insert(0, transp_times)

        with open(INPUT_DIR+'Dt.csv', 'w+', newline='', encoding = "utf-8") as file:
            writer = csv.writer(file)
            writer.writerow(H_dest)
            writer.writerows(times)
        file.close()

        with open(INPUT_DIR+'Cd.csv', 'w+', newline='', encoding = "utf-8") as file:
            writer = csv.writer(file)
            writer.writerow(H_dest)
            writer.writerows(Costs)
        file.close()

    file.close
    
    with open('data_NL/H.pickle', 'wb') as handle:
        pickle.dump(H, handle, protocol=pickle.HIGHEST_PROTOCOL)
    

for hospital in hospitals:
    write_files(hospital)
