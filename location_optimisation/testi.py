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
# Path to the directory where this file is stored (ending with a "/")
HOME_DIR = "."
# Path were the data is stored.
INPUT_DIR = HOME_DIR + "/data_"+COUNTRY+"/"

KP_NUM = 10000
SPEED = 30      # km/h
FUEL_COST = 1   # €/km

times = []
Costs = []
hospitals_to_csv = []
H = {}

# Add hospitals to be considered as blood storage mikälie
hospitals = ["rovaniemi","hyvinkää","hämeenlinna","joensuu","kajaani", "kemi", "kokkola","kotka",
             "kuopio","lahti","lappeenranta","lohja","meilahti","mikkeli","oulu","peijas","pori",
             "porvoo","tampere","turku","vaasa","seinäjoki","savonlinna","jyväskylä","jorvi"]

#hospitals = ["jorvi","meilahti","peijas"]

# Hyva districts 
hyva_list = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22]

# Problematic ids that will mess up the files
skip_ids = [81997,111001,232108,352151,467428,327624,70636,252923,109713,114580,
            54275,50274,162455,412121,60408,152713,63819,150796,75107,274219,247952,
            176713,202873,52962,523511]

def handle_squarekms():
    squares = {}
    # Reading existing ata per km² into a list
    df = pd.read_csv(INPUT_DIR+"1x1km_ruudut.csv", sep = ";", 
                 usecols = ["nro","manner","hyv_alue_nro","kunta_nimi","euref_x","euref_y","asukasluku","tie_status","pahpit_saatto_enn","muuonn_saatto_enn", "liikonn_saatto_enn","trauma_saatto_enn"])
    sorted_version = df.sort_values(by = ["trauma_saatto_enn"], ascending = False)

    # Checking that rows fulfill the following requirements:
    rows = sorted_version.values.tolist()

    # Cleaning out problematic square km² ids that will mess up the code
    rows = [row for row in rows if int(row[0]) not in skip_ids]


    a = len(rows)
    print("Rows count at the  beginning: ", a)
    # No NULLs allowed
    #rows = [row for row in rows if row[2] != "#NULL!"]
    # Point in the continent
    # rows = [row for row in rows if int(row[1]) == 1]
    # Point in among selected hyvinvointialue areas
    # rows = [row for row in rows if int(row[2]) in hyva_list]

    b = len(rows)
    print("Rows count after cleaning the set: ", b)   
    c = a-b 
    print("Total count of deleted rows: ", c)

    for row in rows:
        square_index = int(row[0])
        city_name = row[3]
        lat = float(row[4])
        lon = float(row[5])
        population = int(row[6])
        road_status = row[7]
        assaults = float(row[8].replace(",","."))
        otheraccs = float(row[9].replace(",","."))
        trafficaccs = float(row[10].replace(",","."))
        alltraumas = 10*float(row[11].replace(",",".")) # for 10 years time
        trauma_sum = assaults+otheraccs+trafficaccs

        # Taking all points with the same predicted trauma count as the last point
        lowest_trauma_count = 10*float(rows[KP_NUM-1][11].replace(",","."))

        if alltraumas >= lowest_trauma_count:
            squares[square_index]=[lat, lon, city_name, population, road_status, assaults, otheraccs, trafficaccs, alltraumas]
        
    return(squares)

def write_files(hospital, square_km):
    
    Cost = []
    transp_times = []
    Opds = []
    Ods = []
    # Reading existing data into a list
    #rows = list(csv.reader(open(INPUT_DIR+hospital+"_etäisyydet.csv", "r",encoding = "utf-8"), delimiter=","))

    df = pd.read_csv(INPUT_DIR+hospital+"_etäisyydet.csv", sep = ",",  usecols = ["Alkuperäinen","Kohde","Etäisyys_tulos"])
    rows = df.sort_values(by = ["Alkuperäinen"]).values.tolist()

     # Cleaning out problematic square km² ids that will mess up the code
    rows = [row for row in rows if int(row[0]) not in skip_ids]

    # Creating F_testi-file containing ID of origin, type, and random lat&lon
    # Making also pickles needed for the optimization
    with open(INPUT_DIR+"F.csv", 'w', newline='', encoding = "utf-8") as file:
        
        n = 0
        row_count = 0
        writer = csv.writer(file)
        writer.writerow(['name', 'type', 'lat', 'lon'])
        
        hospital_to_add = [hospital, "E", 1, 1] # Check the hospital coordinates!!!
        hospitals_to_csv.insert(0, hospital_to_add)

        print(f"Processing {hospital}")
        print(f"Going to read {len(rows)}")

        for row in rows:  

            row_count = row_count +1 
            square_id = int(row[0])
            dist = float(row[2]) # in km
            t = dist/SPEED # time in hours
            cost = dist * FUEL_COST

            if square_id in square_km:    #  
                
                info = square_km[square_id]
                lat = info[0]
                lon = info[1]

                num_of_accs = [info[8], 0]
                Ods.insert(n, num_of_accs)
                num_of_products = float(info[8]) # Assumed that 1 product is used per accident
                Opds.insert(n,[num_of_products,0])
                H[n]=square_id
                
                writer.writerow([square_id, "H", lat, lon])
                H[n] = str(int(row[0]))
        
                hours = int(t//1)   # full hours
                hours_r = t%1       # remaining hours 0.XXX

                hours_to_m  = hours_r*60
                mins = int(hours_to_m//1) # full minutes
                mins_r = hours_to_m%1   # remaining minutes
                sec = int(mins_r*60)    
                
                if hours > 23:
                     transp_time = datetime.time(23,59,59)    
                else:
                    transp_time = datetime.time(hours, mins, sec)
                transp_times.insert(n, str(transp_time))
                #transp_time = datetime.time(hours, mins, sec)
                #transp_times.insert(n, str(transp_time))
                Cost.insert(n, cost)
                n += 1
        
       

        writer.writerows(hospitals_to_csv)


    print(f"{row_count} rows read, found {n} matching ids")
    #print("After hospital ", hospital, ", cost contains: ", Cost)
    #print("Costs contain: ", Costs)
    
    H_dest = list(H.values())
    #print("Values in H.dest(); ", H_dest)
    a = type(H_dest[0])
    #print("Type of the values in H_dest: ", a)
    

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

    Od = np.array(Ods)
    OPd = np.array(Opds)
    
    with open('data_NL/Od.pickle', 'wb') as handle:
        pickle.dump(Od, handle, protocol=pickle.HIGHEST_PROTOCOL)

    with open('data_NL/OPd.pickle', 'wb') as handle:
        pickle.dump(OPd, handle, protocol=pickle.HIGHEST_PROTOCOL)
    
    with open('data_NL/H.pickle', 'wb') as handle:
        pickle.dump(H, handle, protocol=pickle.HIGHEST_PROTOCOL)
    

# Getting all useful information per km²:
square_km = handle_squarekms() 

n= 0
for key in square_km:
    if n<10:
        print(key, ":", square_km[key])
    if n == 10:
        print("11th element is:", key, ":", square_km[key])
    if n == 999:
        print("1000th element is:", key, ":", square_km[key])
    if n == 9999:
        print("10000th element is:", key, ":", square_km[key])
    if n == len(square_km)-1:
        print("Last element in the dictionary is ", key, ":", square_km[key])
    n = n+1

print("Number of keys in dictionary;")
print(len(square_km))


# Creating .csvs and .pickles
for hospital in hospitals:
    write_files(hospital, square_km)




# Let's see what is inside the pickles
#with open(INPUT_DIR+'H.pickle', 'rb') as handle:
#		H = pickle.load(handle)

#print("H.pickle contains: ", H)


# Matrix cells contain the total number of products in direct transports .
#with open(INPUT_DIR+'OPd.pickle', 'rb') as handle:
    #OPd = pickle.load(handle)

#print("OPd.pickle contains: ",OPd)

# Matrix cells contain the total number of direct transports.
#with open(INPUT_DIR+'Od.pickle', 'rb') as handle:
    #Od = pickle.load(handle)

#print("Od.pickle contains: ",Od)
