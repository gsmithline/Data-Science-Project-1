#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Mar  8 17:38:35 2020

@author: gabesmithline
"""
#These are the libraries used for this code
#Allows to run python in shell
import psycopg2
#Lets you write queried data to CSV
import csv
from csv import writer
from csv import reader

#This connects Python to the terminal
#The user should only change user and database

try:
    connection = psycopg2.connect(user = "smithlig",
                                  password = "",
                                  host = "localhost",
                                  port = "5432",
                                  database = "smithlig")

    cursor = connection.cursor()
    # Print PostgreSQL Connection properties
    #These are the properties to connect your python script to the terminal
    print ( connection.get_dsn_parameters(),"\n")
    ##This runs the query which we did in PSQL
    query = "SELECT species_id, category, parks.park_name, abundance, latitude, longitude, acres FROM flora_fauna, parks where parks.park_name = flora_fauna.park_name;"
    #This fetches the data from the query
    cursor.execute(query)
    data = cursor.fetchall()
  
  #Make sure to upload a blank CSV, this is where you will put the queried data
   # This puts the queried data in the uploaded blank csv
    with open("combined.csv",'w') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerows(data)






#This prints the error message if there is one
except (Exception, psycopg2.Error) as error :
    print ("Error while connecting to PostgreSQL", error)



