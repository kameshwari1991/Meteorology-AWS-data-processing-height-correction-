#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Nov 30 15:42:52 2021

@author: kameshwari
"""
# import ht_correction_anuradha as dbthtcorr
import htcorrection as dbthtcorr

import pandas as pd
import os
import glob
import numpy as np

# awk -F" " '{ print $1,",",$2,",",$3}' bp15n90e_dy.ascii > bp15n90e_dy.csv
#awk -F" " ' { print $1,",",$2,",",$3,",",$4,",",$5,",",$6,",",$7,",",$8,",",$9,",",$10,",",$11 } ' met15n90e_dy.ascii >  met15n90e_dy.csv
inpath1 = 'folder path '
os.chdir(inpath1)
files = glob.glob('*.csv') 

for filename in files:
    metdata = pd.read_csv(inpath1 + filename)
    metdata.columns = metdata.columns.str.replace(" ","")
    ws_ht = 3; dbt_ht = 4; # If you have variable heights, 
                            #you can index the heights array while giving the arguments to the below function
    dbt10 = []; sph10 = []; ws10 = []; rh10 = []
    for ii in range(len(metdata.index)):
        (dbt10_0,sph10_0,ws10_0,rh10_0) = dbthtcorr.dbt_ht_correction(metdata.AIRT[ii],ws_ht,\
                                              metdata.SST[ii],metdata.SLP[ii],\
                                                  metdata.RH[ii],metdata.WSPD[ii],\
                                                      dbt_ht,"","",1)
        dbt10.append(dbt10_0); sph10.append(sph10_0); ws10.append(ws10_0); rh10.append(rh10_0); 
        
    metdata['SLP'] = metdata.SLP; metdata['dbt10'] = np.round(dbt10,2); metdata['sph10'] = np.round(sph10,2);
    metdata['ws10'] = np.round(ws10,2); metdata['rh10'] = np.round(rh10,2);       
    metdata.to_csv(inpath1 + filename[:-4] + '_htcorrected.csv', index = False)

    

# dbthtcorr.dbt_ht_correction(26.77,3,27.5,1013,78.9,1.8,4,"","",1)    
    
    
    
    
    
    
