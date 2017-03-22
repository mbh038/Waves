# -*- coding: utf-8 -*-
"""
Created on Wed Mar 22 13:54:47 2017
@author: mbh
"""

import numpy as np
import random as rd

def airFlowSpeed(filename="wave1hr_001.csv"):
            
    with open(filename,'r') as file:
        data  = file.readlines()        
    wd=[[x for x in line.rstrip().split(',') ] for line in data]
    id=[int(wd[x][0]) for x in range(1,len(wd))]
    swhBin=[int(wd[x][1]) for x in range(1,len(wd))]
    periodBin=[int(wd[x][2]) for x in range(1,len(wd))]
    
#    print(id[:10])
    print(swhBin[:10])
    print(periodBin[:10])
    
    swhBinWidth= 0.25
    periodBinWidth=0.5
    
    swh=[swhBinWidth*(x-1+rd.random()) for x in swhBin]
    period=[periodBinWidth*(x-1+rd.random()) for x in periodBin]
    
    print(swh[:10])
    print(period[:10])
    
    
    
