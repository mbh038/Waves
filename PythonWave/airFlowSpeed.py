# -*- coding: utf-8 -*-
"""
Created on Wed Mar 22 13:54:47 2017
@author: mbh
"""

import numpy as np
import random as rd
import matplotlib.pyplot as plt

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
    
    
def wellsCoeffs():
    
    
    CDfilename="../data/specs/wells_cd.csv"
    
    with open(CDfilename,'r') as file:
        data  = file.readlines() 
    pairs_cD=[[x for x in line.rstrip().split(',') ] for line in data]
    print(float(pairs_cD[2][0]))
    
    alpha_cD=[float(pairs_cD[x][0]) for x in range(1,len(pairs_cD))]
    cD=[float(pairs_cD[x][1]) for x in range(1,len(pairs_cD))]    
    
    print(alpha_cD[:10])
    print(cD[:10])


    CLfilename="../data/specs/wells_cl.csv"
    
    with open(CLfilename,'r') as file:
        data  = file.readlines() 
    pairs_cL=[[x for x in line.rstrip().split(',') ] for line in data]
    print(float(pairs_cL[2][0]))
    
    alpha_cL=[float(pairs_cL[x][0]) for x in range(1,len(pairs_cL))]
    cL=[float(pairs_cL[x][1]) for x in range(1,len(pairs_cL))]    
    
    print(alpha_cL[:10])
    print(cL[:10])
    
    plt.figure()
    plt.plot(alpha_cL[:],cL[:],'b-',label='cL')
    plt.plot(alpha_cD[:],cD[:],'r-',label='cD')
    plt.xlabel("Angle of attack")
    plt.ylabel("Drag coefficients")
    plt.legend(loc='upper left')
