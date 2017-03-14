# -*- coding: utf-8 -*-
"""

waveTMP

Created on Fri Mar 10 06:01:22 2017
@author: mbh
"""
import numpy as np

def readData(filepath="../data/cleaned/wave/wave001.csv"):
    data = np.genfromtxt(filepath, dtype=float, delimiter=',', names=True)
    swh=data['SWHm']
    period=data['Periods']    
    return swh,period
    
def tpmSWH(swh):
    
    maxBin=max(reference$bin)
    
    tpm=np.zeros()    
# loop to generate TPM
for (i in 1:nrow(reference)){
    tpm[reference$bin[i],reference$bin[i+1]] <-tpm[reference$bin[i],reference$bin[i+1]] + 1
}