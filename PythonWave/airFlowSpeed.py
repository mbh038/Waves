# -*- coding: utf-8 -*-
"""
Created on Wed Mar 22 13:54:47 2017
@author: mbh
"""

import numpy as np
import time
import math
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
#    print(float(pairs_cD[2][0]))
    
    alpha_cD=[float(pairs_cD[x][0]) for x in range(1,len(pairs_cD))]
    cD=[float(pairs_cD[x][1]) for x in range(1,len(pairs_cD))]    
    
#    print(alpha_cD[:10])
#    print(cD[:10])


    CLfilename="../data/specs/wells_cl.csv"
    
    with open(CLfilename,'r') as file:
        data  = file.readlines() 
    pairs_cL=[[x for x in line.rstrip().split(',') ] for line in data]
#    print(float(pairs_cL[2][0]))
    
    alpha_cL=[float(pairs_cL[x][0]) for x in range(1,len(pairs_cL))]
    cL=[float(pairs_cL[x][1]) for x in range(1,len(pairs_cL))]    
    
#    print(alpha_cL[:10])
#    print(cL[:10])
        
    alpha=[x for x in range(0,91)]
    
    cl_interp=np.interp(alpha,alpha_cL,cL)
    cd_interp=np.interp(alpha,alpha_cD,cD)
    
#    plt.figure()
#    plt.plot(alpha[:],cl_interp[:],'g-',label='Linterp')
#    plt.plot(alpha[:],cd_interp[:],'b-',label='Dinterp')
#    plt.xlabel("Angle of attack")
#    plt.ylabel("Drag coefficients")
#    plt.legend(loc='upper left')
#    
#    plt.figure()
#    plt.plot(cd_interp[:],cl_interp[:],'b-',label='cL')
#    plt.xlabel("Drag coefficient cD")
#    plt.ylabel("Lift coefficient cL")

    
    return alpha,cl_interp,cd_interp
    

def Ft():
    
    alpha,cl,cd=wellsCoeffs()
    
    
    
    alpharad=[alpha[i]*np.pi/180 for i in range(len(alpha))]
    
    gamma=[cl[i]/cd[i] for i in range(len(alpha))]
    
    for i in range(21):
        gamma[i]=1000
    
#    plt.figure()
#    plt.plot(alpha[21:],gamma[21:],'b-',label='gamma')
#
#    plt.xlabel("Angle of attack")
#    plt.ylabel("L/D ratio")
#    plt.legend(loc='upper left')  
    
    ft=[cl[i]*(math.sin(alpharad[i])-math.cos(alpharad[i])/gamma[i]) for i in range(len(alpha))]
    
#    plt.figure()
#    plt.plot(alpha[:],ft[:],'b-',label='ft')
#    plt.xlabel("Angle of attack")
#    plt.ylabel("ft ")
#    plt.legend(loc='upper left')
    
    return gamma,alpharad,ft
    
def wtPower(omega,va,alphas,cls,cds):
    
    rmin=0.1
    rmax=1.75
    alphamin=math.atan(va/(omega*rmax))
    alphamax=math.atan(va/(omega*rmin))
#    print(alphamin,alphamax)
    steps=100
    dalpha=(alphamax-alphamin)/steps
    
    
#    for i in range(len(gamma)):
#        print (gamma[i],alpharad[i],ft[i])
        
    pTotal=0
    for alpha in np.arange(alphamin,alphamax,dalpha):
        cd=np.interp(alpha, alphas, cds)
        cl=np.interp(alpha, alphas, cls)
        gamma=cl/cd
        dP=va*cl*(1-1/(gamma*math.tan(alpha)))
        pTotal+=dP*dalpha
    return pTotal
     

def wtTime(rpm):
    
    wtOmega=(2*3.14/60)*rpm
    
    alphas,cls,cds=wellsCoeffs()
    
    va=10
    p=wtPower(wtOmega,va,alphas,cls,cds)
    return p
    
#average power for given swh, period and rpm    
def P(alphas,cls,cds,swh,T,d1,d2,rpm):
    alphas,cls,cds=wellsCoeffs()
    wtOmega=(2*3.14/60)*rpm
    ts=[n*T/20 for n in range(20)]
    omega=2*np.pi/T
    psum=0
    for t in ts:
        vw=(swh/2)*omega*math.cos(omega*t)
        va=abs(((d2/d1)**2)*vw)
        psum+=wtPower(wtOmega,va,alphas,cls,cds)
        psum/=20
    return psum
    
#time variation of Wells power output, given wave data
def pWave(rpm,filename="../data/synthetic/wave/wave1hr_001.csv",d1=1,d2=1):
    
    t=time.clock()     
    with open(filename,'r') as file:
        data  = file.readlines() 

    wd=[[x for x in line.rstrip().split(',') ] for line in data]
    id=[int(wd[x][0]) for x in range(1,len(wd))]
    swhBin=[int(wd[x][1]) for x in range(1,len(wd))]
    periodBin=[int(wd[x][2]) for x in range(1,len(wd))]
    
#    print(id[:10])
#    print(swhBin[:10])
#    print(periodBin[:10])
    
    swhBinWidth= 0.25
    periodBinWidth=0.5
    
    swh=[swhBinWidth*(x-1+rd.random()) for x in swhBin]
    period=[periodBinWidth*(x-1+rd.random()) for x in periodBin] 
    
    alphas,cls,cds=wellsCoeffs()
    power=[]
    for i in range(0,len(swh)):#range(len(swh)):
        power.append(P(alphas,cls,cds,swh[i],period[i],d1,d2,rpm))

    
#    plt.plot(swh)
#    plt.plot(period)
    
#    plt.plot(power)
    
#    opfilename="../results/powerVsRpm"+str(rpm)+"/wave1hr_001"+"rpm"+str(rpm)+".csv"
#
#    f = open(opfilename,'w')
#    for i in range(len(power)):
##        print(i,power[i])
#        f.write(str(power[i])+'\n') #Give your csv text here.
#    ## Python will convert \n to os.linesep
#    f.close() 

        
    maxPower=max(power)
    minPower=min(power)
    meanPower=sum(power)/len(power)
    
    return minPower,maxPower,meanPower,power
    
def optimiseRpm():
#    rpms=[x for x in range(50,550,50)]
    rpms=[x for x in range(2,10,2)]
    
    minpower,maxpower,meanpower=[],[],[]
    for rpm in rpms:
        print("rpm=: ",rpm)
        minp,maxp,meanp=pWave(rpm,filename="../data/synthetic/wave/wave1hr_001.csv",d1=1,d2=1)
        minpower.append(minp)
        maxpower.append(maxp)
        meanpower.append(meanp)
        
        print("min:",minp,"max:",maxp,"mean:",meanp)
        
    
    plt.plot(rpms,meanpower)    
    return rpms,minpower,maxpower,meanpower

def monthlyMeanPower(rpm):
    
    filename="../results/powerVsRpm/wave1hr_001"+"rpm"+str(rpm)+".csv"
    with open(filename,'r') as file:
        data  = file.readlines() 
    power=[float(line.rstrip()) for line in data]
    hpm=8760//12
    monthlyMeans=[]
    for month in range(12):
        powersum=0
        for hour in range(hpm*month,hpm*(month+1)):
            powersum+=power[hour]
        monthlyMeans.append(powersum/hpm)
    plt.plot(monthlyMeans)
    return monthlyMeans
            
    
def wtTrials(rpm=20):
     
    waveFileNameStem="../data/synthetic/wave/wave1hr_"
    waveFileNameTail=".csv"
    
    
          
    for i in range (1,101):
        if i<10:
            fileNum="00"+str(i)
        if i>=10 and i<100:
            fileNum="0"+str(i)
        if i==100:
            fileNum="100"
            
        print("File"+fileNum+" being processed")
         
        waveFileName=waveFileNameStem+fileNum+waveFileNameTail
        powerFileName="../results/powerVsYearRpm"+str(rpm)+"/wave1hr_"+fileNum+"rpm"+str(rpm)+".csv" 
        minp,maxp,meanp,power=pWave(rpm,waveFileName,d1=1,d2=1)
        
        f = open(powerFileName,'w')
        for j in range(len(power)):
#        print(i,power[i])
            f.write(str(power[j])+'\n') #Give your csv text here.
    ## Python will convert \n to os.linesep
        f.close() 

        print(minp,maxp,meanp)
        
        plotPower(i,rpm)
        
#        powerFileName="../results/powerVsYearRpm"+str(rpm)+"/wave1hr_"+fileNum+"rpm"+str(rpm)+".csv"
#
#        f = open(powerFileName,'w')
#        for i in range(len(power)):
#            f.write(str(power[i])+'\n') 
#        f.close()
#        print ("File"+fileNum+" done")
         
def plotPower(fileId=1,rpm=20):
    
    if fileId<10:
        fileNum="00"+str(fileId)
    if fileId>=10 and fileId<100:
        fileNum="0"+str(fileId)
    if fileId>99:
        fileNum="100"   
     
    powerFileName="../results/powerVsYearRpm"+str(rpm)+"/wave1hr_"+fileNum+"rpm"+str(rpm)+".csv"
    waveFileName="../data/synthetic/wave/wave1hr_"+fileNum+".csv"
    with open(waveFileName,'r') as file:
        data  = file.readlines() 

    wd=[[x for x in line.rstrip().split(',') ] for line in data]
    id=[int(wd[x][0]) for x in range(1,len(wd))]
    swhBin=[int(wd[x][1]) for x in range(1,len(wd))]
    periodBin=[int(wd[x][2]) for x in range(1,len(wd))]
    
    swhBinWidth= 0.25
    periodBinWidth=0.5
    
    swh=[swhBinWidth*(x-1+rd.random()) for x in swhBin]
    period=[periodBinWidth*(x-1+rd.random()) for x in periodBin] 
    maxSwh=max(swh)
    
    with open(powerFileName,'r') as file:
        data  = file.readlines() 
    power=[float(line.rstrip()) for line in data]
    maxPower=max(power)
    scaledSwh=[x*maxPower/maxSwh for x in swh]
    hours=[x for x in range(1,8761)]
    
    plt.plot(hours,power)
    plt.plot(hours,scaledSwh,'r-')
    
def powerAnalysisMeans(rpm=20):
    
    opFileName="../results/powerVsYearRpm"+str(rpm)+"/wave1hr_rpm"+str(rpm)+"_powerMeanSummary.csv"
    fop = open(opFileName,"a+")
    for i in range (1,101):
        if i<10:
            fileNum="00"+str(i)
        if i>=10 and i<100:
            fileNum="0"+str(i)
        if i==100:
            fileNum="100"
            
        powerFileName="../results/powerVsYearRpm"+str(rpm)+"/wave1hr_"+fileNum+"rpm"+str(rpm)+".csv" 
        with open(powerFileName,'r') as file:
            data  = file.readlines()    
        power=[float(line.rstrip()) for line in data]     
        hpm=8760//12
        monthlyMeans=[]
        for month in range(12):
            powersum=0
            for hour in range(hpm*month,hpm*(month+1)):
                powersum+=power[hour]
            monthlyMeans.append(powersum/hpm)
        for j in range(len(monthlyMeans)):
#        print(i,power[i])
            fop.write(str(monthlyMeans[j])+',') #Give your csv text here. 
        fop.write("\n")
      
def powerAnalysisMaxs(rpm=20):
    
    opFileName="../results/powerVsYearRpm"+str(rpm)+"/wave1hr_rpm"+str(rpm)+"_powerMaxSummary.csv"
    fop = open(opFileName,"a+")
    for i in range (1,101):
        if i<10:
            fileNum="00"+str(i)
        if i>=10 and i<100:
            fileNum="0"+str(i)
        if i==100:
            fileNum="100"
            
        powerFileName="../results/powerVsYearRpm"+str(rpm)+"/wave1hr_"+fileNum+"rpm"+str(rpm)+".csv" 
        with open(powerFileName,'r') as file:
            data  = file.readlines()    
        power=[float(line.rstrip()) for line in data]     
        hpm=8760//12
        monthlyMaxs=[]
        for month in range(12):
            powermax=0
            for hour in range(hpm*month,hpm*(month+1)):
                if power[hour]>powermax:
                    powermax=power[hour]
            monthlyMaxs.append(powermax)
        for j in range(len(monthlyMaxs)):
#        print(i,power[i])
            fop.write(str(monthlyMaxs[j])+',') #Give your csv text here. 
        fop.write("\n")            
        
    