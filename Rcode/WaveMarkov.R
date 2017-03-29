## Wave Markov

# generates synthetic wind data, given a CPM

library(dplyr)

# get cpm matrix
cpmSWH<-read.table("../tpm/wave/wave_cpmSWH.csv",sep=",")
cpmSWH
cpmT<-read.table("../tpm/wave/wave_cpmT.csv",sep=",")
cpmT


## read in tidy data
path<-"../data/cleaned/wave/"
filehandle<-"wave001.csv"
fullname<-paste0(path,filehandle)
data<-read.csv(fullname,stringsAsFactors=FALSE,sep=",")
#rename columns
colnames(data)<-c("date","time","SWH","wavePeriod")
# convert time to POSIXct
timeStamp<-as.character(paste(data$date,data$time,sep=" "))
# convert time to POSIXct
timeStamp<-dmy_hms(timeStamp)


## TPM
reference<-data.frame(timeStamp,data$SWH,data$wavePeriod)
colnames(reference)<-c("timeStamp","SWH","wavePeriod")

reference$bin <- cut(reference$SWH, breaks = c(seq(0., ceiling(max(reference$SWH)), by = .25)), labels = 0:as.integer(ceiling(max(reference$SWH))/0.25-1))
reference$bin <-as.integer(reference$bin)
table(reference$bin)
sum(table(reference$bin))

maxSWH=max(reference$SWH)
maxT=max(reference$wavePeriod)
minT=min(reference$wavePeriod)


opfilepathstem<-"../data/synthetic/wave/wave1hr_"
opfilepathtail<-".csv"
opColNames<-c("hour","SWH","WavePeriod")

ndata<-24*365

for (file in 1:100){
    
    # set up output file numbers
    opfilehandle<-as.character(file)
    if (file < 10){
        opfilehandle<-paste0("00",opfilehandle)
    }
    if (file >= 10 && file < 100){
        opfilehandle<-paste0("0",opfilehandle)
    }
    opfilename<-paste0(opfilepathstem,opfilehandle,opfilepathtail)

    ## Stochastic generation of synthetic data
    h=numeric(ndata)
    wp=numeric(ndata)
    #randomly choose first wave height
    h[1]=floor((maxSWH*.99)*runif(1))+1
    wp[1]=floor(minT+((maxT-minT)*.99)*runif(1))+1
    print(h[1],wp[1])
    
    for (i in 2:ndata){
        colIndex=runif(1)
        j=1
        while (cpmSWH[round(h[i-1],0),j] < colIndex){
            j=j+1
        }
        h[i]=j
    }
    for (i in 2:ndata){
      colIndex=runif(1)
      j=1
      while (cpmT[round(wp[i-1],0),j] < colIndex){
        j=j+1
      }
      wp[i]=j
    }    
    
    # summary(h)
    # summary(reference$bin)
    
    # v=v-1+runif(ndata)
    # mean(v)-mean(reference$V)
    # sd(v)-sd(reference$V)
    # summary(v)
    # summary(reference$V)
    # library(MASS)
    # vFit<-fitdistr(v+.01, 'weibull')
    # print (vFit)
    # refFit<-fitdistr(reference$V+.01, 'weibull')
    # print(refFit)
    # hist(v,xlim=c(0,20),prob=TRUE)
    # d = dweibull(seq(0,20,.2),vFit$estimate[1],vFit$estimate[2])
    # points(seq(0,20,.2),d,type='l',col=2)
    # 
    mypar(2,1)
    plot(h[1:1000],type="l")
    lines(reference$SWH[1:1000],type="l",col="red")
    plot(wp[1:1000],type="l")
    lines(reference$wavePeriod[1:1000],type="l",col="red")
    
    newdata<-data.frame(1:ndata,h,wp)
    names(newdata)<-opColNames
    write.csv(newdata,opfilename,row.names=FALSE)
    print(paste("File",opfilehandle,"done",sep=" "))

}