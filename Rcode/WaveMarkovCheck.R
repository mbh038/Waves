## Wave Markov
library(dplyr)

##Get example real data

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

reference<-data.frame(timeStamp,data$SWH,data$wavePeriod)
colnames(reference)<-c("timeStamp","SWH","wavePeriod")

reference$bin <- cut(reference$SWH, breaks = c(seq(0., ceiling(max(reference$SWH)), by = .25)), labels = 0:as.integer(ceiling(max(reference$SWH))/0.25-1))
reference$bin <-as.integer(reference$bin)
table(reference$bin)
sum(table(reference$bin))

maxSWH=max(reference$SWH)


ipfilepathstem<-"../data/synthetic/wave/wave1hr_"
ipfilepathtail<-".csv"

ndata<-24*365

bin_size=0.25

library(rafalib)
for (file in 1:100){
    
    # set up input file numbers
    ipfilehandle<-as.character(file)
    if (file < 10){
        ipfilehandle<-paste0("00",ipfilehandle)
    }
    if (file >= 10 && file < 100){
        ipfilehandle<-paste0("0",ipfilehandle)
    }
    ipfilename<-paste0(ipfilepathstem,ipfilehandle,ipfilepathtail)

    newdata<-read.csv(ipfilename)
    print(paste("File",ipfilehandle,"in",sep=" "))

    ndata<-nrow(newdata)
    h<-newdata[,2]
    
#     print(summary(v))
#     summary ((reference$bin[1:ndata]))
    
    h=(h-1+runif(ndata))*bin_size
    mean(h)-mean(reference$SWH[1:ndata])
    sd(h)-sd(reference$SWH[1:ndata])
    #print (summary(v))
    #print(summary(reference$V[1:ndata]))
    library(MASS)
    hFit<-fitdistr(h+.01, 'weibull')
    print (hFit)
    refFit<-fitdistr(reference$SWH[1:ndata]+.01, 'weibull')
    print(refFit)
    
    mypar(2,1)
    print(paste(round(mean(h),2),round(mean(reference$SWH[1:ndata]),2),round(hFit$estimate[1],2),round(hFit$estimate[2],2),round(refFit$estimate[1],2),round(refFit$estimate[2],2),sep=" "))
    hist(h,xlim=c(0,10),breaks=20,prob=TRUE)
    d = dweibull(seq(0,10,.2),hFit$estimate[1],hFit$estimate[2])
    points(seq(0,10,.2),d,type='l',col=2)

    plot(h[1:1000],type="l",ylim=c(0,10))
    lines(reference$SWH[1:1000],type="l",col="red")
    


}

