## Wave analysis

#generates transition (tpmT) and cumulative (cpmT) probability matrices, given .csv file of data

#data file must be in format:
#header line
#data,time, T, wavePeriod


library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

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


## bins for wave heights
reference<-data.frame(timeStamp,data$SWH,data$wavePeriod)
colnames(reference)<-c("timeStamp","SWH","wavePeriod")
#put SWH in binSWHs, binwidthSWH=0.25m
reference$binSWH <- cut(reference$SWH, breaks = c(seq(0., ceiling(max(reference$SWH)), by = .25)), labels = 0:as.integer(ceiling(max(reference$SWH))/0.25-1))
reference$binSWH <-as.integer(reference$binSWH)
table(reference$binSWH)
sum(table(reference$binSWH))
maxbinSWH=max(reference$binSWH)

#bins for wave period
binseq<-c(seq(floor(min(reference$wavePeriod)), ceiling(max(reference$wavePeriod)), by = .5))
reference$binT <- cut(reference$wavePeriod, breaks = binseq, labels=seq(1,length(binseq)-1))
reference$binT <-as.integer(reference$binT)
table(reference$binT)
sum(table(reference$binT))
maxbinT=max(reference$binT)

#generate tpm and cpm for wave heights
tpmSWH<-matrix(0, nrow = maxbinSWH, ncol =maxbinSWH)
spmSWH<-numeric(maxbinSWH)
cpmSWH<-matrix(0, nrow = maxbinSWH, ncol =maxbinSWH)

# loop to generate tpmSWH
for (i in 1:nrow(reference)){
    tpmSWH[reference$binSWH[i],reference$binSWH[i+1]] <-tpmSWH[reference$binSWH[i],reference$binSWH[i+1]] + 1
}

tpmSWH[1,]

print(tpmSWH)
sum(tpmSWH[1,])

for (i in 1 :nrow(tpmSWH)){
    spmSWH[i]=sum(tpmSWH[i,])
}
spmSWH
sum(spmSWH)


for (i in 1 :nrow(tpmSWH)){
    tpmSWH[i,]=tpmSWH[i,]/sum(tpmSWH[i,])
}

# calculate cumulative probabilities
for (i in 1 : nrow(tpmSWH)){
    for (j in 1 :ncol(tpmSWH)){
        cpmSWH[i,j]=sum(tpmSWH[i,1:j])
    }
}
print (cpmSWH)

#repeat for wave periods
tpmT<-matrix(0, nrow = maxbinT, ncol =maxbinT)
spmT<-numeric(maxbinT)
cpmT<-matrix(0, nrow = maxbinT, ncol =maxbinT)

# loop to generate tpmT
for (i in 1:nrow(reference)){
  tpmT[reference$binT[i],reference$binT[i+1]] <-tpmT[reference$binT[i],reference$binT[i+1]] + 1
}

tpmT[1,]

print(tpmT)
sum(tpmT[1,])

for (i in 1 :nrow(tpmT)){
  spmT[i]=sum(tpmT[i,])
}
spmT
sum(spmT)


for (i in 1 :nrow(tpmT)){
  tpmT[i,]=tpmT[i,]/sum(tpmT[i,])
}

# calculate cumulative probabilities
for (i in 1 : nrow(tpmT)){
  for (j in 1 :ncol(tpmT)){
    cpmT[i,j]=sum(tpmT[i,1:j])
  }
}
print (cpmT)

write.table(cpmSWH,"../tpm/wave/wave_cpmSWH.csv",sep=",",row.names=FALSE,col.names=FALSE)
write.table(cpmT,"../tpm/wave/wave_cpmT.csv",sep=",",row.names=FALSE,col.names=FALSE)


