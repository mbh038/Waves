## Wave analysis

#generates transition (TPM) and cumulative (CPM) probability matrices, given .csv file of data

#data file must be in format:
#header line
#data,time, SWH, wavePeriod


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


## TPM
reference<-data.frame(timeStamp,data$SWH,data$wavePeriod)
colnames(reference)<-c("timeStamp","SWH","wavePeriod")

reference$bin <- cut(reference$SWH, breaks = c(seq(0., ceiling(max(reference$SWH)), by = .25)), labels = 0:as.integer(ceiling(max(reference$SWH))/0.25-1))
reference$bin <-as.integer(reference$bin)
table(reference$bin)
sum(table(reference$bin))

maxBin=max(reference$bin)

tpm<-matrix(0, nrow = maxBin, ncol =maxBin)
spm<-numeric(maxBin)
cpm<-matrix(0, nrow = maxBin, ncol =maxBin)

# loop to generate TPM
for (i in 1:nrow(reference)){
    tpm[reference$bin[i],reference$bin[i+1]] <-tpm[reference$bin[i],reference$bin[i+1]] + 1
}

tpm[1,]

print(tpm)
sum(tpm[1,])

for (i in 1 :nrow(tpm)){
    spm[i]=sum(tpm[i,])
}
spm
sum(spm)


for (i in 1 :nrow(tpm)){
    tpm[i,]=tpm[i,]/sum(tpm[i,])
}

# calculate cumulative probabilities
for (i in 1 : nrow(tpm)){
    for (j in 1 :ncol(tpm)){
        cpm[i,j]=sum(tpm[i,1:j])
    }
}
print (cpm)

write.table(cpm,"../tpm/wave/wave_cpm.csv",sep=",",row.names=FALSE,col.names=FALSE)


