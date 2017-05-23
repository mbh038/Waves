
library(rafalib)

mypar(4,1)
p1<-read.csv('../data/cleaned/Wave/wave001.csv',header=TRUE,stringsAsFactors=FALSE)
p1$hour<-seq(1,nrow(p1))
str(p1)
plot(p1$hour,p1$SWH.m.,type='l',xlab='hour',ylab='wave height',col='blue')
p2<-read.csv('../data/synthetic/wave/wave1hr_002.csv',header=TRUE,stringsAsFactors=FALSE)
plot(p2$hour,p2$SWH/4,type='l',xlab='hour',ylab='wave height')
p3<-read.csv('../data/synthetic/wave/wave1hr_003.csv',header=TRUE,stringsAsFactors=FALSE)
plot(p3$hour,p3$SWH/4,type='l',xlab='hour',ylab='wave height')
p4<-read.csv('../data/synthetic/wave/wave1hr_004.csv',header=TRUE,stringsAsFactors=FALSE)
plot(p4$hour,p4$SWH/4,type='l',xlab='hour',ylab='wave height')


mypar(1,1)
#Mean power vs rpm
df0<-read.csv('../results/powerVsRpm/meanPowerVSrpm.csv',header=TRUE, stringsAsFactors=FALSE)
# df$pmin=as.numeric(df$pmin)
str(df0)
plot(df0$rpm,df0$pmean,xlim=c(0,300),ylim=c(0,6),type='o',col='red',pch=19,xlab="rpm",ylab="Relative power")

mypar(2,1)
#Plot mean powers for each month, from 100 files
df1<-read.csv('../results/powerVsYearRpm20/wave1hr_rpm20_powerMeanSummary.csv',header=FALSE, stringsAsFactors=FALSE)
df1$V13<-NULL
names(df1)<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
str(df1)
boxplot(df1, use.cols = TRUE,xlab="Month",ylab="Proportional to power")
# title(main="Monthly variation in mean and maximum power")

#Plot mean powers for each month, from 100 files
df2<-read.csv('../results/powerVsYearRpm20/wave1hr_rpm20_powerMaxSummary.csv',header=FALSE, stringsAsFactors=FALSE)
df2$V13<-NULL
names(df2)<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
str(df2)
boxplot(df2, use.cols = TRUE,xlab="Month",ylab="Proportional to power")


