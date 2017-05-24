#plots the graphs fr Richard's project

library(rafalib)
#lift and drag coefficients

pdf('../figures/cdcl.pdf')
mypar(2,1)
coeffs<-read.csv('../data/specs/wellsCoeffs.csv',header=TRUE,stringsAsFactors=FALSE)
str(coeffs)
plot(coeffs$alpha,coeffs$cD,
     type='l',
     col='blue',
     xlab="Angle of attack",
     ylab="Lift/drag coefficient",
     ylim=c(0,2),
     cex.axis=0.8,
     cex.lab=0.8)
lines(coeffs$alpha,coeffs$cL,type='l',col='red')
legend('topleft',c("cL","cD"),lty=c(1,1),col=c("red","blue"),cex=0.7)

plot(coeffs$alpha,coeffs$liftToDragRatio,
     type='l',
     col='blue',
     xlab="Angle of attack",
     ylab="Lift to drag ratio",
     ylim=c(0,300),
     cex.axis=0.8,
     cex.lab=0.8)
dev.off()

#Plots of synthetic and real wave data
pdf('../figures/wavePlots.pdf')
mypar(4,1)
p1<-read.csv('../data/cleaned/Wave/wave001.csv',header=TRUE,stringsAsFactors=FALSE)
p1$hour<-seq(1,nrow(p1))
str(p1)
plot(p1$hour,p1$SWH.m.,type='l',xlab='hour',ylab='wave height (m)',xlim=c(0,9000),col='blue')
p2<-read.csv('../data/synthetic/wave/wave1hr_002.csv',header=TRUE,stringsAsFactors=FALSE)
plot(p2$hour,p2$SWH/4,type='l',xlab='hour',ylab='wave height (m)',xlim=c(0,9000))
p3<-read.csv('../data/synthetic/wave/wave1hr_003.csv',header=TRUE,stringsAsFactors=FALSE)
plot(p3$hour,p3$SWH/4,type='l',xlab='hour',ylab='wave height (m)',xlim=c(0,9000))
p4<-read.csv('../data/synthetic/wave/wave1hr_004.csv',header=TRUE,stringsAsFactors=FALSE)
plot(p4$hour,p4$SWH/4,type='l',xlab='hour',ylab='wave height (m)',xlim=c(0,9000))
dev.off()


#Mean power vs rpm
pdf('../figures/meanPowerVsRpm.pdf')
mypar(1,1)
df0<-read.csv('../results/powerVsRpm/meanPowerVSrpm.csv',header=TRUE, stringsAsFactors=FALSE)
# df$pmin=as.numeric(df$pmin)
str(df0)
smoothingSpline = smooth.spline(df0$rpm, df0$pmean, spar=0.2)


plot(df0$rpm,df0$pmean,
     xlim=c(0,300),
     ylim=c(0,6),
     type='o',
     col='red',
     pch=19,
     xlab="rpm",
     ylab="Relative power",
     cex.axis=1.3,
     cex.lab=1.3)
# lines(smoothingSpline,)
dev.off()

#Plot mean and max powers for each month, from 100 files
pdf('../figures/boxplots.pdf')
mypar(2,1)
df1<-read.csv('../results/powerVsYearRpm20/wave1hr_rpm20_powerMeanSummary.csv',header=FALSE, stringsAsFactors=FALSE)
df1$V13<-NULL
names(df1)<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
str(df1)
boxplot(100*df1, use.cols = TRUE,
        xlab="Month",
        ylab="Proportional to power",
        cex.axis=0.9,
        cex.lab=0.8)
# title(main="Monthly variation in mean and maximum power")

df2<-read.csv('../results/powerVsYearRpm20/wave1hr_rpm20_powerMaxSummary.csv',header=FALSE, stringsAsFactors=FALSE)
df2$V13<-NULL
names(df2)<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
str(df2)
boxplot(100*df2, use.cols = TRUE,
        xlab="Month",
        ylab="Proportional to power",
        cex.axis=0.9,
        cex.lab=0.8)
dev.off()

pdf("../figures/tpmcpm.pdf")
mypar(2,1)
library(plotrix)
#tpm and cpm matrices for SWH
tpm<-read.csv("../tpm/wave/wave_tpmSWH.csv")
cpm<-read.csv("../tpm/wave/wave_cpmSWH.csv")

color2D.matplot(tpm,xlab="next wave height",ylab="last wave height")
color2D.matplot(cpm,xlab="next wave height",ylab="last wave height")
dev.off()

#plot annual variation of power for rpm=20 and rpm=200
pdf('../figures/rpm20rpm200Power.pdf')
mypar(1,1)
df20<-read.csv("../results/powerVsRpm/wave1hr_001rpm20.csv",header=FALSE)
df200<-read.csv("../results/powerVsRpm/wave1hr_001rpm200.csv",header=FALSE)
df20$id<-seq(1,nrow(df20))
df200$id<-seq(1,nrow(df200))
names(df20)<-c("P","hour")
names(df200)<-c("P","hour")
plot(df20$hour,100*df20$P,type='l',col='blue',xlab="Time (hours)",ylab="Relative ower ")
lines(df200$hour,100*df200$P,type='l',col='red')
dev.off()


