#Plot mean powers for each month, from 100 files

df<-read.csv('../results/powerVsYearRpm20/wave1hr_rpm20_powerSummary.csv',header=FALSE, stringsAsFactors=FALSE)
df$V13<-NULL
names(df)<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
str(df)
boxplot(df, use.cols = TRUE,xlab="Month",ylab="Proportional to power")