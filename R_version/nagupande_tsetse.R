library("plyr")
#*************************TSETSE DATA***************************************
tsetse <- read.csv("nagupande_tsetse_R.csv") # read in dataset

par(mfcol=c(2,1),mai=c(1,1,0,0),cex=0.8)
# plot of numbers of mammals shot by month of the study
plot(1:72, tsetse$flyround,pch=19,col="blue",bty="n", cex.main=0.8,log="y",
     xlab="Month of study",ylab="Numbers of tsetse")
abline(v=10)
#*************************TEMP DATA*********************************

temp <- read.csv("lusulu_temperature_R.csv") # read in dataset
month.means <- ddply(temp,.(),transform,meant=(ctmin+ctmax)/2)
month.means2 <- ddply(month.means,.(year,month),summarise,meantemp=mean(meant))
month.means <- month.means2[1:12,]

plot(1:72,rep(month.means[,3],6),bty="n",ylim=c(10,40),
     xlab="Month of study",ylab="Mean monthly temp",pch=19,col="red")
