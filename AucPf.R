rawdata1 <- read.csv(".csv",header=T,skip=40,nrows=2710)
rawdata2 <- read.csv(".csv",header=T,skip=40,nrows=2710)
rawdata3 <- read.csv(".csv",header=T,skip=40,nrows=2710)
name<-"Pf"


library("signal")
fil_N <- 500 
fs <- 10
fn <- fs/2
fc <- c(0.01,1)
fc_norm <- fc/fn
fir_filter <- fir1(fil_N, fc_norm, type="pass")

firProcessed1<-matrix(0,2710,22)
firProcessed2<-matrix(0,2710,22)
firProcessed3<-matrix(0,2710,22)


for(i in 1:22){
  firProcessed1[,i] <- filtfilt(fir_filter,rawdata1[ ,i+1])
}
for(i in 1:22){
  firProcessed2[,i] <- filtfilt(fir_filter,rawdata2[ ,i+1])
}
for(i in 1:22){
  firProcessed3[,i] <- filtfilt(fir_filter,rawdata3[ ,i+1])
}


library("stats")
addAverage <- ( firProcessed1 + firProcessed2 + firProcessed3 )/3
MovAverage<-stats::filter(addAverage,rep(1,10),sides=1)/10
Average<-MovAverage[-1:-9,]


AUCfname<-paste("AUCf",name,sep="")
AUCsname<-paste("AUCs",name,sep="")
AUCf<-matrix(0,nrow=1, ncol=22)
AUCs<-matrix(0,nrow=1, ncol=22)

for(i in 1:22){
  AUCf[1,i]<- sum(Average[601:1200,i])/10
  }
for(i in 1:22){
  AUCs[1,i]<- sum(Average[2101:2700,i])/10
  }

assign(name,Average)
assign(AUCfname,AUCf)
assign(AUCsname,AUCs)

AUCfnameCsv<-paste("AucPO",name,".csv",sep="")
AUCsnameCsv<-paste("AucPA",name,".csv",sep="")

write.csv(AUCf,file=AUCfnameCsv)
write.csv(AUCs,file=AUCsnameCsv)
