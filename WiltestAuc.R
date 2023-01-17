POAUCs<-read.csv(".csv",header=T,row.names=1,nrows=22)
PAAUCs<-read.csv(".csv",header=T,row.names=1,nrows=22)
wilrt<-matrix(0,nrow=1, ncol=22)

for(i in 1:22){
  wilrt[1,i]<-wilcox.test(
    x=unlist(POAUCs[,i]),
    y=unlist(PAAUCs[,i]),
    paired=T)$p.value
}

write.csv(wilrt,"Wilcox result.csv")