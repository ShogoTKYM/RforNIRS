AUCLVSA<-AUCL-AUCPLO
PLASALV<-PainLevelPL-PainLevelPLO
PLASAAVLV<-rowMeans(PLASALV)

AUCPMSA<-AUCPM-AUCPMO
PLASAPM<-PainLevelPM-PainLevelPMO
PLASAAVPM<-rowMeans(PLASAPM)

AUCSAALL<-rbind(AUCLVSA,AUCPMSA)

PLASAALL<-rbind(PLASALV,PLASAPM)
PLASAAVALL<-rowMeans(PLASAALL)



spmrt<-matrix(0,nrow=1, ncol=22)

for(i in 1:22){
  spmrt[1,i]<-cor.test(
    x=AUCSAALL[,i],
    y=PLASAAVALL,
    method = "spearman",
    )$p.value
}

write.csv(spmrt,"spearman result.csv")