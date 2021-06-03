
dat1<-read.csv('스트레스인지율2017.csv')
te2<-as.character(dat1[,1])

te3.busan<-strsplit(te2,'부산')

te3.ulsan<-strsplit(te2,'울산')

te3.daejeon<-strsplit(te2,'대전')

te3.incheon<-strsplit(te2,'인천')

te3.daegu<-strsplit(te2,'대구')

te3.kwangju<-strsplit(te2,'광주')

busan.eval<-NA;ulsan.eval<-NA;daejeon.eval<-NA;incheon.eval<-NA;daegu.eval<-NA;kwangju.eval<-NA

for ( jj in 1:nrow(dat1)){
  print(jj)
  te4<-te3.busan[[jj]]
  busan.eval[jj]<-ifelse(te4[1]=='',TRUE,FALSE)
  te4<-te3.ulsan[[jj]]
  ulsan.eval[jj]<-ifelse(te4[1]=='',TRUE,FALSE)
  te4<-te3.daejeon[[jj]]
  daejeon.eval[jj]<-ifelse(te4[1]=='',TRUE,FALSE)
  te4<-te3.incheon[[jj]]
  incheon.eval[jj]<-ifelse(te4[1]=='',TRUE,FALSE)
  te4<-te3.daegu[[jj]]
  daegu.eval[jj]<-ifelse(te4[1]=='',TRUE,FALSE)
  te4<-te3.kwangju[[jj]]
  kwangju.eval[jj]<-ifelse(te4[1]=='',TRUE,FALSE)
}


dat1$busan.eval<-busan.eval
dat1$ulsan.eval<-ulsan.eval
dat1$daejeon.eval<-daejeon.eval
dat1$incheon.eval<-incheon.eval
dat1$daegu.eval<-daegu.eval
dat1$kwangju.eval<-kwangju.eval




summary(dat1)


dat1$loc<-ifelse(dat1$busan.eval==1,'0.busan',
                 ifelse(dat1$ulsan.eval==1,'1.ulsan',
                        ifelse(dat1$daejeon.eval==1, '2.daejoen',
                               ifelse(dat1$incheon.eval==1, '3.incheon',
                                      ifelse(dat1$daegu.eval==1, '4.daegu',
                                             ifelse(dat1$kwangju.eval==1,'5.kwangju','6.others'))))))



dat1$loc<-as.factor(as.character(dat1$loc))


anova(out<-lm(스트레스인지율~loc,dat1))
with(dat1,boxplot(스트레스인지율~loc,col=2:7))





kruskal.test(스트레스인지율~loc,dat1)


library(multcomp)

tukey<-glht(out,linfct=mcp(loc='Tukey'))
summary(tukey)
plot(tukey)

