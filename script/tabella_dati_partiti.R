###Costruisco tabella con dati per partito
vinc=rep('X1',dim(tab)[1])
for (i in 1:dim(tab)[1]){
  vinc[i]=P[tab[i,P]==max(tab[i,P])]
}
View(vinc)
summary(vinc)
tab=cbind(tab,vinc)
attach(tab)
vinc
k=0
for (i in 4:length(P)){
  k=k+sum(vinc==P[i])
}
k
# In nessuna provincia ha vinto un partito che non sia PD,Lega o M5S
part=c('X1','X2','X3')
Pred=c('uni','aste','stranieri','redditobasso','disoccup')
m=rep(0,length(Pred))
for (i in 1:3){
  mn=rep(0,3,length(Pred))
  for (j in 1:length(Pred)){
    mn[j]=weighted.mean(tab[vinc==part[i],Pred[j]],popol[vinc==part[i]])
  }
  m=cbind(m,mn)
}
m=m[,2:4]
row.names(m)=Pred
m=t(m)
row.names(m)=part
zone=rep(0,4)
for (i in 1:3){
  ztot=sum(vinc==part[i])
  zn=sum(Zone[vinc==part[i]]=='N')
  zc=sum(Zone[vinc==part[i]]=='C')
  zs=sum(Zone[vinc==part[i]]=='S')
  zone=cbind(zone,c(ztot,zn,zc,zs))
}
zone
zone=zone[,2:4]
row.names(zone)=c('Tot','Nord','Centro','Sud')
zone=t(zone)
View(zone)
m=cbind(m,zone)
m=t(m)
m=t(m)
tabella_dati_partiti=m
rm(m)
View(tabella_dati_partiti)
