rm(list=ls())
######librerie usate
library(ggplot2)
library(car)
library(GGally)
library(ggplot2)
####
tab=read.table('tabella_nuova',header=T)

View(tab)
dim(tab)
summary(tab)
print(sapply(tab, typeof))
P=c("X1","X2","X3","X4","X5","X6","X7","X8")
dev.new()
m <- mapply(tab[P], FUN=as.numeric)
barplot(colMeans(m),names.arg=c("PD","Lega",
                                "M5S","ForzaItalia","FratelliItalia",
                                "+europa","europaverde","lasinistra"),col = c('red','green','yellow','skyblue','blue','grey','green','red'))
# calcolo della densità di popolazione
dens=km_sup/km_popol
tab=cbind(tab,dens)

#esempio di correlazione
pairs(tab[ , c('stranieri','denunce_su_abitante')], pch = 16)


pairs(tab[ , c('aste','FasciaEta2','FasciaEta3','FasciaEta4')], pch = 16)
x11()
ggpairs(tab[ , c('aste','FasciaEta2','FasciaEta3','FasciaEta4')])

attach(tab)
pairs(tab[,c('aste','uni','stranieri','redditobasso','FasciaEta2','FasciaEta4')])
ggpairs(tab[,c('aste','uni','stranieri','redditobasso','FasciaEta2','FasciaEta4')])
g = lm( aste ~ disoccup  + uni + stranieri + redditobasso + FasciaEta2 + FasciaEta3+ FasciaEta4 + FasciaEta5, data = tab)
summary( g )
vif(g)
AIC(g)
######
g = lm( aste ~ disoccup  + uni + stranieri + redditobasso + FasciaEta2 + FasciaEta3, data = tab)
summary(g)
vif(g)
AIC(g)
plot(summary(g)$residuals)
abline(h=0,lwd=2,col='red')
shapiro.test(summary(g)$residuals)
res2=(summary(g)$residuals-mean(summary(g)$residuals))/sd(summary(g)$residuals)
plot(res2)
abline(h=2,lwd=2,col='red')
abline(h=-2,lwd=2,col='blue')
res_new=summary(g)$residuals[abs(res2)<2]
res_new_st=(res_new-mean(summary(g)$residuals))/sd(summary(g)$residuals)
plot(res_new_st)
abline(h=2,lwd=2,col='red')
abline(h=-2,lwd=2,col='blue')
shapiro.test(res_new_st)
tab2=tab[,c('aste','disoccup','uni','stranieri','redditobasso','FasciaEta2','FasciaEta3')]
CrVa=CV(tab2,tab,0.8,20)
plot(CrVa[,1])
mean(CrVa[,1])
######
g = lm( aste ~ disoccup  + uni + stranieri + FasciaEta2 + FasciaEta3, data = tab)
summary(g)
vif(g)
AIC(g)
plot(summary(g)$residuals)
abline(h=0,lwd=2,col='red')
shapiro.test(summary(g)$residuals)
res2=(summary(g)$residuals-mean(summary(g)$residuals))/sd(summary(g)$residuals)
plot(res2)
abline(h=2,lwd=2,col='red')
abline(h=-2,lwd=2,col='blue')
res_new=summary(g)$residuals[abs(res2)<2]
res_new_st=(res_new-mean(summary(g)$residuals))/sd(summary(g)$residuals)
plot(res_new_st)
abline(h=2,lwd=2,col='red')
abline(h=-2,lwd=2,col='blue')
shapiro.test(res_new_st)
tab2=tab[,c('aste','disoccup','uni','stranieri','redditobasso','FasciaEta2','FasciaEta3')]
CrVa=CV(tab2,tab,0.8,20)
plot(CrVa[,1])
mean(CrVa[,1])