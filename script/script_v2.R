rm(list=ls())
#######librerie usate
library(ggplot2)
library(car)
library(GGally)
library(MASS)
#######
tab=read.table('tabella_nuova',header=T)

View(tab)
dim(tab)
summary(tab)
print(sapply(tab, typeof))
P=c("X1","X2","X3","X4","X5","X6","X7","X8")
dev.new()
m <- mapply(tab[P], FUN=as.numeric)
barplot(colMeans(m),names.arg=c("PD","Lega",
                               "M5S","Forza Italia","Fratelli D'Italia",
                               "+Europa","EuropaVerde","LaSinistra"),
        col = c('#CC0000','#4c9900','#EEE728','skyblue','#004c99','grey','#66ff66','red'),
        main='Risultati Elezioni Europee 26/05/2019 in Italia')


attach(tab)
# Proviamo a creare un modello di regressione lineare con le covariate che abbiamo
g = lm( aste ~ disoccup  + uni + stranieri + redditobasso + disoccup + FasciaEta2 + FasciaEta3+ FasciaEta4 + FasciaEta5, data = tab)
summary( g )
#redditobasso poco significativa, la elimino

g = lm( aste ~disoccup  + uni + stranieri + disoccup + FasciaEta2 + FasciaEta3+ FasciaEta4 + FasciaEta5, data = tab)
gs=summary( g )
gs
A=AIC(g)
R2=gs$adj.r.squared
errstd=mean(gs$residuals)
#calculate the VIF for each predictor variable in the model
vif(g)
#vif>5 : FasciaEta2,FasciaEta3,FasciaEta4,FasciaEta5
## Elimino fascia eta2
g1 = lm( aste ~disoccup  + uni + stranieri + FasciaEta3+ FasciaEta4 + FasciaEta5, data = tab)
gs=summary( g1 )
gs
#FasciaEta3,4,5 poco significative
A=c(A,AIC(g1))
R2=c(R2,summary(g1)$adj.r.squared)
errstd=c(errstd,mean(gs$residuals))
## Elimino fascia eta 5
g2 = lm( aste ~disoccup  + uni + stranieri + FasciaEta2 + FasciaEta3+ FasciaEta4, data = tab)
gs=summary( g2 )#fascia et? poco significativa
A=c(A,AIC(g2))
R2=c(R2,summary(g2)$adj.r.squared)
errstd=c(errstd,mean(gs$residuals))
#confronto gli AIC
A #AIC del secondo modello ? pi? basso
#Provo ad usare un modello senza fasciaeta2,5
g3 = lm( aste ~uni + stranieri + disoccup + FasciaEta3, data = tab) # .fe4
gs=summary( g3 )
gs
vif(g3)
A=c(A,AIC(g3))
R2=c(R2,summary(g3)$adj.r.squared)
errstd=c(errstd,mean(gs$residuals))
###
#Analisi di outlier e punti di leva
Analisi=data.frame(A,R2,errstd)

res_std=g3$res/gs$sigma

plot( g3$fit, res_std, xlab = "Fitted", ylab = "Normalized residuals", 
      main = "Residuals vs Fitted Values", pch = 16 )

abline( h = 0, lwd = 2, lty = 2, col = 'red' )

watchout_ids_rstd = which( abs( res_std ) > 2) 
watchout_rstd = res_std[ watchout_ids_rstd ] 
watchout_rstd


X = model.matrix( g3 )
lev=hat(X)

watchout_points_lev = lev[ which( lev > 2 * 6/107 ) ]
watchout_ids_lev = seq_along( lev )[ which( lev > 2 * 6/107 ) ]

plot( g3$fitted.values, res_std, ylab = "Standardized Residuals", main = "Standardized Residuals");
abline( h = c(-2,2), lty = 2, col = 'orange' )
points( g3$fitted.values[watchout_ids_rstd],
        res_std[watchout_ids_rstd], col = 'red', pch = 16 )
points( g3$fitted.values[watchout_ids_lev],
        res_std[watchout_ids_lev], col = 'orange', pch = 16 )
legend('topright', col = c('red','orange'),
       c('Standardized Residuals', 'Leverages'), pch = rep( 16, 2 ), bty = 'n' )


## Gaussianità dei residui

qqnorm( g3$res, ylab = "Raw Residuals", pch = 16 )
qqline( g3$res ,col='red')

shapiro.test( g3$res )

#Provo a togliere nuoro alla posizione 95

tab2=tab[-95,]

g4 = lm( aste ~uni + stranieri + disoccup + FasciaEta3, data = tab2)
plot( g4$fitted.values, g4$residuals, ylab = "Standardized Residuals", main = "Standardized Residuals");

summary(g4)

#modello migliore



#### CROSS VALIDAZIONE

prova=CV(table,tab,7,20)

###

vittoria<-colnames(tab[P])[apply(tab[P],1,which.max)]
vittoria
gt=lm(aste~vittoria +Zone)
summary(gt)

shapiro.test(gt$residuals)

bc1=boxcox(gt)
lambda1 <- bc1$x[which.max(bc$y)]
lambda1=-2
mod1=lm(((aste^lambda1-1)/lambda1)~vittoria +Zone)

shapiro.test(mod1$residuals)

anova(mod1)


boxplot(aste ~ vittoria )





####

bc=boxcox(aste ~ disoccup + uni + FasciaEta4 + FasciaEta5, data = tab,lambda=c(-2,2))

lambda <- bc$x[which.max(bc$y)]

g1 <- lm(((aste^lambda-1)/lambda) ~ disoccup + uni + redditobasso + stranieri , data = tab)

par(mfrow=c(2,2))
plot(g1)

plot( g1$fit, g1$res, xlab = "Fitted", ylab = "Residuals", 
      main = "Residuals vs Fitted Values", pch = 16 )

abline( h = 0, lwd = 2, lty = 2, col = 'red' )

par(mfrow=c(1,1))

qqnorm( g1$res, ylab = "Raw Residuals", pch = 16 )
qqline( g1$res ,col='red')

shapiro.test( g1$res )
plot(disoccup,aste)
abline(g,col='red')

par(mfrow=c(1,1))

# osservo outlier

boxplot( aste ~ Zone, xlab = 'Nord-Centro-Sud', ylab = 'Percentuali astensionismo',
         main = 'Astensionismo in italia' )
abline( h = mean( aste ) ,col='red')


bc=boxcox(aste ~ Zone , data = tab)
lambda <- bc$x[which.max(bc$y)]

mod1 <- lm(((aste^lambda-1)/lambda) ~ Zone , data = tab)

qqnorm(mod1$res)
qqline(mod1$res,col='red')

shapiro.test(mod1$res)
summary(mod1)
anova( mod1 ) # rifiuto che le medie siano uguali

# domanda: posso accettare che le medie tra centro e nord siano uguali?????


mod1 <- lm(((aste[1:73]^lambda-1)/lambda) ~ Zone[1:73] , data = tab)
qqnorm(mod1$res)
qqline(mod1$res,col='red')

shapiro.test(mod1$res)
summary(mod1)
anova( mod1 ) # posso accettare che le medie siano unguali

dev.new()
boxplot( aste[1:73] ~ Zone[1:73], xlab = 'Nord-Centro-Sud', ylab = 'Percentuali astensionismo',
         main = 'Astensionismo in italia',
         col = c('#66C2A5','#FC8D62') )
mean(aste[which(Zone=='N')])
mean(aste[which(Zone=='C')])
mean(aste[which(Zone=='S')])
mean(aste[1:73])

# idee sparse

pairs(tab[which(popol<500000), c('aste','FasciaEta5')], pch = 16)

fasce=c('aste','FasciaEta2','FasciaEta3','FasciaEta4','FasciaEta5')

pairs(tab[which(Zone=='S'), c('aste','FasciaEta5')], pch = 16)

pairs(tab[which(Zone=='N'), c('aste','FasciaEta2')], pch = 16)


ggpairs(data = tab[,fasce], title ="Relationships between predictors & response",
        lower = list(continuous=wrap("points", alpha = 0.5, size=0.1)))

dev.new()

par(mfrow=c(2,2))
pairs(tab[which(Zone=='S'), c('aste','X1')], pch = 16)
pairs(tab[which(Zone=='N'), c('aste','X1')], pch = 16)


### altro


plot( ellipse( g, c( 3, 4 ) ), type = "l", xlim = c( -10, 2 ) ,ylim=c(-1,1))
points( 0, 0 )
points( g$coef[ 3 ] , g$coef[ 4 ] , pch = 18 )


#esempio di correlazione
pairs(tab[ , c('stranieri','denunce_su_abitante')], pch = 16)


pairs(tab[ , c('aste','FasciaEta2','FasciaEta3','FasciaEta4')], pch = 16)
x11()
ggpairs(tab[ , c('aste','FasciaEta2','FasciaEta3','FasciaEta4')])





