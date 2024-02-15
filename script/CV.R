CV<-function(table,tab,pr,K){
  n=dim(table)[1]
  m=floor(pr*n)
  #Creo partizione
  training.samples=sort(sample(1:n,m))
  #Modello
  model=lm(data=table[training.samples,])
  #Calcolo errori
  response=table[-training.samples,1]
  predictions=predict(model,table[-training.samples,],)
  sq_err=mean((predictions-response)^2)
  abs_err=mean(abs(predictions-response))
  r2=summary(model)$adj.r.squared
  dv_std=var(predictions-response)
  A=AIC(model)
  #estrazioni casuali
  for (i in 1:K){
    #Creo partizione
    training.samples=sort(sample(1:n,m))
    #Modello
    model=lm(data=table[training.samples,])
    #Calcolo errori
    response=table[-training.samples,1]
    predictions=predict(model,table[-training.samples,],)
    sq_err=c(sq_err,mean((predictions-response)^2))
    abs_err=c(abs_err,mean(abs(predictions-response)))
    r2=c(r2,summary(model)$adj.r.squared)
    dv_std=c(dv_std,var(predictions-response))
    A=c(A,AIC(model))
  }
  #levo provincie del nord
  for (i in 1:5){
    #Creo partizione
    test.samples=sort(sample(1:47,n-m))
    #Modello
    model=lm(data=table[-test.samples,])
    #Calcolo errori
    response=table[test.samples,1]
    predictions=predict(model,table[test.samples,],)
    a=mean((predictions-response)^2)
    b=mean(abs(predictions-response))
    sq_err=c(sq_err,a)
    abs_err=c(abs_err,b)
    r2=c(r2,summary(model)$adj.r.squared)
    dv_std=c(dv_std,var(predictions-response))
    A=c(A,AIC(model))
  }
  #levo provincie del centro
  for (i in 1:3){
    #Creo partizione
    test.samples=sort(sample(c(1:47,74:n),n-m))
    #Modello
    model=lm(data=table[-test.samples,])
    #Calcolo errori
    response=table[test.samples,1]
    predictions=predict(model,table[test.samples,],)
    a=mean((predictions-response)^2)
    b=mean(abs(predictions-response))
    sq_err=c(sq_err,a)
    abs_err=c(abs_err,b)
    r2=c(r2,summary(model)$adj.r.squared)
    dv_std=c(dv_std,var(predictions-response))
    A=c(A,AIC(model))
  }
  #levo provinicie del sud
  for (i in 1:3){
    #Creo partizione
    test.samples=sort(sample(74:n,n-m))
    #Modello
    model=lm(data=table[-test.samples,])
    #Calcolo errori
    response=table[test.samples,1]
    predictions=predict(model,table[test.samples,],)
    a=mean((predictions-response)^2)
    b=mean(abs(predictions-response))
    sq_err=c(sq_err,a)
    abs_err=c(abs_err,b)
    r2=c(r2,summary(model)$adj.r.squared)
    dv_std=c(dv_std,var(predictions-response))
    A=c(A,AIC(model))
  }
  #partiziono per popolazione
  attach(tab)
  ord=rep(0,n)
  ord[1]=51
  for (i in 2:n){
    ord[i]=as.numeric(row.names(tab)[tab$popol==max(tab$popol[-ord[1:i-1]])])
  }
  detach(tab)
  for (i in 0:3){
    #Creo partizione
    test.samples=sort(ord[i*(n-m)+1:(i+1)*(n-m)])
    #Modello
    model=lm(data=table[-test.samples,])
    #Calcolo errori
    response=table[test.samples,1]
    predictions=predict(model,table[test.samples,],)
    sq_err=c(sq_err,mean((predictions-response)^2))
    abs_err=c(abs_err,mean(abs(predictions-response)))
    r2=c(r2,summary(model)$adj.r.squared)
    dv_std=c(dv_std,var(predictions-response))
    A=c(A,AIC(model))
  }
  #Unisco i risultati
  CrVa=cbind(sq_err,abs_err,dv_std,r2,A)
}
