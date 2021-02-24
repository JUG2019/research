library(raster)

simetria=function(x) {
m3=mean((x-mean(x))^3)
return(m3)}


kurtosis=function(x) {
m4=mean((x-mean(x))^4) 
return(m4)}

momento5=function(x) {
m5=mean((x-mean(x))^5) 
return(m5)}

directorio="d:\\datos/cubos snv"
subdirs=paste(directorio,dir(directorio),sep="/")
lista=numeric(0)
for(subd in subdirs){
  archivos=paste(subd,dir(subd),sep="/")
  fotos=stack(archivos)
  matriz<-as.matrix(fotos)
  m<-apply(matriz,2,mean)
  d<-apply(matriz,2,sd)
  sm<-apply(matriz,2,simetria)
  kr<-apply(matriz,2,kurtosis)
  m5<-apply(matriz,2,momento5)
  #crear matriz2, filas:características, col=long de onda
  lista=c(lista,as.vector(rbind(m,d,sm,kr,m5)))
}
X=matrix(lista,nrow=length(subdirs),byrow=TRUE)
y=c(1,1,1,1,1,0)
#iteración 1
Z=matrix(crossprod(X,y),nrow=5)# crossprod es producto transpuesta X por y
a=svd(Z)
wj=a$u[,1]
wk=a$v[,1]
T=X %*% (wk %x% wj)#producto de X*(wk Kronecher wj)
T
plot(1:48,T,col=y+1)

b<-(solve(t(T)%*%T)%*%t(T))%*%y

y<-y-T%*%b
X<-X-(T%*%wj)%*%t(wk)

#iteración 2

Z=matrix(crossprod(X,y),nrow=5)# crossprod es producto transpuesta X por y
a=svd(Z)
wj=a$u[,1]
wk=a$v[,1]
T=X %*% (wk %x% wj)#producto de X*(wk Kronecher wj)
T
plot(1:6,T,col=y+1,)

b<-(solve(crossprod(T,T))%*%t(T))%*%y

y<-y-T%*%b
X<-X-(T%*%wj)%*%t(wk)


