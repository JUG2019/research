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

directorio="c:\\datos/cubosSNV"
subdirs=paste(directorio,dir(directorio),sep="/")
#directorio="d:\\datos/Hojas"
#subdirs=paste(directorio,dir(directorio),sep="/")

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
  lista=c(lista,as.vector(rbind(m,d,sm,kr,m5)))# lista 
}
X92=matrix(lista,nrow=length(subdirs),byrow=TRUE)
write(t(X104), file = "c:/datos/"d:/ugarte/datos/datos npls/datanplsb.csv", ncolumns=2600,append=FALSE)

