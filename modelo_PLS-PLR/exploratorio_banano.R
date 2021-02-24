#genera modelo que será consdierado en gráfico biplot
#incluye kmedias #final 
#graba en archivo resultados de modelamiento con 102 individuos con colores
# agreda un dataset adicional (entregado por Ronald Criollo) para predicción. 
# lo estandariza 
 
library(dplyr) 

library(MultBiplotR)
source(file = "D:/ugarte/R/programas/tesis/R/PLSRBinary.R")
source(file = "D:Ugarte/R/programas/tesis/R/RidgeBinaryLogistic.R")
source(file = "D:Ugarte/R/programas/tesis/R/RidgeBinaryLogisticFit.R")

#normaliza=function(x) {
#snv=(x-mean(x))/sd(x) 
#return(snv)}

#normaliza=function(x) {
#snv=(x-med[x])/desv[x]) 
#return(snv)}

col_names=read.table("D:/ugarte/datos/datos ban 03 2019/col_names.csv",header = FALSE, dec="." ,  sep = ",")

col_names2 = as.character(col_names)


#son 104 

#Lee datos
datos=read.table("D:/ugarte/datos/datos ban 03 2019/datos032019.csv",header = FALSE, dec="." ,  sep = ",")

datos1= filter(datos, datos$V7!=1)
malas = c(9,26,40,41,45) # imágenes con errores

datos2 = datos1[-malas,]

nfila_d2=dim(datos2)[1]
ncolum_d2=dim(datos2)[2]

Xcal= datos2[,8:527]
Ycal = datos2[,3]
Xcal_a=scale(Xcal,center = TRUE,scale=TRUE)

colnames(Xcal_a) <- col_names2

## prueba multicolinealidad
#library(lme4)
## kappa, aka condition number.
## kappa < 10 is reasonable collinearity,
## kappa < 30 is moderate collinearity,
## kappa >= 30 is troubling collinearity
#kappa.mer(m.natural) # 12.53
#kappa.mer(m6) # 1.00, properly centered

## variance inflation factor, aka VIF
## values over 5 are troubling.
## should probably investigate anything over 2.5.
#max(vif.mer(m.natural)) # 14.47
#max(vif.mer(m.orthogonal)) # 1

#calcula  VIF
library(usdm)
df_Xcal = as.data.frame(Xcal_a)
vif_Xcal= vif(df_Xcal)

# graba archivo vif
write.table((vif_Xcal), file = "D:/ugarte/datos/datos ban 03 2019/vif_Xcal.txt", sep=" ", append=FALSE, col.names=TRUE, row.names=FALSE, dec=".", quote= FALSE)


library(MASS)
cor_Xcal= cor(Xcal_a)

det(cor_Xcal)


# graba archivo corr
write.table((cor_Xcal), file = "D:/ugarte/datos/datos ban 03 2019/cor_Xcal.txt", sep=" ", append=FALSE, col.names=TRUE, row.names=TRUE, dec=".", quote= FALSE)




med<-apply(Xcal,2,mean)
desv<-apply(Xcal,2,sd)



#Xcal= apply(Xcal,2,normaliza)

sev_train= datos2[,2]



#datos nuevos

datos_add=read.table("D:/ugarte/datos/datos ban 03 2019 adicionales/datos_add_03_2019.csv",,header = FALSE, dec="." ,  sep = ",")

total_add <-datos_add %>% group_by(V2) %>% summarize(n=n())

nfila_add=dim(datos_add)[1]
ncolum_add=dim(datos_add)[2]

Xpred= datos_add[,8:527]
Ypred = datos_add[,3]

Xpred= apply(Xpred,2,normaliza)


#Xpred=scale(Xpred,center = TRUE,scale=TRUE)

sev_test = datos_add[,2]




lambda=0.1	
comp=2
PLSban=PLSR1BinFit(Ycal, Xcal,S=2, penalization = lambda)


df.X=PLSban$ScaledX
df.b=as.data.frame(PLSban$YWeights)
df.u = as.data.frame(PLSban$XScores) #scores
df.v <- as.data.frame(PLSban$XLoadings)#direcciones
df.w= as.data.frame(PLSban$XWeights)

names(df.u) <- c('x1var', 'x2var')
names(df.v) <- c('x1var', 'x2var')

# calcula contribuciones
A=as.matrix(df.u)
B=as.matrix(df.v)
Cont=CalculateContributions(df.X,A,B)
RowCon=Cont$RowContributions
ColCon=Cont$ColContributions
TColCon=apply(ColCon,2,sum)

Fit=as.data.frame(Cont$Fit)
CorXA=Cont$Structure



#K-means clustering #####################

set.seed(20)
clusters <- kmeans(df.u, 3)
df.u$grupo <- clusters$cluster

# agrega columna train/test 1= train 2= test

tr_ts=c(rep.int(1,nfila_d2)) #training = 1

df.u$tr_ts <- tr_ts

# agrega respuesta y nivel de severidad
df.u$y <- Ycal

df.u$sev <- sev_train

# tabla grupos

grupo = c(1,2,3)
nam_grp= c('Sanas', 'Enfermas1', 'Enfermas2')
t_grupos= as.data.frame(nam_grp)
t_grupos=cbind(grupo,t_grupos)

#tabla train_test tr_ts
tr_ts = c(1,2)
nam_tr_ts= c('train', 'test')
t_tr_ts= as.data.frame(nam_tr_ts)
t_tr_ts=cbind(tr_ts,t_tr_ts)



## completa df.v
var_name=rownames(df.v) 
#vgrupos=c(rep("gold1",12),rep("lightsteelblue2",100),rep("skyblue2",100),rep("dodgerblue2",116),rep("lightsalmon1",60),rep("salmon2",60),rep("tomato1",72))
vgrupos=c(rep("violet",36),rep("blue",41),rep("dodgerblue",18),rep("green1",61),rep("yellow",9),rep("orange",31),rep("tomato2",133), rep("grey50",191))
vbanda=c(rep(1,12), rep(2,316), rep(3,192))

v= as.matrix(df.v, ncol=2) 
#v2=v%*%t(v)
#v=(df.v/v2)/50 
v=as.data.frame(v)
v=cbind(v,vgrupos)
v=cbind(v,vbanda)

#tabla banda

vbanda = c(1,2,3)
nam_banda= c('Ultravioleta', 'Visible Spectrum','Near Infrared')
t_banda= as.data.frame(nam_banda)
t_banda=cbind(vbanda,t_banda)


################
#predicción

B=df.b
B=as.matrix(B)

	
W=df.w
P1=df.v

Xpred=as.matrix(Xpred)
Ypred = as.matrix(Ypred)


S=2
result=list()
I1=dim(Xpred)[1] 
J=dim(Xpred)[2]   
  
if (is.numeric(Ypred)) {Ypred= as.matrix(Ypred)}
I2=dim(Ypred)[1]
K=dim(Ypred)[2]
     
  
if (!(I1==I2)) stop('The number of rows of both matrices must be the same')
I=I1

Xp = Xpred
T=matrix(0, I, S) # #filas #componentes
C=matrix(0, K, S) # columnas de Y #componentes
#P1=matrix(0, J, S) # Columnas de X #componentes
Q=matrix(0, K, S) #  Columnas de Y #componentes
freq=matrix(1,I,1) # 

Xp=matrix(Xp,nrow=I)#arregado con matrix

X1=Xp #inicializa de X1
i=1
w=matrix(W[,1],nrow=J)

t=X1 %*% w # calcula ts con W calibración
 
T[,1]=t
#p=t(Xp) %*% t/ sum(t^2) #calcula p`
#P[,1]=p
p=P1[,1]
X1=X1-t %*% t(p) #calcula residuo
 
i=2 
t=X1 %*% W[,2] # calcula ts con W calibración
 
T[,2]=t
#p=t(Xp) %*% t/ sum(t^2) #calcula pn
#P[,2]=p
p=P1[,2]
X1=X1-t %*% t(p) #calcula residuo

## completa T
unos=matrix(1,I,1)
T=cbind(unos,T)

T= as.matrix(T)


G = T %*%(B)# +const # regresion funcion enlace + constante(revisar)

Px = (exp(G)/(1 + exp(G)))# p(x)
residuals = Ypred - Px
Prediction = as.numeric(Px > 0.5)# discrimina prob >0.5 = 1

#agrega filas a df.u



m_pred = cbind(T[,2:3], rep.int(4,nfila_add), rep.int(2,nfila_add), Ypred,rep.int(4,nfila_add))

colnames(m_pred) <- colnames(df.u)

df.u = rbind(df.u,m_pred)

###### graba información para gráfico predicción

ProbY_pred =cbind(Px, Prediction, Ypred, datos_add[,2])
colnames(ProbY_pred) = c("Px","Pr","y", "sev")

#plot(1:102,ProbY,col=Yin+3,pch=16, xlab='leaf', ylab='Prob. Y')
#title('Prediction Y (CV)')

# graba archivo para predicción de nuevas
write.table((ProbY_pred), file = "D:/ugarte/R/programas/ggbiplot/ggbiplotBI/ProbY_pred.txt", sep=" ", append=FALSE, col.names=TRUE, row.names=FALSE, dec=",", quote= FALSE)


#################
#Xscores
write.table((df.u), file = "D:/ugarte/R/programas/ggbiplot/ggbiplotBI/df.u.agr.txt", sep=" ", append=FALSE, col.names=TRUE, row.names=FALSE, dec=",", quote= FALSE)
#XLoadings
write.table((v), file = "D:/ugarte/R/programas/ggbiplot/ggbiplotBI/v.agr.txt", sep=" ",append=FALSE, col.names=TRUE, row.names=FALSE, dec=",", quote= FALSE)
#YWeigths; coeficientes beta
write.table((df.b), file = "D:/ugarte/R/programas/ggbiplot/ggbiplotBI/df.b.agr.txt", sep=" ",append=FALSE, col.names=TRUE, row.names=FALSE, dec=",", quote= FALSE)
#Contributions
write.table((Fit), file = "D:/ugarte/R/programas/ggbiplot/ggbiplotBI/Fit.agr.txt", sep=" ",append=FALSE, col.names=TRUE,row.names=FALSE, dec=",", quote= FALSE)

#grupos
write.table((t_grupos), file = "D:/ugarte/R/programas/ggbiplot/ggbiplotBI/t_grupos.agr.txt", sep=" ", append=FALSE, col.names=TRUE,row.names=FALSE, quote= FALSE)

#train o test
write.table((t_tr_ts), file = "D:/ugarte/R/programas/ggbiplot/ggbiplotBI/t_tr_ts.agr.txt", sep=" ", append=FALSE, col.names=TRUE,row.names=FALSE, quote= FALSE)

#banda
write.table((t_banda), file = "D:/ugarte/R/programas/ggbiplot/ggbiplotBI/t_banda.agr.txt", sep=" ",append=FALSE, col.names=TRUE,row.names=FALSE, quote= TRUE)



#df.u=read.table("d:/ugarte/R/programas/ggbiplot/ggbiplotBI/df.u.txt")










remove(df.u)
remove(v)
remove(df.b)
remove(Fit)
remove(t_grupos)
