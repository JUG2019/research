 
#graba archivo para gráfico hsbiplot banano colores 104 ( no incluye test)
# elimina imágenes con error en máscara 
# egalley

#install.packages("tidyverse")
#library(tidyverse)
#library(extrafont)
#loadfonts(device = "win")
#font_import()
library(ggplot2)
#library(ggforce)
#library(ggthemes)
library(dplyr)
library(scales)
library(grid)
#library(devtools)
 
library(showtext)

df.u=read.table("D:/ugarte/R/programas/ggbiplot/ggbiplotBI/df.u.masc.txt",header =TRUE, dec="," , sep = " ")
v=read.table("D:/ugarte/R/programas/ggbiplot/ggbiplotBI/v.masc.txt",header = TRUE, dec=",", sep = " ")
df.b=read.table("D:/ugarte/R/programas/ggbiplot/ggbiplotBI/df.b.masc.txt",header = TRUE, dec="," , sep = " ")
Fit=read.table("D:/ugarte/R/programas/ggbiplot/ggbiplotBI/Fit.masc.txt",header = TRUE, dec="," , sep = " ")
t_grupos=read.table("D:/ugarte/R/programas/ggbiplot/ggbiplotBI/t_grupos.masc.txt", header = TRUE, dec="," , sep = " ")
t_banda=read.table("D:/ugarte/R/programas/ggbiplot/ggbiplotBI/t_banda.masc.txt", header = TRUE, dec="," , sep = " ")
t_tr_ts=read.table("D:/ugarte/R/programas/ggbiplot/ggbiplotBI/t_tr_ts.masc.txt",header = TRUE, dec="," ,  sep = " ")
 
y = df.u$y  

#contributions
Fit1=round(Fit[1,1]*100, digits=2)
Fit2=round(Fit[2,1]*100, digits=2)


#names

names(df.u) <- c('x1var', 'x2var')
names(df.v) <- names(df.u)
names(df.b) <- "Beta"
rownames(df.b) <- c('const','Comp 1', 'Comp 2')
names(Fit) <- "Percentage"
rownames(Fit) <- c('x1var', 'x2var')
choices = 1:2
u.axis.labs <- paste('PLS component ', choices, sep=' ')



#K-means clustering #####################

set.seed(20)
clusters <- kmeans(df.u, 3)
df.u$groups <- clusters$cluster

clusters <- df.u$grupo

nsev <- df.u$sev

#Agrega numero de hojas
numhoja= seq(1:length(y)) # se numeran 

windowsFonts(Times=windowsFont("TT Times New Roman"))

# Base plot
  
g <- ggplot(data = df.u, aes(x = x1var, y = x2var, color=factor(nsev),label=numhoja )) + xlab(u.axis.labs[1]) + ylab(u.axis.labs[2]) + 

     labs(colour="Banana\nleaves") + coord_fixed(xlim = c(-60, 40), ylim = c(-30, 30))+
    scale_colour_manual(label= c( "Healthy", "Pre-symptomatic", "Severity 1", "Severity 2"),values = c("green4","turquoise1","blue4","red3"))
   g=g + guides(shape=FALSE)

g<-g+theme( title=element_text(size=16,face="bold", family="times"),
         plot.title=element_text(hjust=0.5),
         axis.text=element_text(size=10),
        axis.title=element_text(size=12, hjust=0.5, family="times"),
         legend.text = element_text( size = 13),
	   legend.title =element_text( size = 14))
   
# show varibles 

g <- g +   geom_abline(intercept=0, slope=v[,2]/v[,1], size=0.25,color=v$vgrupos, alpha=2/3)
   
# ejes
   g= g+geom_hline(yintercept=0, color = "black", size=1, linetype="dashed")+
       geom_vline(xintercept = 0, color = "black", size=1, linetype="dashed")

# puntos
  g= g+ geom_point(alpha=1, aes(shape=factor(y)), size=3, show.legend = TRUE)#+
  g= g+ geom_text(aes(label=ifelse(numhoja>13&numhoja<22,as.character(numhoja),'')),hjust=-2,vjust=0)

# Draw linea prob =0.5 
   g <- g + 
      geom_abline(intercept=(-df.b[1,])/df.b[3,], slope=-(df.b[2,]/df.b[3,])
              , color='black', size=2, linetype="dotted") +
       annotate("text", label = "Px = 0.5" , x = -17, y = 5  , size = 4,fontface="bold", colour = "black" )

# dibuja elipses #########################  

g<- g+ stat_ellipse(data=df.u[,1:2],level = 0.99, size=0.7)

# texto spectro
     g<-g + annotate("text", x = -50, y = 20, label = "VISIBLESPECTRUM 380-780nm", colour = "black",angle=0,fontface="bold",size=4)+
            annotate("text", x = 8, y = 25, label = "NEARINFRARED 780-1350nm",colour = "black",angle=0,fontface="bold", size=4)

print(g)


ggsave("D:/ugarte/usal/RIP/estudio doctoral/ACADEMICO USAL/resumenes/hsbiplotssinvar2.png", dpi = 600, device ="png")

