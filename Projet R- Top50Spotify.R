spotify <- read.table(file="C:/Users/PC/Desktop/R-ENSC/spotify.txt",
                      sep = ',',
                      header=TRUE
                      )
head(spotify)
str(spotify)

# On transforme le type de la variable "Table" en valeur num�rique
spotify <- transform(spotify, Table = as.numeric(Table))
summary(spotify)
attach(spotify)

boxplot(Popularity)
title("Popularit�")

boxplot(Beats.Per.Minute)
title("Battements par minute")

HistoPerso <- function(x,from,to,title,xlab,ylab){
  #affichage de l'histogramme de x entre "from" et "to" par pas de "by"
  hist(x,prob=TRUE,breaks=seq(from,to,by=(to-from)/7),main=title,xlab=xlab,ylab=ylab)
  #affichage de l'estimation � noyau de la densit� avec largeur de fen�tre width
  lines(density(x,width=2*(summary(x)[5]-summary(x)[2])), xlim=c(min(x)-sd(x),max(x)+sd(x)))
} 
# afficher 4 graphiques sur une m�me fen�tre 
par(mfrow=c(2,2)) 

HistoPerso(Popularity,70,95,"Histogramme de la popularit� ","Popularit�","Densit�")
HistoPerso(Beats.Per.Minute,85,190,"Histogramme  des BPM","BPM","Densit�")
HistoPerso(Danceability,29,90,"Histogramme du caract�re dansant ","Caract�re dansant","Densit�")
HistoPerso(Acousticness..,1,75,"Histogramme de l'acoustique","Acoustique","Densit�")


#On va chercher � �tablir qu'elle sont les liens entre la position (plus ou moins �lev�e ) 
#d'un morceau dansle top 50 et les autres parm�tres. Pour cela on d�finit cette fonction permettant
#de repr�senter un param�tre en fonction de l'autre et ainsi trouver une potentielle corr�lation.
LinReg <- function(x,y,xlab,ylab){
  cor = cor.test(x,y, method="pearson")
  reg <- lm(y ~ x)  
  coef = coefficients(reg)
  
  plot(x,y, ylab=ylab, xlab=xlab)
  abline(reg, col='red', lwd=2)
  title(paste("Coef:",round(cor(x,y),2)," | ",
              ylab,"=", round(coef[1],0),
              "+",xlab,"*",round(coef[2],0)))
} # Calcul du coef de corr�lation, trace un
# graphique avec une approximation lin�aire.
LinReg(id,Popularity,"Place dans le Top 50","Popularit�")

cor=cor.test(id,Popularity, method="pearson")
#Coefficient de corr�lation
cor(id,Popularity)

LinReg(id,Beats.Per.Minute,"Place dans le Top 50","BPM")
cor=cor.test(id,Beats.Per.Minute, method="pearson")
#Coefficient de corr�lation
cor(id,Beats.Per.Minute)

LinReg(id,Length.,"Place dans le Top 50","Carct�re dansant")
cor=cor.test(id,Length., method="pearson")
#Coefficient de corr�lation
cor(id,Length.)

chisq.test(Danceability,Popularity)

shapiro.test(Popularity)

#diamond <- transform(diamond, Table = as.numeric(Table))

# On charge le plugin PCAmixdata, qui permet de r�aliser une ACP
require(PCAmixdata)

# On stocke le r�sultats de l'ACP dans la variable res.
# X.quanti : Variables Quantitatives  X.quali : Variables Qualitatives 
res<-PCAmix(X.quanti=spotify[c(1,5,6,7,8,9,10,11,12,13,14 )],X.quali = spotify[, c(2,3,4 )],graph=FALSE)
                                                  
# On affiche toutes les valeurs propres de la diagonalisation de la matrice de nos variables.
round(res$eig,digit=2)

# Affiche la qualit� de repr�sentation
# des variables sur les dimensions. 
round(res$quanti$cos2,digit=3)

par(mfrow=c(2,2)) 
plot(res,axes=c(1,2),choice="cor")
plot(res,axes=c(1,3),choice="cor")
plot(res,axes=c(1,4),choice="cor")
plot(res,axes=c(1,5),choice="cor")


plot(res,axes=c(1,2),choice="ind", 
     coloring.ind = spotify$Genre, label=FALSE)
plot(res,axes=c(1,3),choice="ind", 
     coloring.ind = spotify$Genre, label=FALSE)


