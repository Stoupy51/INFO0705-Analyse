
#vider la mémoire
rm(list=ls())

# Chargement des données "data_analysis.csv"
players <- read.csv(file.choose(), row.names=1, header=TRUE, stringsAsFactors = TRUE)
print(summary(players))
str(players)

#fonction pour centrage-réduction
CR <- function(x){
  n <- length(x)
  m <- mean(x)
  v <- (n-1)/n*var(x) 
  return((x-m)/sqrt(v))
}

#appliquer la fonction sur les variables continues 1:11
players.cont <- data.frame(lapply(subset(players,select=1:11),CR)) 
print(players.cont)
summary(players.cont)

#codage disjonctif complet
library(ade4)
players.disc <- acm.disjonctif(subset(players,select=12:13))
View(players.disc)
#fonction pour pondération des indicatrices
PF <- function(x){ 
  m <- mean(x) 
  return(x/sqrt(m))
}

#appliquer la pondération sur les indicatrices
players.disc.pond <- data.frame(lapply(players.disc,PF))

#données transformées envoyées à l'ACP
players.pour.acp <- cbind(players.cont,players.disc.pond) 
rownames(players.pour.acp) <- rownames(players) 
print(round(players.pour.acp,3))

#acp avec le package ade4
library(ade4)
acp.players <- dudi.pca(players.pour.acp,center=T, scale=F, scannf=F)

#valeurs propres
print(round(acp.players$eig,5))

#coordonnées ACP des variables : Gkh
#pour les quanti -> corrélations avec les facteurs 
print(acp.players$co[,1:2])

#**** pour les quali -> calculs supplémentaires nécessaires **** 
#récupérer coord. acp des modalités (12 à 30 dans acp.players$co)
moda <- acp.players$co[12:30,1:2]

#fréquence des modalités
freq.moda <- colMeans(players.disc)

#calcul des moyennes conditionnelles sur les 2 premiers facteurs
coord.moda <- moda[,1]*sqrt(acp.players$eig[1]/freq.moda)
coord.moda <- cbind(coord.moda,moda[,2]*sqrt(acp.players$eig[2]/freq.moda)) 
print(coord.moda)

#coordonnées des individus
print(round(acp.players$li[,1:2],5))

#carré des corrélations 1er facteur
r2 <- acp.players$co[1:11,1]^2

#carré du rapport de corrélation, var. qualitatives
eta2 <- NULL
eta2[1] <- sum(acp.players$co[12:23,1]^2)
eta2[2] <- sum(acp.players$co[24:30,1]^2) 

#valeurs à sommer
criteres <- c(r2,eta2) 
names(criteres) <- colnames(players) 
print(criteres)

#critère de l’AFDM – 1er facteur
lambda1 <- sum(criteres)
print(lambda1)

#confrontation avec résultat (v.p.) de l’ACP 
#sur variables transformées – 1er facteur 
print(acp.players$eig[1])

##############################  AFDM avec FactoMineR ###################
#chargement du package
library(FactoMineR)
#lancement de la procédure 
afdm.players <- FAMD(players, ncp=2) 
#affichage des résultats 
print(summary(afdm.players))

##############################  AFDM avec ade4 ###################
#chargement du package
library(ade4)
#lancement de la procédure
dudi.players <- dudi.mix(players, scannf=F, nf=2)
#affichage des valeurs propres.
print(round(dudi.players$eig,5))
#coordonnées factorielles var./moda.
print(round(dudi.players$co,5))
#graphique des variables/modalités
scatter(dudi.players,posieig="top",clab.row=0)

##############################  AFDM avec PCAmixdata ###################
#chargement de la librairie
library(PCAmixdata)
#lancement de la procédure
pcamix.players <- PCAmix(players[1:11], players[12:13], ndim=2, graph=T) 
#valeurs propres
print(round(pcamix.players$eig,5))
#corrélations
print(round(pcamix.players$quanti.cor,5))
#coord. des modalitésdudi.mix de ADE4 
print(round(pcamix.players$categ.coord,5))


