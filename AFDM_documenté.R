# ----------------------------------------------------------
# Documentation du Code R pour l'Analyse Factorielle Discriminante Multiple (AFDM)
# ----------------------------------------------------------

# Vider la mémoire
rm(list=ls())

# Charger les données "data_analysis.csv"
players <- read.csv(file.choose(), row.names=1, header=TRUE, stringsAsFactors=TRUE)
print(summary(players))
str(players)

# Cette fonction, notée CR, est définie pour effectuer le centrage-réduction sur un vecteur numérique donné.
# Le centrage-réduction est une étape courante dans l'analyse de données,
# visant à standardiser les variables en les ramenant à une échelle commune.
CR <- function(x){
  n <- length(x)
  m <- mean(x)
  v <- (n-1)/n * var(x) 
  return((x - m) / sqrt(v))
}

# Appliquer la fonction de centrage-réduction sur les variables continues (celles de 1 à 11)
players.cont <- data.frame(lapply(subset(players, select=1:11), CR)) 
print(players.cont)
summary(players.cont)

# Codage disjonctif complet :
# Méthode courante pour représenter des variables
# qualitatives nominales dans les analyses multivariées.
library(ade4)
colonnes_qualitatives = subset(players, select=12:13)
players.disc <- acm.disjonctif(colonnes_qualitatives)
View(players.disc)

## PONDÉRATION DES INDICATRICES
# Fonction pour pondération des indicatrices :
# Cette fonction permet de normaliser les indicatrices générées lors du codage disjonctif complet (CDC)
# des variables qualitatives. La normalisation est réalisée en divisant chaque élément de l'indicatrice
# par la racine carrée de sa moyenne. Cette étape est cruciale dans le processus
# d'Analyse Factorielle Discriminante Multiple (AFDM) pour garantir une contribution équilibrée
# des variables qualitatives à l'analyse factorielle, en prenant en compte leurs variances respectives.
PF <- function(x) {
  m <- mean(x) 
  return(x / sqrt(m))
}

# Appliquer la pondération sur les indicatrices
players.disc.pond <- data.frame(lapply(players.disc, PF))

# Données transformées envoyées à l'ACP
players.pour.acp <- cbind(players.cont, players.disc.pond) 
rownames(players.pour.acp) <- rownames(players) 
print(round(players.pour.acp, 3))

##
# ACP avec le package ade4
library(ade4)
acp.players <- dudi.pca(players.pour.acp, center=T, scale=F, scannf=F)

# Valeurs propres
print(round(acp.players$eig, 5))

# Coordonnées ACP des variables : Gkh
# Pour les quanti -> corrélations avec les facteurs 
print(acp.players$co[,1:2])

# **** Pour les quali -> calculs supplémentaires nécessaires **** 
# Récupérer les coordonnées ACP des modalités (12 à 30 dans acp.players$co)
moda <- acp.players$co[12:30, 1:2]

# Fréquence des modalités
freq.moda <- colMeans(players.disc)

# Calcul des moyennes conditionnelles sur les 2 premiers facteurs
coord.moda <- moda[,1] * sqrt(acp.players$eig[1] / freq.moda)
coord.moda <- cbind(coord.moda, moda[,2] * sqrt(acp.players$eig[2] / freq.moda)) 
print(coord.moda)

# Coordonnées des individus
print(round(acp.players$li[,1:2], 5))

# Carré des corrélations 1er facteur
r2 <- acp.players$co[1:11,1]^2

# Carré du rapport de corrélation, var. qualitatives
eta2 <- NULL
eta2[1] <- sum(acp.players$co[12:23,1]^2)
eta2[2] <- sum(acp.players$co[24:30,1]^2) 

# Valeurs à sommer
criteres <- c(r2, eta2) 
names(criteres) <- colnames(players) 
print(criteres)

# Critère de l’AFDM – 1er facteur
lambda1 <- sum(criteres)
print(lambda1)

# Confrontation avec le résultat (v.p.) de l’ACP 
# sur les variables transformées – 1er facteur 
print(acp.players$eig[1])

# ----------------------------------------------------------
# AFDM avec FactoMineR
# ----------------------------------------------------------
# Charger le package
library(FactoMineR)
# Lancer la procédure 
afdm.players <- FAMD(players, ncp=2) 
# Afficher les résultats 
print(summary(afdm.players))

# ----------------------------------------------------------
# AFDM avec ade4
# ----------------------------------------------------------
# Charger le package
library(ade4)
# Lancer la procédure
dudi.players <- dudi.mix(players, scannf=F, nf=2)
# Afficher les valeurs propres
print(round(dudi.players$eig, 5))
# Coordonnées factorielles var./moda.
print(round(dudi.players$co, 5))
# Graphique des variables/modalités
scatter(dudi.players, posieig="top", clab.row=0)

# ----------------------------------------------------------
# AFDM avec PCAmixdata
# ----------------------------------------------------------
# Charger la librairie
library(PCAmixdata)
# Lancer la procédure
pcamix.players <- PCAmix(players[1:11], players[12:13], ndim=2, graph=T) 
# Valeurs propres
print(round(pcamix.players$eig, 5))
# Corrélations
print(round(pcamix.players$quanti.cor, 5))
# Coordonnées des modalités <-> dudi.mix de ADE4 
print(round(pcamix.players$categ.coord, 5))

