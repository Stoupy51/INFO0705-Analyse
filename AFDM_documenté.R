# ----------------------------------------------------------
# Documentation du Code R pour l'Analyse Factorielle Discriminante Multiple (AFDM)
# ----------------------------------------------------------

# Vider la mémoire
rm(list=ls())

# Charger les données "data_analysis.csv"
players = read.csv(file.choose(), row.names=1, header=TRUE, stringsAsFactors=TRUE)
print(summary(players))
str(players)



## Fonctions utilitaires
# Cette fonction est définie pour effectuer le centrage-réduction sur un vecteur numérique donné.
# Le centrage-réduction est une étape courante dans l'analyse de données,
# visant à standardiser les variables en les ramenant à une échelle commune.
CentrageReduction = function(x) {
	n = length(x)
	m = mean(x)
	v = (n-1)/n * var(x) 
	return ((x - m) / sqrt(v))
}

# Fonction pour pondération des indicatrices :
# Cette fonction permet de normaliser les indicatrices générées lors du codage disjonctif complet (CDC)
# des variables qualitatives. La normalisation est réalisée en divisant chaque élément de l'indicatrice
# par la racine carrée de sa moyenne. Cette étape est cruciale dans le processus
# d'Analyse Factorielle Discriminante Multiple (AFDM) pour garantir une contribution équilibrée
# des variables qualitatives à l'analyse factorielle, en prenant en compte leurs variances respectives.
PonderationF = function(x) {
	m = mean(x) 
	return (x / sqrt(m))
}


### Transformation des variables
## Centrage-réduction des variables continues
# Appliquer la fonction de centrage-réduction sur les variables continues (celles de 1 à 11 selon notre dataset)
players.cont = data.frame(lapply(subset(players, select = 1:10), CentrageReduction)) 
print(players.cont)
summary(players.cont)



## Codage disjonctif complet (CDC) des variables qualitatives
# Méthode courante pour représenter des variables
# qualitatives nominales dans les analyses multivariées.
# Pour simplifier, cette méthode est utilisée pour coder les variables qualitatives nominales en variables indicatrices.
# Cela va générer un dataframe contenant autant de variables indicatrices que le nombre de valeurs distinctes.
library(ade4)
colonnes_qualitatives = subset(players, select=11:12) # On sélectionne les variables qualitatives
players.disc = acm.disjonctif(colonnes_qualitatives)
View(players.disc)

# Appliquer la pondération sur les indicatrices afin de garantir une contribution équilibrée des variables qualitatives
players.disc.pond = data.frame(lapply(players.disc, PonderationF))

# Concaténation des variables continues et qualitatives codées en indicatrices
players.pour.acp = cbind(players.cont, players.disc.pond) 
rownames(players.pour.acp) = rownames(players) # Ajouter les noms des individus car le dataframe génère automatiquement des nombres
print(round(players.pour.acp, 3))


### Analyse Factorielle Discriminante Multiple (AFDM)
## Analyse en Composantes Principales (ACP) sur les variables transformées
# L'ACP est une méthode de visualisation de données multivariées,
# qui permet de représenter les variables continues et qualitatives codées en indicatrices
# sous forme de projections sur un espace de dimensions inférieures.
# Cette méthode est utilisée pour visualiser les variables continues et qualitatives codées
# en indicatrices dans le contexte de l'analyse factorielle.
library(ade4)
"
Paramètres de la fonction dudi.pca():
    players.pour.acp: C'est le dataframe sur lequel l'ACP sera effectuée. Il contient à la fois les variables continues centrées-réduites (players.cont) et les variables qualitatives codées en indicatrices avec pondération (players.disc.pond).
    center = T: Cela indique que le centrage des variables est effectué. Dans le contexte de l'ACP, cela signifie que la moyenne de chaque variable est soustraite, assurant que le centre de la distribution des données est à l'origine.
    scale = F: Cela indique que la mise à l'échelle des variables n'est pas effectuée. Dans le contexte de l'ACP, cela signifie que les variables n'ont pas été divisées par leur écart type.
    scannf = F: Cela indique que la scree plot (graphique de l'éboulis des valeurs propres) n'est pas générée. La scree plot est un outil visuel pour aider à déterminer le nombre de composantes principales à retenir.
"
acp.players = dudi.pca(players.pour.acp, center = T, scale = F, scannf = F)

# Affichage des valeurs propres
print(round(acp.players$eig, 5))

# Coordonnées ACP des variables continues
# Pour les quantitatives, c'est la corrélation avec les facteurs 
print(acp.players$co[,1:2])

# Pour les qualitatives, on calcule les coordonnées ACP à partir des modalités.
# Récupérer les coordonnées ACP des modalités (12 à 30 dans acp.players$co)
moda = acp.players$co[11:29, 1:2]

# Fréquence des modalités
freq.moda = colMeans(players.disc)

# Calcul des moyennes conditionnelles sur les 2 premiers facteurs
# Les moyennes conditionnelles sont des moyennes des individus pondérées par les fréquences des modalités
coord.moda = moda[,1] * sqrt(acp.players$eig[1] / freq.moda)
coord.moda = cbind(coord.moda, moda[,2] * sqrt(acp.players$eig[2] / freq.moda)) 
print(coord.moda)

# Affichage des coordonnées des individus
print(round(acp.players$li[,1:2], 5))

# Carré des corrélations Comp1 sur les variables continues
# Cela permet de comparer la contribution des variables continues à l'analyse factorielle.
r2 = acp.players$co[1:10, 1] ^ 2

# Somme des carrés des corrélations pour les variables qualitatives
eta2 = NULL
eta2[1] = sum(acp.players$co[11:23,1]^2)	# favorite_world
eta2[2] = sum(acp.players$co[23:29,1]^2)	# geolocation

# Unification de r² et eta²
criteres = c(r2, eta2) 
names(criteres) = colnames(players)	# Ajouter les noms des variables
print(criteres)

# Critère de l’AFDM – 1er facteur
# Confrontation avec le résultat (valeurs propres) de l’ACP
# sur les variables transformées
lambda1 = sum(criteres)
print(lambda1)
print(acp.players$eig[1])



# ----------------------------------------------------------
# AFDM avec FactoMineR
# ----------------------------------------------------------
# Charger le package et lancer la procédure
library(FactoMineR)
afdm.players = FAMD(players, ncp=2) # ncp = 2 : 2 facteurs principaux

# Afficher les résultats 
print(summary(afdm.players))

