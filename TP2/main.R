# Title     : TODO
# Objective : TODO
# Created by: orgev
# Created on: 09/02/2021

# Chargement de la librairie nécessaire:
library(FactoMineR)

# Chargement des données
foot <- read.csv2("Data/foot_pca.csv", row.names = 1)
# Les données que l'on va étudier concernent les équipes de football des 5 championnats majeurs européens:
# Angleterre, France, Espagne, Allemagne, Italie
# Pour chaque équipe de ces championnats, on a recolté des informations à propos de 15 variables (saison 2018/2019) :
# - Buts : nb de buts marqués dans la saison
# - Tirs : nb de tirs moyen effectués par match
# - Jaune : nb de cartons jaunes pris par l'équipe dans la saison
# - Rouge : -------------- rouges------------------------------
# - Possession : possession de balle (en pourcentage)
# - PassesReuss : % de passes réussies
# - DuelsAériens : nb moyen de duels aériens gagnés par match
# - TirsContre : nb de tirs moyen subis par match
# - Tacles : nb de tacles moyen effectués par match
# - Interception : nb d'interception moyen effectuées par match
# - Fautes : nb de fautes moyen effectuées par match
# - Hors jeu : nb de hors jeu moyen par match
# - TirsCadres : nb de tirs cadrés moyen effectués par match
# - Dribbles : nb de dribbles moyen par match
# - FautesSubies : nb de fautes moyen subies par match

#########################################################
###      Première partie: Description des données   #####
#########################################################

head(foot, n = 5) # Cette commande affiche les 5 premières lignes du tableau de données
dim(foot) # Cette commande vous indique le nb de lignes (individus) et de variables (colonnes)
summary(foot) # Cette commande vous permet d'avoir des informations sur chacune des variables du tableau:
# - minimum, maximum, quartiles et moyenne pour les variables quantitatives
# - répartition des individus dans les différentes modalités pour les variables qualitatives

# Questions :
foot[which.min(foot$Jaune),] # - Quelle est la valeur minimale de cartons jaunes pris par une équipe ? Quelle est cette équipe ?
sum(foot$Ligue %like% "France") # - Combien d'équipes participent à la ligue française ?

# Diagramme en camembert de la répartition des équipes par Ligue
pie(table(foot$Ligue), main = "Répartition des équipes par Ligue", labels = paste(names(table(foot$Ligue)), round(table(foot$Ligue)*100/98,1),"%"))
# Note : la commande $ permet de récupérer toutes les valeurs d'une colonne (en l'appelant par son nom, ici la colonne Ligue)

# A l'aide de la comande hist, faites un histogramme des valeurs de la variable Fautes.
# N'oubliez pas de rajouter un titre au graphe (main = "...") et un nom aux axes (xlab = "..." et ylab="...")
hist(foot$Fautes, main = "Nombre de fautes commises par équipe", xlab = "Nombre de fautes commises", ylab = "Nombre d'équipes")

# Utiliser la commande summary appliquée à la colonne Fautes pour récupérer les informations
# sur cette variable
summary(foot$Fautes)

# Appliquer cette même commande summary, mais cette fois-ci uniquement aux équipes de la ligue anglaise
# Aide : les indices des lignes dans lesquelles sont les équipes anglaises sont données par:
idx_anglais <- which(foot$Ligue=="Angleterre")
# et donc foot[idx_anglais,] permet de récupérer uniquement les lignes des équipes anglaises
summary(foot[idx_anglais,])

# Question : Que constatez-vous pour les équipes anglaises et le nombre de fautes ?
# Min.   : 8.600
# 1st Qu.: 9.675
# Median :10.700
# Mean   :10.535
# 3rd Qu.:11.300
# Max.   :12.700
# Les variables sont centrées et basses comparé aux autre ligues

# Question : Tracer sur une même figure 5 histogrammes, qui représentent le nombre de fautes
# pour chaque ligue différente
par(mfrow = c(5, 1))
hist(foot$Fautes[idx_anglais])
hist(foot$Fautes[which(foot$Ligue=="France")])
hist(foot$Fautes[which(foot$Ligue=="Allemagne")])
hist(foot$Fautes[which(foot$Ligue=="Italie")])
hist(foot$Fautes[which(foot$Ligue=="Espagne")])
# Aide :
# par(mfrow = c(5,1)) permet de créer une fenetre graphique vierge mais qui contient 5 sous-fenetres
# la commande xlim = c(a,b) dans la fonction plot permet d'ajuster l'échelle de l'axe des abscisses à l'intervalle [a,b]
# N'hésitez pas à utiliser cette commande pour rendre les 5 histogrammes plus jolis en les traçant à la même échelle

# La commande boxplot permet de dessiner la boite à moustache d'une variable (en la mettant en paramètre)
# Question : Dessiner la boîte à moustaches de la variable Fautes (sur tout le tableau de données)
boxplot(foot$Fautes, main = "Boites à moustache des fautes")
# Boxplot permet également de dessiner la boîte à moustache par modalités d'une variable qualitative (ici Ligue):
boxplot(foot$Fautes~foot$Ligue, main = "Boites à moustache des fautes par ligues") # 5 boites à moustache : 1 par ligue
# Retrouvez-vous la conclusion que l'on avait fait à propose des équipes anglaises et du nb de fautes ?
# On retrouve ce que l'on avait dit

# Essayez de la même façon d'étudier la variable DuelsAeriens en fonction de la ligue,
# ainsi que la variable Jaune en fonction de la ligue. Quelles conclusions pouvez vous faire ?
boxplot(foot$DuelsAeriens~foot$Ligue, main = "Boites à moustache des duels aériens par ligues")
boxplot(foot$Jaune~foot$Ligue, main = "Boites à moustache des duels aériens par ligues")

# La commande cor permet de calculer la corrélation entre 2 variables, par exemple:
cor(foot$Buts, foot$Possession)
# Ces deux variables sont-elles corrélées ?
# c'est plutot corrélé

# On peut également obtenir la corrélation entre une variable et les autres:
cor(foot$Buts, foot[,1:15])
# Question : Quelle est la variable la plus corrélée à Buts ? Et la moins corrélée ?
# + = tirs cadres
# - = rouge
# Regardez la corrélation d'une autre variable de votre choix à toutes les autres.
cor(foot$Tacles, foot[,1:15])
# + = interception
# - = rouge

#########################################################
###      Deuxième partie: ACP sur ces données       #####
#########################################################

foot_acp <- PCA(foot, quali.sup = 16)
# La commande PCA permet de faire l'ACP. On lui indique que la 16ème colonne (variable Ligue)
# est une variable qualitative qui ne doit pas être utilisée pour faire l'ACP (mais on pourra la
# visualiser tout de même)
# Par défaut, la fonction PCA fait une ACP centrée-réduite. Pourquoi était-ce obligatoire ici ?


# Vous devriez avoir à l'écran les deux graphes de l'ACP : le premier plan des individus et celui des variables.
# On les étudiera plus en détail tout à l'heure.

# La première chose à faire pour analyser les résultats d'une ACP est de décider
# combien d'axes vont être nécéssaires pour analyser les données.
# Pour cela, on doit regarder les valeurs propres de la matrice des corrélations. Elles
# sont données par la commande:
foot_acp$eig
# Questions:
# - Combien y a t'il de valeurs propres ? Est-ce normal ? 15
# - Que vaut la première valeur propre ? # 5,83544
# - Quelle est le pourcentage d'inertie expliqué par le premier axe ? # 38,9029
# - ----------------------------------------------------------- plan ? # 52,0256 et un second à 12,2348 + 6,60818
# - Combien d'axes décidez-vous de conserver pour mener cette analyse ? # 4 axes car ils sont >= 1

# Diagramme en baton des valeurs propres :
x11()
barplot(foot_acp$eig[,1])

# La commande foot_acp$var  fournit les coordonnéees des
# variables sur les axes factoriels (coord), leurs cosinus carrés(cos2, qualité de projection) et leurs contributions (contrib).
x11()
barplot(foot_acp$var$coord[,1])
barplot(foot_acp$var$cos2[,1])
# Questions :
# - Quelles sont les variables les plus corrélées à l'axe 1 ? l'axe 2 ? l'axe 3 ? l'axe 4? Possession et passes réussies
# - Comment pouvez-vous intérpréter le premier axe factoriel ? le deuxième ?



# La commande foot_acp$ind permet de récupérer les mêmes informations mais au niveau des individus.
# Questions :
foot_acp$ind
# - Quels sont les individus qui ont le plus contribué à créer l'axe 1 ? Manchester City
# Aide : round(foot_acp$ind$contrib[,'Dim.1'], 2) vous donne les contributions du premier axe dans un vecteur
# et la commande 'sort' permet de trier par ordre croissant un vecteur
# - Que pouvez-vous dire de ces individus ? Man city, PSG et FCB sont loins devants
# - Que peut-on dire de particulier sur l'équipe de Crotone ? 2.65, ils sont moyennenement présent


# - Quels sont les individus qui ont le plus contribué à créer l'axe 2 ? # Bournemouth, Schalke 04, Brighton
# - Que pouvez vous dire de Bournemouth ? De Schalke 04 ? # Ils sont très haut placé

x11()
plot(foot_acp, choix = "ind", axes = c(1,2), habillage = 16)
# On a colorié les équipes en fonction de leur ligue.
# Question : où se situent principalement les équipes anglaises ? En bas
# Quelles conclusions peut-on en tirer ? Ils sont négatifs sur l'axe 2

# On va maintenant faire la même chose pour les axes 3 et 4
x11()
plot(foot_acp, choix = "ind", axes = c(3,4), habillage = 16)
x11()
plot(foot_acp, choix = "var", axes = c(3,4))
# Question : - où se situent les équipes allemandes sur le graphe du deuxième plan ? A droite
# - Quelles conclusions peut-on en tirer ? Ils ont un impact positif sur l'axe 1
# Où est Saint-Etienne sur le deuxième plan ? Que peut on dire de cette équipe ? En haut a gauche, ils sont positifs sur l'axe 2 et négatif sur le 1

# Observez la position de Real Madrid et de Nantes sur ce deuxième plan. Peut-on alors dire que ces
# deux équipes se ressemblent ? Ils sont au meme niveau