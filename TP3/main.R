# Title     : TODO
# Objective : TODO
# Created by: orgev
# Created on: 19/02/2021

library(cluster)
library(FactoMineR)

tel <- read.table("Data/tel1984.txt", row.names = 1, header = T, dec = ",")
tel

resclus <- kmeans(tel[, 1:7], 4, algorithm = "Lloyd")
resclus$cluster # Ajaccio est isolé

plot(tel$igqs, tel$ezaa, col = resclus$cluster)

tel <- cbind(tel, as.factor(resclus$cluster))
colnames(tel)[9] <- "Classe km" # On bind les values du cluster dans la tableau tel

catdes(tel[, c(1:7, 9)], num.var = 8) # Sélection 1 à 7 et 9
# Ajaccio fait parti du groupe 4, le résultat est NULL

tel_acp <- PCA(tel, quali.sup = 9) # Ces groupes sont cohérents
tel_acp$eig # Les valeurs propres sont différentes de celles du td3 !

# --------------------------------

resclusnorm <- kmeans(scale(tel[, 1:7]), 4, algorithm = "Lloyd")
resclusnorm$size # 6 8 1 7 pour les tailles
resclusnorm$withinss # 7.562273 12.172112  0.000000 10.315933

plot(tel$igqs, tel$ezaa, col = resclusnorm$cluster) # Le classsement est meilleur

tel <- cbind(tel, as.factor(resclusnorm$cluster))
colnames(tel)[10] <- "Classe kmnorm"

catdes(tel[,c(1:7,10)], num.var = 8)

tel_acp2 <- PCA(tel, quali.sup = 10) # Je crois mes ACP ne marchent pas
tel_acp2$eig