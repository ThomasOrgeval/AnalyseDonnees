# Title     : TODO
# Objective : TODO
# Created by: orgev
# Created on: 22/02/2021

library(FactoMineR)

jo <- data(JO)
# C'est un tableau de contingence
# Les pays sont les individus
# Les épreuves sont les variables

barplot(table(JO)) # 1211 cases ont 0 en valeur
rev(sort(apply(JO, 2, sum)))
# Le Kenya a remporté 12 médaille en 300 mettres haies

resca <- CA(JO)
round(resca$eig, 2) # 23 axes permettent de représenter tous les profils-lignes
# c'est normal car il y a 24 variables (min(24 - 1, 58 - 1))

barplot(resca$eig[, 1], names.arg = 1:23) # 100 / 23 = 4,34
# On prend les 9 premières variables car elles sont > 4.34%
# 24.38% d'inertie sont expliqués sur le premier plan factoriel

row <- round(resca$row$contrib, 2)
col <- round(resca$col$contrib, 2)
plot(resca, axes = 1:2)
# 10 000m, 5 000m, 3 000m haies. Les épreuves d'endurance influence plus l'axe 1
# Disque, Marteau. les épreuves de lancer

# Kenya, Ethiopie contribuent au premier axe (meilleur en endurance)
# Usa, Lituanie contribuent au second (meilleur en lancer)

# Japon, Portugal, Brésil (Disque, 20km)
# Afrique du sud, Corée du sud (Marathon, Hauteur)

plot(resca, axes = 3:4)
# Disque, javelot / Russie, Lituanie
# Javelot, 20km / Finlande, Tchéquie

# 4x400m, Decathlon / France, Danemark
# Poids, 400m / Chine, Ouganda

library(cluster)
resclassif <- agnes(resca$col$coord[, 1:4], method = "ward")
plot(resclassif) # On prend 6 classes

plot(resca$col$coord[, 1:2], col = cutree(resclassif, 6))

paysward <- cbind.data.frame(t(JO)[, 1:24], as.factor(cutree(resclassif, 6)))
colnames(paysward)[25] <- "Classward"
catdes(paysward, num.var = 25)
#               Eta2      P-value
# 20km     0.5813327 7.275277e-09
# Javelot  0.5440040 6.106744e-08
# 50km     0.4301754 1.447551e-05
# Disque   0.3753790 1.307870e-04
# Marteau  0.3704969 1.573138e-04
# 5000m    0.3168753 1.067953e-03
# 10000m   0.2603613 6.512512e-03
# 1500m    0.2601502 6.554103e-03
# Marathon 0.2535430 7.987575e-03
# 800m     0.2110035 2.669405e-02