# Title     : TODO
# Objective : TODO
# Created by: orgev
# Created on: 03/02/2021

tabac <- read.table("tabac.txt", header = T, row.names = 1)
tabac # permet d’afficher le tableau

tabac$Vente # permet de r ́ecup ́erer un vecteur avec toutes les valeursde la variable Ventes
tabac$Prix # permet de r ́ecup ́erer un vecteur avec toutes les valeursde la variable Prix

mean(tabac$Vente) # permet de r ́ecup ́erer la moyenne de la variableVente
mean(tabac$Prix) # idem pour Prix

plot(tabac$Prix, tabac$Vente, main = "Consommation de tabac en France", col = "blue", lwd = 2,
     xlab = "Prix relatif de vente (en euros)", ylab = "Nombre decigarettes vendues (en milliards)") # tracé plus sophistique

reg <- lm(Vente ~ Prix, data = tabac)
# Pour utilser la commande lm, on doit indiquer le nom de la variable
# cible (ici Vente),le signe ~, le nom de la variable explicative (ici Prix)
# et le nom du tableau de donn ́ees (ici tabac)

reg$coefficients #Affiche (Intercept) Prix

abline(reg, col = "red") # Affiche la droite de régression

summary(reg) # Fait un sommaire
round(reg$residuals, 3) # récupère les résidus avec 3 chiffres après la virgule
reg$fitted.values # Calcule les valeurs prédites par ^y

scer <- sum((tabac$Vente - reg$fitted.values)^2) # ou sum(reg$residuals^2) sum(yi - ^y)²
scet <- sum((tabac$Vente - mean(tabac$Vente))^2) # sum(yi - moy(y))²
scem <- scet - scer # scet - scer ou sum(^y - moy(y))²
r2 <- summary(reg)$r.squared # ou scem/scet
varRes <- scer / (reg$df.residual) # summary(reg)$sigma^2 # Estimation de la variance résiduelle scer/n-2

new <- data.frame(288.3) # cr ́eation du nouvel individu avec sa valeurde Prix
colnames(new) <- "Prix"  # label de la variable explicative
vals <- predict(reg, new, interval = "confidence") # pour pr ́ediction moyenne