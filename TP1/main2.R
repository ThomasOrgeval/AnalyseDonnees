# Title     : TODO
# Objective : TODO
# Created by: orgev
# Created on: 03/02/2021

mat <- read.table("Data/materiau.txt", header = T)
mat

reg1 <- lm(Y ~ X1, data = mat)
reg2 <- lm(Y ~ X2, data = mat)

scer1 <- sum(reg1$residuals^2) # ou sum(reg$residuals^2) sum(yi - ^y)²
scet1 <- sum((mat$X1 - mean(mat$X1))^2) # sum(yi - moy(y))²
scem1 <- scet1 - scer1

scer2 <- sum(reg2$residuals^2) # ou sum(reg$residuals^2) sum(yi - ^y)²
scet2 <- sum((mat$X2 - mean(mat$X2))^2) # sum(yi - moy(y))²
scem2 <- scet2 - scer2 # scet - scer ou sum(^y - moy(y))²

regmul <- lm(Y ~ X1 + X2, data = mat)   # on précise le nom des variables explicatives OU
regmulbis <- lm(Y ~ ., data = mat)    # on prend toutes les variables autre que Y en utilisant