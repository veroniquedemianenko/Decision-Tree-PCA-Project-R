rm(list = ls())

library(cellranger)

data = read.csv("~/Desktop/Income_Inequality.csv",sep=";")
data <- na.omit(data)
View(data)

# Question 1
databis <- data[,-1]
databis <- databis[,-1]
databis <- databis[,-1]
View(databis) # tableau avec seulement les variables quantitatives

colMeans(databis,na.rm=TRUE) # Moyenne de chaque colonne
sapply(databis,function(col)sd(col,na.rm=TRUE)) # Écart-type de chaque colonne 
sapply(databis,function(col)var(col,na.rm=TRUE)) # Variance de chaque colonne 
for (col in colnames(databis)){
  boxplot(databis[[col]], main=paste("Boxplot de",col)) }

# Question 2
# a)
set.seed(1234)
index <- sample(1:nrow(data),round(0.70*nrow(data)))
train <- data[index,]
test <- data[-index,]
nrow(train)
nrow(test)

#b)
library(rpart)
library(rpart.plot)
library(caret)

fulltree <- rpart(Income_Inequality~., data=train, method="class")
rpart.plot(fulltree, main = "Arbre initial")

printcp(fulltree)       
varImp(fulltree) 

# Elagage
cart_fit <- rpart::rpart(Income_Inequality~., data=train)
min_ind <- which.min(cart_fit$cptable[, "xerror"])
min_cp <- cart_fit$cptable[min_ind, "CP"]
pruned_tree <- rpart::prune(cart_fit, cp = min_cp)
rpart.plot(pruned_tree, main = "Arbre élagué")
# L'élagage ne change rien

# Validation croisée
library(caret)

hyper_grid <- expand.grid(cp = seq(0.01, 0.5, by = 0.01))
train_control <- trainControl(method = "cv", number = 5)
model <- train(Income_Inequality ~ ., data = train, method = "rpart", trControl = train_control, tuneGrid = hyper_grid)
model
# On utilise cp = 0.01, donc le modèle initial est optimal


#c)
# Taux d'erreur et matrice de confusion
predicted <- predict(fulltree, test, type="class")

error1 = sum(test$Income_Inequality != predicted)/length(predicted) 
error1

predicted <- ordered(predicted)
actual <- ordered(test$Income_Inequality)

matrice_confusion = table(predicted,actual, dnn=c("Predicted","reelle")) 
matrice_confusion

error2 = 1-((matrice_confusion[1,1]+matrice_confusion[2,2])/sum(matrice_confusion))
error2

# Sensibilité et spécificité
sensibilite <- sensitivity(matrice_confusion)
sensibilite
specificite <- specificity(matrice_confusion)
specificite

# Précision
precision <- sum(diag(matrice_confusion)) / sum(matrice_confusion) ## ou 1-error
precision

# F1-Score
f1_score <- 2 * (precision * sensibilite) / (precision + sensibilite)
f1_score

# Courbe ROC
library(ROCR)

Predprob <- predict(fulltree, newdata = test,type = "prob")
Predprob = as.data.frame(Predprob)

Prediction <- prediction(Predprob[2],test$Income_Inequality)
performance <- performance(Prediction, "tpr","fpr")

plot(performance,main = "ROC Curve",col = 2,lwd = 2)
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")

aucDT <- performance(Prediction, measure = "auc")
aucDT <- aucDT@y.values[[1]]
aucDT

# Indice de Gini
gini <- 2*aucDT - 1
gini

## interpréter


# Question 3
#b)
library(solitude)
library(tidyverse)
library(mlbench)
library(rsample)


iso = isolationForest$new(sample_size = 870)
iso$fit(data)

score_anomalie = data %>%
  iso$predict() %>%
  arrange(desc(anomaly_score))
score_anomalie

#c)
head(score_anomalie,10)
tail(score_anomalie,10)

# comparer, analyser et interpréter

#d)
top_anom <- head(score_anomalie,50)
top_anom <- unlist(top_anom)

data_anom <- data[-as.numeric(top_anom), ]
View(data_anom)


data_anombis <- data_anom[,-1]
data_anombis <- data_anombis[,-2]
View(data_anombis) # tableau avec seulement les variables quantitatives


colMeans(data_anombis,na.rm=TRUE) # Moyenne de chaque colonne
sapply(data_anombis,function(col)sd(col,na.rm=TRUE)) # Écart-type de chaque colonne 
sapply(data_anombis,function(col)var(col,na.rm=TRUE)) # Variance de chaque colonne 
for (col in colnames(data_anombis)){
  boxplot(data_anombis[[col]], main=paste("Boxplot de",col)) }

set.seed(1234)
index_anom <- sample(1:nrow(data_anom),round(0.70*nrow(data_anom)))
train_anom <- data_anom[index_anom,]
test_anom <- data_anom[-index_anom,]
nrow(train_anom)
nrow(test_anom)

fulltree_anom <- rpart(Income_Inequality~., data=train_anom, method="class")
rpart.plot::rpart.plot(fulltree_anom)

printcp(fulltree_anom)       
varImp(fulltree_anom) 

cart_fit_anom <- rpart::rpart(Income_Inequality~., data=train_anom)
min_ind_anom <- which.min(cart_fit_anom$cptable[, "xerror"])
min_cp_anom <- cart_fit_anom$cptable[min_ind_anom, "CP"]
pruned_fit_anom <- rpart::prune(cart_fit_anom, cp = min_cp_anom)
rpart.plot::rpart.plot(cart_fit_anom)
rpart.plot::rpart.plot(pruned_fit_anom)




fulltree_anom <- rpart(Income_Inequality~., data=train_anom, method="class")
rpart.plot(fulltree_anom, main = "Arbre initial")

printcp(fulltree_anom)       
varImp(fulltree_anom) 

# Elagage
cart_fit_anom <- rpart::rpart(Income_Inequality~., data=train_anom)
min_ind_anom <- which.min(cart_fit_anom$cptable[, "xerror"])
min_cp_anom <- cart_fit_anom$cptable[min_ind_anom, "CP"]
pruned_tree_anom <- rpart::prune(cart_fit_anom, cp = min_cp_anom)
rpart.plot(pruned_tree_anom, main = "Arbre élagué")
# L'élagage ne change rien

# Validation croisée
library(caret)

hyper_grid_anom <- expand.grid(cp = seq(0.01, 0.5, by = 0.01))
train_control_anom <- trainControl(method = "cv", number = 5)
model_anom <- train(Income_Inequality ~ ., data = train_anom, method = "rpart", trControl = train_control_anom, tuneGrid = hyper_grid_anom)
model_anom


# Taux d'erreur et matrice de confusion
predicted_anom <- predict(fulltree_anom, test_anom, type="class")

error1_anom = sum(test_anom$Income_Inequality != predicted_anom)/length(predicted_anom) 
error1_anom

predicted_anom <- ordered(predicted_anom)
actual_anom <- ordered(test_anom$Income_Inequality)

matrice_confusion_anom = table(predicted_anom,actual_anom, dnn=c("Predicted","reelle")) 
matrice_confusion_anom

error2_anom = 1-((matrice_confusion_anom[1,1]+matrice_confusion_anom[2,2])/sum(matrice_confusion_anom))
error2_anom

# Sensibilité et spécificité
sensibilite_anom <- sensitivity(matrice_confusion_anom)
sensibilite_anom
specificite_anom <- specificity(matrice_confusion_anom)
specificite_anom

# Précision
precision_anom <- sum(diag(matrice_confusion_anom)) / sum(matrice_confusion_anom) ## ou 1-error
precision_anom

# F1-Score
f1_score_anom <- 2 * (precision_anom * sensibilite_anom) / (precision_anom + sensibilite_anom)
f1_score_anom

# Courbe ROC
library(ROCR)

Predprob_anom <- predict(fulltree_anom, newdata = test_anom,type = "prob")
Predprob_anom = as.data.frame(Predprob_anom)

Prediction_anom <- prediction(Predprob_anom[2],test_anom$Income_Inequality)
performance_anom <- performance(Prediction_anom, "tpr","fpr")

plot(performance_anom,main = "ROC Curve",col = 2,lwd = 2)
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")

# AUC
aucDT_anom <- performance(Prediction_anom, measure = "auc")
aucDT_anom <- aucDT_anom@y.values[[1]]
aucDT_anom

# Indice de Gini
gini_anom <- 2*aucDT_anom - 1
gini_anom

# Question 4
#a) On va standardiser les données car elles ne sont pas toutes de la même unité, ainsi on peut essayer de leur donner un poids égal

#b) On réutilise les fonctions du TP1
# Étape 1 : Cetnrage et réduction des données
CentrerRéduire <- function(X) {
  Xcr <- scale(X)
  return(Xcr)}

# Étape 2 : Calcul de la matrice de variance-covariance
CalculMatCov <- function(Xcr) {
  matcov <- cov(Xcr)
  return(matcov)}

# Étape 3 : Calculer les vecteurs propres et les valeurs propres
CalculVecteursPropres <- function(matcov) {
  decomp <- eigen(matcov)
  vecp <- na.omit(decomp$vectors)
  valp <- na.omit(decomp$values)
  return(list(vecp, valp))}

# Étape 4 : Réduction de la dimension, avec k le nombre de dimensions à conserver
ChoixComposantesPrincipales <- function(vecp, k) {
  compr <- vecp[, 1:k]
  return(compr)}

# Étape 5 : Projeter les données sur les nouvelles composantes
ProjectionDonnées <- function(Xcr, compr) {
  proj <- Xcr %*% compr
  return(proj)}

# Fonction ACP
ACP <- function(X,k){
  matcov <- CalculMatCov(X)
  decomp <- CalculVecteursPropres(matcov)
  ACP_résultat <- list()
  ACP_résultat[[1]] <- decomp[[1]]
  ACP_résultat[[2]] <- decomp[[2]]
  ACP_résultat[[3]] <- ChoixComposantesPrincipales(ACP_résultat[[1]], k) 
  return(ACP_résultat)}

X <- train[,-1]
X <- X[,-1]
X <- X[,-1]
X <- X[, -ncol(X)]

Xcr <- CentrerRéduire(X)

# Méthode 1 pour trouver k : règle de Cattell
matcov <- CalculMatCov(Xcr)
eltpropres <- CalculVecteursPropres(matcov)
valp <- eltpropres[[2]]

plot(c(1:18),valp,main="Graphique de valeurs propres",xlab="Numéro de la composante",ylab="Valeur propre")

# S'il ne faut retenir que les facteurs avant le changement de pente on garde que les 4 premières composantes

# Méthode 2 pour trouver k : règle de Kaiser-Guttman
matcov <- CalculMatCov(Xcr)
eltpropres <- CalculVecteursPropres(matcov)
valp <- eltpropres[[2]]
valp

# On ne retient que les composantes avec une valeur propre supérieure à 1, soit les 4 premières composantes

# Méthode 3 pour trouver k : règle Karlis-Saporta-Spinaki
seuil <- 2 * sqrt( (ncol(databis)-1) / (nrow(databis)-1) )
seuil

# On ne retient que les composantes avec une valeur propre supérieure au seuil, soit les 7 premières composantes

# Méthode 4 pour trouver k : inertie cumulée
InertieExpliquee <- function(X,valp) {
  inexp <- list()
  for (i in 1:ncol(X)) {
    inexp[i]<- valp[i]/sum(valp)
  }
  return(inexp)}

for (i in 1:18) {
  X<-ACP(Xcr,i)
  inertie_exp<-InertieExpliquee(Xcr,X[[2]])
}
inertie_cum<-cumsum(inertie_exp)
inertie_cum

# On obtient plus de 80% à partir de k=5, il est donc possible de réduire le nombre de variables à au moins 5.

# On retient k = 4

ACP_résultat <- ACP(Xcr,4)
compr <- ACP_résultat[[3]]
compr
proj <- ProjectionDonnées(Xcr, compr)
proj <- as.data.frame(proj)

library(ggplot2)

proj$class <- as.factor(train[, ncol(train)])

#ggplot(proj, aes(x = V1, y = V2, color = class)) + geom_point() + labs(title = "Nuage des points dans le plan (V1, V2) après ACP",x = "V1", y = "V2") +theme_minimal()


#c)
Coordonnées <- function(X,k) {
  coord <- list()
  for (i in (1:nrow(X))) {
    X[i,] <- as.matrix(X[i,])
    coord[[i]] <- as.list(X[i,] %*% ACP(X,k)[[3]])}
  return(coord)}

Qualité <- function(X, k) {
  coord <- Coordonnées(X)
  numérateur <- numeric(nrow(X))  
  dénominateur <- 0
  Q2 <- numeric(nrow(X))
  for (i in (1:nrow(X))) {
    elt <- coord[[i]]
    for (j in (1:k)) {
      numérateur[i] <- numérateur[i] + (elt[[j]])**2
      dénominateur <- dénominateur + elt[[j]]**2
    }
    Q2[i] <- numérateur[i]/dénominateur
  }
  return(Q2)}

Xtest <- test[,-1]
Xtest <- Xtest[,-1]
Xtest <- Xtest[,-1]
Xtest <- Xtest[, -ncol(X)]

Xtestcr <- CentrerRéduire(Xtest)
Q2 <- Qualité(Xtestcr,4)


# problème avec la fonction alors qu'elle marchait avant donc mettre en screen au-dessus mais avec les résultats d'en dessous
projtest <- ProjectionDonnées(Xtestcr, compr)
numérateur <- numeric(nrow(Xtest))  
dénominateur <- 0
Q2 <- numeric(nrow(Xtest))
for (i in (1:nrow(Xtest))) {
  elt <- projtest[i,]
  for (j in (1:4)) {
    numérateur[i] <- numérateur[i] + (elt[[j]])**2
    dénominateur <- dénominateur + elt[[j]]**2
  }
  Q2[i] <- numérateur[i]/dénominateur
}
Q2 <- as.matrix(Q2)
View(Q2)



tri <- order(Q2)
head(tri,10)

# Le problème étant qu'on a plus les bon numéros d'individus, il faut faire le lien entre les 10 numéros ici et le numéro de ligne correspondant dans projtest

indiv_mal_proj <- numeric()
for (i in (1:10)){
  id <- head(tri,10)[[i]]
  indiv_mal_proj[[i]] <- rownames(projtest)[id]
}
indiv_mal_proj <- as.numeric(indiv_mal_proj)
indiv_mal_proj

colMeans(databis[indiv_mal_proj,],na.rm=TRUE) # Moyenne de chaque colonne
sapply(databis[indiv_mal_proj,],function(col)sd(col,na.rm=TRUE)) # Écart-type de chaque colonne 
sapply(databis[indiv_mal_proj,],function(col)var(col,na.rm=TRUE)) # Variance de chaque colonne 
for (col in colnames(databis[indiv_mal_proj,])){
  boxplot(databis[indiv_mal_proj,][[col]], main=paste("Boxplot de",col)) }



# Question 5
# On travaille avec la partie train, donc la matrice X

#a)
train_quant <- train[,-1]
train_quant <- train_quant[,-1]
train_quant <- train_quant[,-1]
View(train_quant)

var <- apply(train_quant, 2, function(x) var(x, na.rm = TRUE))
V <- diag(var) # matrice de variance totale

trainAFD <- train[,-1]
trainAFD <- trainAFD[,-1]

train_HL<-split(trainAFD,trainAFD$Income_Inequality)

## calcul des variances inter : 

B<-list()
HL<-c("H","L")
i<-0
for (j in colnames(train_quant)) {
  i<-i+1
  B[i]<-0
  for (k in HL) {
    traink<-train_HL[[k]]
    diff <- nrow(traink) * (mean(traink[[j]]) - mean(train_quant[[j]]))^2
    B[i]<-as.numeric(B[i])+diff
  }
  B[i]<-as.numeric(B[i])/nrow(train_quant)
  
}
B # c'est la liste des variances inter


## calcul des variances intra

W<-list()
Wk<-list()
l<-0

for (j in colnames(train_quant)) {
  l<-l+1 #numéro de la colonne j
  W[l]<-0
  Wk[l]<-0
  for (k in HL) {
    traink<-train_HL[[k]] #les individus étant H ou L en income inequality
    traink<-traink[,-1]
    trainkj<-traink[[l]] # variable j des individus H ou L
    for (i in (1:nrow(traink))) { #chaque individu i H ou L 
      diff<-(trainkj[i]-mean(trainkj))^2
      Wk[l]<-as.numeric(Wk[l]) + diff
    }
    Wk[l]<-as.numeric(Wk[l])/length(trainkj)
    W[l]<-as.numeric(W[l])+as.numeric(Wk[l])
  } 
  W[l]<-as.numeric(W[l])/nrow(train)
}
W  

matB<-diag(B)
matW<-diag(W)

sigma <- cov(train_quant)
sigma

sig<-solve(s) %*% matB

Diagonaliser <- function(X){
  res_eigen <- eigen(X)
  return(list(res_eigen$values,res_eigen$vectors))
}

sigmaInvB<-Diagonaliser(sig)

#On utilise les mêmes méthodes, et nous retrouvons bien k=4.

library(ade4)

afd<-dudi.mix(train,add.square=FALSE, scannf= TRUE, 2)





