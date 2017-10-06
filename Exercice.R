
#initialisation de l'environnement de travail
setwd("~/Desktop/Repertoire")

#Lecture du fichier contenant notre jeu de données
voit <- read.csv2("Etude.csv", header = TRUE)




# Chargement des données avec la sélection des colonnes
attach(voit)
names(voit)

base = subset(voit, select = c(cible, X1, X2, X3, X4, X5, X6, X7,
                                       X8, X9, X10, X11, X12, X13, X14, X15)) 
                                       
head(base)
tail(base)
str(base)
summary(base)

#La variable cible $cible$ doit être traitée en type facteur
base$cible <- factor(base$cible)
summary(base)

# Suppression des valeurs manquantes
base <- na.omit(base)

# Analyse de la ditribution
attach(base)

detach(voit)

summary(base)


# Représentation graphique de chaque variable
par(mfrow = c(3, 2))
hist(x = X1, col = "lightblue", main = "Variable X1", xlab = "", ylab = "")
hist(x = X2, col = "orange", main = "Variable X2", xlab = "", 
     ylab = "")
hist(x = X3, col = "red", main = "Variable X3", xlab = "", 
     ylab = "")
hist(x = X4, col = "violet", main = "Variable X4", xlab = "", 
     ylab = "")
hist(x = X5, col = "green2", main = "Variable X5", xlab = "", ylab = "")
hist(x = X6, col = "slategray", main = "Variable X6", xlab = "", ylab = "")
hist(x = X7, col = "slategray", main = "Variable X7", xlab = "", ylab = "")
hist(x = X8, col = "slategray", main = "Variable X8", xlab = "", ylab = "")
hist(x = X9, col = "slategray", main = "Variable X9", xlab = "", ylab = "")
hist(x = X10, col = "slategray", main = "Variable X10", xlab = "", ylab = "")
hist(x = X11, col = "slategray", main = "Variable X11", xlab = "", ylab = "")
hist(x = X12, col = "slategray", main = "Variable X12", xlab = "", ylab = "")
hist(x = X13, col = "slategray", main = "Variable X13", xlab = "", ylab = "")
hist(x = X14, col = "slategray", main = "Variable X14", xlab = "", ylab = "")
hist(x = X15, col = "slategray", main = "Variable X15", xlab = "", ylab = "")

#Représentation de la variable cible
par(mfrow = c(1, 1))
pairs(base, col = base$cible)


#Représentation avec le package ggplot
par(mfrow = c(1, 2))
require(ggplot2)
# affichage graphique avec ggplot2
qplot(X2, X4, data = base, facets = cible ~ .)

#Calcul du coefficient de corrélation entre les deux variables X2 et X4
cor(base$X2, base$X4)


#Représentation graphique de la densité locale à l'aide de la fonction kde2d de l'extension MASS.
tmp <- base[, c("X2", "X4")]
tmp <- tmp[complete.cases(tmp), ]
filled.contour(kde2d(base$X2, base$X4), color = terrain.colors)


modele <- glm(cible~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15 , data = base, family = binomial(logit))
summary(modele)

# modèle trivial réduit à la constante
str_constant <- "~ 1"
# modèle complet incluant toutes les explicatives potentielles
str_all <- "~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15"

require(MASS)

# Régresson logistique
#La fonction glm (pour "generalized linear models") 
modele <- glm(cible~ 1, data = base, family = binomial)

#Le critère d’Akaike (AIC) est un critère utilisé pour la sélection de modèles
modele.forward <- stepAIC(modele, scope = list(lower = str_constant, upper = str_all), 
                          trace = TRUE, data = base, direction = "forward")

# affichage du modèle final
summary(modele.forward)

#Le modele Stepwise
modele <- glm(cible ~ 1, data = base, family = binomial)
modele.stepwise <- stepAIC(modele, scope = list(lower = str_constant, upper = str_all), 
                           trace = TRUE, data = base, direction = "both")

# affichage du modèle final
summary(modele.stepwise)


#Réalisation d'un autre modèle de régression logisitique

logit = function(formula, lien = "logit", data = NULL) {
  glm(formula, family = binomial(link = lien), data)
}

m.logit <- logit(cible ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15, data = base)
# résultats du modèle
summary(m.logit)



install.packages("effects", repos = "http://R-Forge.R-project.org")

#La fonction predict avec l'argument type="response" permet d'appliquer notre modele logistique a
#un tableau de donnees et renvoie pour chaque individu la probabilite qu'il ait vecu le phenomene etudie.

cible.pred <- predict(modele, type = "response", newdata = base)
head(base$model.pred)

#Une manière de tester la qualite d'un modele est le calcul d'une matrice de confusion, c'est-a-dire le
#tableau croisé des valeurs observees et celles des valeurs predites en appliquant le modele aux donnees
#d'origine.

table(cible.pred > 0.5, base$cible)

