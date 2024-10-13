#Installation des packages 
install.packages("CCA")
install.packages("ggplot2")

library(CCA)
library(ggplot2)


#Jeu de données
data(mtcars)
summary(mtcars)



#Création de deux matrices 
##Sélection des groupes de variables : 
##En séparant les variables en caractéristiques de conception (X) et performances (Y), 
##pour examiner comment les caractéristiques techniques affectent les performances.
X <- as.matrix(mtcars[, c(2,3,5,6,8,11)])  # Groupe 1 : caractéristiques de conception
Y <- as.matrix(mtcars[, c(1,4,7,9,10)])  # Groupe 2 : caractéristiques de performances



#Analyse canonique des corrélations avec la fonction cc() :
#fournit des résultats sous forme de scores canoniques
#qui représentent les projections des données originales sur les axes canoniques
ccs <- cc(X, Y)

#Calcul manuel de la corrélation entre les scores des premières et des deuxièmes 
#paires canoniques (axes canoniques) obtenus à partir des résultats de cc()
ufvcc1 <- ccs$scores$xscores[ , 1]
dfvcc1 <- ccs$scores$yscores[ , 1]
cat ("Correlation of the first canonical pair: ",cor(ufvcc1, dfvcc1))

ufvcc2 <- ccs$scores$xscores[ , 2]
dfvcc2 <- ccs$scores$yscores[ , 2]
cat ("\nCorrelation of the second canonical pair: ",cor(ufvcc2, dfvcc2))


#Analyse canonique des corrélations avec la fonction cancor():
#Cette fonction retourne directement les coefficients de corrélations canoniques, les vecteurs canoniques
#et les coefficients de corrélation entre les groupes de variables.
cancor_results <- cancor(X, Y)
print(cancor_results)



#Visualisation des corrélations canoniques
#Créer un data frame avec les corrélations et les noms des axes canoniques
correlation_values <- ccs$cor
correlation_results <- data.frame(
  Canonical = paste("Canonical", 1:length(correlation_values)),
  Corrélation = correlation_values
)

#Tracer les corrélations canoniques
ggplot(data = correlation_results, aes(x = Canonical, y = Corrélation)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Corrélations canoniques entre les groupes de variables",
       x = "Axes canoniques",
       y = "Corrélation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
#ce graphique permet de comparer facilement la force des corrélations entre les différents axes canoniques.



#Création d'un graphique de dispersion montrant les scores canoniques pour les deux premiers axes canoniques,
#permettant d'observer les relations entre les scores canoniques du groupe X et du groupe Y.
##Extraction des scores canoniques
ufvcc1 <- ccs$scores$xscores[ , 1]
dfvcc1 <- ccs$scores$yscores[ , 1]

#Préparation des scores extrêmes
sdr <- sort(ufvcc1)
sdr <- sdr[c(1, length(sdr) - 1)]
ext <- match(sdr, ufvcc1)

#Création du graphique
plot( ufvcc1, dfvcc1, cex.lab = 1.5,
      xlab = "User features canonical scores", 
      ylab = "Driver features canonical scores",
      pch = 16, cex = 1, col = "red", 
      xlim = sdr * c(1.3, 2), 
      frame.plot=FALSE)
text(ufvcc1, dfvcc1, 
     labels = rownames(mtcars),
     pos = 4, 
     cex = 0.6, 
     col = "blue")
title("Premier Axe Canonique")

# Any interesting pattern or grouping in the plot?
# Answer: First canonical pair plot identifies the two extreme.
# 
# Smaller car in upper right hand side corner (Fiat 128)
# Large luxury car in lower left corner (Cadillac Fleetwood)
# There is some evidence that there are groups of individual cars on this scale. There are four cars located near the Cadillac Fleetwood The other three are Chrysler Imperial, Lincoln Continental, and Maserati Bora.
# 
# The first canonical variate for User Features is plotted against the first canonical variate for Driver Features in the above scatter plot for the first canonical variate pair. The regression line shows how well the data fits.


# Création du graphique pour le deuxième axe canonique
plot(ufvcc2, dfvcc2, cex.lab = 1.5,
     xlab = "User features canonical scores", 
     ylab = "Driver features canonical scores",
     pch = 16, cex = 1, col = "violet",
     xlim = sdr * c(1.3, 2), 
     frame.plot=FALSE)
text(ufvcc2, dfvcc2, 
     labels = rownames(mtcars),
     pos = 4,
     cex = 0.6,
     col = "darkgreen")
title("Deuxième Axe Canonique")







#Visualisation des Poids des Variables : on ne voit pas les vecteurs ?!
plt.cc(ccs, d1 = 1, d2 = 2, type = "v", var.label = TRUE)


#ne fonctionne pas !!
# Récupérer les scores canoniques
xscores <- ccs$scores$xscores  # Scores canoniques pour le groupe X
yscores <- ccs$scores$yscores  # Scores canoniques pour le groupe Y

# Calculer les poids des variables à partir des scores canoniques
weights_X <- apply(xscores, 2, function(x) cor(x, Y[,1]))  # Exemple pour le premier groupe
weights_Y <- apply(yscores, 2, function(y) cor(X[,1], y))

# Tracer les poids des variables
ggplot(weights_df, aes(x = Variable, y = Poids, fill = Groupe)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Poids des Variables dans l'Analyse Canonique des Corrélations",
       x = "Variables",
       y = "Poids") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Ajuste l'angle des étiquettes de l'axe x
