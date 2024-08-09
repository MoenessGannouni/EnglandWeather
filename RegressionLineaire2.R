View(data)
summary(data)
str(data)
# data cleaning
missing_values<-sum(is.na(data))
missing_values

attach(data)

install.packages("caTools") 
library(caTools)

# Diviser les données en ensembles d'entraînement (80%) et de test (20%)
set.seed(123) # Pour rendre les résultats reproductibles

# Diviser les données en ensembles d'entraînement (80%) et de test (20%)
split <- sample.split(data$TEMPERATURE..C., SplitRatio = 0.8)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)




#model de la regression lineaire multiple
model1 <- lm(TEMPERATURE..C. ~ WIND.SPEED..km.h. + PRESSURE..millibars. + HUMIDITY, data=train_data)

summary(model1)

# Faire des prédictions sur l'ensemble de test
predictions <- predict(model1, newdata=test_data)

# Calculer l'erreur de prédiction (RMSE par exemple)
rmse <- sqrt(mean((predictions - test_data$TEMPERATURE..C.)^2))
print(paste("RMSE:", rmse))


# Prendre un échantillon aléatoire de 10 valeurs à partir de l'ensemble de test
sample_indices <- sample(1:nrow(test_data), 10)
sample_data2 <- test_data[sample_indices, ]

# Faire des prédictions sur l'échantillon de test
sample_predictions <- predict(model1, newdata=sample_data2)

# Créer un tableau avec les données de l'échantillon et les prédictions
sample_results <- cbind(sample_data2, Predicted_Temperature=sample_predictions)

# Afficher l'échantillon avec les prédictions
print(sample_results)















  # Affichage des points et des prédictions sur l'ensemble de test pour WIND.SPEED..km.h.
ggplot(test_data, aes(x=WIND.SPEED..km.h., y=TEMPERATURE..C.)) +
  geom_point() +
  geom_line(aes(y=predictions), color='blue', size=1) +
  xlab("Vitesse du vent (km/h)") +
  ylab("Température (°C)") +
  ggtitle("Régression linéaire multiple : Température en fonction de la vitesse du vent")




# Équation linéaire
equation <- as.expression(bquote(italic("Température (°C)") == .(round(coef(model)[1], 2)) ~ "+" ~ .(round(coef(model)[2], 2)) ~ "*" ~ italic("Vitesse du vent (km/h)") ~ "+" ~ .(round(coef(model)[3], 2)) ~ "*" ~ italic("Pression (millibars)") ~ "+" ~ .(round(coef(model)[4], 2)) ~ "*" ~ italic("Humidité")))


# Affichage des points, des prédictions et de l'équation linéaire sur l'ensemble de test pour WIND.SPEED..km.h.
ggplot(test_data, aes(x=WIND.SPEED..km.h., y=TEMPERATURE..C.)) +
  geom_point() +
  geom_line(aes(y=predictions), color='blue', size=1) +
  annotate("text", x=20, y=30, label=equation, parse=TRUE, size=4) +
  xlab("Vitesse du vent (km/h)") +
  ylab("Température (°C)") +
  ggtitle("Régression linéaire multiple : Température en fonction de la vitesse du vent, de la pression et de l'humidité")
