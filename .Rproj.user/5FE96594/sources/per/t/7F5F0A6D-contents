View(data)
summary(data)
str(data)
na.fail(data)
# Pas de données manquantes

attach(data)
# Encodage des variables cibles ( SUMMARY, PRECIP) en utilisant Label Encoding
# la fonction factor() est utilisée pour convertir les variables SUMMARY et PRECIP.TYPE en facteurs,
# puis as.numeric() est utilisée pour convertir ces facteurs en valeurs numériques.
SUMMARY_enc<-as.numeric(as.factor(SUMMARY))
PRECIP.TYPE_enc<-as.numeric(as.factor(PRECIP.TYPE))

# On divise la dataset en "training and testing sets" en utilisant la methode "sample()"
set.seed(123)
sample_index<-sample(1:nrow(data), 0.8*nrow(data))
train_data<-data[sample_index,]
test_data<-data[-sample_index,]

str(train_data)

missing_values <- complete.cases(train_data$SUMMARY, train_data$TEMPERATURE..C., train_data$WIND.SPEED..km.h., train_data$PRESSURE..millibars., train_data$HUMIDITY)
train_data <- train_data[missing_values, ]

set.seed(123)
# Construction des modeles de regression linéaire multiple en utilisant methode "lm()"
summary_model<-lm(SUMMARY_enc ~ TEMPERATURE..C. + WIND.SPEED..km.h. + PRESSURE..millibars. + HUMIDITY, data = train_data)

