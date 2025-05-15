library(randomForest)
library(caret)

# Convertir Recycling_Programs a factor si no lo está
sustainableFashion$Recycling_Programs <- as.factor(sustainableFashion$Recycling_Programs)
sustainableFashion$Sustainability_Rating <- as.factor(sustainableFashion$Sustainability_Rating)

# Definir las características y la variable objetivo
X <- sustainableFashion[, c("Sustainability_Rating", "Eco_Friendly_Manufacturing", "Carbon_Footprint_MT", 
                               "Water_Usage_Liters", "Waste_Production_KG", "Recycling_Programs", 
                               "Average_Price_USD")]
y <- sustainableFashion$Market_Trend
# Librerías necesarias
library(xgboost)
library(caret)

# Supón que tu conjunto de datos ya está preparado, con variables como 'Market_Trend' como objetivo
# y las variables relacionadas con la sostenibilidad como predictoras

# Preparar los datos
# Convertir variables categóricas a factor
sustainableFashion$Market_Trend <- as.factor(sustainableFashion$Market_Trend)

# Separar las variables predictoras (X) y la variable objetivo (y)
X_train <- sustainableFashion[, c("Sustainability_Rating", "Eco_Friendly_Manufacturing", "Carbon_Footprint_MT", 
                          "Water_Usage_Liters", "Waste_Production_KG", "Recycling_Programs", 
                          "Average_Price_USD")]
y_train <- sustainableFashion$Market_Trend

str(X_train)  # Ver la estructura de los datos
X_train <- data.frame(lapply(X_train, function(x) as.numeric(as.factor(x))))
X_train_matrix <- as.matrix(X_train)  # Convertir a matriz numérica
dtrain <- xgb.DMatrix(data = X_train_matrix, label = as.numeric(y_train) - 1)  # Convertir a índice basado en 0

# Convertir las variables en formato adecuado para XGBoost
dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = as.numeric(y_train) - 1)  # Convertir a 0-based index

# Definir los parámetros del modelo XGBoost
params <- list(
  objective = "multi:softmax",  # Clasificación multiclase
  num_class = length(unique(y_train)),  # Número de clases en Market_Trend
  eta = 0.1,  # Tasa de aprendizaje
  max_depth = 6,  # Profundidad máxima de los árboles
  subsample = 0.8,  # Porcentaje de datos a usar para cada árbol
  colsample_bytree = 0.8  # Proporción de características a usar por árbol
)

# Entrenar el modelo con XGBoost
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100  # Número de iteraciones
)

# Predicciones sobre datos de prueba
X_test <- sustainableFashion[, c("Sustainability_Rating", "Eco_Friendly_Manufacturing", "Carbon_Footprint_MT", 
                        "Water_Usage_Liters", "Waste_Production_KG", "Recycling_Programs", 
                        "Average_Price_USD")]
y_test <- sustainableFashion$Market_Trend

str(X_test)
X_test <- data.frame(lapply(X_test, function(x) as.numeric(as.factor(x))))
X_test_matrix <- as.matrix(X_test)  # Convertir a matriz numérica
dtest <- xgb.DMatrix(data = X_test_matrix)

predictions <- predict(xgb_model, dtest)

# Convertir las predicciones a las clases originales
predictions <- factor(predictions, levels = 0:(length(unique(y_train)) - 1), labels = levels(y_train))

# Evaluar el modelo
confusion <- confusionMatrix(predictions, y_test)
print(confusion)

# Entrenar el modelo XGBoost (ya lo has entrenado en tu código)
# Obtener la importancia de las características
importance_xgb <- xgb.importance(model = xgb_model)

# Visualizar la importancia de las características
xgb.plot.importance(importance_xgb, main = "Importancia de las Características (XGBoost)")

# AUC-ROC
library(pROC)
roc_curve <- roc(as.numeric(y_test) - 1, as.numeric(predictions) - 1)
auc(roc_curve)

# Generar la curva ROC
roc_curve <- roc(as.numeric(y_test) - 1, as.numeric(predictions) - 1)

# Visualizar la curva ROC
plot(roc_curve, main = "Curva ROC", col = "blue", lwd = 2)

# Supongamos que tienes los datos cargados en un dataframe llamado `sustainableFashion`

# Crear segmentos de precio
sustainableFashion$Price_Segment <- ifelse(sustainableFashion$Average_Price_USD < 20, 'Bajo',
                                           ifelse(sustainableFashion$Average_Price_USD <= 50, 'Medio', 'Alto'))
##################################################################################

# Escalar los datos numéricos
data1_scaled <- scale(data1_selected_numeric)

# Cargar la librería para el gráfico del codo
library(factoextra)

# Determinar el número óptimo de clústeres con el método del codo
fviz_nbclust(data1_scaled, kmeans, method = "wss")

# Aplicar el algoritmo K-means
set.seed(123)  # Para reproducibilidad
kmeans_model1 <- kmeans(data1_scaled, centers = 3)

# Ver los resultados del modelo
kmeans_model1

# Ver las primeras filas del dataframe con los clústeres
head(data1_clustered)

# Aplicar PCA
pca_result <- prcomp(data2_scaled, center = TRUE, scale. = TRUE)
# Seleccionar las primeras dos componentes principales para visualizar
pca_data <- data.frame(pca_result$x)
# Ver la varianza explicada
summary(pca_result)

# Escalar los datos
data2_scaled <- scale(data2_selected)

# Determinar el número óptimo de clústeres con el método del codo
fviz_nbclust(data2_scaled, kmeans, method = "wss")

# Aplicar el algoritmo K-means
set.seed(123)  # Para reproducibilidad
# Ajustar K-means con múltiples reinicios
kmeans_model2 <- kmeans(data2_scaled, centers = 4, nstart = 25)

# Ver los resultados del modelo
kmeans_model2

# Ver las primeras filas del dataframe con los clústeres
head(data2_clustered)

# Visualizar los resultados del primer modelo
library(ggplot2)

ggplot(data1_clustered, aes(x = Sustainability_Rating, y = Average_Price_USD, color = as.factor(Cluster))) +
  geom_point(size = 3) +
  labs(title = "Segmentación de Precios según Ecología",
       x = "Nivel de Ecología",
       y = "Precio de la Prenda",
       color = "Cluster") +
  theme_minimal()

# Visualizar los resultados del segundo modelo
ggplot(data2_clustered, aes(x = Age, y = Purchase.Amount..USD., color = as.factor(Cluster))) +
  geom_point(size = 4) +
  labs(title = "Segmentación del Gasto en Ropa según Edad",
       x = "Edad",
       y = "Gasto en Ropa",
       color = "Cluster") +
  theme_minimal()

# Calcular el índice de silueta
library(cluster)
silhouette_score1 <- silhouette(kmeans_model1$cluster, dist(data1_scaled))
plot(silhouette_score1)

silhouette_score2 <- silhouette(kmeans_model2$cluster, dist(data2_scaled))
plot(silhouette_score2)
