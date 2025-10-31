library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(corrplot)


#Hacemos una copia de las tablas

tiendas <- Affiliated_Outlets
reparto <- DeliveryDay
oos <- OoSDay
ruta <- RouteDay
ventas <- SalesDay
producto<-Product

#Cambiamos el formato de las fechas a date ya que en los datos originales no se nos indican las horas
reparto$Delivery_DAY <-as.Date(reparto$Delivery_DAY)
oos$OoS_DAY <-as.Date(oos$OoS_DAY)
ruta$Route_DAY<-as.Date(ruta$Route_DAY)
ventas$Sales_DAY<-as.Date(ventas$Sales_DAY)

#Estructura de las variables y resumen
str(tiendas)
summary(tiendas)

str(reparto)
summary(reparto)

str(ventas)
summary(ventas)

str(oos)
summary(oos)

str(ruta)
summary(ruta)

str(producto)
summary(producto)

#No hay valores nulos

## --------------------------------
## ANÁLISIS EXPLORATORIO DE DATOS
## --------------------------------

#Eliminamos los valores negativos
ventas <- ventas %>% filter(Sales_Uds >= 0)
reparto <- reparto %>% filter(Delivery_Uds >= 0)

# Outliers en ventas
boxplot(ventas$Sales_Uds, main = "Boxplot de Ventas - Detección de Outliers", col = "lightblue")

# Outliers en entregas
boxplot(reparto$Delivery_Uds, main = "Boxplot de Entregas - Detección de Outliers", col = "lightgreen")

# Boxplot de ventas por tienda
ventas %>%
  group_by(Affiliated_Code) %>%
  summarise(total_ventas = sum(Sales_Uds)) %>%
  ggplot(aes(x = "", y = total_ventas)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Boxplot del Total de Ventas por Tienda", y = "Total Ventas")

 
# Histograma de entregas
ggplot(reparto, aes(x = Delivery_Uds)) +
  geom_histogram(bins = 50, fill = "lightgreen", color = "black") +
  theme_minimal() +
  labs(title = "Histograma de Unidades Entregadas", x = "Unidades Entregadas", y = "Frecuencia")

# Evolución de ventas en el tiempo
ventas %>%
  mutate(Semana = floor_date(Sales_DAY, "week")) %>%
  group_by(Semana) %>%
  summarise(Ventas_Semana = sum(Sales_Uds)) %>%
  ggplot(aes(x = Semana, y = Ventas_Semana)) +
  geom_line(color = "blue") +
  theme_minimal() +
  labs(title = "Ventas semanales", x = "Semana", y = "Ventas")

# Evolución de entregas en el tiempo
reparto %>%
  mutate(Semana = floor_date(Delivery_DAY, "week")) %>%
  group_by(Semana) %>%
  summarise(Entregas_Semana = sum(Delivery_Uds)) %>%
  ggplot(aes(x = Semana, y = Entregas_Semana)) +
  geom_line(color = "green") +
  theme_minimal() +
  labs(title = "Entregas semanales", x = "Semana", y = "Entregas")

# Tiempo medio entre entregas por tienda
frecuencia_entregas <- reparto %>%
  arrange(Affiliated_Code, Delivery_DAY) %>%
  group_by(Affiliated_Code) %>%
  mutate(dias_entre = as.numeric(Delivery_DAY - lag(Delivery_DAY))) %>%
  summarise(media_dias_entre_entregas = mean(dias_entre, na.rm = TRUE))

summary(frecuencia_entregas$media_dias_entre_entregas)

# Roturas por producto
oos %>%
  group_by(Product_Code) %>%
  summarise(Roturas = n()) %>%
  arrange(desc(Roturas)) %>%
  top_n(10, Roturas) %>%
  ggplot(aes(x = reorder(Product_Code, Roturas), y = Roturas)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 10 Productos con más Roturas", x = "Producto", y = "Nº de Roturas")

# Roturas por tienda
oos %>%
  group_by(Affiliated_Code) %>%
  summarise(Roturas = n()) %>%
  arrange(desc(Roturas)) %>%
  top_n(10, Roturas) %>%
  ggplot(aes(x = reorder(Affiliated_Code, Roturas), y = Roturas)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 10 Tiendas con más Roturas", x = "Tienda", y = "Nº de Roturas")

# MATRIZ DE CORRELACIÓN
# Cruzamos datos de ventas, reparto y tiendas
ventas_reparto <- ventas %>%
  group_by(Affiliated_Code) %>%
  summarise(Sales_Total = sum(Sales_Uds))

reparto_total <- reparto %>%
  group_by(Affiliated_Code) %>%
  summarise(Delivery_Total = sum(Delivery_Uds))

correlacion_data <- ventas_reparto %>%
  left_join(reparto_total, by = "Affiliated_Code") %>%
  left_join(tiendas %>% select(Affiliated_Code, Engage, Management_Cluster), by = "Affiliated_Code") %>%
  drop_na()

# Matriz de correlación
cor_matrix <- cor(correlacion_data %>% select(-Affiliated_Code))

# Graficar la matriz
corrplot(cor_matrix, method = "color", addCoef.col = "black", number.cex = 0.7, tl.cex = 0.8)

## ------------------------------
## MODELO PREDICTIVO ARIMA VENTAS
## ------------------------------

# Preparación de datos para series temporales
library(forecast)
library(tseries)
library(mFilter)

# Agregamos ventas por semana
ventas_semanales <- ventas %>%
mutate(Semana = floor_date(Sales_DAY, "week")) %>%
  group_by(Semana) %>%
  summarise(Ventas_Semana = sum(Sales_Uds))%>%
  complete(Semana = seq.Date(min(Semana), max(Semana), by="week"), 
           fill = list(Ventas_Semana = 0)) 

ggplot(ventas_semanales, aes(x = Semana, y = Ventas_Semana)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  ggtitle("Ventas Semanales") +
  xlab("Fecha") +
  ylab("Unidades Vendidas") +
  theme_minimal()

#Vamos a predecir los datos para 4 semanas aprox.
entrenamiento <- ventas_semanales[1:23,2]
validacion<- ventas_semanales[24:27,2]


entrenamiento_st <- ts(entrenamiento, frequency = 52,  # 52 semanas = 1 año
                       start = c(2015,1))

validacion_st <- ts(validacion,frequency = 52,   
                    start = c(2015,24))

# Modelo ARIMA semanal
modelo_arima <- auto.arima(entrenamiento_st)
prediccion <- forecast(modelo_arima, 4)
plot(prediccion)

prediccion$mean
predicciondatos<-prediccion$mean
plot(predicciondatos)


#Descomposición
descomposicion <- hpfilter(entrenamiento_st, freq=52)
tendenciaventasmensuales<-descomposicion$trend
plot(descomposicion$trend)


#Validación de los datos
plot(validacion_st)
lines(predicciondatos,col="blue")
accuracy(validacion_st,predicciondatos)


## ---------------------------------------
## MODELO PREDICTIVO ARIMA SERIE INCOMPLETA
## ---------------------------------------

# Empleamos 10 semanas
r <- ventas_semanales[1:10, 2]


r_serie <- ts(r$Ventas_Semana, frequency = 52, start = c(2015, 10))  # Semana 10 del año 2015
modelo_Rarima <- auto.arima(r_serie)
prediccionR <- forecast(modelo_Rarima, h = 17)

 
prediccionR$mean
prediccionRdatos<-prediccionR$mean
plot(prediccionRdatos)

plot(r_serie)


## --------------------------------
## MODELO PREDICTIVO ARIMA ENTREGAS
## --------------------------------

entregas_semanales <- reparto %>%
  mutate(Semana = floor_date(Delivery_DAY, "week")) %>%
  group_by(Semana) %>%
  summarise(Entregas_Semana = sum(Delivery_Uds))%>%
  complete(Semana = seq.Date(min(Semana), max(Semana), by="week"), 
           fill = list(Entregas_Semana = 0)) 

ggplot(entregas_semanales, aes(x = Semana, y = Entregas_Semana)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  ggtitle("Entregas Semanales") +
  xlab("Fecha") +
  ylab("Unidades Entregadas") +
  theme_minimal()


#Vamos a predecir los datos para 5 semanas aprox.
entrenamiento_entregas <- entregas_semanales[1:27,2]
validacion_entregas<- entregas_semanales[28:32,2]


entrenamiento_entregas_st <- ts(entrenamiento_entregas, frequency = 52,  # 52 semanas = 1 año
                       start = c(2015,1))

validacion_entregas_st <- ts(validacion_entregas, frequency = 52,   
                    start = c(2015,28))


# Modelo ARIMA semanal
modelo_arima_entregas <- auto.arima(entrenamiento_entregas_st)

prediccion_entregas <- forecast(modelo_arima_entregas, 5)
plot(prediccion_entregas)

prediccion_entregas$mean
predicciondatos_entregas<-prediccion_entregas$mean
plot(predicciondatos_entregas)

#Descomposición
descomposicion_entregas <- hpfilter(entrenamiento_entregas_st, freq=52)
tendenciaentregasmensuales<-descomposicion_entregas$trend
plot(descomposicion_entregas$trend)


#Validación de los datos
plot(validacion_entregas_st)
lines(predicciondatos_entregas,col="blue")

autoplot(validacion_entregas_st, series = "Real") +
  autolayer(predicciondatos_entregas, series = "Predicción", color = "blue") +
  ggtitle("Validación vs Predicción - ARIMA") +
  ylab("Entregas Semanales") +
  xlab("Semana") +
  theme_minimal()

accuracy(validacion_entregas_st,predicciondatos_entregas)

## --------------------------------------
## PREDICCION PARA LAS PRÓXIMAS 5 SEMANAS
## --------------------------------------

entregas_final <- ts(entregas_semanales$Entregas_Semana, frequency = 52,  # 52 semanas = 1 año
                                start = c(2015,11))
# Modelo ARIMA semanal
modelo_entregas_final <- auto.arima(entregas_final)

prediccion_entregas_final <- forecast(modelo_entregas_final, 5)
plot(prediccion_entregas_final)

prediccion_entregas_final$mean
predicciondatos_entregas<-prediccion_entregas_final$mean


#Descomposición
descomposicion_entregas_final <- hpfilter(entregas_final, freq=52)
tendenciaentregasmensuales_final<-descomposicion_entregas_final$trend
plot(descomposicion_entregas_final$trend)


## ---------------------------
## SEGMENTACIÓN CON K-MEANS
## ---------------------------

# Preparación de datos para clustering
library(cluster)
library(factoextra)

# Creamos un dataset con características de las tiendas
tiendas_cluster <- ventas %>%
  group_by(Affiliated_Code) %>%
  summarise(
    total_ventas = sum(Sales_Uds),
    frecuencia_compra = n(),
    productos_distintos = n_distinct(Product_Code)
  ) %>%
  left_join(
    oos %>%
      group_by(Affiliated_Code) %>%
      summarise(roturas = n()),
    by = "Affiliated_Code"
  ) %>%
  left_join(
    reparto %>%
      group_by(Affiliated_Code) %>%
      summarise(
        total_entregas = sum(Delivery_Uds),
        frecuencia_entrega = n()
      ),
    by = "Affiliated_Code"
  ) %>%
  left_join(
    tiendas %>% select(Affiliated_Code, Engage, Management_Cluster, Tam_m2),
    by = "Affiliated_Code"
  ) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(roturas = ifelse(is.na(roturas), 0, roturas))

tiendas_cluster<-tiendas_cluster %>% 
  select(-Affiliated_Code, -Tam_m2)

str(tiendas_cluster)

# Determinación del número óptimo de clusters

fviz_nbclust(tiendas_cluster, kmeans, method = "silhouette") +
  ggtitle("Método de Silouette para determinar número óptimo de clusters")


fviz_nbclust(tiendas_cluster, kmeans, method = "wss") +
  ggtitle("Método del codo para determinar el número óptimo de clusters")

# Aplicación de K-Means con k=3
kmeans_result <- kmeans(tiendas_cluster, 3)
kmeans(tiendas_cluster, 3)

# Calcular coeficiente de silouette
sil <- silhouette(kmeans_result$cluster, dist(tiendas_cluster))

# Mostrar promedio del coeficiente de silouette
sil_promedio <- mean(sil[, 3])
cat("Coeficiente promedio de silouette:", round(sil_promedio, 3), "\n")

# Visualización de los clusters

summary(tiendas_cluster)
str(tiendas_cluster)
fviz_cluster(kmeans_result, data = tiendas_cluster,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point")

# Añadimos los clusters al dataset original
tiendas_cluster$cluster <- as.factor(kmeans_result$cluster)

# Visualización de características por cluster
ggplot(tiendas_cluster, aes(x = total_ventas, y = roturas, color = cluster)) +
  geom_point(alpha = 0.6) +
  ggtitle("Segmentación de Tiendas por Ventas y Roturas") +
  xlab("Total Ventas") +
  ylab("Total Roturas")

# Guardamos los resultados del clustering
write.csv(tiendas_cluster, "segmentacion_tiendas_kmeans.csv")



## --------------------------------------
#####K-MEANS CON DATOS NORMALIZADOS#####
## --------------------------------------
tiendas_cluster_normalizadas <- ventas %>%
  group_by(Affiliated_Code) %>%
  summarise(
    total_ventas = sum(Sales_Uds),
    frecuencia_compra = n(),
    productos_distintos = n_distinct(Product_Code)
  ) %>%
  left_join(
    oos %>%
      group_by(Affiliated_Code) %>%
      summarise(roturas = n()),
    by = "Affiliated_Code"
  ) %>%
  left_join(
    reparto %>%
      group_by(Affiliated_Code) %>%
      summarise(
        total_entregas = sum(Delivery_Uds),
        frecuencia_entrega = n()
      ),
    by = "Affiliated_Code"
  ) %>%
  left_join(
    tiendas %>% select(Affiliated_Code, Engage, Management_Cluster, Tam_m2),
    by = "Affiliated_Code"
  ) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(roturas = ifelse(is.na(roturas), 0, roturas))

tiendas_cluster_normalizadas<-tiendas_cluster_normalizadas %>% 
  select(-Affiliated_Code, -Tam_m2)

# Determinación del número óptimo de clusters

fviz_nbclust(tiendas_cluster_normalizadas,kmeans)

fviz_nbclust(tiendas_cluster_normalizadas, kmeans, method = "silhouette") +
  ggtitle("Método de Siluette para Determinar Número Óptimo de Clusters")

# Aplicación de K-Means normalizado con k=3
kmeans_norm <- kmeans(scale(tiendas_cluster_normalizadas), 3)
kmeans(scale(tiendas_cluster_normalizadas), 3)

# Calcular coeficiente de silouette
sil1 <- silhouette(kmeans_norm$cluster, dist(tiendas_cluster_normalizadas))

# Mostrar promedio del coeficiente de silouette
sil_promedio <- mean(sil1[, 3])
cat("Coeficiente promedio de silouette:", round(sil_promedio, 3), "\n")


# Visualización de los clusters

summary(tiendas_cluster_normalizadas)
str(tiendas_cluster_normalizadas)
fviz_cluster(kmeans_result, data = tiendas_cluster_normalizadas,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point")

# Añadimos los clusters al dataset original
tiendas_cluster_normalizadas$cluster <- as.factor(kmeans_norm$cluster)

# Visualización de características por cluster
ggplot(tiendas_cluster_normalizadas, aes(x = total_ventas, y = roturas, color = cluster)) +
  geom_point(alpha = 0.6) +
  ggtitle("Segmentación de Tiendas por Ventas y Roturas") +
  xlab("Total Ventas") +
  ylab("Total Roturas")

# Guardamos los resultados del clustering
write.csv(tiendas_cluster_normalizadas, "segmentacion_tiendas_normalizadas_kmeans.csv")


