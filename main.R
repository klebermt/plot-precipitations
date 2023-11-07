#variabilidad mensual
source('tabla.R')
#ruta de la carpeta
ruta_carpeta <- "data"

# Utiliza list.files() para obtener la lista de archivos en la carpeta
archivos_en_carpeta <- list.files(path = ruta_carpeta, pattern = "\\.csv$", full.names = TRUE)

# Crear una lista de dataframes
lista_de_dataframes <- list()

# Recorre la lista de archivos y crea dataframes
for (archivo in archivos_en_carpeta) {
  # Extrae el nombre del archivo sin la extensión
  nombre_dataframes <- sub(".csv$", "", basename(archivo))
  
  # Carga el archivo CSV en un dataframe con el mismo nombre
  lista_de_dataframes[[nombre_dataframes]] <- read.csv(archivo, header = FALSE, sep = ' ')
}

#Damos forma a la tabla
for (archivo in archivos_en_carpeta) {
  # Extrae el nombre del archivo sin la extensión
  nombre_dataframes <- sub(".csv$", "", basename(archivo))
  
  # Carga el archivo CSV en un dataframe con el mismo nombre
  lista_de_dataframes[[nombre_dataframes]] <- setTable(lista_de_dataframes[[nombre_dataframes]])
}

#Agregamos la media mensual
df_estaciones <- addMean(lista_de_dataframes)

# Carga el paquete ggplot2 si aún no lo tienes instalado
library(ggplot2)

# Reorganizar el dataframe de ancho a largo (tidy data)
library(tidyr)
nuevo_dataframe_long <- gather(df_estaciones, mes, precipitacion, -estaciones)

# Crear el gráfico de línea
ggplot(nuevo_dataframe_long, aes(x = mes, y = precipitacion, group = estaciones, color = estaciones)) +
  geom_line() +
  labs(title = "Precipitación por Mes (Estaciones)",
       x = "Mes",
       y = "Precipitación") +
  scale_x_discrete(limits = unique(nuevo_dataframe_long$mes)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = -45, hjust = 0),  # Rotar los nombres de los meses
        axis.title = element_text(size = 12))  # Ajustar el tamaño de la fuente del título del eje
