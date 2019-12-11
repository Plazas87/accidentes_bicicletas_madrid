library(dplyr)
library(leaflet)
library(reshape2)
library(lubridate)
library(curl)
library(ggplot2)
library(ggmap)
library(pander)


# Obtener el dataset desde la página de datos abiertos de Madrid
accidentes_bici <- read.csv(curl("https://datos.madrid.es/egob/catalogo/300110-18-accidentes-bicicleta.csv"),
                            sep = ";",
                            header = T,
                            encoding = 'UTF-8',
                            colClasses = "character")


# Adecuar la tabla y limpiar los datos
accidentes_bici$X..La.correspondencia.de.los.c.f3.digos.se.encuentra.descrito.en.la.estructura.del.fichero. <- NULL
colnames(accidentes_bici) <- c("N_EXPEDIENTE","FECHA","HORA","CALLE","NUMERO","DISTRITO",
                               "TIPO_ACCIDENTE", "ESTADO_METEREOLOGICO", "TIPO_VEHICULO", 
                               "TIPO_PERSONA", "RANGO_DE_EDAD", "SEXO", "LESIVIDAD")


# Arreglo de encodings. Debido a los diferentes simbolos que tiene el el archivo y los nombres originales de las columnas, fue 
# necesario realizar el siguiente procedimiento.
accidentes_bici$CALLE <- as.character(accidentes_bici$CALLE) 
accidentes_bici$TIPO_ACCIDENTE <- as.character(accidentes_bici$TIPO_ACCIDENTE) 
accidentes_bici$ESTADO_METEREOLOGICO <-as.character(accidentes_bici$ESTADO_METEREOLOGICO)
accidentes_bici$TIPO_PERSONA <- as.character(accidentes_bici$TIPO_PERSONA)
accidentes_bici$RANGO_DE_EDAD <- as.character(accidentes_bici$RANGO_DE_EDAD)
accidentes_bici$DISTRITO <- as.character(accidentes_bici$DISTRITO) 

Encoding(accidentes_bici$CALLE) <- 'unicode'
Encoding(accidentes_bici$TIPO_ACCIDENTE) <- 'unicode'
Encoding(accidentes_bici$ESTADO_METEREOLOGICO) <- 'unicode'
Encoding(accidentes_bici$TIPO_PERSONA) <- 'unicode'
Encoding(accidentes_bici$RANGO_DE_EDAD) <- 'unicode'
Encoding(accidentes_bici$DISTRITO) <- 'unicode'

# Finalmente se guarda la tabla en la carpeta como csv en utt-8 dependencias para su uso posterior 
write.csv(accidentes_bici, file = 'resources/AccidentesBicicletas_2019.csv', fileEncoding = 'UTF-8')

# Leer la tabla limpia y con el formato utf-8 correcto para el análisis
accidentes_bici <- read.csv("resources/AccidentesBicicletas_2019.csv",
                            sep = ",",
                            header = T,
                            encoding = 'UTF-8',
                            colClasses = "character")


# Separar la columan de fecha en DIA, MES, AÑO 
accidentes_bici$FECHA <- dmy(accidentes_bici$FECHA)
accidentes_bici$ANO <- year(accidentes_bici$FECHA)
accidentes_bici$MES <- month(accidentes_bici$FECHA)
accidentes_bici$DIA <- day(accidentes_bici$FECHA)
#accidentes_bici$FECHA <- NULL 


# Segun la información del dataset los valores de levisidad que corresponden "sin asistencia sanitaria" son 14 y "", por ello,
# se remplazan los valores "" por 14 
accidentes_bici$LESIVIDAD <- sapply(accidentes_bici$LESIVIDAD, function(x){
  if (x == "") x <- "14"
  else x
})

# -------------------------------------------------------- Area de graficos 

# ---------------------Grafico del total de accidentes
# Totol de accidentes registrados 
total_accidentes <- nrow(accidentes_bici)
total_accidentes <- paste(c('Total de accidentes registrados:', as.character(total_accidentes)), collapse = " ")

# ---------------------- Numero de accidentes por su gravedad
# Grafico de número 
g <- ggplot(accidentes_bici, aes(LESIVIDAD, fill= LESIVIDAD)) + 
  geom_bar() + 
  labs(x = "Gravedad del accidente", y = 'Número de accidentes', caption = total_accidentes ) + 
  scale_fill_brewer(palette = 'Paired') +
  theme_minimal()
g 


# -----------------------------------Grafica de accidentes por mes
accidentes_bici$MES_NAMES <- 0
add_months_names <- function(x){
  if (x == 1) {
    x <- "Enero"
  }
  if (x == 2) {
    x <- "Febrero"
  }
  if (x == 3) {
    x <- "Marzo"
  }
  if (x == 4) {
    x <- "Abril"
  }
  if (x == 5) {
    x <- "Mayo"
  }
  if (x == 6) {
    x <- "Junio"
  }
  if (x == 7) {
    x <- "Julio"
  }
  if (x == 8) {
    x <- "Agosto"
  }
  if (x == 9) {
    x <- "Septiembre"
  }
  if (x == 10) {
    x <- "Octubre"
  }
  if (x == 11) {
    x <- "Octubre"
  }
  if (x == 12) {
    x <- "Diciembre"
  }
}


accidentes_bici$MES_NAMES <- sapply(accidentes_bici$MES, add_months_names)

p <- ggplot(accidentes_bici, aes(MES, fill= MES)) + 
  geom_bar() + 
  labs(x = "Año 2019 - Meses", y = 'Número de accidentes') + 
  scale_fill_brewer(palette="Paired") +
  theme_minimal()
p




  