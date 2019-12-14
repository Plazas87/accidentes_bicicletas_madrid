library(curl)

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
