library(dplyr)
library(leaflet)
library(reshape2)
library(lubridate)
library(ggplot2)
library(ggmap)


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
library(scales)

g <- ggplot(accidentes_bici, aes(LESIVIDAD, fill= LESIVIDAD)) + 
  geom_bar() + 
  labs(title = "Pendiente seleccionar título", 
       subtitle = "(1973-74)",
       tag = "Figure 1",
       x = "Gravedad del accidente", 
       y = 'Número de accidentes', 
       caption = total_accidentes) + 
  scale_y_continuous(breaks = c(20,40,60,80,100,120,140,160,180,200,220,240,260,280)) + 
  scale_fill_brewer(palette = 'RdGy') +
  theme_minimal() + 
  theme(legend.position = 'none')
g 

# lo siguiente es el codigo adicional para probar los ograficos de plotly en R si este rsulta ser mejor 
# Pues obtaré por dejar este commo definitivo, pop rahora va ganando el de de barras

# library(plotly)
# 
# temporal_table_one <- accidentes_bici %>% 
#   group_by(MES, MES_NAMES) %>% 
#   summarise(Total = n())
# 
# p <- plot_ly(temporal_table_one, x = ~MES, y = ~Total, 
#              type = 'bar', 
#              color = ~MES,
#              colors = 'RdGy',
#              name = 'Lesividad')
#   # add_trace(y = ~MES, name = 'LA Zoo') %>%
#   # layout(yaxis = list(title = 'Count'), barmode = 'stack')
# 
# p
# # Create a shareable link to your chart
# # Set up API credentials: https://plot.ly/r/getting-started
# chart_link = api_create(p, filename="bar-stacked")
# chart_link

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
  x
}

# ---------------------------------------------------------------------------
accidentes_bici$MES_NAMES <- sapply(accidentes_bici$MES, add_months_names)

temporal_table <- accidentes_bici %>% 
  group_by(MES, MES_NAMES) %>% 
  summarise(Total = n())

p <- ggplot(accidentes_bici, aes(MES, fill= MES_NAMES)) + 
  geom_bar() + 
  labs(title = "Pendiente seleccionar título", 
       subtitle = "(1973-74)",
       caption = "Hecho con ggplot2 package",
       tag = "Figure 2",
       colour = "Gears",
       x = "Año 2019 - Meses", 
       y = 'Número de accidentes') + 
  scale_y_continuous(breaks = c(10, 30, 50, 70, 90, 110)) +
  scale_x_discrete(limit = temporal_table$MES, labels = temporal_table$MES_NAMES) +
  scale_fill_brewer(palette="RdGy") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none')
p 
# --------------------------------------------------------------------------------------
# Se arregla el vector de los nombres para poder modificar el nombre de los labals de las facetas 
t <- temporal_table$MES_NAMES
names(t)<- temporal_table$MES

# Se plotea la gráfica con todos los elementos para lograr
w <- ggplot(accidentes_bici, aes(LESIVIDAD, fill= LESIVIDAD)) + 
  geom_bar() + 
  labs(title = "Pendiente seleccionar título", 
       subtitle = "(1973-74)",
       tag = "Figure 3",
       x = "Gravedad del accidente", 
       y = 'Número de accidentes', 
       caption = total_accidentes) + 
  scale_fill_brewer(palette = 'RdGy') +
  theme_bw() +
  theme(legend.position = 'none', 
        strip.background = element_rect(fill="black"),
        strip.text.x = element_text(size = 10, color = "white")) + 
  facet_wrap(~MES,nrow = 2 , labeller = labeller(MES = t))

w 
# --------------------------------------------------------------------------------

# accidentes por distrito
library(rgdal)
library(Hmisc)
distrito_accidentes <- accidentes_bici %>% 
  group_by(DISTRITO) %>% 
  summarise(Total = n())


distrito_accidentes$DISTRITO <- capitalize(tolower(distrito_accidentes$DISTRITO))
geo_madrid <- rgdal::readOGR("resources/Geo/madrid-districts.geojson")

distrito_accidentes$DISTRITO <- levels(geo_madrid$name)
distrito_accidentes$DISTRITO

tmp <- as.data.frame (geo_madrid$name)
tmp$id <- geo_madrid$cartodb_id
tmp <- tmp[order(tmp$`geo_madrid$name`),]
distrito_accidentes$id <- tmp$id
distrito_accidentes <- distrito_accidentes[order(distrito_accidentes$id),]


pal <- colorNumeric(
  palette = "Paired",

  domain = distrito_accidentes$Total)
leaflet(geo_madrid) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.8,
              fillColor = ~pal(distrito_accidentes$Total),
              label = ~distrito_accidentes$Total) %>% 
  addLegend(pal = pal, values = ~distrito_accidentes$Total, opacity = 1.0)


# ------------------------------- variación de la gravedad de los accidentes en el año

z <- accidentes_bici %>% 
  group_by(LESIVIDAD,MES, MES_NAMES) %>% 
  summarise(Total = n())

p <- ggplot(z, aes(x=MES, y=Total, color = LESIVIDAD)) + 
  geom_line(aes(color = LESIVIDAD), size=1) +
  geom_point(aes(color = LESIVIDAD), size= 2) +
  scale_color_brewer(palette='Paired') +
  labs(title = "Pendiente seleccionar título",
       subtitle = "(1973-74)",
       caption = "Hecho con ggplot2 package",
       tag = "Figure 2",
       x = "Año 2019 - Meses",
       y = 'Número de accidentes') +
  scale_y_continuous(breaks =  c(0,5, 10,15,20,25,30,35,40), limits = c(0,40)) +
  scale_x_discrete(limit = z$MES, labels = z$MES_NAMES) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p 

 



  