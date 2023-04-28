###         NICOLAS CASTELAO, LEGAJO: 22I310
###         POSTER, INTRODUCCION A DATA SCIENCE

library(ggplot2)
library(dplyr)
library(lubridate)
library(sf)
library(scales)

theme_set(theme_classic() +
            theme(plot.background = element_rect(fill = "transparent", color = "transparent"),
                  legend.background = element_rect(fill = "transparent"),
                  panel.background = element_rect(fill = "transparent")))
# Dataset inicial
provdata <- read.csv("C:/Users/gusta/Desktop/rProyect/Covid/Datasets/casos provincias.csv")
provdata <- select(provdata, residencia_provincia_nombre, fecha_apertura, everything())[,1:2]
names(provdata) <- c("provincia", 'fecha')
provdata <- filter(provdata, provincia != "SIN ESPECIFICAR")
provdata$fecha <- as.Date(provdata$fecha)

# Dataframe creado a partir del dataset original, utilizado para los mapas de calor
provdata2 <- mutate(provdata, mes = floor_date(fecha, "month"))
provdata2 <- select(provdata2,provincia,mes)
provdata2.counts <- provdata2 %>% #Contar los casos para cada provincia en cada mes
  group_by(mes, provincia) %>%
  count()
    # Hacerlo per capita
poblacionprov <- as.numeric(c("3120612","17569053","3556522","1426426","190641","3978984",
                              "540905","762067","2014533","333473","1197553", "366022", "1142963",
                              "606041","1440672","726590","1280960","1054028","1703186","603120",
                              "818234", "429556", "384607", "797955")) 
poblacion_df <- data.frame(provincia = unique(provdata2$provincia), poblacion = poblacionprov)
provdata2.counts <- left_join(provdata2.counts, poblacion_df, by = "provincia")
provdata2.counts <- provdata2.counts %>%
  mutate(casos_per_capita = n / poblacion)
#----------------------- MAPA DE CALOR --------------------------------------------------------------------------------------------------- 
ggplot(provdata2.counts, aes(x = provincia, y = mes, fill = casos_per_capita)) +
  geom_tile() +
  scale_fill_gradientn(colours = c("#DEEBF7", "#33CCFF", "#08306B"),
                       limits = c(0, 0.1),
                       na.value = "#08306B", # Lo utilizo para los valores mayores al limite. No hay otros NAs.
                       name = "",
                       guide = guide_colorbar()) +
  labs(x = "Casos per capita", y = "") +
  coord_flip() +
  scale_y_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme(legend.text = element_text(size = 8, face = "italic"),
        axis.text.x = element_text(angle = 39, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"))


#----------------------- MAPA DE CALOR EN MAPA DE ARGENTINA --------------------------------------------------------------------------------------------------- 
mapadecalor <- function(fechas){
  mapaargentina <- st_read("C:/Users/gusta/Desktop/rProyect/Covid/Datasets/Shapefiles/ARG_adm1.shp")
  names(mapaargentina)[5] <- "provincia"
  provdata_fechafiltro <- filter(provdata2, mes==fechas)
  provdata2.counts <- provdata_fechafiltro %>%
    group_by(provincia) %>%
    summarise(casos = n())
  mapaargentina <- left_join(mapaargentina, provdata2.counts, by = "provincia")
  
  ggplot() +
    geom_sf(data = mapaargentina, aes(fill = casos)) +
    scale_fill_gradientn(colours = c("#DEEBF7", "#33CCFF", "#08306B"),
                         na.value = "#08306B",
                         labels = comma) +
    theme_void() 
}

mapadecalor(as.Date("2021-06-01"))












