###         NICOLAS CASTELAO, LEGAJO: 22I310
###         POSTER, INTRODUCCION A DATA SCIENCE

# NOTA: Se utiliza el termino LatAm para simplificar, pero los analisis no incluyen a todos los paises de America Latina
library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate)


theme_set(theme_classic() +
            theme(plot.background = element_rect(fill = "transparent", color = "transparent"),
                  legend.background = element_rect(fill = "transparent"),
                  panel.background = element_rect(fill = "transparent")))

# Datasets iniciales
#   Dataset sobre casos, muertes y vacunas
coviddata <- read.csv("C:/Users/gusta/Desktop/rProyect/Covid/Datasets/All countries covid cases.csv")
coviddata <- subset(select(coviddata, Country_code, everything())[,2:8])
coviddata$Date_reported <- as.Date(coviddata$Date_reported)

# Dataframe creado a partir del dataset inicial
latamcovid <- coviddata %>% 
  filter(Country %in% c("Argentina", "Brazil", "Colombia", "Ecuador", "Venezuela (Bolivarian Republic of)", "Chile", "Peru", "Uruguay", "Paraguay", "Bolivia (Plurinational State of)"))
latamcovid <- filter(latamcovid, Date_reported == "2023-03-09")
latamcovid[2,2] <- "Bolivia"
latamcovid[10,2] <- "Venezuela"
latamcovid$Country <- factor(latamcovid$Country, 
                             levels = c("Argentina", "Brazil", "Colombia", "Ecuador", 
                                        "Venezuela", "Chile", "Peru", "Uruguay", "Paraguay", "Bolivia"),
                             ordered = TRUE)
poblacion <- c("46000000","12000000", "214000000", "20000000", "52000000", "18000000", "7000000", "34000000", "3500000", "28000000")
for(i in 1:10){
  latamcovid[i,7] <- latamcovid[i,7] / as.numeric(poblacion[i])
  latamcovid[i,5] <- latamcovid[i,5] / as.numeric(poblacion[i])
}
latamcovid <- mutate(latamcovid, dthcsccapita = Cumulative_deaths / Cumulative_cases)

# Dataframe creado a partir del dataset inicial, con el fin de hacer un grafico de barras apiladas
# BORRAR
regressioncovid_deaths <- coviddata %>% 
  select(Date_reported, Country, Cumulative_cases, Cumulative_deaths) %>%
  group_by(Country) %>%
  summarise(max_date = max(Date_reported),
            Country = last(Country),
            Cumulative_cases = last(Cumulative_cases),
            Cumulative_deaths = last(Cumulative_deaths))
regressioncovid_vac <- totalvaccinationdata %>%
  select(location, date, 
         people_fully_vaccinated, 
         people_vaccinated, 
         total_vaccinations) %>%
  group_by(location) %>%
  summarise(max_date = max(date),
            people_fully_vaccinated = last(people_fully_vaccinated),
            people_vaccinated = last(people_vaccinated),
            total_vaccinations = last(total_vaccinations))
regressioncovid <- left_join(regressioncovid_vac, 
                             regressioncovid_deaths %>% 
                               select(Country, Cumulative_cases, Cumulative_deaths),
                             by = c("location" = "Country")) %>% 
  arrange(location)
regressioncovid$people_vaccinated <- as.numeric(regressioncovid$people_vaccinated)

# Dataframe creado a partir del dataframe anterior, para el grafico de barras apiladas
# Este gráfico finalmente no fue utilizado para el poster
barras_apiladas_latam <- regressioncovid %>%
  filter(location %in% c("Argentina", "Brazil", "Colombia", "Ecuador", "Venezuela",
                         "Chile", "Peru", "Uruguay", "Paraguay", "Bolivia")) %>%
  select(location, Cumulative_cases, Cumulative_deaths, people_vaccinated) %>%
  arrange(location)
bal <- barras_apiladas_latam
bal[2,3] <- 22373
bal[2,2] <- 1195651
bal[10,3] <- 5854
bal[10,2] <- 552297
bal_melted <- pivot_longer(bal, cols = -location, names_to = "variable", values_to = "value")
bal_melted <- left_join(bal_melted, bal, by = "location")
bal_melted <- bal_melted %>%
  mutate(total = Cumulative_cases + Cumulative_deaths + people_vaccinated) %>%
  mutate(pct_cum_cases = Cumulative_cases / total)
bal_melted$value <- as.numeric(bal_melted$value)
bal_melted <-  mutate(bal_melted, variable = recode(variable, "Cumulative_cases" = "Casos totales", 
                                                    "Cumulative_deaths" = "Muertes totales", 
                                                    "people_vaccinated" = "Personas vacunadas"))

# Dataframe creado a partir del dataset inicial, utilizado en grafico de densidad
casoslatam <- filter(coviddata, Country %in% c("Argentina","Brazil","Chile","Colombia","Ecuador","Peru",
                                               "Uruguay","Paraguay","Venezuela (Bolivarian Republic of)", "Bolivia (Plurinational State of)"))
casoslatam$Country <- ifelse(casoslatam$Country == "Bolivia (Plurinational State of)", "Bolivia", casoslatam$Country)
casoslatam$Country <- ifelse(casoslatam$Country == "Venezuela (Bolivarian Republic of)", "Venezuela", casoslatam$Country)

# Dataset inicial, utilizado directamente para grafico de barras sobre desempleo
unemp <- read.csv("C:/Users/gusta/Desktop/rProyect/Covid/Datasets/Unemployment, male and female.csv")
unemp <- filter(select(unemp, ref_area.label, sex.label, time, obs_value, everything())[,1:4], ref_area.label != "Guyana" & ref_area.label != "Venezuela, Bolivarian Republic of")
names(unemp) <- c("Pais", "Sexo", "Year", "Valor")
unemp$Sexo <- substring(unemp$Sexo, 6)
unemp$Year <- as.character(unemp$Year)
unemp$Sexo <- factor(unemp$Sexo, levels = c("Total", "Male", "Female"))
unemp <- mutate(unemp, Sexo = ifelse(Sexo == "Female", "Mujer", 
                                     ifelse(Sexo == "Male", "Hombre", 
                                            ifelse(Sexo == "Total", "Total", "Total"))))

# Dataframe creado a partir del dataset anterior, utilizado para el grafico de barras sobre variacion del desempleo
unemp_diff <- unemp %>%
  pivot_wider(names_from = Sexo, values_from = Valor) %>%
  group_by(Pais) %>%
  summarize(
    Male_Diff = Hombre[Year == 2021] - Hombre[Year == 2019],
    Female_Diff = Mujer[Year == 2021] - Mujer[Year == 2019],
    Total_Diff = Total[Year == 2021] - Total[Year == 2019]
  ) %>%
  pivot_longer(cols = -Pais, names_to = "Sexo", values_to = "Diff")
unemp_diff$Sexo <- str_sub(unemp_diff$Sexo, 1, -6)


###------------------------ PIE CHART ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Este gráfico no fue utilizado para el póster
ggplot(data = latamcovid) +
  geom_bar(aes(x = "", y = dthcsccapita, fill = reorder(Country, -dthcsccapita)), 
           width = 0.5, stat = "identity", position = "fill") +
  scale_fill_manual(values = c("#08306B", "#08519C", "#2171B5", "#33CCFF", "#4292C6", "#6BAED6", "#A6FBFF", "#9ECAE1", "#C6DBEF", "#DEEBF7")) +
  coord_polar(theta = "y") +
  labs(title = "", fill = "", x = "", y = "") +
  theme_void() +
  theme(legend.position = "right", legend.margin = margin(t = 1, r = 0, b = 1, l = -0, unit = "cm"),
        plot.background = element_rect(fill = "transparent", color = "transparent"),
        legend.background = element_rect(fill = "transparent", color = "transparent"),
        plot.margin = unit(c(0, 2, 1, 2), "cm"))

###------------------------ LOLLYPOP CHART ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Este gráfico fue añadido al póster luego de la presentación oral
ggplot(latamcovid, aes(x = reorder(Country, dthcsccapita), y = dthcsccapita)) +
  geom_segment(aes(xend = reorder(Country, dthcsccapita), yend = 0), color = "#DEEBF7", size = 4) +
  geom_point(size = 8, color = "#08306B", fill = "#08306B", shape = 21) +
  labs(x = "", y = "Proporcion muertes/casos") +
  coord_flip()

##--------------------------------- GRAFICO DE BARRAS APILADAS -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Este gráfico no fue utilizado para el póster

ggplot(data = bal_melted, aes(x = reorder(location, pct_cum_cases), y = value)) +
  geom_bar(aes(fill = variable), stat = "identity", position = "fill") +
  scale_fill_manual(values = c("Casos totales" = "#1f77b4", "Muertes totales" = "#08306B", "Personas vacunadas" = "#A6FBFF")) +
  labs(x = "", y = "porcentual", fill = "")

##------------------- DENSITY PLOT SOBRE CASOS ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


ggplot(data = casoslatam, aes(x = as.Date(Date_reported), fill = Country, y = Cumulative_deaths)) +
  geom_bar(stat = "identity", width = 1) +
  labs(x = "", y = "muertes acumuladas", fill = "") +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
  scale_y_continuous(breaks = seq(0,1500000,200000)) +
  theme(legend.position = "right", 
        axis.text.x = element_text(angle = 39, hjust = 1, face = "bold")) +
  scale_fill_manual(values = c("#08306B", "#08519C", "#2171B5", "#33CCFF", "#4292C6", "#6BAED6", "#A6FBFF", "#9ECAE1", "#C6DBEF", "#DEEBF7")) 
  

##------------------- GRAFICO DE BARRAS SOBRE DESEMPLEO ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ggplot() +
  geom_bar(data = unemp, aes(x = reorder(Pais,-Valor), y = Valor, fill = Sexo, alpha = Year), stat = "identity", position = position_dodge(width = 1)) +
  labs(x = "", y = "Tasa de desempleo", fill = "", alpha = "") +
  scale_fill_manual(values = c("#1f77b4", "pink","black")) +
  scale_alpha_manual(values = c(`2019` = 0.6, `2020` = 1, `2021` = 0.7)) +
  theme(legend.position = "right")

##------------------- GRAFICO SECUNDARIO DE BARRAS SOBRE VARIACION DE DESEMPLEO ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ggplot(unemp_diff, aes(x = reorder(Pais, -Diff), y = Diff, fill = Sexo)) +
  geom_bar(stat = "identity", position = position_dodge(), color = ifelse(unemp_diff$Diff > 0, "red", "green")) +
  labs(x = "", y = "Cambio en tasa de desempleo (2019 a 2021)", fill = "") +
  theme(legend.position = "right") +
  scale_fill_manual(values = c("pink", "#1f77b4", "black"), guide = "none") +
  scale_y_continuous(breaks = seq(-1,4,1)) +
  coord_flip()



