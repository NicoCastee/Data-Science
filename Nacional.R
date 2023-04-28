###         NICOLAS CASTELAO, LEGAJO: 22I310
###         POSTER, INTRODUCCION A DATA SCIENCE

library(ggplot2)
library(data.table)
library(tidyverse)
library(dplyr)

theme_set(theme_classic() +
    theme(plot.background = element_rect(fill = "transparent", color = "transparent"),
      legend.background = element_rect(fill = "transparent"),
      panel.background = element_rect(fill = "transparent")))

# Dataset inicial sobre muertes, casos y vacunaciones.
coviddata <- read.csv("C:/Users/gusta/Desktop/rProyect/Covid/Datasets/All countries covid cases.csv")
coviddata <- subset(select(coviddata, Country_code, everything())[,2:8])
coviddata$Date_reported <- as.Date(coviddata$Date_reported)

# Dataset inicial sobre vacunaciones
totalvaccinationdata <- fread("C:/Users/gusta/Desktop/rProyect/Covid/datasets/Vac data.csv", quote="", fill = TRUE)
totalvaccinationdata <- rename(totalvaccinationdata, people_vaccinated_percapita = people_vaccinated_per_hundred,
         people_fully_vaccinated_percapita = people_fully_vaccinated_per_hundred)
totalvaccinationdata$people_fully_vaccinated_percapita <- totalvaccinationdata$people_fully_vaccinated_percapita/100
totalvaccinationdata$people_vaccinated_percapita <- totalvaccinationdata$people_vaccinated_percapita/100

# Dataframes sobre muertes, casos y vacunaciones en Argentina
data_argentina <- coviddata %>%
  filter(Country == "Argentina") %>%
  mutate(
    casesvsdeathnew = New_cases - New_deaths,
    New_deaths = New_deaths / 46000000 * 1000,
    New_cases = New_cases / 46000000 * 1000,
    Cumulative_deaths = Cumulative_deaths / 46000000 * 1000
  )
totalvac_argentina <- filter(subset(select(totalvaccinationdata, date, location, people_fully_vaccinated_percapita, people_vaccinated_percapita, everything())[,1:5]), location == "Argentina")

#----------------------- GRAFICO CON TODA LA INFORMACION SOBRE ARGENTINA --------------------------------------------------------------------------------------------------- 
ggplot() + 
  geom_line(data = data_argentina, aes(x = as.Date(Date_reported), y = New_deaths, color = "Muertes nuevas")) +
  geom_line(data = data_argentina, aes(x = Date_reported, y = New_cases, color = "Casos nuevos")) +
  geom_line(data = totalvac_argentina, aes(x = date, y = people_fully_vaccinated_percapita, color = "Completamente vacunados")) +
  geom_line(data = totalvac_argentina, aes(x = date, y = people_vaccinated_percapita, color = "Vacunados")) +
  geom_ribbon(data = data_argentina, 
              aes(x = as.Date(Date_reported), ymin = New_cases, ymax = New_deaths),
              fill = "#C6DBEF",
              alpha = 0.5) +
  scale_color_manual(values = c("Casos nuevos" = "#2171B5", "Muertes nuevas" = "blue", "Vacunados" = "#33CCFF", "Completamente vacunados" = "black")) +
  scale_x_date(date_labels = "%d/%m/%Y", date_breaks = "6 months") +
  labs(x = "", y = "cada 1000 personas (per capita la vacunacion)", color = "")


#----------------------- GRAFICO SECUNDARIO PARA MEJORAR LA INTERPRETACION --------------------------------------------------------------------------------------------------- 
ggplot(data = data_argentina) +
  geom_line(aes(y = New_deaths, x = as.Date(Date_reported)), color = "blue") +
  scale_x_date(date_labels = "%d/%m/%Y", date_breaks = "6 months") +
  labs(x = "", y = "muertes nuevas cada 1000 personas") 








