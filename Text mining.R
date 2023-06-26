###         NICOLAS CASTELAO, LEGAJO: 22I310
###         POSTER, INTRODUCCION A DATA SCIENCE

library(tidyverse)
library(tidytext)
library(wordcloud)

tokenizeText <- function(textFile) {
  textFile <- file.choose()
  textFile.scanned <- scan(textFile, what = "character", sep = "\n") #separa los textos cada \m
  textFile.scanned <- paste(textFile.scanned, collapse = " ")
  textFile.scanned <- tolower(textFile.scanned) # pasa todo a minuscula
  textFile.scanned.l <- strsplit(textFile.scanned, "[^A-Za-z0-9_áéíóúÁÉÍÓÚñÑ']")
  textFile.scanned.v <- unlist(textFile.scanned.l)
  notBlanks <- which(textFile.scanned.v != "")
  textFile.scanned.v.withoutBlanks <- textFile.scanned.v[notBlanks]
  return(textFile.scanned.v.withoutBlanks)
}

#------------------ La Izquierda Diario -----------------------------------------------------------------------------------------------
izq <- tokenizeText(izqText)
izq.t <- table(izq)

s.izq <- sort(izq.t, decreasing = TRUE)

freq.izq <- as.data.frame(100*(s.izq / length(izq)))
# Stop words
palabras <- read.csv("C:/Users/gusta/Desktop/rProyect/Covid/Textos/stop_words.txt", header = FALSE, stringsAsFactors = FALSE)
palabras2 <- c("24h","re","d","erte","len","","abalo","cdc","ere","ute","ag","n95","bs","sé","pae","ve","m","pr","mg","as","afp","ido","fit","ile","dr","di","n","1m","clr","vi","fm","etc","lili","cemic","u","ee","figaro","hs","uti","uu","zamdra","x","enero","febrero","marzo","mayo","abril","junio","julio","agosto","septiembre","setiembre","octubre","noviembre","diciembre")
freq.izq <- freq.izq %>% 
  filter(!(izq %in% palabras$V1)) %>%
  filter(!(izq %in% palabras2))
freq.izq <- freq.izq %>%
  filter(!grepl("^\\d+$", izq) | is.numeric(izq))

izq.plot1 <- ggplot(data = freq.izq[1:50,]) +
  geom_col(aes(x = izq, y = Freq)) +
  coord_flip()
izq.plot1

# Nube de palabras
word_freq.izq <- as.data.frame(freq.izq)

# Defino los colores
n <- length(word_freq.izq$Freq)  # Número de palabras
col_func <- colorRampPalette(c("#33CCFF", "#08306B"))  
colors <- col_func(n)
names(colors) <- word_freq.izq$izq

wordcloud(words = word_freq.izq$izq,
          freq = word_freq.izq$Freq,
          random.order = FALSE,
          colors = colors)

#------------------ La Derecha Diario -----------------------------------------------------------------------------------------------
der <- tokenizeText(derText)
der.t <- table(der)

s.der <- sort(der.t, decreasing = TRUE)

freq.der <- as.data.frame(100*(s.der / length(der)))
# Stop words
palabras3 <- c("epoc","ds","m2","pdf","wu","p","chen","bld","bri","fu","ing","erna","s","albertodías","ncie","djillali","qr","tcu","ee","por","V","v","abe","1ro","ssa","re","ad","nio","ant","rdif","2021por","cov")

freq.der <- freq.der %>% 
  filter(!(der %in% palabras$V1)) %>%
  filter(!(der %in% palabras2)) %>%
  filter(!(der %in% palabras3))
freq.der <- freq.der %>%
  filter(!grepl("^\\d+$", der) | is.numeric(der))

der.plot1 <- ggplot(data = freq.der[1:50,]) +
  geom_col(aes(x = der, y = Freq)) +
  coord_flip()
der.plot1

# Nube de palabras
word_freq.der <- as.data.frame(freq.der)

# Defino los colores
n <- length(word_freq.der$Freq)  # Número de palabras
col_func <- colorRampPalette(c("#33CCFF", "#08306B"))  # Cambia los colores aquí según tus preferencias
colors <- col_func(n)
names(colors) <- word_freq.der$der

wordcloud(words = word_freq.der$der,
          freq = word_freq.der$Freq,
          random.order = FALSE,
          colors = colors)

#----------------- Clasificacion de temas -------------------------------------------------------------------------------------------
# Ordeno en grupos para ver de que temas habla cada diario

covid <- c("covid", "pandemia", "pandemiaes", "coronavirus", "pandemiaes", "vacuna", "vacunas", "pacientes", "paciente", "caso", "casos", "virus",
           "cuarentena", "estricta", "obligatoria", "cuarentenas", "estrictas", "obligatorias", "medidas",
           "médicos", "médicas", "médico", "médica", "medicina", "cura", "curas", "oms", "test", "tests","positivo",
           "negativo", "positivos", "negativos","situación", "sanitaria", "sanitarias", "salud", "aislamiento",
           "aislamientos", "hospital","hospitales","cama","camas","emergencia", "restricciones", "restricción",
           "vacunatorio","circulación","encerradas","encerrados","encerrado","encerrada","dosis","protocolo","protocolos",
           "vacunación","vacunados","vacunadas","vacunado","vacunada","sanitario","sanitarios")
gobiernos <- c("trump","biden","alberto","fernández","china","estados","unidos","eeuu","usa","argentina","arg",
               "presidente","ministros","presidentes","ministros","ministra","ministras","chino","estadounidense",
               "us","fmi","nueva","york","buenos","aires","joe","donald","alemania","australia","méxico","mexico",
               "bolsonaro","jair","brazil","brasil","áfrica","mundial","mundiales","xi","taiwán","mundo","kong",
               "hong","india","larreta","horacio","cristina","kirchner","francés","k","antik","anti-k","fondo","monetario","internacional",
               "yi","chile","pou","lacalle","uruguay","francia","franceses","europeo","europa","nueva","zelanda")
ecopol <- c("pobreza","economia","económicas","social","iva","impuestos","impuesto","empresa","empresas","empresarios",
            "comunismo","comunista","comunistas","salario","remuneración","básica","inflación","déficit","fiscal","monetario",
            "presupuesto","despidos","despido","salarial","salariales","ajuste","ajustes","redistribución","redistribuciones",
            "banco","bancos","deuda","deudas","externa","interna","laboral","labor","laburo","dinero","pago","compra","compras","pagos",
            "docentes","docente","producto","interno","bruto","bruta","neta","neto","ganancias","privada","privado",
            "pública","público","recorte","jefe","jefes","patrón","patrones","aumento","aumentos","paritarias",
            "trabajador","trabajadores","trabajo","educación")

covid.izq <- 100*(length(which(izq == covid)) / length(covid))
gob.izq <- 100*(length(which(izq == gobiernos)) / length(gobiernos))
ecopol.izq <- 100*(length(which(izq == ecopol)) / length(ecopol))
covid.der <- 100*(length(which(der == covid)) / length(covid))
gob.der <- 100*(length(which(der == gobiernos)) / length(gobiernos))
ecopol.der <- 100*(length(which(der == ecopol)) / length(ecopol))

izq.total <- c(covid.izq, gob.izq, ecopol.izq)
der.total <- c(covid.der, gob.der, ecopol.der)

total <- data.frame(izq.total, der.total)
rownames(total) <- c("COVID", "Internacional","Economía")


total_modificado <- total %>%
  rownames_to_column(var = "palabra") %>%
  pivot_longer(cols = c(izq.total, der.total),
               names_to = "diario",
               values_to = "frecuencia")

theme_set(theme_classic() +
            theme(plot.background = element_rect(fill = "transparent", color = "transparent"),
                  legend.background = element_rect(fill = "transparent"),
                  panel.background = element_rect(fill = "transparent")))

ggplot(total_modificado, aes(x = palabra, y = frecuencia, fill = diario)) +
  geom_col(position = "dodge") +
  labs(x = "",
       y = "Frecuencia",
       fill = "") +
  scale_fill_manual(values = c("#08306B", "#33CCFF"),
                    labels = c("La Derecha Diario", "La Izquierda Diario")) 








