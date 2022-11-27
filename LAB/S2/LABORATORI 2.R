# Llegim i mostrem el fitxer .csv

vendes <- read.csv("Sales.csv", stringsAsFactors = T)

View(vendes)



# Ens diu el tipus de la variable 

class(vendes$Total.Profit) 

class(vendes$Region)



# Ens diu tots els camps, de quin tipus son

sapply(vendes, class)



# Per saber les categories d'una variable categorica utilitzem la funcio levels

levels(vendes$Sales.Channel)

levels(vendes$Item.Type)



# Conjunt de freqüencies absolutes

freq = table(vendes$Item.Type)

freq

freqr = prop.table(freq) # Aquesta proporció esta en tant per 1

freqr

freqr1 = round(freqr*100, digits = 1) # Fa la proporcio en tant per 100

freqr1



# Diagrama de barres

V1 <- vendes$Item.Type # es vol representar graficament la frequencia relativa i absoluta

fV1 <- table(V1)

frV1 = prop.table(fV1)

barplot(fV1)  # frequencia absoluta

barplot(frV1) # frequencia relativa

barplot(frV1, main = "Freq. rel. del tipus de producte",  col = "green", border = "blue", density = 30)

# Vull que em retorni la posicio de l'eix de les 'x'

posx <- barplot(fV1, xlab = "Tipus de producte", ylab = "Freq. vendes", xatx = "n")

posx


noms <- names(table(V1))

noms

text(cex = 0.8, adj = c(0,0), xpd = T, x = posx, y = 50, noms, srt = 90)



# Informacio sobre dues variables de frequencia absoluta relativa quantitatives

V2 <- vendes$Sales.Channel

freq2 = table(V1, V2)

freq2

barplot(freq2, beside = T, col = rainbow(12))

legend(17.4, 189, legend = noms, fill = rainbow(12), cex =0.8)



# Utilitzant la llibreria qcc

library(qcc)

PC1 <- pareto.chart(table(V1), ylab = "Absoluta", ylab2 = "Relativa acumulada", main = "Grafica de Pareto de vendes")



# Fem el grafic circular per els tants percents de la variable V1

V1 = vendes$Item.Type

PPV1 = round(prop.table(table(V1))*100, digits = 1)

noms = levels(V1)

nomsc <-  paste(noms, PPV1, "%")

noms

pie(PPV1, label = noms, radius = 1, col = rainbow(length(noms)))



# Variables de tipus quantitatiu

UP <- vendes$Unit.Price

class(UP)

barplot(UP)

summary(UP)



# Crear nova variable en el data frame

vendes$PQ[UP < 50] <- "Barat"

View(vendes)

vendes$PQ[UP >= 50 & UP < 150] <- "Normal"

vendes$PQ[UP >= 150 & UP < 300] <- "Car"

vendes$PQ[UP >= 300] <- "Molt car"

View(vendes)

class(vendes$PQ)



# Agafa el PQ i el converteix a factor

vendes$PQ <- as.factor(vendes$PQ)

sapply(vendes, class)
