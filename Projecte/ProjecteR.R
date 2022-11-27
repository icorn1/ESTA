#Començem amb les instruccions per llegir un fitxer en format excel, .xlsx.

install.packages ("XLConnect")
library(XLConnect)
data = readWorksheetFromFile("sampledatafoodsales.xlsx", sheet = 2)
?readWorksheetFromFile
View(data)
sapply(data,class)
summary(data$UnitPrice)
preu<-data$UnitPrice

hist (preu, breaks = seq(1.3, 3.5, by= 0.1))
# es pot veure clarament al histograma que no hi ha cap tipus d'indici per pensar que els preus estan distribuits de forma normal.
# tot i aixo, ens assegurarem amb les funcions d'R
qqnorm(preu)
qqline(preu)
#es pot veure clarament com els valors no segueixen cap tipus de relacio normal, si aixi fos, veuriem els puntets negres seguint la linia al grafic.
shapiro.test(preu)
#La H0 del test és que els x estan distribuïts segons una
#normal. Com que p << 0.02, hem de rebutjar H0 i concloure
#que les dades no corresponen a una normal.

p = mean(preu-2) #per tenir un valor entre 0 i 1
n = as.integer(mean(data$TotalPrice))
mu = n*p
sigma = sqrt(n*p*(1-p))
h=1/(sigma*sqrt(2*pi)) #valor maxim funcio de densitat de la normal

#dibuixem la funcio de densitat de la binomial.
k=0:n
f<-dbinom(k,n,p) #funcio de densitat (d)
plot(k,f,type='h', col="blue",ylim=0:1.1*h)
#text
tt=sprintf("Funcio de densitat\n      d'una Bi(%d, %1.1f)",n,p)
text(x=0.75*n, y=0.65*h, labels=tt, cex=1, col="blue")
#fi text
#funcio de densitat de la normal.
x=seq(mu-4*sigma, mu+4*sigma, by=0.01*sigma)
f<-dnorm(x,mu,sigma)
lines(x,f,type="l", col="red")
#text
tt=sprintf("Funcio de densitat\n      d'una N(%1.1f, %1.1f)",mu,sigma)
text(x=0.65*n, y=0.85*h, labels=tt, cex=1, col="red")

boxplot(data$TotalPrice)
summary(data$TotalPrice)


freq = table(data$Product)
freq_rel = prop.table(freq)
round(freq_rel*100,digits=2)
barplot(freq_rel, main="Freqüencia relativa de tipus de vendes", border = "red", col ="blue", density=30)

posx <- barplot(freq_rel ,xlab="Tipus de producte",ylab= "Frequencia", xaxt="n")
labs <- names(table(data$Product))
text (cex= 0.8, adj = c(0, 0), xpd=T, x=posx, y=50, labels = labs, srt = 90)

posx <- barplot(freq_rel, xlab = "Tipus de producte", ylab = "Freq. vendes", xatx = "n")

noms <- names(table(data$Product))

noms

productes <- data$Product
par(mar=c(7,4,2,2))
barplot(height=freq_rel, col="#80B1FC", names.arg=labs, las=2)


X = c(0,0,0,0,0,0,0,0,0)
for(i in 1:nrow(data)){
  if(data[i,"Product"] == "Arrowroot"){
    X[1] = X[1] + data[i,"Quantity"]
  }
  else if(data[i,"Product"] == "Banana"){
    X[2] = X[2] + data[i,"Quantity"]
  }
  else if(data[i,"Product"] == "Bran"){
    X[3] = X[3] + data[i,"Quantity"]
  }
  else if(data[i,"Product"] == "Carrot"){
    X[4] = X[4] + data[i,"Quantity"]
  }
  else if(data[i,"Product"] == "Chocolate Chip"){
    X[5] = X[5] + data[i,"Quantity"]
  }
  else if(data[i,"Product"] == "Oatmeal Raisin"){
    X[6] = X[6] + data[i,"Quantity"]
  }
  else if(data[i,"Product"] == "Potato Chips"){
    X[7] = X[7] + data[i,"Quantity"]
  }
  else if(data[i,"Product"] == "Pretzels"){
    X[8] = X[8] + data[i,"Quantity"]
  }
  else if(data[i,"Product"] == "Whole Wheat"){
    X[9] = X[9] + data[i,"Quantity"]
  }
}
index = X * freq_rel
freq_rel
index
indexVal
text(cex = 0.1, adj = c(0,0), xpd = T, x = posx, y = 50, noms, srt = 90)

names(data)
                       

A3$Rel = A3$Horsepower/A3$Weight
A3$Rel<-round(A3$Rel, digits = 3)  #ARRODONIR

A3$QRel[A3$Rel <= 0.032]<-"Pesat"
A3$QRel[A3$Rel > 0.032 & A3$Rel <= 0.04]<-"Normal"
A3$QRel[A3$Rel > 0.04]<-"Àgil"
View(A3)





