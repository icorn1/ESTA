#Exercici 1 Array tridimensional:

#Aquest exercici desconec com resoldre'l.


#Exercici 2:
myDF = data.frame(nom=c("Ricard", "Eric", "Jaime", "Tim", "Enric", "Eva"), relacio=c("amic", "amic", "amic", "amic", "familiar", "familiar"), edat=c(19, 19, 19, 21, 57, 46))
myDF #myDF es un data frame que conte nom, edats i relacio de diferentes persones

myDF <- myDF[order(myDF$edat,decreasing = T),] #Ordenem segons l'edat
write.csv(myDF,file="elMeuDataFrame.csv") #El guardem



#Exercici 3:
help("next") #La comanda help ens ajuda a comprendre les comandes.

x = 0

while (1){ #Utilitzem un bucle infinit.
  
  if(x == 3) {break;} #Aquesta es la condicio de sortida del bucle
  
  else {
    print(x)
    x = x + 1
    next  #Passem a la seguent iteracio.
  }
  
}


#Exercici 4: Data frame gran
hw_25000 <- read.csv("C:\\Users\\ixcor\\Desktop\\Ixent\\UNI\\ESTA\\LAB\\S1\\hw_25000.csv")
hw_25000$Height.cms = hw_25000$Height.Inches.*2.54
hw_25000$Weight.Kgs = hw_25000$Weight.Pounds./2.205

hw_25000$Index_Masa_Corporal = hw_25000$Weight.Kgs/((hw_25000$Height.cms/100)^2) #Creem l'index de masa corporal

hw_25000 <- hw_25000[order(hw_25000$Index_Masa_Corporal,decreasing = T),] #Ordenem per Index de masa mes alt
hw_25000 #resultat
write.csv("C:\\Users\\ixcor\\Desktop\\Ixent\\UNI\\ESTA\\LAB\\S1\\hw_25000.csv")


#Exercici 5: Funcio amb dataframe 
columnes <- function(data_frame){
  
  llista <- list()
  
  cols = names(data_frame) #retorna el nom de les columnes
  
  i = 1
  
  llargada = nrow(data_frame) #es constant
  
  for(col in data_frame){
      
      nom = cols[i] 
      
      
      
      llista <- c(llista, nom)     #afegim a la llista el nom de la columna
      llista <- c(llista, llargada)#i tambe la llargada
      
      i = i + 1
  }
 
  return (llista)  #tornem la llista
}


l = columnes(myDF) #funciona
l

