XXX = read.csv("anova.csv", stringsAsFactors=T)

sapply(XXX, class) #per saber els tipus de les variables
x1 = na.omit(XXX$Crtl)
x2 = na.omit(XXX$Tract.1)
x3 = na.omit(XXX$Tract.2)


#fem un boxplot
nivells = names(XXX)
boxplot(x1,x2,x3,names=nivells,ylab="Concentracio Hormona")

#graus de llibertat
m=3
n1 = length(x1)
n2 = length(x2)
n3 = length(x3)
n=n1+n2+n3

#mitjanes
tx1 = mean(x1)
tx2 = mean(x2)
tx3 = mean(x3)

tx = 1/n*(sum(x1)+sum(x2)+sum(x3))

#Sumes de quadrats
VT = sum((x1-tx)^2)+sum((x2-tx)^2)+sum((x3-tx)^2)
VR = sum((x1-tx1)^2)+sum((x2-tx2)^2)+sum((x3-tx3)^2)
VE = n1*(tx1-tx)^2 + n2*(tx2-tx)^2 + n3*(tx3-tx)^2
#Yahoooo

sT2 = VT/(n-1)
sR2 = VR/(n-m)
sE2 = VE/(m-1)

#estadistic
z= sE2/sR2
#Test
alpha = 0.05
Za = qf(1-alpha,m-1,n-m)
# z pertany a la regio de rebiug (z ??? RR -> Rebutjem H0 (Mitjanes iguals) -> Mitjanes diferents)

#p-valor
p=1-pf(z,m-1,n-m)
#p-valor = 2.66*10^-10,   


#Ara ho farem amb la funcio aov.
c1=c(x1,x2,x3)
nx1 = rep(nivells[1],n1)
nx2 = rep(nivells[2],n2)
nx3 = rep(nivells[3],n3)
c2=c(nx1,nx2,nx3)

data =data.frame(dades=c1, tractament=c2)
resultat = aov(dades-tractament, data)
summary(resultat)

  
  