3+4
sqrt(12)
#Ctrl + L Borrar terminal
x=3
x
x<-4
x
5->x
x
x->y
y
3->y
# x -> 6 NO
Joan = "Joan"
cat(Joan,"Batet")   #No actualitza variable 
Joan <- capture.output(cat(Joan, "Batet"))    #Actualitza variable
help("cat")
cat(Joan, "Batet", sep = " | ")

save.image("pra1.RData")  #Es guarda a Files
rm(Joan,x,y)              #Es borra les variables
load("pra1.RData")        #Es recupera les variables

rivers
rivers[74]
rivers[42:56]
rivers[c(42:56,5:9)]
rivers[rivers>999]
rivers[rivers<250 | rivers>2000]
length(rivers)
rivers[142] = 9
rivers[142]

noms=c("Elia","Biel","Ari") #F pels demes XD
names(noms)=c("germana", "cosi", "cosina")
noms["cosina"]


(-3:7)
help("seq")
seq(from=4.5, to=25, by=0.5)
seq(from=2, to=100, length.out=20)

c(1:5)*10
c(1:5)*c(-1,1,-1,1,-1)
c(1:6)*(1:5)#error
c(1:6)*(1:3)


y=matrix(cos(1:12),ncol = 2)
y
y=matrix(cos(1:12),ncol = 2,byrow=TRUE)
y
y[2,]
y[,1]

yyy = array(1:16,dim=c(2,2,4))    #4 matrius de 2x2
yyy
yyy = array(1:16,dim=c(4,2,2))    #2 matrius de 4x2
yyy
yyy = array(1:16,dim=c(2,2,2,2))
yyy

llista = list(Joan,x,rivers)
ls()
#borrar tot: rm(list = ls())

letters
A=sample(letters,size=100,replace=TRUE) #Agafa un vector mostra de letters de tamany 100
A
table(A)
x=sample(c("c","+"),size=10000,replace=T,prob=c(0.5,0.5)) #alterar probabilitat
table(x)

#DataFrames
CW <- ChickWeight
CW
View(CW)
CW[3]   #3era columna 
CW[3,]  #3er element (fila)

cotxes <- mtcars
View(cotxes)
cotxes$kpl = cotxes$mpg*1.609/3.7854
cotxes <- cotxes[order(cotxes$hp,decreasing = T),]
write.csv(cotxes,file="elsmeuscotxes.csv")
