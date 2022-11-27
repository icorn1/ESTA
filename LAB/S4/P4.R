set.seed(1)
x=rnorm(1000)+rexp(1000,1)
summary(x)
hist(x, breaks=seq(-3,8,by=0.25))
qqnorm(x) #Test gràfic de normalitat
qqline(x) #Recta que passa pel 1er i 3er quartil
shapiro.test(x) #Test numeric. Si p << 0.02, NO ES NORMAL
#si volguessis fer una normal: x = rnorm(1000)+0.01*rexp(1000,1)

#Per aproximar una bionomial(n,p), es pot usar una VA tal que mu = np, sig = sqrt(np(1-p))
p = 0.3
n = 40
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

#Sumem 400 binomails + 400 exponencials + 400 students
N = 400
lambda = 0.2 #exponencial
p=0.1 #p de la binomial
nbi=2 #n de la binomial
nt=3 #n de la Student

#Generem dades amb 3000 mostres de cada VA
M=3000 # #de mostres (cada una es la suma de 1200 valors)
x=rep(0,M)
for(i in 1:N){
  x=x+rexp(M,lambda)
  x=x+rbinom(M,nbi,p)
  x=x+rt(M,nt)
}
min = quantile(x,0)
max = quantile(x,1)
d=(max-min)/floor(sqrt(M))
hist(x,freq=F, breaks = seq(min,max,by=d), main=NULL)
#freq = f perque dibuixi les relatives i no absolutes.

#quina es la normal que ho aproxima?
mu=mean(x)
sigma=sd(x)
h=1/(sigma*sqrt(2*pi))
xn=seq(mu-4*sigma, mu+4*sigma, by=0.01*sigma)
f<-dnorm(xn,mu,sigma)
lines(xn,f,type="l",col="red")
tt=sprintf("Funcio de densitat\n      d'una N(%1.1f, %1.1f)",mu,sigma)
text(x=mu+2.1*sigma, y=h, labels=tt, cex=1, col="red")


#Intervals de Confiança
N=2000 #nombre de mostres
alpha=0.01
a=1-alpha/2
Za = qnorm(a)
mu=4
sigma=2
n=1000 #grandaria mostres
M=0 #contador
for(i in 1:N){
  x=rnorm(n,mu,sigma)
  tx=mean(x)
  L1=tx-Za*sigma/sqrt(n) #formula vista a teoria
  L2=tx+Za*sigma/sqrt(n)
  if(mu>=L1 & mu<= L2){
    M=M+1
  }
}
M/N*100    #percentatge real d'intervals que contenen el parametre 
(1-alpha)*100#percentatge teoric d'intervals que contenen el parametre
