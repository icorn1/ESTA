#
#
#
# qf(alpha, n1-1, n2-1)-> Per quan desviacio1/desviacio2 = 1, Ff(19,19)(Zalpha) = alpha -> Zalpha = qf()
# pf(z, n1-1, n2-1) -> per p-valor P(Z1 <= z) = Ff(19,19)(z) = pf() = pvalor.

#LOWER tail: 
# el p-valor ???s P(N(0,1)>z)
# pnorm(z,lower.tail=F)
#NO LOWER TAIL
# el p-valor ???s P(N(0,1)<z)
# pnorm(z,lower.tail=T)

#q -> calcul IC
#p -> calcul P VALOR

#mux -muy = 0 -> qt


# model A

## prob1 -> probabilitat
PAp=0.37
PBp=0.06
PABp=0.03
P0p=0.35
PAm=0.09
PBm=0.015
PABm=0.005
P0m=0.08
P=c(PAp,PBp,PABp,P0p,PAm,PBm,PABm,P0m)
sum(P)

# (a)
PGp=PAp+PBp+PABp+P0p
PGA=PAp+PAm
PG0=P0p+P0m
PGAiGp=PAp
# (b)
PGpiG0=P0p
PGpCG0 = PGpiG0/PG0
PG0CGp = PGpiG0/PGp
PGpiG0==PGp*PG0
# com que no s???n iguals, no s???n independents
# (c)
PG0OGp = PG0+PGp-PGpiG0

## prob 2 normals
# X=punts Jeff, N(98,4)
# Y=punts Josh, N(96,6)
# D=X-Y = N(98-96,sqrt(1^2*4^2+(-1)^2*6^2))
muD=98-96
sigmaD=sqrt(1^2*4^2+(-1)^2*6^2)
# (a)
# P(guanya Jeff) = P(D>0)
PJeff=1-pnorm(0,muD,sigmaD)
# (b)
# Jeff100=nombre de partides guanyades per en Jeff, de 100 jugades
# Jeff100 = Bi(100,PJeff)
# Volem calcular P(Jeff guanyi m???s partides que Josh)=
# =P(Jeff100>=51) = 1- P(Jeff100<=50)
1-pbinom(50,100,PJeff)
# o tamb???
pbinom(50,100,PJeff,lower.tail = F)
# (c)
# aproximem Jeff100 per una normal 
# Jeff100A=N(100*PJeff,sqrt(100*PJeff*(1-PJeff)))
muA=100*PJeff
sigmaA=sqrt(100*PJeff*(1-PJeff))
# el c???lcul aproximat ???s llavors
# P(Jeff100>=51) aprox P(Jeff100A> 50.5) = 1-P(Jeff100A<= 50.5)
1-pnorm(50.5,muA,sigmaA)
# o tamb???
pnorm(50.5,muA,sigmaA,lower.tail = F)
# (d)
# el menys cre???ble ???s que el nombre de punts que aconsegueix un i altre siguin 
# VAs independents. ???s dif???cil imaginar un xoc interactiu entre els dos on aix???
# passi. Aix??? descriu m???s b??? una competici??? tipus cursa o concurs 
# entre individus separats i sense interacci??? (ni tant sols psicol???gica), 
# i no un joc.

## prob 3 
n1=25
n2=38
tx1=3
s1=0.7
tx2=3.5
s2=1.2

# (a) IC mu desconeguda
alpha=0.02
a=1-alpha/2
ta=qt(a,n1-1)
# l'IC ???s [L1,L2] amb
L1=tx1-ta*s1/sqrt(n1)
L2=tx1+ta*s1/sqrt(n1)

# (b) #tc var/var = 1
# H0: var1/var2 = 1
# H1: var1/var2 != 1
# estad???stic mostral
z=s1^2/s2^2
kk=pf(z,n1-1,n2-1)
p=2*min(kk,1-kk)
# com que p =0.007 < 0.02, rebutgem la H0 i
# per tant decidim que les vari???ncies s???n diferents.
# (c)
# H0: mu1 = mu2
# H1: mu1 < mu2
alpha=0.05
# estad???stic mostral
z=(tx1-tx2)/(sqrt(s1^2/n1+s2^2/n2))
# graus de llibertat Student
kappa=(s1^2/n1+s2^2/n2)^2/(s1^4/(n1^2*(n1-1))+s2^4/(n2^2*(n2-1)))
kappa=round(kappa)
# la regi??? de rebuig ???s (-inf,Za) amb Za tal que
# P(Z<Za)=alpha, amb Z una Student amb kappa graus de llibertat.
Za=qt(0.05,kappa)
# com que z=-2.085 < Za=-1.67, la z est??? a la RR i
# per tant rebutgem H0 i acceptem H1, ???s a dir, que el
# sistema R1 t??? una mitjana inferior al R2.

## prob 4 #anova
m=4
n1=35
n2=40
n3=30
n4=38
VT=2400.5
VE=520.2
n=n1+n2+n3+n4
# (a) # ez
VR=VT-VE
sR2=VR/(n-m)
sE2=VE/(m-1)
# (b) # calcul RR (-inf, Zalpha) OJO; Si es calcula RA lower.tail=F
alpha=0.02
CL=1-alpha
z=sE2/sR2
# la RR ???s de la forma (Za,inf),
# on Za ???s tal que P(Z>Za)=alpha
# amb Z una Fischer(m-1,n-m)
Za=qf(CL,m-1,n-m)
# com que z=12.82 > Za=3.39, tenim que z est??? a la RR
# i rebutgem per tant H0, ???s a dir, acceptem que hi
# ha difer???ncia entre els tractaments.

# El p-valor ???s igual a la P(Z>z):
1-pf(z,m-1,n-m)
# surt p=1.9e-7, i per tant aix??? confirma que rebutgem H0

## prob 5
# El nombre de 6 que s'obtenen ???s una Bi(n,p)
# que aproximem per una N(n*p,sqrt(n*p*(1-p)))
# i la proporci??? de 6 ???s llavors la normal
# P=N(p,sqrt(p*(1-p)/n))
# de manera que Z=(P-p)/sqrt(p*(1-p)/n) ???s una N(0,1)
alpha=0.02
CL=1-alpha
p=1/6
# (a)
n=100
x=19
tp=x/n
# H0: p=1/6
# H1: p>1/6
# estad???stic
z=(tp-p)/sqrt(p*(1-p)/n)
# la RR ???s (Za,inf) amb P(Z>Za)=0.05
Za=qnorm(CL)
# Com que z=0.626 < Za= 2.057, acceptem H0 
# i per tant que p=1/6 i el dau no est??? trucat
# a favor del 6.

# el p-valor ???s P(N(0,1)>z)
1-pnorm(z,lower.tail=T) # =pnorm(z,lower_tail=F)
# com que surt p=0.266 >> 0.02 mantenim H0

# (b)
n=2000
x=370
tp=x/n
# H0: p=1/6
# H1: p>1/6
# estad???stic
z=(tp-p)/sqrt(p*(1-p)/n)
# la RR ???s (Za,inf) amb P(Z>Za)=0.02
Za=qnorm(CL)
# Com que z=2.2 > Za= 2.057, rebutgem H0 
# i per tant que p=1/6 i acceptem que el dau est??? trucat
# a favor del 6.
pnorm(z,lower.tail=F)
# d???na p=0.0139 <0.02 i per tant rebutgem H0. 
# Mantindr???em H0 si el nivell de significaci??? l'agaf???ssim
# m???s petit que 0.0139:
Zal=qnorm(1-0.01389)

# En els dos casos la proporci??? experimental de 6 
# ???s superior al valor no trucat 1/6=0.167, per??? en 
# el segon cas el valor  0.185, malgrat ser inferior
# al 0.19 del primer cas, est??? soportat per molts m???s
# llan???aments que fan que sigui molt m???s improbable 
# per a un dau no trucat a fvor del 6.

## prob 6

n=30
tx=2.45
ty=25.34
covxy=1.6464
varx=0.775
# (a)
b1=covxy/varx
b0=ty-b1*tx
# el model lineal ???s y^*=b0+b1*x*
# i per a x*=2.6 tenim y* igual a
b0+b1*3.2
# (b)
sR=0.1629
VE=104.44-3.014
# El contrast ???s 
# H0: el model no explica les dades
# H1: el model explica les dades
# estad???stic z=VE/sR^2
z=VE/sR^2
# La VA ???s Z=F(1,n-2), i el p-valor 
# ???s P(Z>z):
p=1-pf(z,1,n-2)
# Com que p=6.86e-14<<<< 0.02, rebutgem H0 i acceptem
# que el model lineal ???s bo.

Ta = qt(1-0.02/2, n-2)
yt = sR*sqrt((1/n)+((3.2-tx)^2)/((n-1)*varx))
IC1=26.933-yt*Ta
IC2=26.933+yt*Ta

n=100
pt = 19/n
p = 1/6
alpha = 0.02
#H0: pt = p0
#H1: pt > p0
z=(pt-p)/(sqrt(p*(1-p)/n))
#F(N(0,1))(Za)= 1-alpha
Za = qnorm(1-alpha)
#z < Za -> z està a la RA. Acceptem H0
#P-valor = P(N(0,1)> z)
pval = 1-pnorm(z)
#pvalor > alpha, mantenim H0


n=2000
pt = 370/n
p = 1/6
alpha = 0.02
#H0: pt = p0
#H1: pt > p0
z=(pt-p)/(sqrt(p*(1-p)/n))
#F(N(0,1))(Za)= 1-alpha
Za = qnorm(1-alpha)
#z < Za -> z està a la RA. Acceptem H0
#P-valor = P(N(0,1)> z)
pval = 1-pnorm(z)
#pvalor > alpha, mantenim H0

# En els dos casos la proporció experimental de 6 
# és superior al valor no trucat 1/6=0.167, però en 
# el segon cas el valor  0.185, malgrat ser inferior
# al 0.19 del primer cas, està soportat per molts més
# llançaments que fan que sigui molt més iMprobable 
# per a un dau no trucat a favor del 6.


#p = P(Z>= z) -> 1-p...()
#p = P(Z<= z) -> p...()