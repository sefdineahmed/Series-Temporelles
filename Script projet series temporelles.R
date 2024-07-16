

#Les packages

library(tseries)
library(forecast)
library(urca)
library(ggplot2)

## 1) DESCRIPTION DE LA SERIE

## Le chargement de la base de données


donnees=read.table(file.choose(), sep=",", header=TRUE)
attach(donnees)
str(donnees)
dim(donnees)
edit(donnees)
head(donnees)

is.ts(IPG2211A2N)

data=ts(IPG2211A2N,start=c(1985,1),end=c(2018,1),frequency=12)

is.ts(data)

#  Visualisation graphique des donnees

autoplot(data)

plot(data,main="Production électrique de jan 1985 - Jan 2018",
     xlab="Temps",ylab="Production")

# 1.1 Décomposition de la série

#############################

decomp=decompose(data,type = "multiplicative")
plot(decomp,lwd=2)


# Ajustement de la tendance par MLS

t=time(IPG2211A2N)

C=decomp$trend

rls=lm(tendance~t)
summary(rls)

## Ajsutement linéaire de l'equation de la tendance

temps=1:397 
temps2=temps^2
temps2
temps3=temps^3
temps3


## Ajuster un modèle linéaire simple

mod=lm(tendance~temps)
summary(mod)

## Ajuster un modèle linéaire multiple

mod1=lm(tendance~temps+temps2)
summary(mod1)

mod2 = lm(tendance~temps+temps2+temps3)
summary(mod2)


## Comparaison de deux modèles multiples par AIC

AIC0=AIC(mod)
AIC0
AIC1=AIC(mod1)
AIC1
AIC2=AIC(mod2)
AIC2


## On conserve le model rlmdeg3


### Etude de l'adéquation du modèle
err=resid(rlmdeg3)
err

# 2. Modélisation la série par la méthodologie de Box & Jenkins

##  Etude de la stationnarité

### Le test de stationnarité de Dickey Fuller


adf.test(data)

test=ur.df(data, type = "trend", selectlags = c("AIC"))
summary(test)

pp.test(data)

kpss.test(data) 

### Avec trois type de text on remarque que la serie est stationnaire ###


## Les corrélogramme

par(mfrow=c(1,2))
###Fonction  d'autocorrélation partielle AR
pacf(data)
### Fonction  d'autocorrélation MA
acf(df) 

model=auto.arima(data)
summary(model)


## Adéquation du model ou validation

## Par l'étude des résidus

####   Calcule des résidus

residus=resid(model)
residus

####  Test de nullite de la moyenne des residus

# H0: la moyenne des residus est nulle
# H1: la moyenne des residus est non nulle

t.test(residus,mu=0,conf.level=0.95)

##La p-value est supérieur à 0.05 donc on accepte H0
##l'inttervalle de confiance de la moyenne des résidus est :[-0,38 ; 0.08]
##On accepte l'hypothèse selon laquelle, la moyenne des résidus
##  pour notre model est egale 0

#### Test de normalité des résidus

# H0: les residus sont gaussiens
# H1: les residus sont non gaussiens

shapiro.test(residus)
jarque.bera.test(residus)

#### Test de bruit blant

Box.test(residus,type="Ljung-Box")
Box.test(residus,type="Box-Pierce")


## La visualisation graphique des résidus

# Distribution des résidus
par(mfrow=c(1,1))
hist(residus,probability='T',n=30,col='blue', main='Histogramme des residus')
lines(density(residus),col='red',lwd=3)
lines(dnorm(0,1),col=3,lwd=3)

par(mfrow=c(1,3))
plot(density(residus),lwd=3,col=3,main='densité de residus')

boxplot(residus,horizontal=TRUE, main="residus",col=3)
hist(residus,probability='T',n=30,col='blue', main='Histogramme des residus')
lines(density(residus),col='red',lwd=3)

checkresiduals(model,col=3,lwd=2)

### Quantiles plot des résidus

par(mfrow=c(1,1))
qqnorm(residus,col=4)
qqline(residus,col="red",lwd=2)

#### Validation par apprentissage test #####


nap=(397*80)/100
nap
ntest=(397*20)/100
ntest
nap=318 
ntest=79

base_app=data[1:317]
base_test=data[318:396]
base_app
base_test


#### Conversion des bases en série temporelle

base_app=ts(base_app,start = c(1985,1,1),end = c(2011,1,6),frequency = 12)
base_app

base_test=ts(base_test,start = c(2011,1,7),end = c(2018,1,1),frequency = 12)
base_test

##### Appretissage

model_app=Arima(base_app,order = c(2,1,1),list(order=c(0,1,1),period=12),
                include.mean=FALSE,method="CSS-ML")
model_app
model

####### Prévision avec la base test

prev_test=predict(neew_model,n.ahead=83)
new_prev=prev_test$pred
new_prev

# Comparaison des valeurs prédites aux valeurs réelles de la base test

plot(base_test, type='l', col=1, lwd=3,, xlab='Date',ylim=c(80,140),
     ylab='production electrique', main='Validation par apprentissage et test')

lines(new_prev,col=19, lwd=3)

lines(new_prev-2*prev_test$se,lty=4,lwd=1,col="blue")
lines(new_prev+2*prev_test$se,lty=4,lwd=1,col="blue")

legend("topleft",legend = c("OBSERVATION REELLES","Prévisions","IC"),
       col=c(1,19,"blue"),lwd=c(2,2,2))

# 3. Prevision par ARIMA de la production electrique


## Pour une année (2019)


p1=predict(model,n.ahead=12)
prev_2019 = p1$pred
prev_2019

#### Pour 3 années (2021)

p3=predict(model,n.ahead=36)
prev_20121 = p3$pred
prev_2021

### Présentation des prévision pour une année


plot(prev_2019, col=19,type='l',lwd=3, xlab='Date',ylim=c(80,140),
     ylab='Production electrique', main='Prévisions pour une année(2019)')

lines(prev_2019-2*p1$se,lty=4,lwd=2,col="blue")
lines(prev_2019+2*p1$se,lty=4,lwd=2,col="blue")

legend("topleft",legend = c("Prévisions","Intervalles de confiance"),
	col=c(19,"blue"),lwd=c(4,4))

### Présentation des prévision pour trois années

plot(prev_20121, type='l', col=19, lwd=3,, xlab='Date',ylim=c(80,140),
     ylab='Production electrique', main='Prévisions pour trois années(2021)')

lines(prev_20121-2*p3$se,lty=4,lwd=2,col="blue")
lines(prev_20121+2*p3$se,lty=4,lwd=2,col="blue")

legend("topleft",legend = c("Prévisions","Intervalles de confiance"),
	col=c(19,"blue"),lwd=c(4,4))

########################################################
######## 4. La prévision par lissage exponentiel #######
########################################################

lis=HoltWinters(data)
summary(lis)
plot(lis,main="Lissage Exponentielle",col="blue",lwd=1)

## Pour une année(2019)
lisprev_2019 = predict(lis, n.ahead=12) 
lisprev_2019

plot(lisprev_2019,col=19,lwd=2,ylim=c(80,140),
	main="Prévisions pour une année (2019)par lissage exponentielle",xlab="Date",ylab="Production electrique")


## Avec les intervalle de confiance
### Pour une années
lis_ic=predict(lis, n.ahead=12,prediction.interval=TRUE) 
lis_ic

valeurs=lis_ic[,1]
valeurs
BI=lis_ic[,3]
BS=lis_ic[,2]

plot(lisprev_2019,col=3,lwd=2,ylim=c(80,140),
	main="Prévisions par lissage pour une année (2019)",xlab="Date",ylab="Production electrique")

lines(BI,col="blue",lty=3,lwd=3)
lines(BS,col="blue",lty=3,lwd=3)

legend("topleft",legend = c("Prévisions","Intervalles de confiance"),
	col=c(3,"blue"),lwd=c(4,4))

### Pour trois années 

lisprev_2021=predict(lis, n.ahead=36,prediction.interval=TRUE) 
lisprev_2021

plot(lisprev_2021[,1],col=3,lwd=2,ylim=c(80,150),
	main="Prévisions par lissage: pour trois années",xlab="Date",ylab="Production")

lines(lisprev_2021[,3],col="blue",lty=3,lwd=3)
lines(lisprev_2021[,2],col="blue",lty=3,lwd=3)

legend("topleft",legend = c("Prévisions","Intervalles de confiance"),
	col=c(3,"blue"),lwd=c(4,4))

## Comparaison des prévision par ARIMA à des prévision par lissage

### Pour une annee

M=1:12

Mois=c("Jan","Fev","Mar","Avr","Mai","Jui","Juil","Aoùt","Sep","Oct","Nov","Dec")

prev_ARIMA=prev_2019
prev_Lis=lisprev_2019

plot(prev_ARIMA,col=3,lwd=3,ylim=c(80,150),xlab="Date",ylab="Production electrique",
	main="Comparaison des prévisions pour une année")

lines(prev_Lis,col=2,lwd=3)

lines(prev_2019-2*p1$se,lty=4,lwd=2,col="blue")
lines(prev_2019+2*p1$se,lty=4,lwd=2,col="blue")

lines(BI,col="black",lty=3,lwd=3)
lines(BS,col="black",lty=3,lwd=3)

legend("topleft",legend = c("ARIMA","LISSAGE","IC(Arima)","IC(Lissage)"),
	col=c(3,2,"blue","black"),lwd=c(3,3,3,3))



### Pour trois annees

prev_ARIMA3=prev_20121
prev_Lis3=lisprev_2021[,1]

plot(prev_ARIMA3,col="orange",lwd=3,ylim=c(80,150),xlab="Date",ylab="Production electrique",
	main="Comparaison des prévisions pour trois années")

lines(prev_Lis3,col="green",lwd=3)

lines(prevision_3-2*p3$se,lty=4,lwd=2,col="blue")
lines(prevision_3+2*p3$se,lty=4,lwd=2,col="blue")

lines(lisprev_2021[,3],col="black",lty=3,lwd=3)
lines(lisprev_2021[,2],col="black",lty=3,lwd=3)

legend("topleft",legend = c("ARIMA","LISSAGE","IC(Arima)","IC(Lissage)"),
	col=c("orange","green","blue","black"),lwd=c(3,3,3,3))
  









