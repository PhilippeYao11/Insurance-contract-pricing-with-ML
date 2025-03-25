#----------------------------------------------#
#            PROJET DATA SCIENCE               #
#----------------------------------------------#


#Libraries-----
library(tidyverse)
library(caret)
library(MASS)
library(glmnet)
library(broom)
library(klaR)
library(e1071)
library(rpart)
library(rpart.plot)
library(car)
library(glmnet)
library(ggplot2)
library(plotly)
library(zoo)
library(caret)
library(data.table)
library(lubridate)
library(dplyr)
library(corrplot)
library(questionr)
library(parallel)

#Path--------
rm(list = ls())

path <- "/Users/olaniranokounlolabiaou/Desktop/Actuariat - Year 3/S1/Data Science/Projet/WD"
setwd(path)

#DATA MANAGEMENT-----

#chargement des donnees qui sont dans le repertoire de travail
data_train12 <- read.csv(file = "dataTrain_12.csv", header = T, sep = ";" ,dec =",", stringsAsFactors = T)
data_test <- read.csv(file = "test.csv", header = T, sep = ";" ,dec =",", stringsAsFactors = T)
#data_train12_bis <- read.csv(file = "dataTrain_12_retraite.csv", h = T, sep = ";")

df <- data_train12
summary(df)

test <- data_test
summary(test)
#Data Cleaning----

#Gender
levels(df$gender) <- c("Female", "Male", "Male")
#summary(df$gender)

#carGroup
df$carGroup <- as.factor(df$carGroup)
test$carGroup <- as.factor(test$carGroup)
#levels(df$carGroup)
#summary(df$carGroup)

#material
df$material <- as.factor(df$material)
test$material <- as.factor(test$material)
#summary(df$material)

summary(df)
summary(test)


#Feature engineering-----

#Creation de la df de regression
dfReg <- df[df$claimValue>0,]
summary(dfReg)


#Creation de la variable sinistre dans la df de base
df$sinistre <- (df$claimValue > 0) * 1
df$sinistre <- as.factor(df$sinistre)


#Data Exploration-----
##Variables categorielles

#affecter à var la v.a qualitative qu'on veut observer
#ne pas oublier de modifier le nom de la var pour modifier le titre

par(mfrow = c(1,1))

#var <- df$gender
#var <- df$carType
var <- df$carGroup
nomVar <- "carGroup"

pie(summary(var), col = rainbow(nlevels(var)), 
    main=paste("Pie Chart ", nomVar, sep = ""))

barplot(summary(var), main=paste("Fréquences absolues ", nomVar, sep = "\n"), 
        col="blue", las=1, ylim = c(0, max(summary(var))))


#gender
#pie(summary(df$gender), col = rainbow(2))
#barplot(summary(df$gender), main="Fréquences absolues sexe", col="blue", las=2,
        #ylim = c(0,20000))
  

#carType
#pie(summary(df$carType))
#barplot(summary(df$gender), main="Fréquences absolues sexe", col="blue", las=2,
       # ylim = c(0,20000))

###Variables quantitatives----
summary(df)
par(mfrow = c(1,2))

####Age----

plot(test$age, main = "Observation des ages \n dans test")
plot(df$age, main = "Observation des ages")
plot(dfReg$age, main = "Observation des ages dans \n la base de Regression")

ageboxtest<- boxplot(test$age, main = "Boxplot des ages \n dans test")
ageboxtest$stats
agebox <- boxplot(df$age, main = "Boxplot des ages" )
mean(df$age)
agebox$stats
propageAbberant <- length(df$age[df$age>agebox$stats[5,1]])/length(df$age)

ageboxReg <- boxplot(dfReg$age, main = "Boxplot des Ages dans \n la base de Regression")
mean(dfReg$age)
ageboxReg$stats
propageAbberantReg <- length(dfReg$age[dfReg$age>ageboxReg$stats[5,1]])/length(dfReg$age)

dfRegAgeExt <- dfReg[dfReg$age > ageboxReg$stats[5,1],]
propAgeAbberant <-length(dfRegAgeExt$age)/length(dfReg$age) #en terme d'effectif
ClaimVAgeAbberant <- sum(dfRegAgeExt$claimValue)
ClaimVTotal <- sum(dfReg$claimValue)
#ClaimVTotal <- sum(df$claimValue)
propCVAgeAbberant <-  ClaimVAgeAbberant/ClaimVTotal
####CarValue----

par(mfrow = c(1,3))

plot(test$carValue, main = "Observation des Valeurs \n des Voitures \n dans test")
plot(df$carValue, main = "Observation des Valeurs \n des Voitures")
plot(dfReg$carValue, main = "Observation des Valeurs \n des voitures dans \n la base de Regression")

carValueBoxtest <- boxplot(test$carValue, main = "Boxplot des valeurs de \n voiture dans la \n test")
carValuebox <- boxplot(df$carValue, main = "Boxplot des valeurs de \n voiture" )
carValueboxReg <- boxplot(dfReg$carValue, main = "Boxplot des valeurs de \n voiture dans la \n base regression")


carValueBoxtest <- boxplot(test$carValue, main = "Boxplot des valeurs de \n voiture dans la \n test")
carValuebox <- boxplot(df$carValue, ylim = c(0, 50000),
                       main = "Boxplot des valeurs de \n voiture" )
carValueboxReg <- boxplot(dfReg$carValue, ylim = c(0, 50000),
                          main = "Boxplot des valeurs de \n voiture dans la \n base regression")

dfRegCarValueExt <- dfReg[dfReg$carValue > carValueboxReg$stats[5,1],]
propCarValueAbberant <-length(dfRegCarValueExt$carValue)/length(dfReg$carValue) #en terme d'effectif
ClaimVCarValueAbberant <- sum(dfRegCarValueExt$claimValue)
ClaimVTotal <- sum(dfReg$claimValue)
ClaimVTotal <- sum(df$claimValue)
propCVCarValAbberant <-  ClaimVCarValueAbberant/ClaimVTotal


####Age et CarValue -----
#On regarde après elimination des CarValue extremes ce qui reste des Ages extremes
dfRegNoCarValExt <- dfReg[dfReg$carValue <= carValueboxReg$stats[5,1],]
dfRegNoCarValExtAgeExt <- dfRegNoCarValExt[dfRegNoCarValExt$age > ageboxReg$stats[5,1],]
dfRegCarValEtAgeExt <- dfReg[(dfReg$carValue > carValueboxReg$stats[5,1]) | (dfReg$age > ageboxReg$stats[5,1]),]

propCarValEtAgeAbberant <-length(dfRegCarValEtAgeExt$carValue)/length(dfReg$carValue) #en terme d'effectif
ClaimVCarValEtAgeAbberant <- sum(dfRegCarValEtAgeExt$claimValue)
ClaimVTotal <- sum(dfReg$claimValue)
propCVCarValEtAgeAbberant <-  ClaimVCarValEtAgeAbberant/ClaimVTotal



plot(df$claimValue)

hist(df$claimValue, col="lightblue", main="Montants Indemnises")
plot(density(df$claimValue), main = "Density claimValue")

PositiveClaimV <- df$claimValue[df$claimValue>0]

View(dfReg)
PositiveClaimV2 <-dfReg$claimValue[dfReg$claimValue<posClaimVbox$stats[5,1]]
plot(density(PositiveClaimV2), main = "Density claimValue")
rug(PositiveClaimV2, ticksize = 0.03)
plot(density(log(PositiveClaimV2), main = "Density claimValue"))
plot(density(-log(PositiveClaimV2), main = "Density claimValue"))



####ClaimValue-----




plot(dfReg$claimValue, ylab = "ClaimValue", main = "Claim Value Plot")
hist_pos_claimV <- hist(dfReg$claimValue, col="lightblue", main="Montants Indemnises", 
     ylim = c(0, 8000))
plot(hist_pos_claimV$density, type = "l")
plot(density(dfReg$claimValue), main = "Density of Positive Claim Value")
abline(a = 0, b = 1, col = "red")

plot(density(log(dfReg$claimValue)))
qqPlotLogClaim<-qqnorm(log(dfReg$claimValue))


posClaimVbox <-boxplot(dfReg$claimValue, ylim = c(0, 10000))
posClaimVbox$stats
#les claimValue extremes
extremeClaimV <- dfReg[dfReg$claimValue > posClaimVbox$stats[5,1],]
extremeClaimV <- dfReg[dfReg$claimValue > 4044.927,]

summary(dfReg$claimValue)
summary(extremeClaimV$claimValue)
View(extremeClaimV)
length(extremeClaimV$claimValue)/length(dfR)

boxplot(dfReg$claimValue, horizontal = T)

posClaimVbox$stats

#les claimValue extremes


plot(x = c(1:30000),  y = data_train12$claimValue, type = "l")
plot(density(data_train12$claimValue))

dotchart(data_train12$claimValue, main="Serie Brute", pch=20)
dotchart(sort(data_train12$claimValue), main="Serie Ordonnee", pch=20)

claimVbox <- boxplot(df$claimValue)
claimVbox$stats








