#-----------------------------------------------------------------------------------------#
#                                     PROJET DATA SCIENCE                                 #
#-----------------------------------------------------------------------------------------#

#---------Libraries--------#

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
library(ggplot2)
library(plotly)
library(zoo)
library(data.table)
library(lubridate)
library(dplyr)
library(corrplot)
library(questionr)
library(parallel)
library(randomForest)
library(tree)
library(xgboost)
library(pROC)
library(ROCR)
library(stats)


#----------Path--------
rm(list = ls())

path <- "C:/Users/Philolivier/Downloads/Documents ISFA/Master 2 Actuariat/Semestre 1/Projet Data Science/projet 2023"
setwd(path)
#====================================================================================#
#       Annexe contient les fonctions necessaires au fonctionnement du script        #
#====================================================================================#
source("Annexe.R")

#====================================================================================#
#                                    Load Data                                       #
#====================================================================================#

Train <- read.csv(file='dataTrain_12.csv',header = TRUE, sep = ";", stringsAsFactors = TRUE,dec = ",")

Test <- read.csv('test.csv',header = TRUE, sep = ";" , stringsAsFactors = TRUE, dec = ",")

Test.estime <- read.csv('first_results.csv',header = TRUE, sep = ";" , stringsAsFactors = TRUE, dec = ",")
# Test.estime : contient les résultats d'une premiere régression log-normale sur la base "test" initiale

#====================================================================================#

#----------------------- Calcul des probas et couts moyens --------------------------#

#====================================================================================#

#=========== Bases de classification  ==================#
train.classif <- mise_en_forme(Train, is.classif = T, is.train = T)
test.classif <- mise_en_forme(Test, is.classif = T, is.train = F)

#=========== Bases de regression  ==================#
train.reg <- mise_en_forme(Train, is.classif = F, is.train = T)
test.reg <- mise_en_forme(Test, is.train = F, is.classif = F)



#=========== Calcul des probas ==================#

set.seed(123)
proba <- probas(train.classif, test.classif)

#=========== Calcul des couts moyens ==================#

set.seed(123)
prime_pure <- cout_moyen(Test.estime,Test)

#=========== Mise en forme et export ==================#

premium_12 <- exportation(proba,prime_pure, Test)


essai <- read.csv('premium_12.csv',header = TRUE, sep = "," , stringsAsFactors = TRUE, dec = ",")
view(essai)
