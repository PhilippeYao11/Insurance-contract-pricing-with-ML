#-----------------------------------------------------#
           #Importation des données
#-----------------------------------------------------#

datapos<-read.csv2("C:/Users/Philolivier/Downloads/Documents ISFA/Master 2 Actuariat/Semestre 1/Projet Data Science/projet 2023/trainRegression.csv",sep=";",header=T,stringsAsFactors = T)
hemin<-"C:/Users/Philolivier/Downloads/Documents ISFA/Master 2 Actuariat/Semestre 1/Projet Data Science/projet 2023"
datatest<-read.csv2(paste(chemin,"/test.csv", sep =""), header=T, sep=";", dec = ",",stringsAsFactors = T)
#On réimporte la base test initiale pour rajouter la colonne id car on l'avait supprimée pour nos modèles de régression 

datatest2<-read.csv2(paste(chemin,"/datatest2.csv", sep =""), header=T, sep=";", dec = ",",stringsAsFactors = T)



#-----------------------------------------------------#
#Projection de la prime pure avec la régression RIDGE#
#-----------------------------------------------------#


#Il faut rappeler qu'on a fait en amont une régression log-normale sur 
#la base "test" initiale, c'est ce qui permet d'obtenir "datatest2".
#A présent on va la pénaliser par la technique RIDGE puis faire les projection

xRtest <- model.matrix(log(claimValue)~., data = datatest2)[,-1]
xR <- model.matrix(log(claimValue)~., data = datatest2)[,-1]
yR <- log(datatest2$claimValue)

## Ridge
regRidge <- glmnet(xR,yR,family = "gaussian",alpha=0)

## Lambda Tuning
ridgeCV <- cv.glmnet(x = xR, y = yR, lambda = regRidge$lambda, type.measure = "mse", nfolds = 70, alpha = 0)
ridgeCV$lambda.min
## On en deduit le meilleur lambda, et on recalibre le modele avec
regRidgeF <- glmnet(x = xR, y = yR, family = "gaussian", alpha = 0, 
                   lambda = ridgeCV$lambda.min)

xR1 <- model.matrix(log(claimValue)~., data = datatest2)[,-1]
RidgeCV1 <- cv.glmnet(x = xR1, y = log(datatest[,11]), lambda = regRidge$lambda, #(1:50)*10, 
                     type.measure = "mse", nfolds = 10, alpha = 0)
regRidge1 <- glmnet(x = xR1, y = log(datatest2[,11]), family = "gaussian", alpha = 0, 
                   lambda = RidgeCV1$lambda.min)

xRtest <- model.matrix(log(claimValue)~., data = datatest2)[,-1]


averageCost <- predict.glmnet(regRidgeF,  newx=xRtest)

datatest2$claimValue<-averageCost
datatest2 <- transform(datatest2, claimValue = exp(claimValue))
datatest2$id<-datatest$id
datatest2 <- datatest2[, c("id", setdiff(names(datatest2), "id"))]

#On renomme la variable claimValue

datatest2 <- datatest2 %>% rename("averageCost" = "claimValue")

#------------------------------------#
#   Bases des claimValue projetées   #
#------------------------------------#

testfinal<-datatest2[,c("id","averageCost")]
