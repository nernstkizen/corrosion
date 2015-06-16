######################################################################################################################
# Header file
######################################################################################################################

#---------------------------------------------------------------------------------------------------------------------
### Setup
#---------------------------------------------------------------------------------------------------------------------
rm(list=ls())
options(scipen=999)
# options(java.parameters = "-Xmx10000m")
options(java.parameters = "-Xmx1g")



#---------------------------------------------------------------------------------------------------------------------
### Library
#---------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(geoR)
library(fields)
library(ggplot2)
library(directlabels)
library(randomForest)
library(gbm)
library(cvTools)
library(caret)
#---------------------------------------------------------------------------------------------------------------------
### Project folder path
#---------------------------------------------------------------------------------------------------------------------
#repo_path = "C:/Apps/projects/xxx"
repo_path = "Z:/project"



#---------------------------------------------------------------------------------------------------------------------
### Data path
#---------------------------------------------------------------------------------------------------------------------
wd <- getwd()
setwd(file.path(repo_path, "/data"))




#---------------------------------------------------------------------------------------------------------------------
### Load data
#---------------------------------------------------------------------------------------------------------------------
#@@Wear data
weardata<-read.csv('wear.csv',header=TRUE,as.is=TRUE)


#---------------------------------------------------------------------------------------------------------------------
### Reset working dir
#---------------------------------------------------------------------------------------------------------------------
setwd(wd)



#Just for convenience
#SlurryVelocity+BitumenWtPct+SolidsConcWt+UltraFinesPct+FinesPct+d50+P     

#RF


#Wear 600

fitControl <- trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 5,
  repeats=10)


rfGrid <- expand.grid(mtry=c(1,2,3,4,5,6,7))

rfFit.600<- train(wear600 ~ SlurryVelocity+BitumenWtPct+SolidsConcWt+UltraFinesPct+FinesPct+d50+P ,
                  data = weardata,
                  method = "rf",
                  tuneGrid=rfGrid,
                  trControl = fitControl,
                  verbose = FALSE,
                  importance=TRUE)
plot(varImp(rfFit.600),lwd=20)

#Wear 630


fitControl <- trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 5,
  repeats=10)


rfGrid <- expand.grid(mtry=c(1,2,3,4,5,6,7))

rfFit.630<- train(wear630 ~ SlurryVelocity+BitumenWtPct+SolidsConcWt+UltraFinesPct+FinesPct+d50+P ,
                    data = weardata,
                    method = "rf",
                    tuneGrid=rfGrid,
                    trControl = fitControl,
                    verbose = FALSE,
                    importance=TRUE)
plot(varImp(rfFit.630))



#wear 700

fitControl <- trainControl(## 5-fold CV
  method ="repeatedcv",
  number = 5,
  repeats=10)
 

rfGrid <- expand.grid(mtry=c(1,2,3,4,5,6,7))

rfFit.700<- train(wear700 ~ SlurryVelocity+BitumenWtPct+SolidsConcWt+UltraFinesPct+FinesPct+d50+P ,
                    data = weardata,
                    method = "rf",
                    tuneGrid=rfGrid,
                    trControl = fitControl,
                    verbose = FALSE,
                    importance=TRUE)

plot(varImp(rfFit.700))





#Boosting


#Wear 600

fitControl <- trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 5,
  repeats=10)

gbmGrid <- expand.grid(interaction.depth=5,n.trees = 2000, shrinkage=0.01, n.minobsinnode=10)

gbmFit.600<- train(wear600 ~ SlurryVelocity+BitumenWtPct+SolidsConcWt+UltraFinesPct+FinesPct+d50+P ,
                    data = weardata,
                    method = "gbm",
                    tuneGrid=gbmGrid,
                    trControl = fitControl,
                    verbose = FALSE)
plot(varImp(gbmFit.600))

#Wear 630

fitControl <- trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 5,
  repeats=10)

gbmGrid <- expand.grid(interaction.depth=4,n.trees = 2000, shrinkage=0.01, n.minobsinnode=10)

gbmFit.630<- train(wear630 ~ SlurryVelocity+BitumenWtPct+SolidsConcWt+UltraFinesPct+FinesPct+d50+P ,
                    data = weardata,
                    method = "gbm",
                    tuneGrid=gbmGrid,
                    trControl = fitControl,
                    verbose = FALSE)


plot(varImp(gbmFit.630))





#wear 700


fitControl <- trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 5,
  repeats=10)

gbmGrid <- expand.grid(interaction.depth=4,n.trees = 2000, shrinkage=0.01, n.minobsinnode=10)

gbmFit.700<- train(wear700 ~ SlurryVelocity+BitumenWtPct+SolidsConcWt+UltraFinesPct+FinesPct+d50+P ,
                    data = weardata,
                    method = "gbm",
                    tuneGrid=gbmGrid,
                    trControl = fitControl,
                    verbose = FALSE)


plot(varImp(gbmFit.700))






#Multivariate Regression



#Wear 600


n<-dim(weardata)[1]
result600<-rep(0,100)
for (i in 1:600)
{

n1<-base::sample(1:n,size=ceiling(n*0.8))
lm600<-lm(log(wear600+0.01) ~ SlurryVelocity+BitumenWtPct+SolidsConcWt+UltraFinesPct+FinesPct+d50+P ,
          data = weardata[n1,])
lm600 <- stepAIC(lm600, direction="both")
prelm600<-exp(predict(lm600,newdata=weardata[-n1,6:12]))-0.01
trulm600<-weardata[-n1,3]
result600[i]<-sqrt(mean((prelm600-trulm600)^2))
}


#Wear 630
n<-dim(weardata)[1]
result630<-rep(0,100)
for (i in 1:600)
{
  
  n1<-base::sample(1:n,size=ceiling(n*0.8))
  lm630<-lm(log(wear630+0.01) ~ SlurryVelocity+BitumenWtPct+SolidsConcWt+UltraFinesPct+FinesPct+d50+P ,
            data = weardata[n1,])
  lm630 <- stepAIC(lm630, direction="both")
  prelm630<-exp(predict(lm630,newdata=weardata[-n1,6:12]))-0.01
  trulm630<-weardata[-n1,3]
  result630[i]<-sqrt(mean((prelm630-trulm630)^2))
}


#wear 700


n<-dim(weardata)[1]
result700<-rep(0,100)
for (i in 1:600)
{
  
  n1<-base::sample(1:n,size=ceiling(n*0.8))
  lm700<-lm(log(wear700+0.01) ~ SlurryVelocity+BitumenWtPct+SolidsConcWt+UltraFinesPct+FinesPct+d50+P ,
            data = weardata[n1,])
  lm700 <- stepAIC(lm700, direction="both")
  prelm700<-exp(predict(lm700,newdata=weardata[-n1,6:12]))-0.01
  trulm700<-weardata[-n1,3]
  result700[i]<-sqrt(mean((prelm700-trulm700)^2))
}


