setwd("C:/Users/Kevin Macario/Desktop/Uvg/9no Semestre/Mineria de Datos/HT5-Bayes-Ingenuo")
datatest <- read.csv("house-prices-advanced-regression-techniques/test.csv")
datatrain <- read.csv("house-prices-advanced-regression-techniques/train.csv")
prices <- read.csv("house-prices-advanced-regression-techniques/sample_submission.csv")
library(e1071)
library(caret)
library(dplyr)
porcentaje<-0.7
datos<-iris
set.seed(666)

numstest <- sapply(datatest, is.numeric)
numtrain <-sapply(datatrain, is.numeric)

cdatatest <- datatest[,numstest]
cdatatrain <- datatrain[,numtrain]
names(cdatatest)
names(cdatatrain)
cdatatrain$SalePrice <- NULL

cdata <- bind_rows(cdatatest,cdatatrain)
cuantitativas_precios<-merge(x = cdata, y = prices, by = "Id")
cuantitativas_precios <-  na.omit(cuantitativas_precios)
cuantitativas_precios

Q1 <- 168174
Q3 <- 185825
rangointer <- Q3-Q1

uSup <- Q3 + 1.5*rangointer
uSup
uInf <- Q1 - 1.5*rangointer
uInf
library(data.table)
sales <- setDT(cuantitativas_precios)[!(SalePrice %between% c(uInf, uSup))]
sales

