#setwd("C:/Users/Kevin Macario/Desktop/Uvg/9no Semestre/Mineria de Datos/HT5-Bayes-Ingenuo")
setwd("C:/Users/LENOVO/Desktop/Clases/Minería de datos/Github/HT5-Bayes-Ingenuo")
datatest <- read.csv("house-prices-advanced-regression-techniques/test.csv")
datatrain <- read.csv("house-prices-advanced-regression-techniques/train.csv")
prices <- read.csv("house-prices-advanced-regression-techniques/sample_submission.csv")
library(e1071)
library(caret)
library(dplyr)
library(cluster) 
library(caret)

porcentaje<-0.8
set.seed(666)


datatestc = merge(x = datatest, y = prices, by = "Id")

datos = rbind(datatestc, datatrain)
colSums(is.na(datos))
datos$PoolQC<-NULL
datos$Fence <- NULL
datos$MiscFeature<-NULL
datos$FireplaceQu<-NULL
datos$Alley<-NULL
datos<- datos[complete.cases(datos), ]
mask <- unlist(lapply(datos, is.numeric))
datos_numericos <- datos[,mask]
cluster<-kmeans(datos_numericos ,3, iter.max = 200 )
datos$grupo <- cluster$cluster

summary(datos$SalePrice[datos$grupo==1])
summary(datos$SalePrice[datos$grupo==2])
summary(datos$SalePrice[datos$grupo==3])

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
c3 <- rgb(120,160,180, max = 255, alpha = 80, names = "lt.hola")
break1 = floor(sqrt(nrow(datos[datos$grupo ==1,])))
break2 = floor(sqrt(nrow(datos[datos$grupo ==2,])))
break3 = floor(sqrt(nrow(datos[datos$grupo ==3,])))

histo1 <- hist(datos$SalePrice[datos$grupo ==1], breaks  = break1,plot = FALSE )
histo2 <- hist(datos$SalePrice[datos$grupo ==2],breaks = break2, plot = FALSE )
histo3 <- hist(datos$SalePrice[datos$grupo ==3],breaks = break3, plot = FALSE )
plot(histo1, c = c1, xlim = c(4000, 500000 ), ylim = c(0,200), main = "Histogramas 3 clústers", xlab ="Precios")
plot(histo2, c = c2,add = TRUE)
plot(histo3, c = c3,add = TRUE)

###################

corte <- sample(nrow(datos),nrow(datos)*porcentaje)
train<-datos[corte,]
test<-datos[-corte,]


modelo<-naiveBayes(as.factor(grupo)~., data=train, laplace =1)
predBayes<-predict(modelo, newdata = test[,1:38])

cm<-caret::confusionMatrix(predBayes,factor(test$grupo))
cm

