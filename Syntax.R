#MIN MAX Norm
min=apply(Data,2,min)
max=apply(Data,2,max)
DataNorm=as.data.frame(scale(Data,center=min,scale=max-min))

#INSTALL PACKAGE
install.packages("caTools")
install.packages("e1071")
install.packages("GA")
install.packages("parallel")
install.packages("doParallel")
install.packages("caret")
install.packages("rpart")

library("caTools")
library("e1071")
library("GA")
library("parallel")
library("doParallel")
library("caret")
library("rpart")

#MEMASUKKAN DATA
TANORM <- read_excel("D:/kuliah/TA/Aziz/File Dataset/TANORM.xlsx")
Data<-TANORM
View(Data)

#SET FAKTOR
Data$Y=factor(Data$Y,levels=c(-1,1))
summary(Data)

#UNDERSAMPLING
train_down<-downSample(Data[,-ncol(Data)],Data$Y,list=FALSE)
Data<-train_down
#OVERSAMPLING
train_up<-upSample(Data[,-ncol(Data)],Data$Y,list=FALSE)
Data<-train_up

#MENENTUKAN DATA TRAIN DAN TEST
set.seed(123)
split<-sample.split(Data$Class,SplitRatio = 0.8)
train_Data=subset(Data,split==TRUE)
test_Data=subset(Data,split==FALSE)

#CEK DATA TRAIN DAN TEST
nrow(train_Data)
nrow(test_Data)
plot(train_Data)

#MENENTUKAN MODEL SVM
modelSVM<-svm(Class~.,data=train_Data)
modelSVM

#PREDIKSI 
predict.train<-predict(modelSVM,newdata = train_Data[-6])
head(predict.train)
predict.test<-predict(modelSVM,test_Data[-6])
head(predict.test)

ATAU
test_predict<-predict(modelSVM,test_Data)
result_test<-table(Predicted=test_predict,Actual=test_Data$Class)
result_test

#TEST KEAKURATAN
TS.train=table(train_Data[,6],predict.train)
acc.train<-sum(diag(TS.train))/sum(TS.train)*100
acc.train

TS.test=table(test_Data[,6],predict.test)
acc.test<-sum(diag(TS.test))/sum(TS.test)*100
acc.test

ATAU
mean(test_predict==test_Data$Class)*100

#CONFUSION MATRIX
install.packages("caret")
library("caret")
confusionMatrix(table(test_predict,test_Data$Class))

ptm<-proc.time()

#MENDEFINISIKAN FUNGSI
fitnessFunc <- function(x)
{
  model<-svm(Y~.,data = Data,cross=10, scale=FALSE)
  return(model$tot.accuracy)
}

#BATAS ATAS DAN BAWAH
theta_min <- c(p_cost = 2^-2, p_gamma = 2^-2)
theta_max <- c(p_cost = 2^2, p_gamma = 2^2)


fitnesvalue<-c()
solutions<-c()

#GA
results <- ga(type = "real-valued",
              fitness = fitnessFunc, names = names(theta_min),min = theta_min,max = theta_max, 
              selection = gaControl("real-valued")$selection,
              crossover = gaControl("real-valued")$crossover,
              mutation = gaControl("real-valued")$mutation,
              popSize = 10,
              pcrossover=0.8,
              pmutation=0.1,
              maxiter=10, 
              run=10, 
              maxFitness = 10, 
              keepBest=TRUE,
              parallel=TRUE,
              monitor=plot)

summary(results)

#OPTIONAL
solutions=c(solutions,summary(results)[11])
solutions

fitnesvalue=c(fitnesvalue,summary(results)[10])
fitnesvalue

proc.time()-ptm

