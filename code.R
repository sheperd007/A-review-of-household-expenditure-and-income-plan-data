library(readr)
library(tidyverse)
library(plyr); library(dplyr)
library(imputeMissings)
library(ggplot2)
library(hrbrthemes)
library(tidyr)
library(viridis)
library(ROSE)
library(pROC)
Data <- as.data.frame(read_csv("/media/sda6/Projects/Daramad khanevar/DataHD98_6.csv"))
Data=Data[,-1]
View(Data)
Columns<-c()
for(i in 1:(ncol(Data)-4)){
  if(sum(is.na(Data[i]))/nrow(Data) > 0.4)
    Columns<-c(Columns,i)
}
Data=Data[-Columns]
summary(Data)
#We know that the presence of variance indicates the presence of information in variables.
#Now if the variable has no variance it is worthless and we remove it from our data set.
Columns<-c()
for (i in 1:ncol(Data)) {
  if(length(unique(na.omit(Data[,i])))==1)
    Columns<-c(Columns,i)
}
Data=Data[-Columns]
summary(Data)
#Now we go Deep on Remaining variables
View(Data)
rm(Columns,i)
summary(Data)
#imputation of Edu and InEdu
temp = is.na(Data$InEdu)
unique(Data[temp ,]$Savad)
Data[temp ,]$Edu = 0
Data[temp ,]$InEdu = 2
rm(temp)
#Convert Categorical Value to Factors
factors=c( "C.Ostan" ,"Tedad.a","Gender","Savad","InEdu","Edu","Faaliat",
           "T.shaghel","T.M.S","T.O","N.S","Masleh" , "sookht.p","sookht.g","sookht.ab")
for (i in factors) {
  Data[,i]=factor(Data[,i])
}
rm(factors,i)
#Imputation of T.shaghel
#h2o.impute(data, column, method, groupBy)
#a Function for finding modes
Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}
Modes_of_T.shaghel=Data %>%  
  group_by(Tedad.a) %>% 
  summarise(Mode = Modes(na.omit(T.shaghel)))
#With Respect to Modes_of_T.shaghel we Choose 1 for Null Value of T.shaghel
Data$T.shaghel[is.na(Data$T.shaghel)]=1

#Imputing Masaleh
Modes_of_Masleh=Data %>%  
  group_by(C.Ostan) %>% 
  summarise(Mode = Modes(na.omit(Masleh)))
Data$Masleh[is.na(Data$Masleh)]=1

rm(Modes,Modes_of_T.shaghel,Modes_of_Masleh)
#Impute Yaraneh
Data$D_Yarane[is.na(Data$D_Yarane)]=0

#Impute Coumtinios Values
Data=impute(Data, object = NULL, method = "randomForest", flag = FALSE)
#Correct negative value
Data$D_Azad[Data$D_Azad<0]=mean(Data$D_Azad[Data$D_Azad>0])

#Building Income
Data=Data %>% mutate(Daramad = D_Mozd+D_Mozd+D_Motefaraghe+D_Yarane)

#Build Categorical variables
Quantile=quantile(Data$Daramad,0.7)
Data <- Data %>%mutate(upquantile = case_when(
  .$Daramad %>% between(Quantile,max(Data$Daramad)+1) ~ 1,
  .$Daramad %>% between(0,Quantile) ~ 0))

Quantile=quantile(Data$Daramad,0.3)
Data <- Data %>%mutate(downquantile = case_when(
  .$Daramad %>% between(Quantile,max(Data$Daramad)+1) ~ 0,
  .$Daramad %>% between(0,Quantile) ~ 1))
#Barplots
barplot(table(Data$upquantile),main = "People over 70 quantile")
Data$upquantile=factor(Data$upquantile)
Data$downquantile=factor(Data$downquantile)
rm(Quantile)

# Stacked density plot:
mu <- ddply(Data , "N.S", summarise , grp.mean=mean(Daramad))
ggplot(Data , aes(x=Daramad , color=N.S, fill=N.S)) +
   geom_density(alpha=0.3,size=.3)+
   geom_vline(data = mu, aes(xintercept = grp.mean , color = N.S), size=.3)

mu <- ddply(Data , "Faaliat", summarise , grp.mean=mean(Daramad))
ggplot(Data , aes(x=Daramad , color=Faaliat, fill=Faaliat)) +
  geom_density(alpha=0.3,size=.3)+
  geom_vline(data = mu, aes(xintercept = grp.mean , color = Faaliat), size=.3)

mu <- ddply(Data , "Edu", summarise , grp.mean=mean(Daramad))
ggplot(Data , aes(x=Daramad , color=Edu, fill=Edu)) +
  geom_density(alpha=0.3,size=.3)+
  geom_vline(data = mu, aes(xintercept = grp.mean , color = Edu), size=.3)

mu <- ddply(Data , "T.shaghel", summarise , grp.mean=mean(Daramad))
ggplot(Data , aes(x=Daramad , color=T.shaghel, fill=T.shaghel)) +
  geom_density(alpha=0.3,size=.3)+
  geom_vline(data = mu, aes(xintercept = grp.mean , color = T.shaghel), size=.3)
#Removing Useless Data
Data=Data[-c(25:29)]
#Train , Test, Validation
smp_siz = floor(0.7*nrow(Data))
train_ind = sample(seq_len(nrow(Data)),size = smp_siz)  # Randomly identifies therows equal to sample size ( defined in previous instruction) from  all the rows of Smarket dataset and stores the row number in train_ind
train =Data[train_ind,] #creates the training dataset with row numbers stored in train_ind
remaind=Data[-train_ind,]
#test validation
smp_siz = floor(0.5*nrow(remaind))
test_ind = sample(seq_len(nrow(remaind)),size = smp_siz)  # Randomly identifies therows equal to sample size ( defined in previous instruction) from  all the rows of Smarket dataset and stores the row number in train_ind
test =remaind[test_ind,] #creates the training dataset with row numbers stored in train_ind
valid=remaind[-test_ind,]

rm(smp_siz)
#Solve inbalanced Problem 70 Quantile
train=ovun.sample(upquantile ~ ., data = train, method = "over",N = 2200)$data
table(train$upquantile)# data balanced

##############################################################
#Feature Selection
# Quantile 70
# load the library
library(randomForest)
fit_rf = randomForest(train[1:24],train$upquantile, data=train)
#Accuracy
library(caret) 
confusionMatrix(predict(fit_rf,remaind[1:24]),remaind$upquantile)
# Create an importance based on mean decreasing gini

importance(fit_rf)[order(importance(fit_rf),decreasing = TRUE),]
barplot(importance(fit_rf)[order(importance(fit_rf),decreasing = TRUE),])
Names=names(importance(fit_rf)[importance(fit_rf)>20,])
train1=train[,c(Names,"upquantile")]
test1=test[,c(Names,"upquantile")]
valid1=valid[,c(Names,"upquantile")]
remaind1=remaind[,c(Names,"upquantile")]


#Logestic Regression for 70 quantile
model <- glm(upquantile~.,family=binomial(link='logit'),data=train1)
confusionMatrix(factor(ifelse(predict(model,remaind1[-16])>0,1,0)),remaind1$upquantile)
confusionMatrix(factor(ifelse(predict(model,train1[-16])>0,1,0)),train1$upquantile)

#Decision Tree for 70 quantile
library(rpart)
library(rpart.plot)

accuracy_tune <- function(fit,data_test) {
  predict_unseen <- predict(fit, data_test, type = 'class')
  table_mat <- table(data_test$upquantile, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}
Minsplit=c(4,8,25,50,15)
Minbucket=c((round(Minsplit)+1)/3)
Maxdepth=c(3,25,5,7,9)
for (i in 1:length(Maxdepth)) {
  control <- rpart.control(minsplit = Minsplit[i],
                           minbucket = Minbucket[i],
                           maxdepth = Maxdepth[i])
  tune_fit <- rpart(upquantile~., data = train1, method = 'class', control = control)
  print(accuracy_tune(tune_fit,valid1))
  }
#best Choineon test set
control <- rpart.control(minsplit = 4,
                         minbucket = round(5 / 3),
                         maxdepth = 3,)
tune_fit <- rpart(upquantile~., data = train1, method = 'class', control = control)
print(accuracy_tune(tune_fit,test1))

confusionMatrix(predict(tune_fit, test1[-16], type = 'class'),test1$upquantile)
confusionMatrix(predict(tune_fit, train1[-16], type = 'class'),train1$upquantile)
rpart.plot(tune_fit, extra = 101)



#Multi-lyer Perceptron 70 quantile
library(neuralnet)
# fit neural network
pp=preProcess(train1, method = "range")
m <- model.matrix( ~ ., data = predict(pp,train1))
col_list <- paste(c(colnames(m[,-c(1,50)])),collapse="+")
col_list <- paste(c("upquantile1~",col_list),collapse="")
f <- formula(col_list)
pp=preProcess(valid1, method = "range")
Size=list(5,8,12,c(1,2))
m2 <- model.matrix( ~ ., data = predict(pp,valid1))
for (i in 1:length(Size)) {
  nn=neuralnet(f,data=m, hidden=Size[[i]],act.fct = "logistic",linear.output = FALSE)
  Predict=predict(nn,m2[,-c(1,50)],rep=1)
  Predict=ifelse(Predict>0.5,1,0)
  table_mat <- table(valid1$upquantile, Predict)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  print(accuracy_Test)
}

nn=neuralnet(f,data=m, hidden=8,act.fct = "logistic",linear.output = FALSE)
Predict=predict(nn,m[,-c(1,50)],rep=1)
Predict=ifelse(Predict>0.5,1,0)
confusionMatrix(factor(Predict),train1$upquantile)

pp=preProcess(test1, method = "range")
m <- model.matrix( ~ ., data = predict(pp,test1))
Predict=predict(nn,m[,-c(1,50)],rep=1)
Predict=ifelse(Predict>0.5,1,0)
confusionMatrix(factor(Predict),test1$upquantile)


plot(nn)
#KNN 70 quantile
library(class)
Size=c(3,4,5,7,10,15,20,30,40,60,80,100,200,300,400,600,1000)

for (i in 1:length(Size)) {
  model1 <- knn(train = train1[-16], test = valid1[-16],cl=train1$upquantile, k=i)
  x=confusionMatrix(model,valid1$upquantile)$table
  accuracy_Test <- sum(diag(x)) / sum(x)
  print(accuracy_Test)}

model1 <- knn(train = train1[-16], test = train1[-16],cl=train1$upquantile, k=35)
confusionMatrix(model1,train1$upquantile)
model1 <- knn(train = train1[-16], test = test1[-16],cl=train1$upquantile, k=35)
confusionMatrix(model1,test1$upquantile)

#Roc 
#KNN
lrROC <- roc(test1$upquantile ~ as.numeric(model1),plot=TRUE,print.auc=TRUE
             ,col="orange",lwd = 4,print.auc.y=0.2,print.auc.x=0.7,legacy.axes=TRUE)
#Neuran net
lrROC <- roc(test1$upquantile ~ as.numeric(Predict),plot=TRUE,print.auc=TRUE
             ,col="red",lwd = 4,print.auc.y=0.3,print.auc.x=0.7,legacy.axes=TRUE,add=TRUE)
#Decesion tree
lrROC <- roc(test1$upquantile ~ as.numeric(predict(tune_fit, test1[-16], type = 'class')),plot=TRUE,print.auc=TRUE
             ,col="blue",lwd = 4,print.auc.y=0.4,print.auc.x=0.7,legacy.axes=TRUE,add = TRUE)
#Logestic
lrROC <- roc(remaind1$upquantile ~ ifelse(predict(model,remaind1[-16])>0,1,0)
             ,plot=TRUE,print.auc=TRUE,col="green",print.auc.x=0.7,lwd =4,legacy.axes=TRUE,main="ROC Curves",add = TRUE)

legend("bottomright",legend=c("Logistic","tree","net","KNN"),col=c("green","blue","red","orange"),lwd=4)
#########################################################################
#Quantile 30
#Train , Test, Validation
Data=Data[,-25]
smp_siz = floor(0.7*nrow(Data))
train_ind = sample(seq_len(nrow(Data)),size = smp_siz)  # Randomly identifies therows equal to sample size ( defined in previous instruction) from  all the rows of Smarket dataset and stores the row number in train_ind
train =Data[train_ind,] #creates the training dataset with row numbers stored in train_ind
remaind=Data[-train_ind,]
#test validation
smp_siz = floor(0.5*nrow(remaind))
test_ind = sample(seq_len(nrow(remaind)),size = smp_siz)  # Randomly identifies therows equal to sample size ( defined in previous instruction) from  all the rows of Smarket dataset and stores the row number in train_ind
test =remaind[test_ind,] #creates the training dataset with row numbers stored in train_ind
valid=remaind[-test_ind,]

rm(smp_siz)
#Solve inbalanced Problem 30 Quantile
train=ovun.sample(downquantile ~ ., data = train, method = "over",N = 2200)$data
table(train$downquantile)# data balanced

########3

#Feature Selection
# ensure results are repeatable
# load the library
library(randomForest)
fit_rf = randomForest(train[1:24],train$downquantile, data=train)
#Accuracy
library(caret) 
confusionMatrix(predict(fit_rf,remaind[1:24]),remaind$downquantile)
# Create an importance based on mean decreasing gini

importance(fit_rf)[order(importance(fit_rf),decreasing = TRUE),]
barplot(importance(fit_rf)[order(importance(fit_rf),decreasing = TRUE),])
Names=names(importance(fit_rf)[importance(fit_rf)>20,])
train1=train[,c(Names,"downquantile")]
test1=test[,c(Names,"downquantile")]
valid1=valid[,c(Names,"downquantile")]
remaind1=remaind[,c(Names,"downquantile")]


#Logestic Regression for ۳۰ quantile
model <- glm(downquantile~.,family=binomial(link='logit'),data=train1)
confusionMatrix(factor(ifelse(predict(model,remaind1[-16])>0,1,0)),remaind1$downquantile)
confusionMatrix(factor(ifelse(predict(model,train1[-16])>0,1,0)),train1$downquantile)
#Decision Tree for ۳۰ quantile
library(rpart)
library(rpart.plot)

accuracy_tune <- function(fit,data_test) {
  predict_unseen <- predict(fit, data_test, type = 'class')
  table_mat <- table(data_test$downquantile, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}
Minsplit=c(4,8,25,50,15)
Minbucket=c((round(Minsplit)+1)/3)
Maxdepth=c(3,25,5,7,9)
for (i in 1:length(Maxdepth)) {
  control <- rpart.control(minsplit = Minsplit[i],
                           minbucket = Minbucket[i],
                           maxdepth = Maxdepth[i])
  tune_fit <- rpart(downquantile~., data = train1, method = 'class', control = control)
  print(accuracy_tune(tune_fit,valid1))
}
#best Choineon test set
control <- rpart.control(minsplit = 4,
                         minbucket = round(5 / 3),
                         maxdepth = 3,)
tune_fit <- rpart(downquantile~., data = train1, method = 'class', control = control)
print(accuracy_tune(tune_fit,test))

confusionMatrix(predict(tune_fit, test1[-16], type = 'class'),test1$downquantile)
confusionMatrix(predict(tune_fit, train1[-16], type = 'class'),train1$downquantile)

rpart.plot(tune_fit, extra = 101)

#Neural Network 30 quantile
library(neuralnet)
# fit neural network
pp=preProcess(train1, method = "range")
m <- model.matrix( ~ ., data = predict(pp,train1))
col_list <- paste(c(colnames(m[,-c(1,ncol(m))])),collapse="+")
col_list <- paste(c("downquantile1~",col_list),collapse="")
f <- formula(col_list)
pp=preProcess(valid1, method = "range")
Size=list(2,3,10)
m2 <- model.matrix( ~ ., data = predict(pp,valid1))
for (i in 1:length(Size)) {
  nn=neuralnet(f,data=m, hidden=Size[[i]],act.fct = "logistic",linear.output = FALSE)
  Predict=predict(nn,m2[,-c(1,ncol(m2))],rep=1)
  Predict=ifelse(Predict>0.5,1,0)
  table_mat <- table(valid1$downquantile, Predict)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  print(accuracy_Test)
}

nn=neuralnet(f,data=m, hidden=c(1,2),act.fct = "logistic",linear.output = FALSE)
Predict=predict(nn,m[,-c(1,50)],rep=1)
Predict=ifelse(Predict>0.5,1,0)
confusionMatrix(factor(Predict),train1$downquantile)

plot(nn)


#KNN 30 quantile
library(class)
Size=c(3,4,5,7,10,15,20,30,40,60,80,100,200,300,400,600,1000)
for (i in 1:length(Size)) {
  model <- knn(train = train1[-16], test = valid1[-16],cl=train1$downquantile, k=i)
  x=confusionMatrix(model,valid1$downquantile)$table
  accuracy_Test <- sum(diag(x)) / sum(x)
  print(accuracy_Test)}

model <- knn(train = train1[-16], test = test1[-16],cl=train1$downquantile, k=3)
confusionMatrix(model,test1$downquantile)
model <- knn(train = train1[-16], test = train1[-16],cl=train1$downquantile, k=3)
confusionMatrix(model,train1$downquantile)
