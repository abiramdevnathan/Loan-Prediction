getwd()
setwd("C:\\Users\\Abiram\\Documents\\Analytics\\R Practice\\Loan Prediction")

library(dplyr)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(lattice)
library(ggplot2)
library(caret)
library(lpSolve)
library(irr)
library(mlr)
library(car)



#Importing training data___________________________________________________________
train<-read.csv("train.csv",header = TRUE,na.strings = c("NA",""))
test<-read.csv("test.csv",header = TRUE,na.strings = c("NA",""))

#Data Dictionary___________________________________________________________________

#Loan_ID---------Unique Loan ID - F,F
#Gender----------Male/ Female   - F,F
#Married--- -----Applicant married (Y/N) - F,F
#Dependents----- Number of dependents- F,F
#Education------- Applicant Education (Graduate/ Under Graduate) - F,F
#Self_Employed--- Self employed (Y/N)F,F
#ApplicantIncome--Applicant income - Int, Num
#CoapplicantIncome-- Coapplicant income - Num, Num
#LoanAmount--------- Loan amount in thousands- Int,Num
#Loan_Amount_Term--- Term of loan in months- int,int
#Credit_History------ credit history meets guidelines - Int,F
#Property_Area--------- Urban/ Semi Urban/ Rural-F,F
#Loan_Status-----------Loan approved (Y/N)-(1,0)-F,F 

#Data Exploration & Data prep__________________________________________________________________

train1<-train[,-1]#<---
test1<-test[,-1]#<---
#Summary
summarizeColumns(train1)#<---
summarizeColumns(test1)#<---

#Dependent variable in train dataset has no missing values_______
#Data Conversion -train
train1$ApplicantIncome<-as.numeric(train1$ApplicantIncome)#<---
train1$LoanAmount<-as.numeric(train1$LoanAmount)#<---
train1$Credit_History<-as.factor(train1$Credit_History)#<---
train1$Loan_Status<-ifelse(train1$Loan_Status=="Y",1,0)#<---
train1$Loan_Status<-as.factor(train1$Loan_Status)#<---
#Data Conversion -test
test1$ApplicantIncome<-as.numeric(test1$ApplicantIncome)#<---
test1$CoapplicantIncome<-as.numeric(test1$CoapplicantIncome)#<---
test1$LoanAmount<-as.numeric(test1$LoanAmount)#<---
test1$Credit_History<-as.factor(test1$Credit_History)#<---

#Looking for outliers___________________________________________
#Outlier treatment for numeric variables
#ApplicantIncome___
#train__
boxplot(train1$ApplicantIncome,col = "red",main="appi train")
quantile(train1$ApplicantIncome,c(0.25,0.75,0.95))
nrow(train1[which(train1$ApplicantIncome>14547.5),])
train1$ApplicantIncome[which(train1$ApplicantIncome>14547.5)]<-NA#<---
summary(train1$ApplicantIncome)
#test__
boxplot(test1$ApplicantIncome)
quantile(test1$ApplicantIncome,c(0.25,0.75,0.95))
nrow(test1[which(test1$ApplicantIncome>11648),])
test1$ApplicantIncome[which(test1$ApplicantIncome>11648)]<-NA#<---
summary(test1$ApplicantIncome)

#CoapplicantIncome___
#train__
boxplot(train1$CoapplicantIncome)
quantile(train1$CoapplicantIncome,c(0.25,0.75))
nrow(train1[which(train1$CoapplicantIncome>9189),])
train1$CoapplicantIncome[which(train1$CoapplicantIncome>9189)]<-NA#<---
summary(train1$CoapplicantIncome)
#test__
boxplot(test1$CoapplicantIncome)
quantile(test1$CoapplicantIncome,c(0.25,0.75))
nrow(test1[which(test1$CoapplicantIncome>6076.25),])
test1$CoapplicantIncome[which(test1$CoapplicantIncome>6076.25)]<-NA#<---
summary(test1$CoapplicantIncome)

#Loan Amount_________
#train__
boxplot(train1$LoanAmount)
quantile(train1$LoanAmount,c(0.25,0.75,0.95),na.rm = TRUE)
nrow(train1[which(train1$LoanAmount>372),])
train1$LoanAmount[which(train1$LoanAmount>372)]<-NA#<---
summary(train1$LoanAmount)

#test__
boxplot(test1$LoanAmount)
quantile(test1$LoanAmount,c(0.25,0.75),na.rm = TRUE)
nrow(train1[which(test1$LoanAmount>244.625),])
test1$LoanAmount[which(test1$LoanAmount>244.625)]<-NA#<---
summary(test1$LoanAmount)

#Looking for Missing Values___________________________________________________
#train_______________________________
#Married(3),Gender(13),Dependents(15),Self_Employed(32),Loan_Amount_Term(Int)(14),Credit_History(50)
#CoapplicantIncome(18),ApplicantIncome(50),LoanAmount(Num)(61)
#Categorical Missing Values__________
#For Married(3)_____
m1<-train1[which(train1$Married==""),]
ta1<-table(train1$Married,train1$Loan_Status)
ta1/rowSums(ta1)
train1$Married[which(is.na(train1$Married))]<-"Yes"#<---
summary(train1$Married)

#For Gender(13)______
m1<-train1[which(train1$Gender==""),]#Possible influence - Married,Dependents(15),LoanStatus,LoanAmount
ta1<-table(train1$Gender,train1$Loan_Status)
ta1/rowSums(ta1)#dif-3/Male
ta1<-table(train1$Gender,train1$Married)
ta1/rowSums(ta1)#dif-45/Male
ta1<-table(train1$Gender,train1$Dependents)
ta1[,-1]/rowSums(ta1[,-1])#Male

train1$Gender[which(is.na(train1$Gender))]<-"Male"#<---
summary(train1$Gender)

#For Dependents_____
m1<-train1[which(train1$Dependents==""),]#Married #Gender,Loan_Status
ta1<-table(train1$Dependents,train1$Gender)
ta1/rowSums(ta1)#-1
ta1<-table(train1$Dependents,train1$Loan_Status)
ta1/rowSums(ta1)#-1
ta1<-table(train1$Dependents,train1$Married)
ta1/rowSums(ta1)#-1

train1$Dependents[which(is.na(train1$Dependents))]<-1#<---
summary(train1$Dependents)

#For Self-employed__________
se1<-train1[-which(is.na(train1$Self_Employed)),-10]#<---
sem<-train1[which(is.na(train1$Self_Employed)),c(-10,-5)]#<---

#test tree1 for Self_Employed
#tree
tse<-rpart(Self_Employed~.,data = se1,control = rpart.control(cp=0.002),method = "class",parms = list(split="gini"))#<---

#Tree Validation
pt<-predict(tse,type = "class")
confusionMatrix(pt,se1$Self_Employed,positive = "Yes")#Accuracy - 87.46

#Apply this model to missing values
pt1<-predict(tse,type = "class",newdata = sem)#<---
train1$Self_Employed[which(is.na(train1$Self_Employed))]<-pt1#<---
#___________________________
#For Credit_History_________
ch1<-train1[-which(is.na(train1$Credit_History)),]#<---
chm<-train1[which(is.na(train1$Credit_History)),c(-10)]#<---

#test tree for Credit History
#tree
tch<-rpart(Credit_History~.,data = ch1,control = rpart.control(cp=0.002),method = "class",parms = list(split="gini"))#<---

#tree validation
ptt<-predict(tch,type="class")
confusionMatrix(ptt,ch1$Credit_History,positive = "1")#Accuracy -91.31,kappa - 66.23

#Apply this model to missing values
pt2<-predict(tch,type = "class",newdata =chm )#<---
train1$Credit_History[which(is.na(train1$Credit_History))]<-pt2#<---
#___________________________
#Loan_Amount_Term__________
summary(as.factor(train1$Loan_Amount_Term))
train1$Loan_Amount_Term[which(is.na(train1$Loan_Amount_Term))]<-360#<---

#For Numeric Variables_____________________
#CoapplicantIncome,ApplicantIncome,LoanAmount
#Copplicant Income_____
train1$CoapplicantIncome[which(is.na(train1$CoapplicantIncome))]<-1289.13#<---

#ApplicantIncome_(LINEAR REGRESSION)_____
ai<-train1[-which(is.na(train1$ApplicantIncome)),-8]#<---
aim<-train1[which(is.na(train1$ApplicantIncome)),c(-6,-8)]#<---

#Dummy for dep
aimod<-lm(ApplicantIncome~.,ai)
summary(aimod)
step(aimod,direction = "both")

ai$dep0<-ifelse(ai$Dependents==0,1,0)#<---
ai$dep1<-ifelse(ai$Dependents==1,1,0)#<---
ai$dep2<-ifelse(ai$Dependents==2,1,0)#<---
ai$dep3p<-ifelse(ai$Dependents=="3+",1,0)#<---
ai$dep0<-as.factor(ai$dep0)#<---
ai$dep1<-as.factor(ai$dep1)#<---
ai$dep2<-as.factor(ai$dep2)#<---
ai$dep3p<-as.factor(ai$dep3p)#<---

#Dummy for Prop_Area
ai$r<-ifelse(ai$Property_Area=="Rural",1,0)#<---
ai$su<-ifelse(ai$Property_Area=="Semiurban",1,0)#<---
ai$u<-ifelse(ai$Property_Area=="Urban",1,0)#<---
ai$r<-as.factor(ai$r)#<---
ai$su<-as.factor(ai$su)#<---
ai$u<-as.factor(ai$u)#<---

ai<-ai[,c(-3,-10)]#<---
ai<-ai[,c(1,13,3,4,5,6,15,16)]#<---
ai<-ai[,c(-2,-7)]#<---

#Data prep for aim
aim<-aim[,c(1,4,5,6,9)]#<---
aim$u<-ifelse(aim$Property_Area=="Urban",1,0)#<---
aim<-aim[,-5]#<---
aim$u<-as.factor(aim$u)#<---

#Model Check
aimod1<-lm(ApplicantIncome~.,ai)
summary(aimod1)
hist(aimod1$residuals)
plot(aimod1$fitted.values,aimod1$residuals)
qqPlot(aimod1$residuals)

aimod2<-lm(log(ApplicantIncome)~.,ai)
summary(aimod2)
hist(aimod2$residuals)
plot(aimod2$fitted.values,aimod2$residuals)
qqPlot(aimod2$residuals)

aimod3<-lm(sqrt(ApplicantIncome)~.,ai)#<---
summary(aimod3)#<---
hist(aimod3$residuals)
plot(aimod3$fitted.values,aimod3$residuals)
qqPlot(aimod3$residuals)

aimod4<-lm(sqrt(ApplicantIncome)~Gender+Education+Self_Employed+CoapplicantIncome,ai)#<--
summary(aimod4)

#Finalising aimod4
aimpred<-(predict(aimod4,newdata = aim))^2#<---
train1$ApplicantIncome[which(is.na(train1$ApplicantIncome))]<-aimpred#<---

#LoanAmount(61)(Linear Regression)________
la<-train1[-which(is.na(train1$LoanAmount)),]#<---
lam<-train1[which(is.na(train1$LoanAmount)),-8]#<---

lamod<-lm(LoanAmount~.,la)
summary(lamod)
step(lamod,direction = "both")

la<-la[,c(2,4,6,7,9,12,8)]#<---
lam<-lam[,c(2,4,6,7,8,11)]#<---

lamod1<-lm(LoanAmount~.,la)
summary(lamod1)

#Model Check
lamod2<-lm(LoanAmount~Married+ApplicantIncome+CoapplicantIncome+Loan_Amount_Term,la)
summary(lamod2)
hist(lamod2$residuals)
qqPlot(lamod2$residuals)
plot(lamod2$fitted.values,lamod2$residuals)


lamod3<-lm(log(LoanAmount)~Married+ApplicantIncome+CoapplicantIncome+Loan_Amount_Term,la)
summary(lamod3)
hist(lamod3$residuals)
qqPlot(lamod3$residuals)

lamod4<-lm(sqrt(LoanAmount)~Married+ApplicantIncome+CoapplicantIncome+Loan_Amount_Term,la)#<---
summary(lamod4)#<---
hist(lamod4$residuals)
qqPlot(lamod4$residuals)
plot(lamod4$fitted.values,lamod4$residuals)

lapred<-(predict(lamod4,lam))^2#<---
train1$LoanAmount[which(is.na(train1$LoanAmount))]<-lapred#<---

summarizeColumns(train1)#<---

#Data Exp & prep is over_________________________________________________________________

#Logistic Regression model on train1 dataset, target var-Loan_Status______________________________
mod<-glm(Loan_Status~.,train1,family = "binomial")
summary(mod)

step(mod,direction = "both")

#Dummy creation
lr<-train1[,c(2,4,8,10,11,12)]

lr$r<-ifelse(lr$Property_Area=="Rural",1,0)
lr$su<-ifelse(lr$Property_Area=="Semiurban",1,0)
lr$u<-ifelse(lr$Property_Area=="Urban",1,0)
lr<-lr[,c(-5)]

mod1<-glm(Loan_Status~Married+su+Credit_History,lr,family = "binomial")
summary(mod1)#Finalising Model1

#Validation
#Find proportion of 1s in the dataset
summary(lr$Loan_Status)/614

predlog<-predict(mod1,type="response")
predlog<-ifelse(predlog>=.6872964,1,0)

#confusionmatrix
confusionMatrix(predlog,lr$Loan_Status,positive = "1")#Accuracy - 74.92, kappa - 42.8
#___________________________________________________________________________________________
#Classification tree_______________________________________________________________________
tree<-rpart(Loan_Status~.,train1,control = rpart.control(cp=0.002),method = "class",parms = list(split="gini"))#<---
library(randomForest)

#RANDOM FOREST______________________________________________________________
rf<-randomForest(Loan_Status~.,train1,ntree=60)
summary(rf)
predrf<-predict(rf,train1)
confusionMatrix(predrf,as.factor(train1$Loan_Status),positive = "1")#(98.21,95.77)
#_Naive Bayes__________________________________________________________________________

library(e1071)
svm<-naiveBayes(Loan_Status~.,train1)
pred_svm<-predict(svm,train1)
confusionMatrix(pred_svm,as.factor(train1$Loan_Status),positive = "1")

rm(svm)
rm(pred_svm)


#Vali for tree
predtree<-predict(tree,type = "class")#<---
confusionMatrix(predtree,as.factor(train1$Loan_Status),positive = "1")#Accuracy-(83.55,kappa 58.74),(83.88,60.05),(84.04,60.51)

#We will use decision tree model for test dataset___________________________________________

summarizeColumns(test1)
summarizeColumns(train1)

#For direct imput variables________________
#Loan_Amount_Term___
test1$Loan_Amount_Term<-as.numeric(test1$Loan_Amount_Term)#<---
test1$Loan_Amount_Term[which(is.na(test1$Loan_Amount_Term))]<-360#<---

#For Dependents____
summary(test1$Dependents)
test1$Dependents[which(is.na(test1$Dependents))]<-0#<---

#For Gender________
summary(test1$Gender)
test1$Gender[which(is.na(test1$Gender))]<-"Male"#<---

#For CoapplicantIncome____
summary(test1$CoapplicantIncome)
boxplot(test1$CoapplicantIncome)
test1$CoapplicantIncome[which(is.na(test1$CoapplicantIncome))]<-1322#<---

#For Self_Employed___
#Tree
tse1<-test1[-which(is.na(test1$Self_Employed)),]#<---
tsem<-test1[which(is.na(test1$Self_Employed)),]#<---
ttse<-rpart(Self_Employed~.,tse1,control = rpart.control(cp=0.002),method = "class",parms = list(split="gini"))#<---
#vali
ptse<-predict(ttse,type="class")
confusionMatrix(ptse,tse1$Self_Employed)#Accuracy-89.53,89.83
ptsem<-predict(ttse,type = "class",newdata = tsem)#<---
test1$Self_Employed[which(is.na(test1$Self_Employed))]<-ptsem#<---

#For Credit_History___
#tree
tch<-test1[-which(is.na(test1$Credit_History)),]#<---
tchm<-test1[which(is.na(test1$Credit_History)),]#<---
ttch<-rpart(Credit_History~.,tch,control = rpart.control(cp=0.002),method = "class",parms = list(split="gini"))#<---
#vali
pch1<-predict(ttch,type="class")
confusionMatrix(pch1,tch$Credit_History)#Accuracy-85.53,84.62
ptch<-predict(ttch,type = "class",newdata = tchm)#<---
test1$Credit_History[which(is.na(test1$Credit_History))]<-ptch#<---

#Applicant Income___
#Linear Regression___
tai<-test1[-which(is.na(test1$ApplicantIncome)),-8]#<---
taim<-test1[which(is.na(test1$ApplicantIncome)),-8]#<---
tasimod<-lm(ApplicantIncome~.,tai)
summary(tasimod)

tai<-tai[,c(-8,-10)]#<---

tasimod1<-lm(ApplicantIncome~.,tai)
summary(tasimod1)
summarizeColumns(tai)

step(tasimod,direction = "both")
#Dummy for Dependents
tai$dep0<-ifelse(tai$Dependents==0,1,0)#<---
tai$dep1<-ifelse(tai$Dependents==1,1,0)#<---
tai$dep2<-ifelse(tai$Dependents==2,1,0)#<---
tai$dep3p<-ifelse(tai$Dependents=="3+",1,0)#<---
tai$dep0<-as.factor(tai$dep0)#<---
tai$dep1<-as.factor(tai$dep1)#<---
tai$dep2<-as.factor(tai$dep2)#<---
tai$dep3p<-as.factor(tai$dep3p)#<---
summarizeColumns(tai)

tai<-tai[,-3]#<---
tai<-tai[,-9]#<---
tai<-tai[,c(-1,-8)]#<---


tasimod3<-lm(ApplicantIncome~.,tai)
summary(tasimod3)
#Checking Model3
hist(tasimod3$residuals)
qqPlot(tasimod3$residuals)

tasimod4<-lm(sqrt(ApplicantIncome)~.,tai)#<---
summary(tasimod4)#<---
hist(tasimod4$residuals)
qqPlot(tasimod4$residuals)
plot(tasimod4$fitted.values,tasimod4$residuals)

#Data prep for taim
taim<-taim[,c(-8,-1)]#<---

#Dummy for Dependents
taim$dep2<-ifelse(taim$Dependents==2,1,0)#<---
taim$dep3p<-ifelse(taim$Dependents=="3+",1,0)#<---
taim$dep2<-as.factor(taim$dep2)#<---
taim$dep3p<-as.factor(taim$dep3p)#<---

taim<-taim[,-2]#<---
taim<-taim[,c(-4,-7)]#<---

ptai<-(predict(tasimod4,newdata = taim))^2#<---
test1$ApplicantIncome[which(is.na(test1$ApplicantIncome))]<-ptai#<---
test1$ApplicantIncome[which(is.na(test1$ApplicantIncome))]<-4123#<---

summarizeColumns(test1)#<---

#LoanAmount_________
#Regression tree__
tla<-test1[-which(is.na(test1$LoanAmount)),]#<---
tlam<-test1[which(is.na(test1$LoanAmount)),]#<---
ttla<-rpart(LoanAmount~.,tla,control = rpart.control(cp=0.009),method = "anova")#<---
ptla<-predict(ttla,newdata =tlam )#<---
test1$LoanAmount[which(is.na(test1$LoanAmount))]<-ptla#<---


summarizeColumns(test1)

#Data Prep for test data is over_______________________________________________
#Modt is the dataset to which main DT is applied_______________________________
predicted_test<-predict(rf,test1)

predicted_test

#Final dataset
Final<-data.frame(test$Loan_ID,predicted_test)
Final$predicted_test<-ifelse(Final$predicted_test==1,"Y","N")

#Exporting it
write.csv(Final,"Final5.csv")

rm(list)


