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

train1<-train
test1<-test
#Summary
summarizeColumns(train1)
summarizeColumns(test1)

#Dependent variable in train dataset has no missing values_______
#Data Conversion -train
train1$ApplicantIncome<-as.numeric(train1$ApplicantIncome)#<---
train1$LoanAmount<-as.numeric(train1$LoanAmount)#<---
train1$Credit_History<-as.factor(train1$Credit_History)#<---
train1$Loan_Status<-ifelse(train1$Loan_Status=="Y",1,0)#<---
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
nrow(train1[which(train1$ApplicantIncome>10351.26),])
train1$ApplicantIncome[which(train1$ApplicantIncome>10351.26)]<-NA
#test__
boxplot(test1$ApplicantIncome)
quantile(test1$ApplicantIncome,c(0.25,0.75,0.95))
nrow(test1[which(test1$ApplicantIncome>8354),])
test1$ApplicantIncome[which(test1$ApplicantIncome>8354)]<-NA

#CoapplicantIncome___
#train__
boxplot(train1$CoapplicantIncome)
quantile(train1$CoapplicantIncome,c(0.25,0.75))
nrow(train1[which(train1$CoapplicantIncome>5743.125),])
train1$CoapplicantIncome[which(train1$CopplicantIncome>5743.125)]<-NA
#test__
boxplot(test1$CoapplicantIncome)
quantile(test1$CoapplicantIncome,c(0.25,0.75))
nrow(test1[which(test1$CoapplicantIncome>6076.25),])
test1$CoapplicantIncome[which(test1$CopplicantIncome>6076.25)]<-NA
which(is.na(train1$CoapplicantIncome))


#CoapplicantIncome___
par(mfrow=c(1,2))
boxplot(train1$CoapplicantIncome,main="coi train")
boxplot(test1$CoapplicantIncome,main="coi test")





#Looking for Missing Values
#Gender(13),Married(3),Dependents(15),Self_Employed(32),LoanAmount(Num)(22),Loan_Amount_Term(Int)(14),Credit_History(50).

#For Married(3)
m1<-train1[which(train1$Married==""),]
ta1<-table(train1$Married,train1$Loan_Status)
ta1/rowSums(ta1)
train1$Married[which(is.na(train1$Married))]<-"Yes"
summary(train1$Married)

#For Gender
m1<-train1[which(train1$Gender==""),]#Possible influence - Married,Dependents(15),LoanStatus,LoanAmount
ta1<-table(train1$Gender,train1$Loan_Status)
ta1/rowSums(ta1)#dif-3/Male

ta1<-table(train1$Gender,train1$Married)
ta1/rowSums(ta1)#dif-45/Male

ta1<-table(train1$Gender,train1$Dependents)
ta1[,-1]/rowSums(ta1[,-1])#Male

train1$Gender[which(is.na(train1$Gender))]<-"Male"

#For Dependents
m1<-train1[which(train1$Dependents==""),]#Married #Gender,Loan_Status
ta1<-table(train1$Dependents,train1$Gender)
ta1/rowSums(ta1)#-1

ta1<-table(train1$Dependents,train1$Loan_Status)
ta1/rowSums(ta1)#-1

ta1<-table(train1$Dependents,train1$Married)
ta1/rowSums(ta1)#-1

train1$Dependents[which(is.na(train1$Dependents))]<-1

#LoanAmount-3.5% of values missing
m1<-train1[which(is.na(train1$LoanAmount)),]
train1$LoanAmount[which(is.na(train1$LoanAmount))]<-146.4
summary(train1)

#Loan_Amount_Term
summary(as.factor(train1$Loan_Amount_Term))
train1$Loan_Amount_Term[which(is.na(train1$Loan_Amount_Term))]<-360

#Remaining missing vals 
summary(train1)
#Self-employed(32,F), Credit-History(50,F)

#For Self-employed__________________

#Data for non missing values
sfetr<-train1[-which(is.na(train1$Self_Employed)),c(-1,-11)]
summary(sfetr)
sfetrm<-train1[which(is.na(train1$Self_Employed)),c(-1,-11,-6)]

#test tree1 for Self_Employed
#tree
tfse<-rpart(Self_Employed~.,data = sfetr,control = rpart.control(cp=0.002),method = "class",parms = list(split="gini"))
fancyRpartPlot(tfse)

#Tree Validation
pt<-predict(tfse,type = "class")
confusionMatrix(pt,sfetr$Self_Employed,positive = "Yes")#Accuracy - 88.66
kappa2(data.frame(sfetr$Self_Employed,pt))#Kappa -0.454

#Apply this model to missing values
pt1<-predict(tfse,type = "class",newdata = sfetrm)
train1$Self_Employed[which(is.na(train1$Self_Employed))]<-pt1
#_____________________________________
#For Credit_History
chtr<-train1[-which(is.na(train1$Credit_History)),c(-1)]
chtrm<-train1[which(is.na(train1$Credit_History)),c(-1,-11)]

#test tree for Credit History
#tree
tfch<-rpart(Credit_History~.,data = chtr,control = rpart.control(cp=0.002),method = "class",parms = list(split="gini"))
fancyRpartPlot(tfch)

#tree validation
pt<-predict(tfch,type="class")
confusionMatrix(pt,chtr$Credit_History,positive = "1")#Accuracy -90.6,kappa - 63.48

#Apply this model to missing values
pt1<-predict(tfch,type = "class",newdata =chtrm )
train1$Credit_History[which(is.na(train1$Credit_History))]<-pt1
#______________________________________
#Data Exp & prep is over_________________________________________________________________
summary(train1)

#Logistic Regression model on train1 dataset, target var-Loan_Status______________________________
mod<-glm(Loan_Status~.,train1[,-1],family = "binomial")
summary(mod)

step(mod,direction = "both")

#Dummy creation
mods<-train1[,-1]

mods$Dep0<-ifelse(mods$Dependents==0,1,0)
mods$Dep1<-ifelse(mods$Dependents==1,1,0)
mods$Dep2<-ifelse(mods$Dependents==2,1,0)
mods$Dep3p<-ifelse(mods$Dependents=="3+",1,0)

unique(train1$Property_Area)

mods$r<-ifelse(mods$Property_Area=="Rural",1,0)
mods$su<-ifelse(mods$Property_Area=="Semiurban",1,0)
mods$u<-ifelse(mods$Property_Area=="Urban",1,0)
mods<-mods[,c(-3,-11)]

colnames(mods)
mod1<-glm(Loan_Status~Married+Credit_History+su,mods,family = "binomial")
summary(mod1)#Finalising Model1

#Validation
#Find proportion of 1s in the dataset
summary(mods$Loan_Status)/614

pred<-predict(mod1,type="response")
pred<-ifelse(pred>=.6872964,1,0)
pred
#confusionmatrix
confusionMatrix(pred,mods$Loan_Status,positive = "1")#Accuracy - 74.92, kappa - 42.8
#___________________________________________________________________________________________
#Classification tree_______________________________________________________________________
tree<-rpart(Loan_Status~.,mods,control = rpart.control(cp=0.002),method = "class",parms = list(split="gini"))

#Vali for tree
predt<-predict(tree,type = "class")
confusionMatrix(predt,as.factor(mods$Loan_Status),positive = "1")#Accuracy-83.55,kappa - 58.74

#We will use decision tree model for test dataset___________________________________________
test<-read.csv("test.csv",header = TRUE,na.strings = c("NA",""))
summary(test)
str(test)
#Data Prep for test dataset
test$Credit_History<-as.factor(test$Credit_History)
test$CoapplicantIncome<-as.numeric(test$CoapplicantIncome)
test$LoanAmount<-as.numeric(test$LoanAmount)

#Imput Missing values________
test1<-test
#For Gender(11),Dependents(10),Self_Employed(23),LoanAmount(5),Loan_Amount_Term(6),Credit_History(29)

#For LoanAmount
test1$LoanAmount[which(is.na(test1$LoanAmount))]<-136.1

#For Loan_Amount_Term
summary(as.factor(test1$Loan_Amount_Term))
test1$Loan_Amount_Term[which(is.na(test1$Loan_Amount_Term))]<-360

#For Dependents--
#tree
test_dep<-test1[-which(is.na(test1$Dependents)),c(-1,-2,-6,-11)]
tree_dep<-rpart(Dependents~.,test_dep,control = rpart.control(cp=0.002),method = "class",parms = list(split="gini"))
#vali
p1<-predict(tree_dep,type="class")
confusionMatrix(p1,test_dep$Dependents)#Accuracy-65.27
test1$Dependents[which(is.na(test1$Dependents))]<-p1

#For Gender--
#tree
test_gen<-test1[-which(is.na(test1$Gender)),c(-1,-6,-11)]
tree_gen<-rpart(Gender~.,test_gen,control = rpart.control(cp=0.002),method = "class",parms = list(split="gini"))
#vali
p1<-predict(tree_gen,type="class")
confusionMatrix(p1,test_gen$Gender,positive = "Male")#Accuracy-86.24
test1$Gender[which(is.na(test1$Gender))]<-p1

#For Self_Employed
#tree
test_se<-test1[-which(is.na(test1$Self_Employed)),c(-1,-11)]
tree_se<-rpart(Self_Employed~.,test_se,control = rpart.control(cp=0.002),method = "class",parms = list(split="gini"))
#vali
p1<-predict(tree_se,type="class")
confusionMatrix(p1,test_se$Self_Employed)#Accuracy-89.53
test1$Self_Employed[which(is.na(test1$Self_Employed))]<-p1
summary(test1)

#For Credit_History
#tree
test_ch<-test1[-which(is.na(test1$Credit_History)),c(-1)]
tree_ch<-rpart(Credit_History~.,test_ch,control = rpart.control(cp=0.002),method = "class",parms = list(split="gini"))
#vali
p1<-predict(tree_ch,type="class")
confusionMatrix(p1,test_ch$Credit_History)#Accuracy-85.53
test1$Credit_History[which(is.na(test1$Credit_History))]<-p1
summary(test1)

colnames(mods)
colnames(test1)

summary(test1)

#Dummy variables for..? -Dependents,Property_Area
modt<-test1[,-1]

modt$Dep0<-ifelse(modt$Dependents==0,1,0)
modt$Dep1<-ifelse(modt$Dependents==1,1,0)
modt$Dep2<-ifelse(modt$Dependents==2,1,0)
modt$Dep3p<-ifelse(modt$Dependents=="3+",1,0)


modt$r<-ifelse(modt$Property_Area=="Rural",1,0)
modt$su<-ifelse(modt$Property_Area=="Semiurban",1,0)
modt$u<-ifelse(modt$Property_Area=="Urban",1,0)
modt<-modt[,c(-3,-11)]
colnames(modt)
colnames(mods)
colnames(test1)
#Data Prep for test data is over_______________________________________________
#Modt is the dataset to which main DT is applied_______________________________
predicted<-predict(tree,type = "class",newdata = modt)

#Final dataset
Final<-data.frame(test$Loan_ID,predicted)
Final$predicted<-ifelse(Final$predicted==1,"Y","N")

#Exporting it
write.csv(Final,"Final2.csv")

rm(list)


