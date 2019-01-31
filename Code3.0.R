rm(list=ls(all=T))
setwd("O:/path")
getwd()

#************************* Read the data****************************************
Train_data = read.csv("Train.csv",header = T, na.strings = c(" ", "", "NA"))
Test_data = read.csv("Test_data.csv" ,header = T, na.strings = c(" ", "", "NA"))

# ********************** Remove'-'symbol from the phone.number dataset***********
Train_data$phone.number = gsub("-","", Train_data$phone.number)

#dataset$Churn = gsub("."," ", dataset$Churn)
#*************************** Check datatype ************************************
str(Train_data)

#   state                       : Factor w/ 51 levels "AK","AL","AR",..: 17 36 32 36 37 2 20 25 19 50 ...
#  account.length               : int  128 107 137 84 75 118 121 147 117 141 ...
#  area.code                    : int  415 415 415 408 415 510 510 415 408 415 ...
#  phone.number                 : chr  " 3824657" " 3717191" " 3581921" " 3759999" ...
#  international.plan           : Factor w/ 2 levels " no"," yes": 1 1 1 2 2 2 1 2 1 2 ...
#  voice.mail.plan              : Factor w/ 2 levels " no"," yes": 2 2 1 1 1 1 2 1 1 2 ...
#  number.vmail.messages        : int  25 26 0 0 0 0 24 0 0 37 ...
#  total.day.minutes            : num  265 162 243 299 167 ...
#  total.day.calls              : int  110 123 114 71 113 98 88 79 97 84 ...
#  total.day.charge             : num  45.1 27.5 41.4 50.9 28.3 ...
#  total.eve.minutes            : num  197.4 195.5 121.2 61.9 148.3 ...
#  total.eve.calls              : int  99 103 110 88 122 101 108 94 80 111 ...
#  total.eve.charge             : num  16.78 16.62 10.3 5.26 12.61 ...
#  total.night.minutes          : num  245 254 163 197 187 ...
#  total.night.calls            : int  91 103 104 89 121 118 118 96 90 97 ...
#  total.night.charge           : num  11.01 11.45 7.32 8.86 8.41 ...
#  total.intl.minutes           : num  10 13.7 12.2 6.6 10.1 6.3 7.5 7.1 8.7 11.2 ...
#  total.intl.calls             : int  3 3 5 7 3 6 7 6 4 5 ...
#  total.intl.charge            : num  2.7 3.7 3.29 1.78 2.73 1.7 2.03 1.92 2.35 3.02 ...
#  number.customer.service.calls: int  1 1 0 2 3 0 3 0 1 0 ...
#  Churn                        : Factor w/ 2 levels " False."," True.": 1 1 1 1 1 1 1 1 1 1 ...

dim(Train_data)
head(Train_data, 5)

# Unique values in a column
# length(unique(dataset$account.length))
# 212
# table(dataset$account.length)
# mean(dataset$account.length)
# # 101.0648
# 
# length(unique(dataset$number.vmail.messages))
# #46
# table(dataset$number.vmail.messages)
# mean(dataset$number.vmail.messages)
# #8.09901

# Converting Catogrical data into factor of Training dataset
Train_data$area.code = as.factor(Train_data$area.code)
for(i in 1:ncol(Train_data)){
  
  if(class(Train_data[,i]) == 'factor'){
    
    Train_data[,i] = factor(Train_data[,i], labels=(1:length(levels(factor(Train_data[,i])))))
    
  }
}

# Converting Catogrical data into factor of testing dataset
Test_data$area.code = as.factor(Test_data$area.code)
for(i in 1:ncol(Test_data)){
  
  if(class(Test_data[,i]) == 'factor'){
    
    Test_data[,i] = factor(Test_data[,i], labels=(1:length(levels(factor(Test_data[,i])))))
    
  }
}
# For Churn False is denoted by 1 and TRUE is denoted as 2

#************************** Missing Value for Training data ****************************
missing_val = data.frame(apply(Train_data,2,function(x){sum(is.na(x))}))
missing_val
missing_val$Columns = row.names(missing_val)


#************************** Missing Value for Testing data ****************************
missing_val = data.frame(apply(Test_data,2,function(x){sum(is.na(x))}))
missing_val
missing_val$Columns = row.names(missing_val)
#************************* Outlier Analysis **********************
# BoxPlot (Only for Numeric Variables)
library("ggplot2")

numeric_index = sapply(Train_data,is.numeric)

# Select only Numeric Data
numeric_data = Train_data[, numeric_index]

cnames = colnames(numeric_data)

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Churn", fill = Train_data$Churn), data = subset(Train_data))+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red" ,outlier.shape=19,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="Churn")+
           ggtitle(paste("Box plot of Churn for",cnames[i])))
}

gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn3,gn4,gn5,ncol=3)
gridExtra::grid.arrange(gn6,gn7,gn8,ncol=3)
gridExtra::grid.arrange(gn9,gn10,gn11,ncol=3)
gridExtra::grid.arrange(gn12,gn13,gn14=3)
gridExtra::grid.arrange(gn15,ncol=1)

#****************************** Removing Outliers**********************************
df = Train_data

for(i in cnames)
{
  print(i)
  val = Train_data[,i][Train_data[,i] %in% boxplot.stats(Train_data[,i])$out]
  #print(length(val))
  Train_data = Train_data[which(!Train_data[,i] %in% val),]
}

#******************************* After removing Outliers*********************************
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Churn", fill = Train_data$Churn), data = subset(Train_data))+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="Churn")+
           ggtitle(paste("Box plot of Churn for",cnames[i])))
  
}

gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn3,gn4,gn5,ncol=3)
gridExtra::grid.arrange(gn6,gn7,gn8,ncol=3)
gridExtra::grid.arrange(gn9,gn10,gn11,ncol=3)
gridExtra::grid.arrange(gn12,gn13,gn14=3)
gridExtra::grid.arrange(gn15,ncol=1)

#*******************************Feature Selection**********************************
#Blue colour indicates the extrimely Positive correlation  and Dark Red Colour shows Extremely -ve correlation 


library("corrgram")
corrgram(Train_data[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

# Chose only chtogrical variables
factor_index = sapply(Train_data,is.factor)
factor_data = Train_data[,factor_index]

#Dependence between Independent and dependent variable should be high
# Idealy There should be no dependency between indepenents variables
names(factor_data)

for (i in 1:4) 
  
  # Range is 1 to 4 because our factor dataset contain 5 columns only 
  # among which 4 are independent variable so we need to itrate the loop only for independent variables
{
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$Churn,factor_data[,i])))
}

# p<0.05 Means our dependent variable depend on Independent variable

#********************************* Dimension Reduction ****************************************
# Remove all the highly corelated data
Train_data = subset(Train_data, select = -c(total.day.minutes,total.eve.minutes,total.night.minutes, 
                                      total.intl.minutes,phone.number)) 

Test_data = subset(Test_data, select = -c(total.day.minutes,total.eve.minutes,total.night.minutes, 
                                        total.intl.minutes,phone.number))

#*********************************** Feature Scaling **************************
# 1) Normalization =(actual - Min. value)/(Max.value - Min.value)
#2) Standardizatoin/ Z-score
#use Z-score Only if data is normally distrubated
#Normalization
#***********************************Check Normal distrubation ******************
#qqnorm is used for normality plot
qqnorm(Train_data$account.length)
hist(Train_data$account.length)

qqnorm(Train_data$number.vmail.messages)
hist(Train_data$number.vmail.messages)


qqnorm(Train_data$total.day.calls)
hist(Train_data$total.day.calls)

qqnorm(Train_data$total.day.charge)
hist(Train_data$total.day.charge)

qqnorm(Train_data$total.eve.calls)
hist(Train_data$total.eve.calls)

qqnorm(Train_data$total.night.calls)
hist(Train_data$total.night.calls)

qqnorm(Train_data$total.intl.calls)
hist(Train_data$total.intl.calls)

qqnorm(Train_data$number.customer.service.calls)
hist(Train_data$number.customer.service.calls)

cnames

#****************** Storeing all the continious variable for feature scaling *************
cnames = c("account.length","number.vmail.messages","total.day.calls","total.day.charge",
           "total.eve.calls","total.eve.charge",
           "total.night.calls","total.night.charge",
           "total.intl.calls","total.intl.charge","number.customer.service.calls")

cnames1 = c("account.length","number.vmail.messages","total.day.calls","total.day.charge",
            "total.eve.calls","total.eve.charge",
            "total.night.calls","total.night.charge",
            "total.intl.calls","total.intl.charge","number.customer.service.calls")
# Do normalization for continious variables
for(i in cnames)
{
  print(i)
  Train_data[,i] = (Train_data[,i] - min(Train_data[,i]))/
    (max(Train_data[,i] - min(Train_data[,i])))
}

range(Train_data$total.eve.charge)

# Do normalization for continious variables
for(i in cnames1)
{
  print(i)
  Test_data[,i] = (Test_data[,i] - min(Test_data[,i]))/
    (max(Test_data[,i] - min(Test_data[,i])))
}


#**************Divide data into train and test using stratified sampling method **************
# library(caTools) 
# library(caret)
# 
# set.seed(1234)
# train.index = createDataPartition(Train_data$Churn, p = .80, list = FALSE)
# train = Train_data[ train.index,]
# test  = Train_data[-train.index,]

# ***************************** Decision Tree ********************
library(C50)
library(caret) # For SVm and confusionMatrix
library(rpart)
library(pROC)
library(ROCR)
C50_model = C5.0(Churn ~., Train_data, trials = 100, rules = TRUE)
#Summary of DT model
summary(C50_model)
#write(capture.output(summary(C50_model)), "c50Rules_Final.txt")
#Lets predict for test cases
C50_Predictions = predict(C50_model, Test_data[,-16], type = "class")

#Test_data$DT_prediction = C50_Predictions
# write.csv(C50_Predictions,file = file.choose(new = T))

##Evaluate the performance of classification model
ConfMatrix_C50 = table(Test_data$Churn, C50_Predictions)
confusionMatrix(ConfMatrix_C50)

table(Train_data$Churn)
#******************* ROC Curve ***********************************
DT_pred <- predict(C50_model, Test_data[-16], type = 'prob')
auc <- auc(Test_data$Churn ,DT_pred[,2])
plot(roc(Test_data$Churn, DT_pred [,2]),
     main = "ROC for Decision Tree")
auc <- round(auc,3)
legend(0.6,0.2,auc)
Gini = 2*auc - 1
Gini

#Gini coefficient = 61.00%
# FNR = FN/FN+TP 
# 134/(134+90)
# Accuracy = 91.9
#False Negative rate = 0.568 i.e 59.8%
#******************************** Random Forest **************************************
library(randomForest)
RF_model = randomForest(Churn ~ ., Train_data, importance = TRUE, ntree = 1000)

#Predict test data using random forest model
RF_Predictions = predict(RF_model, Test_data[,-16])
# y_pred1 = predict(RF_model,c(12,0.4219,3,1,1,0.00,0.706,0.186,0.2671,0.5625,0.4294,0.6185,
#                              0.1578,0.5375,0.4285))
##Evaluate the performance of classification model
ConfMatrix_RF = table(Test_data$Churn, RF_Predictions)
confusionMatrix(ConfMatrix_RF)
# Accuracy of Decision Tree = 88.12
# False negative Rate = 72.76%
#install.packages("pROC")
#library(pROC)
RF_pred <- predict(RF_model, Test_data[-16], type = 'prob')
auc <- auc(Test_data$Churn ,RF_pred[,2])
plot(roc(Test_data$Churn, RF_pred [,2]),
      main = "ROC for Random Forest")
auc <- round(auc,3)
legend(0.6,0.2,auc)
Gini = 2*auc - 1
Gini

#******************************** Logistic Regression *******************************
logit_model = glm(Churn ~ ., data = Train_data, family = "binomial")

#summary of the model
summary(logit_model)

# write(capture.output(summary(logit_model)),"logistic_Model_Summary.txt")

# predict using logistic regression
logit_Predictions = predict(logit_model, newdata = Test_data, type = "response")

# convert prob
logit_Predictions = ifelse(logit_Predictions > 0.5, 1, 0)

#Evaluate the performance of classification model
table(Actual = Test_data$Churn, Prediction = logit_Predictions)

# Accuracy of Logistic Regression = 87.94
# FAlse Negative Rate of Logistic Regression = 75.0
#install.packages("nnet")
library(nnet)
mymodel <- multinom(Churn ~. , data = Train_data)
# Misclassification Rate
p <- predict(mymodel,Test_data[,-16])
tab <- table(p, Test_data$Churn)

library(ROCR)
library(gplots)
logi_pred <- predict(mymodel,newdata = Test_data[,-16], type = 'prob')
logi_pred <- prediction(logi_pred, Test_data$Churn)

eval <- performance(logi_pred , "acc")
plot(eval)

# Best fit
max <- which.is.max(slot(eval,"y.values")[[1]])
acc <- slot(eval,"y.values")[[1]]
cut <- slot(eval,"x.values")[[1]]
print(c(Accuracy = acc, Cutoff = cut))

roc <- performance(logi_pred, "tpr","fpr")
plot(roc, colorize = T, main = "ROC of Logistic", ylab = "TP", xlab = "FP")
abline(a = 0, b = 1)
#AUC
auc <- performance(logi_pred ,"auc")
auc <- unlist(slot(auc,"y.values"))
auc <- round(auc, 3)
legend(0.6,0.4,auc,title = "AUC")
Gini = 2*auc - 1
Gini
# Gini = 0.524
#************************* Grid search for SVM  ********************************
SVM_model = train(form = Churn ~., data = Train_data, method = 'svmRadial')
SVM_model$bestTune
y_pred = predict(SVM_model, newdata = Test_data[-16])
cm = table(Actual = Test_data$Churn, Prediction = y_pred)

ConfMatrix = table(Test_data$Churn, y_pred)
confusionMatrix(ConfMatrix)

#Accuracy of SVM= 86.66
#*************************** Naive Bayes ***********************************
library(e1071)

#Develop model
NB_model = naiveBayes(Churn ~ ., data = Train_data)

#predict on test cases #raw for probability
NB_Predictions = predict(NB_model, Test_data[,1:15], type = 'class')

#Look at confusion matrix
Conf_matrix = table(observed = Test_data[,16], predicted = NB_Predictions)
confusionMatrix(Conf_matrix)

#Accuracy of Naive Bayes= 88.12
# F-N rate of Naive Bayes = 79.01 

NV_pred <- predict(NB_model, Test_data[,-16], type = 'raw')
auc <- auc(Test_data$Churn ,NV_pred[,2])
plot(roc(Test_data$Churn, NV_pred [,2]),
     main = "ROC for Naive Bayes ")
auc <- round(auc,3)
legend(0.6,0.2,auc)
Gini = 2*auc - 1
Gini
# Gini Coefficent = 0.522
