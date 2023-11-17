install.packages("mlbench")
install.packages("dplyr")
install.packages("caret")
install.packages("ggplot2")
install.packages("class")
install.packages('e1071', dependencies=TRUE)
library(e1071)
library(ggplot2)
library(caret)
library(mlbench)
library(dplyr)
library(class)
data("PimaIndiansDiabetes")
data=(PimaIndiansDiabetes)
names(data)

data=na.omit(data)

pos=data[data$diabetes=='pos',]
neg=data[data$diabetes=='neg',]
par(mfrow=c(1,1))
names(data)

boxplot(data$pregnant~data$diabetes,xlab='pregnant', ylab='')
mean(pos$pregnant)
mean(neg$pregnant)
boxplot(data$insulin~data$diabetes,xlab='insulin', ylab='')
mean(pos$insulin)
mean(neg$insulin)
boxplot(data$age~data$diabetes,xlab='age', ylab='')
mean(pos$age)
mean(neg$age)
boxplot(data$mass~data$diabetes,xlab='mass', ylab='')
mean(pos$mass)
mean(neg$mass)
boxplot(data$glucose~data$diabetes,xlab='glucose', ylab='')
mean(pos$glucose)
mean(neg$glucose)

data[,9]<-sapply(data[,9], as.numeric)
data=data%>%mutate(diab=factor(ifelse(diabetes == "2" , 1,0)))%>% select(pregnant, glucose, insulin, mass, age, diab)

nor <-function(x) {(x -min(x))/(max(x)-min(x))}
data[,1:5] <- sapply(data[,1:5], nor)

set.seed(100)
training.idx=sample(1:nrow(data),size = nrow(data)*0.8)
train.data=data[training.idx, ]
test.data=data[-training.idx, ]

#kNN (CLASSIFICATION)
set.seed(101)
#k= 23 as best tune is 23
knn1<-knn(train.data[,1:5], test.data[,1:5], cl=train.data$diab, k=23)
#evaluate classification performance and check accuracy
model = train(
    diab~., data = train.data, method = "knn",
    trControl = trainControl("cv", number = 10),
    preProcess = c("center","scale"),
    tuneLength = 10
)
model$bestTune
predictions=   predict(model, test.data)
head(predictions)
RMSE(predictions, test.data$diab)#Explain why is is NA
plot(test.data$diab, predictions,main="Prediction performance of kNN regression")

mean(knn1 ==test.data$diab)
table(knn1,test.data$diab)
accuracy =  function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(table(knn1,test.data$diab))

m.svm <- svm(diab ~ glucose+age+mass+pregnant+insulin, data = train.data, kernel = "linear")
summary(m.svm)
pred.svm <- predict(m.svm, newdata=test.data[,1:5])
mean(pred.svm ==test.data$diab)
table(pred.svm, test.data$diab)

set.seed(123)
m.svm.tune<-tune.svm(diab~., data=train.data, kernel="radial", cost=10^(-1:2), gamma=c(.1,.5,1,2))
summary(m.svm.tune)

best.svm = m.svm.tune$best.model
pred.svm.tune = predict(best.svm, newdata=test.data[,1:5])
#evaluate classification performance and check accuracy
mean(pred.svm.tune ==test.data$diab)
#Create the confusion matrix with row=pred.svm.tune, col=diab
table(pred.svm.tune, test.data$diab)

mlogit <- glm(diab ~age+pregnant+glucose+mass+insulin, data = train.data, family = "binomial")
summary(mlogit)
Pred.p <-predict(mlogit, newdata =test.data, type = "logit model")
y_pred_num <-ifelse(Pred.p > 0.5, 1, 0)
y_pred <-factor(y_pred_num, levels=c(0, 1))
mean(y_pred ==test.data$diab)
table(y_pred,test.data$diab)
     



