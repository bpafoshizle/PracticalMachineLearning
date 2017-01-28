# Q1
library(ElemStatLearn) 
data(vowel.train)
data(vowel.test)
vowel.train$y = factor(vowel.train$y)
vowel.test$y = factor(vowel.test$y)
set.seed(33833)

library(caret)
rfModFit = train(y ~ ., data=vowel.train, method="rf")
gbmModFit = train(y ~ ., data=vowel.train, method="gbm")

rfPred = predict(rfModFit, newdata=vowel.test)
gbmPred = predict(gbmModFit, newdata=vowel.test)

rfAcc = sum((rfPred == vowel.test$y)*1)/length(vowel.test$y)
gbmAcc = sum((gbmPred == vowel.test$y)*1)/length(vowel.test$y)

agree = which(rfPred == gbmPred) 
agreeAcc = sum((gbmPred[agree] == vowel.test$y[agree])*1)/length(agree)

# Q2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
set.seed(62433)

rfMod = train(diagnosis ~ ., method="rf", data=training)
gbmMod = train(diagnosis ~ ., method="gbm", data=training)
ldaMod = train(diagnosis ~ ., method="lda", data=training)

rfPred = predict(rfMod, newdata=testing)
gbmPred = predict(gbmMod, newdata=testing)
ldaPred = predict(ldaMod, newdata=testing)

predDF = data.frame(rfPred, gbmPred, ldaPred, diagnosis=testing$diagnosis)

ensMod = train(diagnosis ~ ., method="rf", data=predDF)
ensPred = predict(ensMod, newdata=testing)
accuracy = function(values,prediction){sum((values == prediction)*1)/length(values)}

accuracy(testing$diagnosis,rfPred)
accuracy(testing$diagnosis, gbmPred)
accuracy(testing$diagnosis, ldaPred)
accuracy(testing$diagnosis, ensPred)

# Q3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)
lassoMod = train(CompressiveStrength ~ ., method="lasso", data=training)
plot(lassoMod$finalModel, xvar="penalty", use.color = T)


# Q4
library(lubridate) # For year() function below
dat = read.csv("/users/bpafoshizle/github/local/PracticalMachineLearning/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
tstest = ts(testing$visitsTumblr)

library(forecast)
modFit = bats(tstrain)
fcast = forecast(modFit, level=95, h=nrow(testing))
plot(fcast)
lines(testing$visitsTumblr,col="red")
accuracy(fcast, testing$visitsTumblr)

myFcastAccuracy = function(upper,lower,values){sum((lower < values & values < upper)*1)/length(values)}
myFcastAccuracy(fcast$upper, fcast$lower, testing$visitsTumblr)


# Q5 
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
library(e1071)
library(ModelMetrics)
svm.model = svm(CompressiveStrength ~ ., data=training)
svm.pred = predict(svm.model, newdata = testing)
rmse(testing$CompressiveStrength,svm.pred)
