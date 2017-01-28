# Q1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

training <- segmentationOriginal[segmentationOriginal$Case == "Train",]
testing <- segmentationOriginal[segmentationOriginal$Case == "Test",]
dim(training); dim(testing)
set.seed(125)
modFit <- train(Class ~ ., method="rpart", data=training)
print(modFit$finalModel)


#Q3
library(pgmm)
data(olive)
olive = olive[,-1]
modFit = train(Area ~ ., method="rpart", data=olive)
newdata = as.data.frame(t(colMeans(olive)))
predict(modFit, newdata)

#Q3
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
set.seed(13234)
modFit = train(chd ~ age + alcohol + obesity + tobacco + typea + ldl
               ,method="glm"
               ,family="binomial"
               ,data=trainSA)
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
predTrain = predict(modFit,trainSA)
predTest = predict(modFit,testSA)
missClass(trainSA$chd,predTrain)
missClass(testSA$chd,predTest)


#Q4
library(ElemStatLearn)
library(randomForest)
data(vowel.train)
data(vowel.test)

vowel.train$y = factor(vowel.train$y)
vowel.test$y = factor(vowel.test$y)
set.seed(33833)
modFit = randomForest(y ~ ., data=vowel.train)
