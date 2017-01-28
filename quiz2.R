library(AppliedPredictiveModeling); library(caret)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50, list=F)
training = adData[trainIndex,]
testing = adData[-trainIndex,]


library(AppliedPredictiveModeling); library(ggplot2); library(Hmisc) 
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

plot(training$CompressiveStrength)
qplot(seq_along(training$CompressiveStrength), training$CompressiveStrength, colour=cut2(training$Cement, g=2))
qplot(seq_along(training$CompressiveStrength), training$CompressiveStrength, colour=cut2(training$BlastFurnaceSlag, g=2))
qplot(seq_along(training$CompressiveStrength), training$CompressiveStrength, colour=cut2(training$FlyAsh, g=2))
qplot(seq_along(training$CompressiveStrength), training$CompressiveStrength, colour=cut2(training$Water, g=2))
qplot(seq_along(training$CompressiveStrength), training$CompressiveStrength, colour=cut2(training$Superplasticizer, g=2))
qplot(seq_along(training$CompressiveStrength), training$CompressiveStrength, colour=cut2(training$CoarseAggregate, g=2))
qplot(seq_along(training$CompressiveStrength), training$CompressiveStrength, colour=cut2(training$FineAggregate, g=2))
qplot(seq_along(training$CompressiveStrength), training$CompressiveStrength, colour=cut2(training$Age, g=2))

qplot(training$Superplasticizer)
hist(training$Superplasticizer)


library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
training.IL <- training[, grepl("^IL", colnames(training))]
training.PCA <- preProcess(training.IL, method="pca", thresh=.8)
predict(training.PCA, training.IL)

library(caret)
library(AppliedPredictiveModeling)
library(e1071)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
training.IL <- training[, c(TRUE,grepl("^IL", colnames(training)[-1]))]

modelFit <- train(diagnosis ~ .,method="glm",data=training.IL)

cntrl = trainControl(preProcOptions = list(thresh=.8))
modelFit.PCA <- train(diagnosis ~ .
                      ,method="glm"
                      ,preProcess="pca"
                      ,trControl=cntrl
                      ,data=training.IL)
