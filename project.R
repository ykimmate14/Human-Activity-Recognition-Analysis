library(caret); library(randomForest); library(gbm); library(rpart)

#read the data sets
training <- read.csv("pml-training.csv", header = TRUE, na.strings = c("", "NA", "#DIV/0!"))
testing <- read.csv("pml-testing.csv", header = TRUE, na.strings = c("", "NA", "#DIV/0!"))

#clean the data sets; remove columns whose majority is NA values
training <- training[,colSums(is.na(training)) < 0.1*nrow(training)]
testing <- testing[,colSums(is.na(testing)) < 0.1*nrow(testing)]

set.seed(2424)

#Create a subset of training data set for validation
valIndex <- createDataPartition(y = training$classe, p = 0.3, list = FALSE)
valdata <- training[valIndex,]

training <- training[-valIndex,]

#Create a random forest, gbm(generalized boosted regression modeling), and rpart(recursive partitioning) model
rfmodel <- train(classe ~ ., data = training[,c(-1)], method = "rf", verbose = FALSE)
gbmmodel <- train(classe ~ ., data = training[,c(-1)], method = "gbm", verbose = FALSE)
rpartmodel <- train(classe ~ ., data = training[,c(-1)], method = "rpart")

#Evaluate the models with validation data set
rfpredCV <- predict(rfmodel, valdata)
gbmpredCV <- predict(gbmmodel, valdata)
rpartpredCV <- predict(rpartmodel, valdata)

#accuracy for rf model
rfacc <- confusionMatrix(valdata$classe, rfpredCV)$overall[1]
#accuracy for gbm model
gbmacc <- confusionMatrix(valdata$classe, gbmpredCV)$overall[1]
#accuracy for rpart model
rpartacc <- confusionMatrix(valdata$classe, rpartpredCV)$overall[1]

data.frame(model = c("rf","gbm","rpart"), accuracy = c(rfacc,gbmacc,rpartacc))


testPred <- predict(rfmodel, testing)
testing$pred <- testPred



