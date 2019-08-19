################################################################
# Blackwell Electronics
# Classification: Predict which Brand of Products Customers Prefer

# Creating models using the Random Forest algorithm

#Created by Eirik Espe
################################################################

source("Initial exploration.R")



#---Training and test sets----

#Set seed
set.seed(123)


#Define a 75%/25% train/test split of the dataset
inTraining <- createDataPartition(CompleteResponses$brand, 
                                  p = .75, 
                                  list = FALSE)
training <- CompleteResponses[inTraining,]
testing <- CompleteResponses[-inTraining,]


#---Cross validation----

#Setting a 10-fold cross validation to use on the model
Control <- trainControl(method = "repeatedcv", 
                        number = 10, 
                        repeats = 1)


#---Training model----


# Set tuning grid 
# mtry is the number of variables randomly sampled as candidates at each split
tunegrid <- expand.grid(mtry = c(2, 4, 6, 10, 12))

#Train a Random Forest model with 5 mtry values. All features used.
rfFit1 <- train(brand~., data = training, 
                method = "rf", 
                trControl = Control, 
                tuneGrid = tunegrid)

#Training results output
rfFit1


#Results from the training
train_resultsRF1 <- predict(rfFit1, training)
postResample(train_resultsRF1, training$brand) 
# Accuracy 1
# Kappa 1

#Results on test
test_resultsRF1 <- predict(rfFit1, testing)
postResample(test_resultsRF1, testing$brand)
# Accuracy 0.9244
# Kappa 0.8403


# Variable importance for features in the training model
varImp(rfFit1)
# Plot the variable importance
plot(varImp(rfFit1), top = 15)


# Setting up a Recursive Feature Elimination (RFE)

#Setting a 10-fold cross validation to use on the model
rfselection <- rfeControl(functions = rfFuncs, 
                          method = "repeatedcv", 
                          number = 10, 
                          repeats = 1)


#Run the rfe algorithm
results <- rfe(CompleteResponses[, 1:6], CompleteResponses$brand,
               sizes = c(1:6),
               rfeControl = rfselection)

#Summarize the results
print(results)

#List the top features
predictors(results)
# Top 3 variables are salary, age and credit

#Plot the results
plot(results, type=c("g", "o"))




#--- 2nd Training model----


#Train a Random Forest model with 5 mtry values. Features included are salary,
# age and credit.
rfFit2 <- train(brand ~ salary + age + credit, 
                data = training, 
                method = "rf", 
                trControl = Control, 
                tuneGrid = tunegrid)
#Output
rfFit2


#Results from the training
train_resultsRF2 <- predict(rfFit2, training)
postResample(train_resultsRF2, training$brand) 
# Accuracy 1
# Kappa 1

#Results on test
test_resultsRF2 <- predict(rfFit2, testing)
postResample(test_resultsRF2, testing$brand)
# Accuracy 0.9212
# Kappa 0.8332



#--- 3rd Training model----


#Train a Random Forest model with 5 mtry values. Features included are 
# salary and age.
rfFit3 <- train(brand ~ salary + age, 
                data = training, 
                method = "rf", 
                trControl = Control, 
                tuneGrid = tunegrid)
#Output
rfFit3


#Results from the training
train_resultsRF3 <- predict(rfFit3, training)
postResample(train_resultsRF3, training$brand) 
# Accuracy 0.9998
# Kappa 0.9997

#Results on test
test_resultsRF3 <- predict(rfFit3, testing)
postResample(test_resultsRF3, testing$brand)
# Accuracy 0.9103
# Kappa 0.8100


# Scaling the numeric variables in the training dataset
trainingScaled <- training
trainingScaled[,c(1, 2, 6)] <- apply(trainingScaled[, c(1, 2, 6)], 2, scale)

# Doing the same with the testing dataset
testingScaled <- testing
testingScaled[,c(1, 2, 6)] <- apply(testingScaled[, c(1, 2, 6)], 2, scale)


#--- 4th Training model----


#Train a Random Forest model with 5 mtry values. Features included are 
# salary and age. The features are scaled to bring the features to the 
# same level. 
rfFit4 <- train(brand ~ salary + age, 
                data = trainingScaled, 
                method = "rf", 
                trControl = Control,
                tuneGrid = tunegrid)

#Output
rfFit4


#Results from the training
train_resultsRF4 <- predict(rfFit4, trainingScaled)
postResample(train_resultsRF4, trainingScaled$brand) 
# Accuracy 0.9998
# Kappa 0.9997

#Results on test
test_resultsRF4 <- predict(rfFit4, testingScaled)
postResample(test_resultsRF4, testingScaled$brand)
# Accuracy 0.9107
# Kappa 0.8108



# Creating a confusion matrix, using the model with salary, age and credit
confusionMatrix(data = test_resultsRF2, reference = testing$brand)

# Plot of the errors, using the model with salary, age and credit
testing$predRF <- test_resultsRF2

ggplot(testing, aes(x = age, y = salary)) +
  geom_point(aes(color = (brand != predRF))) +
  labs(title = "RF model errors") +
  scale_color_manual(values = c("white","red")) +
  theme(legend.position="none")


# Plot of the errors, using the RF model with salary, age and credit
testing$predRF <- test_resultsRF2

ggplot(testing, aes(x = age, y = salary)) +
  geom_point(aes(color = (brand != predRF))) +
  labs(title = "RF model errors") +
  scale_color_manual(values = c("white","red")) +
  theme(legend.position="none")


# For comparison with SVM

#--- 1st SVM Training model----


#Train a SVM model with tuneLength = 5. Features included are 
# salary and age. The features are scaled to bring the features to the 
# same level. 
svmFit <- train(brand ~ salary + age, 
                data = trainingScaled, 
                method = "svmRadial", 
                trControl = Control,
                tuneLength = 5)

#Output
svmFit


#Results from the training
train_resultsSVM <- predict(svmFit, trainingScaled)
postResample(train_resultsSVM, trainingScaled$brand) 
# Accuracy 0.9251
# Kappa 0.8411

#Results on test
test_resultsSVM <- predict(svmFit, testingScaled)
postResample(test_resultsSVM, testingScaled$brand)
# Accuracy 0.9285
# Kappa 0.8488