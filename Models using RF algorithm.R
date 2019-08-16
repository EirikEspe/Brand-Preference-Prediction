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





#--- 4th Training model----


#Train a Random Forest model with 5 mtry values. Features included are 
# salary and age. Discretized variables with 8 bins.
svmFit4 <- train(brand ~ salary + age, 
                data = training, 
                method = "svmRadial", 
                trControl = Control, 
                tuneLength = 5)

#Output
svmFit4


#Results from the training
train_resultsSVM4 <- predict(svmFit4, training)
postResample(train_resultsSVM4, training$brand) 
# Accuracy 0.9239
# Kappa 0.8385

#Results on test
test_resultsSVM4 <- predict(svmFit4, testing)
postResample(test_resultsSVM4, testing$brand)
# Accuracy 0.9244
# Kappa 0.8400



# Creating a confusion matrix, using the model with salary, age and credit
confusionMatrix(data = test_resultsRF2, reference = testing$brand)

# Plot of the errors, using the model with salary, age and credit
testing$predRF <- test_resultsRF2

ggplot(testing, aes(x = age, y = salary)) +
  geom_point(aes(color = (brand != predRF))) +
  labs(title = "RF model errors") +
  scale_color_manual(values = c("white","red")) +
  theme(legend.position="none")


# Plot of the errors, using the SVM model
testing$predSVM <- test_resultsSVM4

ggplot(testing, aes(x = age, y = salary)) +
  geom_point(aes(color = (brand != predSVM))) +
  labs(title = "SVM model errors") +
  scale_color_manual(values = c("white","red")) +
  theme(legend.position="none")