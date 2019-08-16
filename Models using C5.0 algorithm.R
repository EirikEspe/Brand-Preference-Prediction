################################################################
# Blackwell Electronics
# Classification: Predict which Brand of Products Customers Prefer

# Creating models using the C5.0 algorithm

#Created by Eirik Espe
################################################################

source("Initial exploration.R")


#---Training and test sets----

#Set seed
set.seed(123)     #or 42 (For the record, 42 is always the right seed)


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

#Train a C5.0 model with tuneLenght = 2. All features used
c5fit1 <- train(brand~., data = training, 
                method = "C5.0", 
                trControl = Control, 
                tuneLength = 2)

#Training results of first model
c5fit1

#Results from the training
train_resultsc51 <- predict(c5fit1, training)
postResample(train_resultsc51, training$brand) 
# Accuracy 0.9282
# Kappa 0.8482

#Results on test
test_resultsc51 <- predict(c5fit1, testing)
postResample(test_resultsc51, testing$brand)
# Accuracy 0.9252
# Kappa 0.8426


# Variable importance for features in the training model
varImp(c5fit1)
# Plot the variable importance
plot(varImp(c5fit1), top = 15)


# Plot of the errors
testing$pred1C5 <- test_resultsc51

ggplot(testing, aes(x = age, y = salary)) +
  geom_point(aes(color = (brand != pred1C5))) +
  labs(title = "C5.0 model error with all variables") +
  scale_color_manual(values = c("white","red")) +
  theme(legend.position="none")

#--- 2nd Training model----

#Train a C5.0 model with tuneLenght = 2. Features included are salary, age,
# car and zipcode
c5fit2 <- train(brand ~ salary + age + car + zipcode, 
                data = training, 
                method = "C5.0", 
                trControl = Control, 
                tuneLength = 2)

#Training results of second model
c5fit2

#Results from the training
train_resultsc52 <- predict(c5fit2, training)
postResample(train_resultsc52, training$brand) 
# Accuracy 0.9311
# Kappa 0.8535

#Results on test
test_resultsc52 <- predict(c5fit2, testing)
postResample(test_resultsc52, testing$brand)
# Accuracy 0.9256
# Kappa 0.8425


# Plot of the errors
testing$pred2C5 <- test_resultsc52

ggplot(testing, aes(x = age, y = salary)) +
  geom_point(aes(color = (brand != pred2C5))) +
  labs(title = "2nd C5.0 model errors") +
  scale_color_manual(values = c("white","red")) +
  theme(legend.position="none")
#--- 3rd Training model----

#Train a C5.0 model with tuneLenght = 2. Features included are salary, age,
# zipcode and credit
c5fit3 <- train(brand ~ salary + age + zipcode + credit, 
                data = training, 
                method = "C5.0", 
                trControl = Control, 
                tuneLength = 2)


#Training results of second model
c5fit3


#Results from the training
train_resultsc53 <- predict(c5fit3, training)
postResample(train_resultsc53, training$brand) 
# Accuracy 0.9297
# Kappa 0.8499

#Results on test
test_resultsc53 <- predict(c5fit3, testing)
postResample(test_resultsc53, testing$brand)
# Accuracy 0.9268
# Kappa 0.8440


#--- 4th Training model----

#Train a C5.0 model with tuneLenght = 2. Features included are salary and age
c5fit4 <- train(brand ~ salary + age, 
                data = training, 
                method = "C5.0", 
                trControl = Control, 
                tuneLength = 2)


#Training results of second model
c5fit4


#Results from the training
train_resultsc54 <- predict(c5fit4, training)
postResample(train_resultsc54, training$brand) 
# Accuracy 0.9250
# Kappa 0.8391

#Results on test
test_resultsc54 <- predict(c5fit4, testing)
postResample(test_resultsc54, testing$brand)
# Accuracy 0.9293
# Kappa 0.8486

# Creating a confusion matrix
confusionMatrix(data = test_resultsc54, reference = testing$brand)
# Showing only the predictions/reference table
confusionMatrix(data = test_resultsc54, reference = testing$brand)$table
# Expressed as percentages
round(prop.table(
  confusionMatrix(data = test_resultsc54, reference = testing$brand)$table
  ), 2)


# Plot of the errors
testing$predC5 <- test_resultsc54

ggplot(testing, aes(x = age, y = salary)) +
  geom_point(aes(color = (brand != predC5))) +
  labs(title = "C5.0 model errors") +
  scale_color_manual(values = c("white","red")) +
  theme(legend.position="none")

