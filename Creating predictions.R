################################################################
# Blackwell Electronics
# Classification: Predict which Brand of Products Customers Prefer

# Creating predictions

# Created by Eirik Espe
################################################################

source("Models using C5.0 algorithm.R")


#---Import incomplete survey
Incomplete <- read_csv("SurveyIncomplete.csv")

#The dataset has 5000 observations.
head(Incomplete)

#Target variable is computer brand (Y = brand) 



# To be able to make the predictions we have to use the same format 
# on our variables as we used to make our training model.

# Change data type for level of education, car brand, zipcode and 
# computer brand preference. Data type set to factor.
Incomplete$elevel <- as.factor(Incomplete$elevel)
Incomplete$car <- as.factor(Incomplete$car)
Incomplete$zipcode <- as.factor(Incomplete$zipcode)
Incomplete$brand <- as.factor(Incomplete$brand)


#View the structure of the data
str(Incomplete)

#Summary of the incomplete data
summary(Incomplete)


################################################################

#---Using the training model created with the C5.0 algorithm


predictions <- predict(c5fit4, Incomplete)


# Put the predictions into the Incomplete dataset
Incomplete$predictions <- predictions



# Plot of the predictions
ggplot(Incomplete, aes(x = predictions)) + 
  geom_bar(aes(fill = predictions), alpha = 0.6) +
  scale_x_discrete(breaks = c(0, 1), label = c("Acer", "Sony")) +
  scale_fill_discrete(name = "Computer\npreference", 
                        label = c("Acer", "Sony")) +
  labs(title = "Predicted computer brand preference", x = "Predictions") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.2)



# Combine the completed survey and the incomplete survey with predictions 

# Create equal number of columns
Incomplete$brand <- NULL

# Combine the data. 
FullSurvey <- rbind(CompleteResponses, 
                    setNames(Incomplete, names(CompleteResponses)))
# (setNames is used because the response variable is named brand in 
# CompleteResponses, while in Incomplete the response variable is named predictions.
# To be able to use rbind(), the column names need to be identical. Thus, setNames
# can fix this, without renaming the response variable in the Incomplete dataset.)


# Plot the full survey data
ggplot(FullSurvey, aes(x = brand)) + 
  geom_bar(aes(fill = brand), alpha = 0.6) +
  scale_x_discrete(breaks = c(0, 1), label = c("Acer", "Sony")) +
  scale_fill_discrete(name = "Computer\npreference", 
                      label = c("Acer", "Sony")) +
  labs(title = "Computer brand preference", subtitle = "Full survey", x = "") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.2)

  
  
