################################################################
# Blackwell Electronics
# Classification: Predict which Brand of Products Customers Prefer

# Initial exploration

#Created by Eirik Espe
################################################################

#Calling on packages. Install the packages if you do not have them already.
library(readr)
library(caret)
library(ggplot2)

#Upload data - Complete Responses
CompleteResponses <- read_csv("CompleteResponses.csv")

#The data set has 9898 observations.
head(CompleteResponses)

#Make a correlation matrix
correlationMatrix <- cor(CompleteResponses)

# Look for highly correlated attributes
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.80)
# Print indexes of highly correlated attributes
print(highlyCorrelated)     
# No attributes with high correlation

#Change data type for level of education, car brand, zipcode and 
#computer brand preference. Data type set to factor.
CompleteResponses$elevel <- as.factor(CompleteResponses$elevel)
CompleteResponses$car <- as.factor(CompleteResponses$car)
CompleteResponses$zipcode <- as.factor(CompleteResponses$zipcode)
CompleteResponses$brand <- as.factor(CompleteResponses$brand)


#Upload the incomplete responses
Incomplete <- read_csv("SurveyIncomplete.csv")

#View the structure of the data
str(CompleteResponses)
#View the structure of the incomplete data
str(Incomplete)

#Summary of the responses
summary(CompleteResponses)
#Summary of the incomplete responses
summary(Incomplete)



#Summary customers preferring Acer
summary(subset(CompleteResponses,CompleteResponses$brand==0))
#Summary customers preferring Sony
summary(subset(CompleteResponses,CompleteResponses$brand==1))



#---Visualization----

# Computer preference and salary boxplot
plot(CompleteResponses$brand, CompleteResponses$salary, 
     names = c("Acer", "Sony"), xlab = "Computer", ylab = "Salary")

# Computer preference and salary violin plot (black and white)
ggplot(CompleteResponses, aes(x = brand, y = salary)) + 
  geom_violin(trim = FALSE) + 
  #stat_summary(fun.y = median, geom="point", shape=23, size=2) + 
  geom_boxplot(width = 0.05) + 
  scale_x_discrete(breaks=c("0", "1"), labels = c("Acer", "Sony"))

# Computer preference and salary - violin plot (with colours)
ggplot(CompleteResponses, aes(x=brand, y=salary, fill=brand)) + 
  geom_violin(trim = FALSE, fill="brown") + 
  #stat_summary(fun.y = median, geom="point", shape=23, size=2) + 
  geom_boxplot(width = 0.05) + 
  scale_x_discrete(breaks = c("0", "1"), labels = c("Acer", "Sony")) + 
  labs(title = "Salary and computer preference", x = "Computer preference", 
       y = "Salary") + 
  scale_fill_discrete(name = "Computer", labels = c("Acer", "Sony")) + 
  theme(plot.title = element_text(hjust = 0.5))

# Relationship between brand preference, salary and age
ggplot(CompleteResponses, aes(x = age, y = salary)) + 
  geom_point(aes(colour = brand)) + 
  scale_colour_discrete(name = "Computer\npreference", 
                        label = c("Acer", "Sony")) + 
  labs(title = "Computer preference for different salary and age groups", 
       x = "Age", y = "Salary")

#Computer preference and age boxplot
plot(CompleteResponses$brand, CompleteResponses$age, 
     names=c("Acer", "Sony"), xlab="Computer", ylab="Age")

# Computer preference and age histogram
ggplot(CompleteResponses, aes(x = age, fill = brand)) + 
  geom_histogram(binwidth = 10, color = "black", position = "identity", 
                 alpha = 0.6) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Age and computer preference", x = "Age") + 
  scale_fill_discrete(name = "Computer\npreference", labels = c("Acer", "Sony")) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  facet_grid(~brand) + 
  scale_x_continuous(breaks = seq(20, 80, by = 10)) + 
  geom_vline(xintercept = median(CompleteResponses$age), color = "black",
             linetype = "dashed", size = 0.4)

#Computer preference per education level
ggplot(CompleteResponses, aes(x = elevel)) + 
  geom_bar(aes(fill = brand), position = "dodge") + 
  scale_x_discrete(breaks=c("0", "1", "2", "3", "4"), 
                   labels=c("Less than\nHigh School", 
                            "High School\ndegree", 
                            "Some College", "4-year College\ndegree", 
                            "Master's, Doctoral or\nProfessional degree")) + 
  theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
  labs(title = "Computer preference per education level", x = "Education") + 
  scale_fill_discrete(name = "Computer preference", 
                      labels = c("Acer", "Sony")) + 
  theme(plot.title = element_text(hjust = 0.5))