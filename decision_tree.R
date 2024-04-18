 
# Karishma Panwar, Rohith Sai Pamidimukkala
# MIS545 02 
# Lab09Group39PamidimukkalaPanwar.R 
# Import a dataset of Indonesian rice farms and generate a decision tree model 
# that will predict a farm's ownership status (farmer-owned or sharecropped) 
# based on other farm data. 

# Install the tidyverse and rpart.plot packages 
# install.packages("tidyverse") 
# install.packages("rpart.plot")

# Load the tidyverse and e1071 library 
library(tidyverse) 
library(rpart) 

library(rpart.plot) 

# Set the working directory to lab 07 folder 
setwd("C:/Users/ual-laptop/Documents/Rworkspace/Lab09")


# Read IndonesianRiceFarms.csv into a tibble called riceFarms 
riceFarms <- read_csv(file = "IndonesianRiceFarms.csv", 
                         col_types = "fniiinf", 
                         col_names = TRUE) 

# Display riceFarms in the console 
print(riceFarms) 

# Display the structure of riceFarms in the console 
str(riceFarms) 

# Display the summary of riceFarms in the console 
summary(riceFarms) 

# Split the data into training and testing
# The set.seed()function is used to ensure that we can get the same result
# every time we run a random sampling process
set.seed(370)

# Create a vector of 75% randomly sampled rows from original dataset 
sampleSet <- sample(nrow(riceFarms), 
                    round(nrow(riceFarms) * 0.75), 
                    replace = FALSE) 

# Put the records from 75% sample into riceFarmsTraining 
riceFarmsTraining <- riceFarms[sampleSet, ] 

# Put all the other records (25%) into riceFarmsTesting 
riceFarmsTesting <- riceFarms[-sampleSet, ] 

# Generate the decision trees model to predict riceFarms based on 
# the other variables in the dataset. 
riceFarmsDecisionTreeModel <- rpart(formula = FarmOwnership ~ ., 
                                    method = "class",
                                    cp = 0.01,
                                    data = riceFarmsTraining
                                    ) 

# Display the decision tree plot
rpart.plot(riceFarmsDecisionTreeModel)

# Predict classes for each record in the testing dataset 
riceFarmsPredictions <- predict(riceFarmsDecisionTreeModel,
                                riceFarmsTesting,
                                type = "class") 

# Display riceFarmsPredictions on the console 
print(riceFarmsPredictions) 

# Evaluate the model by forming a confusion matrix 
riceFarmsConfusionMatrix <- table(riceFarmsTesting$FarmOwnership, 
                                  riceFarmsPredictions) 

# Display the confusion matrix on the console 
print(riceFarmsConfusionMatrix) 

# Calculate the model predictive accuracy and store it into 
# a variable called predictiveAccuracy 
predictiveAccuracy <- sum(diag(riceFarmsConfusionMatrix)) / 
  nrow(riceFarmsTesting) 

# Display the predictive accuracy on the console 
print(predictiveAccuracy) 

# # Generate the second decision trees model to predict riceFarms based on 
# the other variables in the dataset using complexity parameter 0.007 
riceFarmsDecisionTreeModel <- rpart(formula = FarmOwnership ~ ., 
                                    method = "class",
                                    cp = 0.007,
                                    data = riceFarmsTraining
) 

# Display the decision tree plot
rpart.plot(riceFarmsDecisionTreeModel)

# Predict classes for each record in the testing dataset 
riceFarmsPredictions <- predict(riceFarmsDecisionTreeModel,
                                riceFarmsTesting,
                                type = "class") 

# Display riceFarmsPredictions on the console 
print(riceFarmsPredictions) 

# Evaluate the model by forming a confusion matrix 
riceFarmsConfusionMatrix <- table(riceFarmsTesting$FarmOwnership, 
                                  riceFarmsPredictions) 

# Display the confusion matrix on the console 
print(riceFarmsConfusionMatrix) 

# Calculate the model predictive accuracy and store it into 
# a variable called predictiveAccuracy 
predictiveAccuracy <- sum(diag(riceFarmsConfusionMatrix)) / 
  nrow(riceFarmsTesting) 

# Display the predictive accuracy on the console 
print(predictiveAccuracy)
