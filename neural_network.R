# Karishma Panwar

# This code  import a dataset of people and generate a neural network model 
# that will predict if a fisher used a chartered boat service based on their 
# fishing catch rate and their annual income

# Install the tidyverse and neuralnet packages
# install.packages("tidyverse")
# install.packages("neuralnet")

# Load the tidyverse and neuralnet libraries
library(tidyverse)
library(neuralnet)

# Set the working directory to your Lab12 folder
setwd("C:/Users/ual-laptop/Documents/Rworkspace/Lab12")
getwd()

# Read FishingCharter.csv into a tibble called fishingCharter 
fishingCharter = read_csv(file = "FishingCharter.csv",
                          col_types = "lnn",
                          col_names = TRUE)

# Display fishingCharter in the console
print(fishingCharter)

# Display the structure of fishingCharter in the console
str(fishingCharter)

# Display the summary of fishingCharter in the console
summary(fishingCharter)

# Scale the AnnualIncome and CatchRate variables
fishingCharter <- fishingCharter %>%
  mutate(AnnualIncomeScaled = (AnnualIncome - min(AnnualIncome)) /
           (max(AnnualIncome) - min(AnnualIncome)))

fishingCharter <- fishingCharter %>%
  mutate(CatchRateScaled = (CatchRate - min(CatchRate)) /
           (max(CatchRate) - min(CatchRate)))

# Randomly split the dataset into fishingCharterTraining (75% of records) and 
# fishingCharterTesting (25% of records) using 591 as the random seed
set.seed(591)

sampleSet <- sample(nrow(fishingCharter),
                    round(nrow(fishingCharter) * 0.75),
                    replace = FALSE)

fishingCharterTraining <- fishingCharter[sampleSet, ]
  
fishingCharterTesting <- fishingCharter[-sampleSet, ]
# Generate the neural network model to predict CharteredBoat (dependent 
# variable) using AnnualIncomeScaled and CatchRateScaled (independent variables)
fishingCharterNeuralNet <- neuralnet(
  formula = CharteredBoat ~ AnnualIncomeScaled + CatchRateScaled,
  data = fishingCharterTraining,
  hidden = 3,
  act.fct = "logistic",
  linear.output = FALSE
)

# Display the neural network numeric results
print(fishingCharterNeuralNet$result.matrix)

# Visualize the neural network
plot(fishingCharterNeuralNet)

# Use fishingCharterNeuralNet to generate probabilities on the 
# fishingCharterTesting data set and store this in fishingCharterProbability
fishingCharterProbability <- compute(fishingCharterNeuralNet,
                                     fishingCharterTesting)

# Display the probabilities from the testing dataset on the console
print(fishingCharterProbability$net.result)

# Convert probability predictions into 0/1 predictions and store this into 
# fishingCharterPrediction
fishingCharterPrediction <-
  ifelse(fishingCharterProbability$net.result > 0.5, 1, 0)

# Display the 0/1 predictions on the console
print(fishingCharterPrediction)

# Evaluate the model by forming a confusion matrix
fishingCharterConfusionMatrix <- table(fishingCharterTesting$CharteredBoat,
                                       fishingCharterPrediction)

# Display the confusion matrix on the console
print(fishingCharterConfusionMatrix)

# Calculate the model predictive accuracy
predictiveAccuracy <- sum(diag(fishingCharterConfusionMatrix)) /
  nrow(fishingCharterTesting)

# Display the predictive accuracy on the console
print(predictiveAccuracy)
