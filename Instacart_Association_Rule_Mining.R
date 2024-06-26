# Karishma Panwar
# This code imports a dataset of Instacart grocery transactions and generate 
# association rules among items in a transaction.

# Install the tidyverse and rules packages
# install.packages("tidyverse")
install.packages("arules")

# Load the tidyverse and rules libraries
library(tidyverse)
library(arules)

# Set the working directory to your Lab10 folder
setwd("C:/Users/ual-laptop/Documents/Rworkspace/Lab10")

# Read InstacartTransactions.csv into an object called instacartTransactions 
# using the read.transactions() function
instacartTransactions <- read.transactions(file = 'InstacartTransactions.csv',
                                           format = 'single',
                                           header = TRUE,
                                           sep = ',',
                                           cols = c('OrderID',	'ItemID'))

# Display a summary of instacartTransactions
summary(instacartTransactions)

# Display the first three transactions from instacartTransactions on the console
inspect(instacartTransactions[1:3])

# Examine the frequency of a single item: 24852 (bananas)
itemFrequency(instacartTransactions[, "24852"])

# Convert the frequency values in instacartTransactions into a tibble called 
# instacartTransactionsFrequency
instacartTransactionsFrequency <- tibble(
  Items = names(itemFrequency(instacartTransactions)),
  Frequency = itemFrequency(instacartTransactions))

# Display the item frequencies in instacartTransactionsFrequency on the console
print(instacartTransactionsFrequency)

# Display the 10 most frequently purchased items on the console
instacartTransactionsFrequency %>% 
  arrange(desc(Frequency)) %>%
  slice(1:10)

# Generate the association rules model in an object called 
# instacartTransactionRules using the following parameters:
# Support of 0.005, Confidence of 0.2, Minimum length of 2
instacartTransactionRules <- apriori(instacartTransactions,
                                     parameter = list(
                                       support = 0.005,
                                       confidence = 0.2,
                                       minlen = 2))
  
# Display a summary of instacartTransactionRules
summary(instacartTransactionRules)

# Display the first 10 association rules
inspect(instacartTransactionRules[1:10])

# Sort the association rules by lift and view the top 10
instacartTransactionRules %>% 
  sort(by = "lift") %>% 
  head(n = 10) %>%
  inspect()
