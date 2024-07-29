data <- read.csv(file.choose())

# Research question: what are the key medication combinations frequently prescribed together?

library(tidyverse)
library(dplyr)
library(datasets)
library(tidyr)
library(arules) 

dim(data)

# Remove blank rows
mdata <- data[!apply(data == "", 1, all), ]
dim(mdata)

# Add ID Column
mdata$id <- factor(seq.int(nrow(mdata)))

# Factorize 
mdata <- as.data.frame(unclass(mdata), stringsAsFactors = TRUE)

# Pivot data frame 
pivot <- pivot_longer(mdata, cols = 1:20, names_to = "Precription", values_to = "Medication")
head(pivot)

# Keep only relevant variables
pivot <- pivot[,c(1,3)]
pivot <- pivot[!(pivot$Medication == ""), ]

# Create list and convert tibble to data frame
list_data <- as.data.frame(pivot)

# Split data frame
list_data <- split(list_data$Medication, list_data$id)
str(list_data)

# Transactionalized data set 
basket <- as(list_data, "transactions")

# Convert basket to matrix
basket <- as(basket, "matrix")

str(basket)
dim(basket)
head(basket)

# Run apriori function on transactions data set to get association rules 
arules <- apriori(basket, control = list(verbose = F), parameter = list(supp = 0.01, conf = 0.4, minlen = 2))

# Identify redundant rules
redundant <- is.redundant(arules)

# Refined rules
refined_arules <- arules[!redundant]

summary(refined_arules)

# Association Rules Table 
rules_data <- as.data.frame(inspect(refined_arules))

# Inspect top rules sorted by "lift" in descending order 
inspect(head(sort(refined_arules, by = "lift", decreasing = TRUE), 3))

# Check abilify 
medication_frequencies <- table(pivot$Medication)
sorted_medication_frequencies <- sort(medication_frequencies, decreasing = TRUE)
print(sorted_medication_frequencies) 
