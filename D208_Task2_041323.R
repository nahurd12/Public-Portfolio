mdata <- read.csv(file.choose())
head(mdata)

library(plyr)
library(car)
library(ggplot2)
library(fastDummies)
library(tidyverse)
library(dplyr)
library(tidyr)
library(caTools)

# Check for duplicates
sum(duplicated(mdata))

# Check for missing values
colSums(is.na(mdata))

# RESEARCH QUESTION 
# Can we predict the likeliness of Anxiety using the following variables: 
# Age, Gender, VitD_levels, HighBlood, Stroke, Arthritis, Diabetes, Asthma, Services, TotalCharge

unique(mdata$Gender)
unique(mdata$HighBlood)
unique(mdata$Stroke)
unique(mdata$Arthritis) 
unique(mdata$Diabetes)
unique(mdata$Anxiety)
unique(mdata$Asthma)
unique(mdata$Services)

mdata$Gender <- as.factor(mdata$Gender)
mdata$Services <- as.factor(mdata$Services)

# One Hot Encoding + K-1
dummy_cols(mdata, select_columns = c("Gender", "Services"),
           remove_first_dummy = TRUE)

# Ordinal Encoding
# HighBlood
HighBlood <- revalue(x = mdata$HighBlood, replace = c("No" = 0, "Yes" = 1))
mdata$HighBlood <- as.numeric(HighBlood)
unique(mdata$HighBlood)
class(mdata$HighBlood) 

# Stroke
Stroke <- revalue(x = mdata$Stroke, replace = c("No" = 0, "Yes" = 1))
mdata$Stroke <- as.numeric(Stroke)
unique(mdata$Stroke)
class(mdata$Stroke) 

# Arthritis
Arthritis <- revalue(x = mdata$Arthritis, replace = c("No" = 0, "Yes" = 1))
mdata$Arthritis <- as.numeric(Arthritis)
unique(mdata$Arthritis)
class(mdata$Arthritis)

# Diabetes
Diabetes <- revalue(x = mdata$Diabetes, replace = c("No" = 0, "Yes" = 1))
mdata$Diabetes <- as.numeric(Diabetes)
unique(mdata$Diabetes)
class(mdata$Diabetes)

# Anxiety
Anxiety <- revalue(x = mdata$Anxiety, replace = c("No" = 0, "Yes" = 1))
mdata$Anxiety <- as.numeric(Anxiety)
unique(mdata$Anxiety)
class(mdata$Anxiety)

# Asthma
Asthma <- revalue(x = mdata$Asthma, replace = c("No" = 0, "Yes" = 1))
mdata$Asthma <- as.numeric(Asthma)
unique(mdata$Asthma)
class(mdata$Asthma)

summary(mdata$Age) 
summary(mdata$VitD_levels)
summary(mdata$HighBlood)
summary(mdata$Stroke)
summary(mdata$Arthritis)
summary(mdata$Diabetes)
summary(mdata$Anxiety)
summary(mdata$Asthma)
summary(mdata$TotalCharge)
summary(mdata$Gender)
summary(mdata$Services)

# Part 3 - 3 Univariate and Bivariate Analysis 

# Age 
hist(mdata$Age, main = "Age of Patients")

# Gender
Gender_count <- table(mdata$Gender)
barplot(Gender_count, horiz = TRUE, main = "Gender of Patient", names.arg = c("Male", "Female", "Nonbinary"))

# VitD_Levels
hist(mdata$VitD_levels, main = "Vitamin D Level of Patients")

# HighBlood
HighBlood_count <- table(mdata$HighBlood)
barplot(HighBlood_count, horiz = TRUE, main = "High Blood Pressure Present", names.arg = c("No", "Yes"))

# Stroke
Stroke_count <- table(mdata$Stroke)
barplot(Stroke_count, horiz = TRUE, main = "Has the Patient Had a Stroke?", names.arg = c("No", "Yes"))

# Arthritis 
Arthritis_count <- table(mdata$Arthritis)
barplot(Arthritis_count, horiz = TRUE, main = "Does Patient Suffer From Arthritis", names.arg = c("No", "Yes"))

# Diabetes
Diabetes_count <- table(mdata$Diabetes)
barplot(Diabetes_count, horiz = TRUE, main = "Does the Patient Have Diabetes", names.arg = c("No", "Yes"))

# Anxiety
Anxiety_count <- table(mdata$Anxiety)
barplot(Anxiety_count, horiz = TRUE, main = "Does the Patient Have Anxiety", names.arg = c("No", "Yes"))

# Asthma 
Asthma_count <- table(mdata$Asthma)
barplot(Asthma_count, horiz = TRUE, main = "Does the Patient Have Asthma?", names.arg = c("No", "Yes"))

# Services 
Services_count <- table(mdata$Services)
barplot(Services_count, horiz = TRUE, main = "Services Received", las = 1, cex.names = 0.7)

# TotalCharge
hist(mdata$TotalCharge, main = "Average Daily Total Charges")

# Bivariate Analysis

no_yes_label = c("No", "Yes")

# Anxiety vs Age
boxplot(mdata$Age ~ mdata$Anxiety, xlab = "Anxiety", ylab = "Age", names = no_yes_label, main = "Anxiety and Age")

# Anxiety vs Gender
ggplot(mdata, aes(x = Gender, fill = factor(Anxiety))) +
  geom_bar() +
  xlab("Gender") +
  ylab("Count") +
  scale_fill_manual(values = c("steelblue", "azure3"), name = "Anxiety", labels = c("No", "Yes")) +
  ggtitle("Count of Anxiety by Gender")

# Anxiety vs VitD_levels
boxplot(mdata$VitD_levels ~ mdata$Anxiety, xlab = "Anxiety", ylab = "VitD_levels", names = no_yes_label, main = "Anxiety & VitD_levels")

# Anxiety vs HighBlood
ggplot(mdata, aes(x = factor(Anxiety), fill = factor(HighBlood))) +
  geom_bar(position = "stack") +
  scale_fill_discrete(name = "High Blood", labels = c("No", "Yes")) +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(title = "Anxiety and High Blood",
       x = "Anxiety",
       y = "Count")

# Anxiety vs Stroke
ggplot(mdata, aes(x = factor(Anxiety), fill = factor(Stroke))) +
  geom_bar(position = "stack") +
  scale_fill_discrete(name = "Stroke", labels = c("No", "Yes")) +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(title = "Anxiety and Stroke",
       x = "Anxiety",
       y = "Count")

# Anxiety vs Arthritis
ggplot(mdata, aes(x = factor(Anxiety), fill = factor(Arthritis))) +
  geom_bar(position = "stack") +
  scale_fill_discrete(name = "Arthritis", labels = c("No", "Yes")) +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(title = "Anxiety and Arthritis",
       x = "Anxiety",
       y = "Count")

# Anxiety vs Diabetes
ggplot(mdata, aes(x = factor(Anxiety), fill = factor(Diabetes))) +
  geom_bar(position = "stack") +
  scale_fill_discrete(name = "Diabetes", labels = c("No", "Yes")) +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(title = "Anxiety and Diabetes",
       x = "Anxiety",
       y = "Count")

# Anxiety vs Asthma
ggplot(mdata, aes(x = factor(Anxiety), fill = factor(Asthma))) +
  geom_bar(position = "stack") +
  scale_fill_discrete(name = "Asthma", labels = c("No", "Yes")) +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(title = "Anxiety and Asthma",
       x = "Anxiety",
       y = "Count")

# Anxiety vs Services
ggplot(mdata, aes(x = Services, fill = factor(Anxiety))) +
  geom_bar() +
  scale_fill_discrete(name = "Anxiety", labels = c("No", "Yes")) +
  labs(title = "Services and Anxiety",
       x = "Services",
       y = "Count")

# Anxiety vs TotalCharge
boxplot(mdata$TotalCharge ~ mdata$Anxiety, xlab = "Anxiety", ylab = "TotalCharge", names = no_yes_label, main = "Anxiety vs TotalCharge")

#################### 

# Logistic Regression 

initial_model <- glm(Anxiety ~ Age + Gender + VitD_levels + HighBlood + Stroke + Arthritis + Diabetes + Asthma + Services + TotalCharge, data = mdata, family = "binomial")
summary(initial_model)

vif(initial_model)

reduced_model <- step(initial_model, direction = "backward")

summary(reduced_model)

plot(reduced_model)

# Residuals vs Fitted - assesses fit of the model to the data
# Normal Q-Q Plot - assesses if a set of data points follows a normal distribution 
# Scale Location - assesses the assumptions of linearity and homoscedasticity 
# Residuals vs Leverage - identifies influential observations in the model 

# Confidence Interval of reduced model 
confint(reduced_model)

###################
# Confusion Matrix 

split <- sample.split(mdata$Anxiety, SplitRatio = 0.8)
train <- subset(mdata, split == 1)
test <- subset(mdata, split == 0)

nrow(train)
nrow(test)

result <- predict(reduced_model, newdata = test, type = "response")

view(result)
range(result) # Range of probabilities 

table(test$Anxiety, result > 0.31)

write.csv(mdata, "C:\\Users\\nahur\\Desktop\\WGU\\D208 Predictive Modeling\\D208Task2.csv", row.names=FALSE)
