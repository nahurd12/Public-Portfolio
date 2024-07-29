mdata <- read.csv(file.choose())

library(plyr) 
library(caret)
library(ggplot2)
library(naivebayes)
library(caTools)
library(dplyr)
library(e1071)
library(pROC)
library(car)

# Check for duplicates
sum(duplicated(mdata))

# Check for missing values
colSums(is.na(mdata))

# Ordinal Encoding
# SoftDrink
Soft_drink <- revalue(x = mdata$Soft_drink, replace = c("No" = 0, "Yes" = 1))
mdata$Soft_drink <- as.numeric(Soft_drink)
unique(mdata$Soft_drink)
class(mdata$Soft_drink) 

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

# Complication_risk
Complication_risk <- revalue(x = mdata$Complication_risk, replace = c("Low" = 0, "Medium" = 1, "High" = 2))
mdata$Complication_risk <- as.numeric(Complication_risk)
unique(mdata$Complication_risk)
class(mdata$Complication_risk) 

# Overweight
Overweight <- revalue(x = mdata$Overweight, replace = c("No" = 0, "Yes" = 1))
mdata$Overweight <- as.numeric(Overweight)
unique(mdata$Overweight)
class(mdata$Overweight) 

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

# Hyperlipidemia
Hyperlipidemia <- revalue(x = mdata$Hyperlipidemia, replace = c("No" = 0, "Yes" = 1))
mdata$Hyperlipidemia <- as.numeric(Hyperlipidemia)
unique(mdata$Hyperlipidemia)
class(mdata$Hyperlipidemia) 

# BackPain
BackPain <- revalue(x = mdata$BackPain, replace = c("No" = 0, "Yes" = 1))
mdata$BackPain <- as.numeric(BackPain)
unique(mdata$BackPain)
class(mdata$BackPain) 

# Anxiety
Anxiety <- revalue(x = mdata$Anxiety, replace = c("No" = 0, "Yes" = 1))
mdata$Anxiety <- as.numeric(Anxiety)
unique(mdata$Anxiety)
class(mdata$Anxiety) 

# Allergic_rhinitis
Allergic_rhinitis <- revalue(x = mdata$Allergic_rhinitis, replace = c("No" = 0, "Yes" = 1))
mdata$Allergic_rhinitis <- as.numeric(Allergic_rhinitis)
unique(mdata$Allergic_rhinitis)
class(mdata$Allergic_rhinitis)

# Reflux_esophagitis
Reflux_esophagitis <- revalue(x = mdata$Reflux_esophagitis, replace = c("No" = 0, "Yes" = 1))
mdata$Reflux_esophagitis <- as.numeric(Reflux_esophagitis)
unique(mdata$Reflux_esophagitis)
class(mdata$Reflux_esophagitis)

# Asthma
Asthma <- revalue(x = mdata$Asthma, replace = c("No" = 0, "Yes" = 1))
mdata$Asthma <- as.numeric(Asthma)
unique(mdata$Asthma)
class(mdata$Asthma)

# ReAdmis
ReAdmis <- revalue(x = mdata$ReAdmis, replace = c("No" = 0, "Yes" = 1))
mdata$ReAdmis <- as.numeric(ReAdmis)
unique(mdata$ReAdmis)
class(mdata$ReAdmis)

# Children 
mdata$Children <- as.numeric(mdata$Children)

# Age
mdata$Age <- as.numeric(mdata$Age)

# Check for Multicollinearity 
glm_model <- glm(ReAdmis ~ Soft_drink + HighBlood + Stroke + Complication_risk + Overweight + Arthritis + Diabetes +
                   Hyperlipidemia + BackPain + Anxiety + Reflux_esophagitis + Allergic_rhinitis + Asthma + Children + 
                   Age, data = mdata, family = "binomial")

vif(glm_model)

# Analysis 
set.seed(1234)

sample <- sample.split(mdata, SplitRatio = 0.8)
test <- subset(mdata, sample == FALSE)
train <- subset(mdata, sample == TRUE)

train$ReAdmis <- as.factor(train$ReAdmis)

model <- naive_bayes(train$ReAdmis ~ Soft_drink + HighBlood + Stroke + Complication_risk + Overweight + Arthritis + Diabetes +
                      Hyperlipidemia + BackPain + Anxiety + Reflux_esophagitis + Allergic_rhinitis + Asthma + Children + 
                      Age, 
                    data = train)

# Prediction
train_predict <- predict(model, newdata = train)
test_predict <- predict(model, newdata = test)

head(cbind(train_predict, train))
head(cbind(test_predict, test))

# Accuracy 
train_accuracy <- mean(train_predict == train$ReAdmis)
test_accuracy <- mean(test_predict == test$ReAdmis)

cat("Training Accuracy:", train_accuracy, "\n")
cat("Testing Accuracy:", test_accuracy, "\n")

# ROC and AUC
# Calculate the predicted probabilities for the testing data
test_probs <- predict(model, newdata = test, type = "prob")[, "1"]

# Create a roc object
roc_obj <- roc(test$ReAdmis, test_probs)

# Plot the ROC curve
plot(roc_obj, main = "ROC Curve", print.auc = TRUE, auc.polygon = TRUE)

# Calculate the AUC
auc_score <- auc(roc_obj)
cat("AUC:", auc_score, "\n")

# Confusion Matrix - train data
p1 <- predict(model, train)
(tab1 <- table(p1, train$ReAdmis))

# Misclassification Error for training data
1 - sum(diag(tab1)) / sum(tab1)

# Confusion Matrix - test data
p2 <- predict(model, test)
(tab2 <- table(p2, test$ReAdmis))

# Misclassification Error for test data
1 - sum(diag(tab2)) / sum(tab2)

# F1 Score for Test
# Define the confusion matrix
CM_test <- matrix(c(1235, 734, 22, 9), nrow = 2, byrow = TRUE)
# Calculate precision
precision_test <- CM_test[2, 2] / sum(CM_test[, 2])
precision_test
# Calculate recall
recall_test <- CM_test[2, 2] / sum(CM_test[2, ])
recall_test
# Calculate F1-score
f1_score_test <- 2 * (precision_test * recall_test) / (precision_test + recall_test)
f1_score_test

# F1 Score for Train
# Define the confusion matrix
CM_train <- matrix(c(4998, 2882, 76, 44), nrow = 2, byrow = TRUE)
# Calculate precision
precision_train <- CM_train[2, 2] / sum(CM_train[, 2])
precision_train
# Calculate recall
recall_train <- CM_train[2, 2] / sum(CM_train[2, ])
recall_train
# Calculate F1-score
f1_score_train <- 2 * (precision_train * recall_train) / (precision_train + recall_train)
f1_score_train

