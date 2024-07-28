mdata <- read.csv(file.choose())

library(ggplot2)
library(plyr)
library(modeest)
library(visdat)
library(tidyverse)
library(factoextra)

# Check for duplicates
sum(duplicated(mdata))

# Check for missing values
colSums(is.na(mdata))

# Children
hist(mdata$Children, main = "Children (before imputation)")
mean(mdata$Children, na.rm = TRUE)
mdata$Children[is.na(mdata$Children)] <- median(mdata$Children, na.rm = TRUE)
mean(mdata$Children, na.rm = TRUE)
hist(mdata$Children, main = "Children (after imputation)")

# Age
hist(mdata$Age, main = "Age (before imputation)")
mean(mdata$Age, na.rm = TRUE)
mdata$Age[is.na(mdata$Age)] <- median(mdata$Age, na.rm = TRUE)
mean(mdata$Age, na.rm = TRUE)
hist(mdata$Age, main = "Age (after imputation)")

# Income
hist(mdata$Income, main = "Income (before imputation")
mean(mdata$Income, na.rm = TRUE)
mdata$Income[is.na(mdata$Income)] <- median(mdata$Income, na.rm = TRUE)
mean(mdata$Income, na.rm = TRUE)
hist(mdata$Income, main = "Income (after imputation)")

# Overweight
hist(mdata$Overweight, main = "Overweight (before imputation)")
mean(mdata$Overweight, na.rm = TRUE)
mdata$Overweight[is.na(mdata$Overweight)] <- median(mdata$Overweight, na.rm = TRUE)
mean(mdata$Overweight, na.rm = TRUE)
hist(mdata$Overweight, main = "Overweight (after imputation)")

# Anxiety
hist(mdata$Anxiety, main = "Anxiety (before imputation)")
mean(mdata$Anxiety, na.rm = TRUE)
mdata$Anxiety[is.na(mdata$Anxiety)] <- median(mdata$Anxiety, na.rm = TRUE)
mean(mdata$Anxiety, na.rm = TRUE)
hist(mdata$Anxiety, main = "Anxiety (after imputation)")

# Initial_days
hist(mdata$Initial_days, main = "Initial_days (before imputation)")
mean(mdata$Initial_days, na.rm = TRUE)
mdata$Initial_days[is.na(mdata$Initial_days)] <- median(mdata$Initial_days, na.rm = TRUE)
mean(mdata$Initial_days, na.rm = TRUE)
hist(mdata$Initial_days, main = "Initial_days (after imputation)")

# Soft_drink
mdata$Soft_drink[is.na(mdata$Soft_drink)] <- mfv(mdata$Soft_drink, na_rm = TRUE)
head(mdata$Soft_drink)
unique(mdata$Soft_drink)
Soft_drink.num <- revalue(x=mdata$Soft_drink, replace = c("No" = 0, "Yes" = 1))
mdata$Soft_drink <- as.numeric(Soft_drink.num)
hist(mdata$Soft_drink, main = "Soft_drink (after imputation)")

# Check for missing values
colSums(is.na(mdata))

# Ordinal Encoding

# Education
unique(mdata$Education)
education.num <- revalue(x=mdata$Education, replace = c("No Schooling Completed" = 0,
                                                          "Nursery School to 8th Grade" = 1,
                                                          "9th Grade to 12th Grade, No Diploma" = 3,
                                                          "GED or Alternative Credential" = 4,
                                                          "Regular High School Diploma" = 5,
                                                          "Some College, Less than 1 Year" = 6,
                                                          "Some College, 1 or More Years, No Degree" = 7,
                                                          "Professional School Degree" = 8,
                                                          "Associate's Degree" = 9,
                                                          "Bachelor's Degree" = 10,
                                                          "Master's Degree" = 11,
                                                          "Doctorate Degree" = 12))
mdata$Education <- as.numeric(education.num)
str(mdata$Education)

# ReAdmis
unique(mdata$ReAdmis)
ReAdmis.num <- revalue(x=mdata$ReAdmis, replace = c("No" = 0, "Yes" = 1))
mdata$ReAdmis <- as.numeric(ReAdmis.num)

# HighBlood
unique(mdata$HighBlood)
HighBlood.num <- revalue(x=mdata$HighBlood, replace = c("No" = 0, "Yes" = 1))
mdata$HighBlood <- as.numeric(HighBlood.num)

# Stroke
unique(mdata$Stroke)
Stroke.num <- revalue(x=mdata$Stroke, replace = c("No" = 0, "Yes" = 1))
mdata$Stroke <- as.numeric(Stroke.num)

# Overweight
unique(mdata$Overweight)
str(mdata$Overweight)

# Arthritis
unique(mdata$Arthritis)
Arthritis.num <- revalue(x=mdata$Arthritis, replace = c("No" = 0, "Yes" = 1))
mdata$Arthritis <- as.numeric(Arthritis.num)

# Diabetes
unique(mdata$Diabetes)
Diabetes.num <- revalue(x=mdata$Diabetes, replace = c("No" = 0, "Yes" = 1))
mdata$Diabetes <- as.numeric(Diabetes.num)

# Hyperlipidemia
unique(mdata$Hyperlipidemia)
Hyperlipidemia.num <- revalue(x=mdata$Hyperlipidemia, replace = c("No" = 0, "Yes" = 1))
mdata$Hyperlipidemia <- as.numeric(Hyperlipidemia.num)

# BackPain
unique(mdata$BackPain)
BackPain.num <- revalue(x=mdata$BackPain, replace = c("No" = 0, "Yes" = 1))
mdata$BackPain <- as.numeric(BackPain.num)

# Allergic_rhinitis
unique(mdata$Allergic_rhinitis)
Allergic_rhinitis.num <- revalue(x=mdata$Allergic_rhinitis, replace = c("No" = 0, "Yes" = 1))
mdata$Allergic_rhinitis <- as.numeric(Allergic_rhinitis.num)

# Reflux_esophagitis
unique(mdata$Reflux_esophagitis)
Reflux_esophagitis.num <- revalue(x=mdata$Reflux_esophagitis, replace = c("No" = 0, "Yes" = 1))
mdata$Reflux_esophagitis <- as.numeric(Reflux_esophagitis.num)

# Asthma
unique(mdata$Asthma)
Asthma.num <- revalue(x=mdata$Asthma, replace = c("No" = 0, "Yes" = 1))
mdata$Asthma <- as.numeric(Asthma.num)

# Boxplots for all quantitative variables
children_boxplot <- boxplot(mdata$Children, main = "Children")
age_boxplot <- boxplot(mdata$Age, main = "Age")
income_boxplot <- boxplot(mdata$Income, main = "Income")
vit_d_levels_boxplot <- boxplot(mdata$VitD_levels, main = "VitD_levels")
vit_d_supp_boxplot <- boxplot(mdata$VitD_supp, main = "VitD_Supp")
doc_visits_boxplot <- boxplot(mdata$Doc_visits, main = "Doc_visits")
full_meals_boxplot <- boxplot(mdata$Full_meals_eaten, main = "Full_meals_eaten")
initial_days_boxplot <- boxplot(mdata$Initial_days, main = "Initial_days")
total_charge_boxplot <- boxplot(mdata$TotalCharge, main = "TotalCharge")
additional_charge_boxplot <- boxplot(mdata$Additional_charges, main = "Additional_charges")

# Principle Component Analysis 
# Create dataframe
mdata_pca <- mdata[,c(16, 17, 20, 24:27, 43:45)]

# Normalize the data
mdata_pca <- prcomp(mdata[,c(16, 17, 20, 24:27, 43:45)], center = TRUE, scale = TRUE)

# run "mdata_pca$rotation" in console to get loadings 

# Selecting PCs using Kaiser Rule
fviz_eig(mdata_pca, choice = "eigenvalue", addlabels = TRUE)

# Extracting DataFrame
write.csv(mdata, "C:\\Users\\nahur\\Desktop\\WGU\\D206Clean.csv")
