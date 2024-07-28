data <- read.csv("C:\\Users\\nahur\\Desktop\\WGU\\D214 Capstone\\ECommerce\\E-commerce Dataset.csv", header = TRUE)

library(dplyr)
library(tidyr)
library(plyr)

# Check for missing values
colSums(is.na(data))
data <- na.omit(data)

# Check for duplicates
sum(duplicated(data))

unique(data$Gender)
unique(data$Device_Type)
unique(data$Product_Category)
unique(data$Order_Priority)

# See where the empty strings are
empty_count <- sum(data$Order_Priority == "")
print(empty_count)
empty_order_priority <- data[data$Order_Priority == "", ]
print(empty_order_priority)

# Remove empty rows for Order_Priority 
data <- data[data$Order_Priority != "",]

data$Order_Priority <- factor(data$Order_Priority, levels = c("Low", "Medium", "High", "Critical"))

# Total Sales
average_sales <- mean(data$Sales)
hist(data$Sales, main = "Frequency of Sale Prices", col = "azure3", xlab = "Sales in Dollars")
abline(v = average_sales, col = "red")

# Gender Count
Gender_count <- table(data$Gender)
barplot(Gender_count, horiz = TRUE, main = "Count of Customers by Gender", names.arg = c("Male", "Female"), col = c("blue", "pink"))

# Device Type
device_count <- table(data$Device_Type)
barplot(device_count, horiz = TRUE, main = "Device Type Used", col = c("maroon", "navy"))

# Order Priority
order_priority_count <- table(data$Order_Priority)
barplot(order_priority_count, horiz = TRUE, main = "Count of Order Priority",
        col = c("lightgreen", "yellow", "orange", "red"))

# Product Category
avg_sales_product_category <- aggregate(Sales ~ Product_Category, data = data, FUN = mean)
barplot(avg_sales_product_category$Sales, names.arg = avg_sales_product_category$Product_Category, 
        main = "Average Transaction Price by Product Category", 
        xlab = "Product Category", ylab = "Transaction Price", 
        col = c("red", "blue", "purple", "green"))

# Order Priority and Product Category 
cont_table <- table(data$Order_Priority, data$Product_Category)

barplot(cont_table, beside = TRUE, legend.text = TRUE,
        main = "Order Priority by Product Category",
        xlab = "Order Priority", ylab = "Count",
        col = c("lightgreen", "yellow", "orange", "red"),
        args.legend = list(x = "topleft", bty = "n"))

# Top 10 selling products broken down by gender 
product_gender_freq <- table(data$Product, data$Gender)
sorted_product_freq <- sort(rowSums(product_gender_freq), decreasing = TRUE)
top_10_products <- names(sorted_product_freq)[1:10]
gender_breakdown <- product_gender_freq[top_10_products, ]
print(gender_breakdown)

# Device Type and Product Category
device_product_table <- table(data$Device_Type, data$Product_Category)

barplot(device_product_table, beside = TRUE, 
        main = "Product Category Distribution by Device Type",
        xlab = "Device Type", ylab = "Frequency",
        legend.text = TRUE, col = c("maroon", "navy"),
        args.legend = list(x = "topright", bty = "n"))

# ANOVA
anova_result <- aov(Sales ~ Device_Type + Gender + Product_Category + Product + Order_Priority, data = data)
summary(anova_result)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# Residuals vs Fitted Values
residuals <- resid(anova_result)
fitted_values <- fitted(anova_result)

plot(fitted_values, residuals, 
     xlab = "Fitted Values", 
     ylab = "Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")

# QQ Plot
qqnorm(residuals)
qqline(residuals)




