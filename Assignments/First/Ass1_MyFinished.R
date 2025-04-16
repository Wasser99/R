library(tidyverse)
library(psych)
airbnb <- read.csv("airbnb.csv", sep =",", header = TRUE)

## 1.Task
glimpse(airbnb)
str(airbnb) # Types of variables in the initial data set: character (room_type), double (rest of the variables)

# Check for missing data
colSums(is.na(airbnb)) # 0 missing variables

names(airbnb) # Check the current structure and correct index positions of the columns
# Remove variables that are not important
airbnb <- airbnb[,-c(1, 4, 5, 15, 17)] # X, room_shared, room_private, attr_index, rest_index

# Convert categorical variables into factors
airbnb$room_type <- as.factor(airbnb$room_type)
airbnb$host_is_superhost <- as.factor(airbnb$host_is_superhost)
airbnb$weekend <- as.factor(airbnb$weekend)
airbnb$biz <- as.factor(airbnb$biz)
airbnb$multi <- as.factor(airbnb$multi)
str(airbnb) #Check if the conversion worked well


## 2.Task
summary(airbnb)
str(airbnb)

# Check which columns are numeric (TRUE/FALSE for each column)
numeric_check <- sapply(airbnb, is.numeric)

# Print the result to see TRUE/FALSE for each column
print(numeric_check)

# Select only numeric columns from the dataset
numeric_columns <- airbnb[sapply(airbnb, is.numeric)]

# Print the first few rows of the numeric columns to verify
head(numeric_columns)

# Calculate the correlation matrix for the numeric columns
numeric_corellation <- cor(numeric_columns)
# Print the correlation matrix in the console
numeric_corellation 
# Viewer window to see the correlation matrix better
View(numeric_corellation)


# Extract correlations of 'realSum' with other variables
realSum_cor <- numeric_corellation[, "realSum"]
print(realSum_cor)
barplot(realSum_cor[-which(names(realSum_cor) == "realSum")], 
        main = "Correlation of realSum with Numeric Predictors",
        xlab = "Variables", ylab = "Correlation", col = "red", las = 2)


# eliminate not relevant: cleanliness rating and satisfaction overall
airbnb <- airbnb[,-c(7,8)] 

########################################################################################

# Conclusion: The variables that seem to be related to realSum are: 
# person_capacity, bedrooms, dist, metro_dist, attr_index_norm, rest_index_norm, lng, lat

#########################################################################################

## 3. Task

# Fit a linear regression model with all predictors
lm_model <- lm(realSum ~ ., data = airbnb)

# Display summary of the model to see significant variables
summary(lm_model)

###################################################################################################################

#Conclusion: 

# Based on the regression analysis, the variables that have a significant (p-value < 0.005) impact on the price are:
# person_capacity, bedrooms and dist
# The variance in the price is not explained well through this model because of the low R-squared value

###################################################################################################################

## 4. Task

# Perform stepwise selection using backward elimination
stepwise_model <- step(lm_model, direction = "backward")

# Show summary of the final model
summary(stepwise_model)

##############################################################################################################################

# Conclusion: Looking at the summary we can see th impact of each variable on the price(realSum):

# person_capacity: positive impact -> for each additional person that can stay, the price increases by approx. EUR 24,47
# This meets the expectation, because larger accommodations usually charge more to serve more guests

# multi1: negative impact -> each multi-listing host lowers the price by approx. EUR 31.42            
# This does not meet the expectation, because you would think that experienced hosts might charge more

# bedrooms: positive -> each additional bedroom increases the price by approx. EUR 30.60
# Yes, one can expect that more bedrooms can mean an increase in the price, since it offers more capacity and comfort

# dist: negative -> each extra unit of distance from the city center, decreases the price by approx. EUR 13.10
# This is what we expected intuitively, since guests usually prefer to stay near touristic attractions and central areas.

# attr_index_norm: positive -> a one-unit increase in the normalized attraction index will increase the price by approx. EUR 2.3
# This is as expected, since accommodations near touristic attractions tend to have higher prices

# Interpretation of room_type, dist and weekend:
# room_type and weekend are not included in the final model, meaning that they do not significantly impact the price
# on the other hand 'dist' has a significant negative impact on the price, confirming the intuition that accommodations far away from the city center are lower in price

###############################################################################################################################

## 5. Task

AIC(lm_model, stepwise_model)


## 6. Task

# Set a seed for reproducibility
set.seed(123)

# Create a training index using a random sample of row indices (80% of data)
train_index <- sample(1:nrow(airbnb), size = 0.8 * nrow(airbnb), replace = FALSE)

# Split the data into training and testing sets
train_data <- airbnb[train_index, ]
test_data <- airbnb[-train_index, ]

# Fit full model on the training data
lm_trainFull <- lm(realSum ~ ., data = train_data)


summary(lm_trainFull)

# Perform stepwise selection on the full model
stepwise_train <- step(lm_trainFull, direction = "backward")

# Display the summary of the stepwise model
summary(stepwise_train)

# Make predictions using the full model
pred_full_train <- predict(lm_trainFull, newdata = train_data)
pred_full_test <- predict(lm_trainFull, newdata = test_data)

# Make predictions using the stepwise model
pred_step_train <- predict(stepwise_train, newdata = train_data)
pred_step_test <- predict(stepwise_train, newdata = test_data)

# Calculate the MSE for the full model
mse_full_train <- mean((train_data$realSum - pred_full_train)^2)
mse_full_test <- mean((test_data$realSum - pred_full_test)^2)

# Calculate the MSE for the stepwise model
mse_step_train <- mean((train_data$realSum - pred_step_train)^2)
mse_step_test <- mean((test_data$realSum - pred_step_test)^2)

# Print the MSE values
cat("Full Model - Training MSE: ", mse_full_train, "\n")
cat("Full Model - Test MSE: ", mse_full_test, "\n")
cat("Stepwise Model - Training MSE: ", mse_step_train, "\n")
cat("Stepwise Model - Test MSE: ", mse_step_test, "\n")

#The full model has a lower test MSE, so it would be the preferred model based on the given data.
