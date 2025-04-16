## 1. Task ##################################################################################
library(caret)
library(e1071)
# read in the data and have a first look at the structure
bankchurners <- read.csv("BankChurners-1.csv", sep =",", header = TRUE)
head(bankchurners)
str(bankchurners)
summary(bankchurners)

# check for missing data
colSums(is.na(bankchurners))
names(bankchurners)

# remove three unnecassary variables
bankchurners <- bankchurners[, -c(1,22,23)]
str(bankchurners)

# transform categorical variables into factors
bankchurners$Attrition_Flag <- factor(bankchurners$Attrition_Flag)
bankchurners$Gender <- factor(bankchurners$Gender)
bankchurners$Education_Level <- factor(bankchurners$Education_Level)
bankchurners$Marital_Status <- factor(bankchurners$Marital_Status)
bankchurners$Income_Category <- factor(bankchurners$Income_Category)
bankchurners$Card_Category <- factor(bankchurners$Card_Category)
str(bankchurners)

# take a look at the order of the levels
levels(bankchurners$Attrition_Flag)
levels(bankchurners$Gender)
levels(bankchurners$Education_Level)
levels(bankchurners$Marital_Status)
levels(bankchurners$Income_Category)
levels(bankchurners$Card_Category)

# order the levels in a more intuitive way
bankchurners$Attrition_Flag <- factor(bankchurners$Attrition_Flag, 
                                      levels = c("Existing Customer", 
                                                 "Attrited Customer"))

bankchurners$Education_Level <- factor(bankchurners$Education_Level, 
                                       levels = c("Uneducated", "High School", 
                                                  "College", "Graduate", 
                                                  "Post-Graduate", 
                                                  "Doctorate", "Unknown"))

bankchurners$Marital_Status <- factor(bankchurners$Marital_Status, 
                                      levels = c("Single", "Married", 
                                                 "Divorced", "Unknown"))

bankchurners$Income_Category <- factor(bankchurners$Income_Category, 
                                       levels = c("Less than $40K", 
                                                  "$40K - $60K", 
                                                  "$60K - $80K", 
                                                  "$80K - $120K", 
                                                  "$120K +", "Unknown"))

bankchurners$Card_Category <- factor(bankchurners$Card_Category, 
                                     levels = c("Blue", "Silver", 
                                                "Gold", "Platinum"))

str(bankchurners)

# take a look at the summary
summary(bankchurners)

# comments:
summary(bankchurners$Customer_Age)
# judging by the closeness of the mean and median, the customer age seems 
# to be a very symmetric distribution

summary(bankchurners$Income_Category)
# most customers do not exceed an annual income of $40K

summary(bankchurners$Total_Trans_Amt)
# the total transaction amount is mainly around $4K, however there is at least 
# one outlier whose transaction amount exceeded $18K 

# calculate the percentage of customers who left the bank
t <- table(bankchurners$Attrition_Flag)
churned_cust <- (t["Attrited Customer"] / (t["Existing Customer"] + t["Attrited Customer"]))
cat("Percentage of churned customers:", churned_cust * 100, "%") # ~ 16.1 %

# visualizations:
# plot the realtionship between total transaction amount and credit limit 
plot(Total_Trans_Amt ~ Credit_Limit,
     col= as.numeric(Attrition_Flag),
     data = bankchurners,
     xlab = "Credit Limit",
     ylab = "Total Transaction Amount")
legend("topleft", title = "Left Bank", pch = 1, 
       col = c(1, 2), 
       legend= c("No", "Yes"))

# it looks like people with a lower transaction amount are more likely to churn


# plot the realtionship between attrition flag and gender 
plot(bankchurners$Attrition_Flag, bankchurners$Gender,
     main = "Customer Status ~ Gender",
     xlab = "Customer Status",
     ylab = "Gender")
# it looks like women are a little bit more likely to leave the bank

# plot the relationship between churn and total transaction amount (cut off some outliers with ylab)
plot(bankchurners$Attrition_Flag, bankchurners$Total_Trans_Amt,
     ylim= c(0,10000),
     main= "Customer Status ~ Transaction Amount",
     xlab= "Customer Status",
     ylab= "Transaction Amount")

# according to this boxplot, it seems that customers are more likely to leave the bank
# when the transaction amount is lower

# plot the relationship between churn and age
plot(bankchurners$Attrition_Flag, bankchurners$Customer_Age, 
     main= "Customer Status ~ Age",
     xlab= "Customer Status",
     ylab= "Age")

# there seems to be no relation between the two variables

# plot the relationship between churn and education level
plot(bankchurners$Attrition_Flag, bankchurners$Education_Level,
     main= "Customer Status ~ Education",
     xlab= "Customer Status",
     ylab= "Education")

# this plot shows also no significant relationship 
# plot the relationship between chrun and Contact Count
plot(bankchurners$Attrition_Flag, bankchurners$Contacts_Count_12_mon,
     main= "Customer Status ~ Contact Count",
     xlab= "Customer Status",
     ylab= "Contact Count")
# this might show some relation, so that people who had more frequent contact ultimately left the bank 

## 2. Task ##################################################################################################
# Set a seed for reproducibility
set.seed(789)

# Create a training index using a random sample of row indices (80% of data)
train_index <- sample(1:nrow(bankchurners), size = 0.8 * nrow(bankchurners), replace = FALSE)

# Split the data into training and testing sets
training_set <- bankchurners[train_index, ]
test_set <- bankchurners[-train_index, ]

## 3. Task ##################################################################################################
fit_train <- glm(Attrition_Flag ~ .,data = training_set, family = binomial())

fit_train
summary(fit_train)

# the variables which have a significant impact on churn are: GenderM, Dependent_count, Marital_StatusMarried,
# Income_Category$80K-$120K, Income_Category$120K+, Card_CategorySilver, Card_CategoryGold, Total_Relationship_count,
# Months_Inactive_12_mon, Contacts_Count_12_mon, Credit_Limit, Total_Revolving_Bal,Total_Amt_Chng_Q4_Q1,
# Total_Trans_Amt, Total_Trans_Ct, Total_Ct_Chng_Q4_Q1

## 4. Task #################################################################################################
step_train <- step(fit_train, direction = "backward")

step_train
summary(step_train)

# Question: Which variables remain in the final model?
# Answer: A total of 15 variables:
# Customer_Age, Gender, Dependent_count, Marital_Status (Married, Divorced, Unknown), 
# Income_Category ($40K - $60K, $60K - $80K, $80K - $120K, $120K +, Unknown), 
# Card_Category (Silver, Gold, Platinum), Total_Relationship_Count, Months_Inactive_12_mon, 
# Contacts_Count_12_mon, Credit_Limit, Total_Revolving_Bal, Total_Amt_Chng_Q4_Q1, 
# Total_Trans_Amt, Total_Trans_Ct, Total_Ct_Chng_Q4_Q1
 
# Question: How much do the odds for churn change if a customer is contacted once more?
odds_change_0 <- exp(0.5151075) 
cat("If a customer is contacted once more, the odds for churn increase by", odds_change_0,"which is",(odds_change_0 - 1) * 100,"%!")

# Question: How much do the odds for churn change if a customer has a Platinum card instead of a blue credit card?
odds_change_1 <- exp(0.9424723)
odds_change_1

cat("The odds of churn for a customer with a Platinum card are approximately", odds_change_1, "times the odds for a customer with a blue credit card!")

# Question: How much do the odds for churn change if the credit limit of the custumer is increased by 1000 Dollars?
tmp <- exp(-0.0000160 * 1000)
odds_change_2 <- ((1 - tmp) * 100)

cat("The odds of churn decrease by",odds_change_2,"% if the credit limit of a customer is increased by $1000!" )

## 5. Task ###############################################################################################
# control for cross-validation
control <- trainControl(method = "cv", number = 5)

# fit the k-NN model with different values of k (3, 5, 7, 9, 11, 13, 15)

# possibility 2 with all variables from the training set:
knn_model <- train(Attrition_Flag ~ .,
                   data = training_set,
                   method = "knn",
                   trControl = control,
                   preProc = c("scale"),
                   tuneGrid = expand.grid(k = seq(3, 15, by = 2)))

knn_model

## 6. Task ###############################################################################################


nb_model <- train(Attrition_Flag ~ .,
                   data = training_set,
                   method = "naive_bayes",
                   trControl = control)
nb_model


# values suggest that the model performs better for predicting the "Existing Customer" class 
#than the "Attrited Customer" class.


## 7. Task ##############################################################################################


#----- GLM 


# logistic regression predictions
p <- predict(fit_train, newdata = test_set, type = "response")

# Convert 0 and 1 n Y achse to predicted classes
yhat_glm <- ifelse(p > 0.5, "Attrited Customer", "Existing Customer")

yhat_glm <- factor(yhat_glm, levels = levels(test_set$Attrition_Flag))

#caret confusion matrix
CM_glm <- confusionMatrix(yhat_glm, test_set$Attrition_Flag)

# Create confusion matrix manually
tab_glm <- table(predicted = yhat_glm, observed = test_set$Attrition_Flag)
tab_glm

acc_glm = (tab_glm[1,1] + tab_glm[2,2])/sum(tab_glm) # accuracy
acc_glm
rec_glm = tab_glm[2,2]/sum(tab_glm[,2]) # specificity
rec_glm
pre_glm = tab_glm[2,2]/sum(tab_glm[2,]) # neg pred value
pre_glm


#----- STEPWISE

#stepwise prediction
p <- predict(step_train, newdata = test_set, type = "response")

# Convert 0 and 1 n Y achse to predicted classes
yhat_step <- ifelse(p > 0.5, "Attrited Customer", "Existing Customer")

yhat_step <- factor(yhat_step, levels = levels(test_set$Attrition_Flag))

#caret confusion matrix
CM_step <- confusionMatrix(yhat_step, test_set$Attrition_Flag)

# Create confusion matrix manually
tab_step <- table(predicted = yhat_step, observed = test_set$Attrition_Flag)
tab_step

acc_step = (tab_step[1,1] + tab_step[2,2])/sum(tab_step) # accuracy
acc_step
rec_step = tab_step[2,2]/sum(tab_step[,2]) # specificity
rec_step
pre_step = tab_step[2,2]/sum(tab_step[2,]) # neg pred value
pre_step

#----- KNN 

#knn prediction
p_knn <- predict(knn_model, newdata = test_set)

#caret confusion matrix
CM_knn <- confusionMatrix(p_knn, test_set$Attrition_Flag)

# Convert 0 and 1 n Y achse to predicted classes
#yhat_knn <- ifelse(p > 0.5, "Attrited Customer", "Existing Customer")

#yhat_knn <- factor(yhat_knn, levels = levels(test_set$Attrition_Flag))

# Create confusion matrix manually
tab_knn <- table(predicted = p_knn, observed = test_set$Attrition_Flag)
tab_knn

acc_knn = (tab_knn[1,1] + tab_knn[2,2])/sum(tab_knn) # accuracy
acc_knn
rec_knn = tab_knn[2,2]/sum(tab_knn[,2]) # specificity
rec_knn
pre_knn = tab_knn[2,2]/sum(tab_knn[2,]) # neg pred value
pre_knn

#----- NAIVE BAYES

#naive Bayes prediction
p_nb <- predict(nb_model, newdata = test_set)

#caret confusion matrix
CM_nb <- confusionMatrix(p_nb, test_set$Attrition_Flag)

# Create confusion matrix manually
tab_nb <- table(predicted = p_nb, observed = test_set$Attrition_Flag)
tab_nb

acc_nb = (tab_nb[1,1] + tab_nb[2,2])/sum(tab_nb) # accuracy
acc_nb
rec_nb = tab_nb[2,2]/sum(tab_nb[,2]) # specificity
rec_nb
pre_nb = tab_nb[2,2]/sum(tab_nb[2,]) # neg pred value
pre_nb

cat("Accuracy:: GLM:", acc_glm, "| Stepwise Model:", acc_step,"| KNN:", acc_knn,"| Naive Bayes:",acc_nb) # winner Stepwise
cat("Recall:: GLM:", rec_glm, "| Stepwise Model:", rec_step,"| KNN:", rec_knn,"| Naive Bayes:",rec_nb) # winner Naivebayes, 2. stepwise(-0.02)
cat("Precision:: GLM:", pre_glm, "| Stepwise Model:", pre_step,"| KNN:", pre_knn,"| Naive Bayes:",pre_nb) # winner Stepwise

cat("The Stepwise modell performed best in accuray and precision, in recall naive Bayes outperformed it, but only with 0.02.")

