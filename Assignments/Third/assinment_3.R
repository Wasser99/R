library(ggplot2)
library(pdp)
library(gridExtra)
library(rpart)
library(partykit)
library(tidyr)
library(dplyr)
library(ranger)

library(MASS)  # for linear regression


# Load your data
df <- read.csv("housing.csv", header = TRUE, sep = ",")
str(df)
View(df)
df$ocean_proximity <- as.factor(df$ocean_proximity)

# Define a function to plot marginal distributions using density plots
plot_marginal_distributions <- function(df, title, remove_missing = FALSE) {
  # If remove_missing is TRUE, drop rows with any missing values
  if (remove_missing) {
    df <- df %>% drop_na()
  }
  
  # Reshape data to long format for plotting, removing NAs per feature if remove_missing is FALSE
  df_long <- df %>%
    select_if(is.numeric) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
    filter(!is.na(value))  # This only filters NAs for each individual feature
  
  # Plot using ggplot2
  ggplot(df_long, aes(x = value, fill = variable)) +
    geom_density(alpha = 0.7) +
    facet_wrap(~variable, scales = "free") +
    scale_fill_brewer(palette = "Set1") +  # Use a color palette for categorical data
    labs(title = title, x = "Values", y = "Density") +
    #theme_minimal() +
    theme(legend.position = "none")
}

# Plot before removing missing values (NAs are filtered on a per-feature basis)
plot_marginal_distributions(df, "Marginal Distributions of Features (Before Removing Missing Values)", remove_missing = FALSE)


# Plot after removing missing values (removes entire rows with any missing values)
plot_marginal_distributions(df, "Marginal Distributions of Features (After Removing Missing Values)", remove_missing = TRUE)

#df_clean <- df %>% drop_na()
df_clean <- na.omit(df)

str(df_clean)

# since only 207 variables are removed, the change does not have a visible effect on the visaulisation

#Task 2-------------------------------------------------------------------------

tree_model <- rpart(median_house_value ~ ., data = df_clean)
plot(as.party(tree_model))

printcp(tree_model)
plotcp(tree_model)



# This tree suggests that income and proximity to the ocean are primary drivers of housing prices in this dataset
# geographic location (longitude) playing a secondary role where 3.106 <= median income<5.035

# The Complexity Parameter supports this observation, it also shows that il levels out after split 7
#Variables actually used in tree construction:
#[1] longitude       median_income   ocean_proximity


#Task 3-------------------------------------------------------------------------
rf <- ranger(median_house_value ~.,
             data = df_clean,
             importance = "permutation")
rf
plot(as.table(importance(rf)),
     ylab = "Permutation importance")

str(rf)

importance_scores <- as.data.frame(rf$variable.importance)
importance_df <- tibble::rownames_to_column(importance_scores, "Feature")
colnames(importance_df)[2] <- "Importance"

# Plot feature importance using ggplot2
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +  # Flip coordinates for a horizontal bar plot
  labs(title = "Permutation Importance of Features in Random Forest",
       x = "Feature",
       y = "Permutation Importance") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )
# it shows different top feautres compared to trees
# median income, longitude, latitude



#---median income------

# Define a grid of values for median_income (from its minimum to maximum)
grd <- seq(min(df_clean$median_income), max(df_clean$median_income), length.out = 20)

# Sample 300 rows from the data to speed up computation
nd <- df_clean[sample.int(nrow(df_clean), 300), ]

# Generate predictions over the grid of median_income values
prs <- lapply(grd, function(val) {
  nd$median_income <- val  # Set median_income to the current grid value
  predict(rf, data = nd, type = "response")$predictions
})

# Convert results to a data frame for plotting
prs_df <- data.frame(median_income = rep(grd, each = 300),
                     predicted_house_value = unlist(prs))

# Plot using ggplot2

ggplot(prs_df, aes(x = median_income, y = predicted_house_value)) +
  geom_line(stat = "summary", fun = mean, color = "red") +
  labs(title = "Partial Dependence of Median House Value on Median Income",
       x = "Median Income",
       y = "Predicted Median House Value") +
  theme_minimal()

#And plot the results with matplot
matplot(grd, t(do.call("cbind", prs)), type = "l", col = rgb(.1, .1, .1, .1),
        lty = 1, log = "x", xlab = "Median Income", ylab = "Predicted Median housevalue")
#There is a visible uptrend in the predicted median house value, starting around 2.0 median income, going until 10 where it seems to level out


#---longitude-----

grd_long <- seq(min(df_clean$longitude), max(df_clean$longitude), length.out = 20)
nd <- df_clean[sample.int(nrow(df_clean), 300), ]

prs_long <- lapply(grd_long, function(val) {
  nd$longitude <- val  # Set longitude to the current grid value
  predict(rf, data = nd, type = "response")$predictions
})

prs_df <- data.frame(longitude = rep(grd_long, each = 300),
                     predicted_house_value = unlist(prs_long))

matplot(grd_long, t(do.call("cbind", prs_long)), type = "l", col = rgb(.1, .1, .1, .1),
        lty = 1, xlab = "Longitude", ylab = "Predicted Median housevalue")

# There is a small uncertanty around the -122 longitude level where it seems to have a reducing effect on the housevalue,
#but around the 400k level also some increasing.

# Same At the -118 longitude level but there is a more visible decreasment in the housevalue prediction, it seems to level out at -117


#---latitude-----

grd_lat <- seq(min(df_clean$latitude), max(df_clean$latitude), length.out = 20)
nd <- df_clean[sample.int(nrow(df_clean), 300), ]

prs_lat <- lapply(grd_lat, function(val) {
  nd$latitude <- val  # Set latitude to the current grid value
  predict(rf, data = nd, type = "response")$predictions
})

prs_df <- data.frame(latitude = rep(grd_lat, each = 300),
                     predicted_house_value = unlist(prs_lat))

matplot(grd_lat, t(do.call("cbind", prs_lat)), type = "l", col = rgb(.1, .1, .1, .1),
        lty = 1, xlab = "Latitude", ylab = "Predicted Median housevalue")



# There is a stronger uncertanty around the 34 lat, where we can see a double spike ending in a decreasement
#same but weaker around 38. It levels out at 40

# These observation mean that at these levels we can see some change in the median house value prediction 
# so the top 3 important features can influence the median house value prediction at these levels

#Task 4-------------------------------------------------------------------------

# Load necessary libraries


# Initialize parameters
n <- nrow(df_clean)  # Sample size
fold <- 10           # Number of folds
folds <- sample(rep(1:fold, length.out = n))  # Cross-validation folds

# Initialize list to store MSE for each model across folds
mse <- list(tree = numeric(fold), rf = numeric(fold), lm = numeric(fold))

# Cross-validation loop
for (tfold in seq_len(fold)) {
  # Split data into training and test sets
  train_idx <- which(folds != tfold)
  test_idx <- which(folds == tfold)
  
  # Fit models on the training set
  rf <- ranger(median_house_value ~ ., data = df_clean[train_idx, ], importance = "permutation")
  tree <- rpart(median_house_value ~ ., data = df_clean[train_idx, ], method = "anova")
  lm_model <- lm(median_house_value ~ ., data = df_clean[train_idx, ])
  
  # Make predictions on the test set
  p_rf <- predict(rf, data = df_clean[test_idx, ])$predictions
  p_tree <- predict(tree, newdata = df_clean[test_idx, ])
  p_lm <- predict(lm_model, newdata = df_clean[test_idx, ])
  
  # Calculate and store MSE for each model
  actual <- df_clean$median_house_value[test_idx]
  mse$rf[tfold] <- mean((actual - p_rf)^2)
  mse$tree[tfold] <- mean((actual - p_tree)^2)
  mse$lm[tfold] <- mean((actual - p_lm)^2)
}

# Combine results into a data frame for easy plotting
mse_df <- data.frame(Tree = mse$tree, `Random Forest` = mse$rf, `Linear Regression` = mse$lm)

# Plot the cross-validated MSE for each model
boxplot(mse_df, names = c("Tree", "Random Forest", "Linear Regression"),
        ylab = "Cross-validated Mean Squared Error",
        main = "Model Comparison: Cross-validated MSE")

# we use here cv MSE and not brier score because we have a regression task an not a classification 











