library(dplyr)
library(tidyr)
library(caret)
library(forcats)

# Copy the data, remove date due to high cardinality
data_copy <- data.frame(classified_data)
data_copy <- data_copy %>% select(-date)

# Apply one hot encoding to categorical values
data_copy$season <- factor(data_copy$season)
encoded_columns <- dummy::dummy(x = data_copy[, !(colnames(data_copy) %in% c("PM_Dongsihuan"))], int = TRUE)
View(encoded_columns)

#Remove pre-encoded columns
data_copy <- cbind(data_copy, encoded_columns)
data_copy[, c("cbwd", "day_type", "time_type","season")] <- NULL

# Remove redundant columns generated after one-hot encoding
data_copy_encoded <- data_copy %>%
  # Remove variables that are completely collinear with other variables
  # Avoid singularity errors when evaluating linear regression
  select(-day_type_weekend) %>%
  select(-time_type_non.working.hours) %>%    
  select(-cbwd_SE) %>%
  select(-season_4)

# Split data into training and validation sets
set.seed(123)
train_indices <- createDataPartition(data_copy$PM_Dongsihuan, p = 0.7, list = FALSE)
train_data <- data_copy_encoded[train_indices, ]
test_data <- data_copy_encoded[-train_indices, ]

# Train a linear regression model
model <- caret::train(PM_Dongsihuan ~ ., data = train_data, preProcess=c("center", "scale"), method = "lm")

summary(model)

# Make predictions on testing set
predictions <- predict(model, newdata = test_data)
test_data$predictions <- predictions

rsq <- function (x, y) cor(x, y) ^ 2

# Evaluate performance using MSE, RMSE and R Squared metrics
mse <- mean((test_data$predictions - test_data$PM_Dongsihuan)^2)
rmse <- sqrt(mse)
rsquared <- rsq(test_data$predictions, test_data$PM_Dongsihuan)


#RMSE, RSquared evaluation

model
cat("MSE: ", paste0(mse), "\n")
cat("RMSE: ", paste0(rmse), "\n")
cat("R Squared: ", paste0(rsquared), "\n")
cat("PM_Dongsihuan range: ", paste0(range(test_data$PM_Dongsihuan)), "\n")

#Graph
# Create a list of line plots for each continuous variable
print(ggplot(test_data, aes(x = PM_Dongsihuan, y = predictions)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE))

# Tuning by performing PCA (Principal Component Analysis) and Cross Validation

# Conduct PCA and Visualise results
train_data_PCA <- train_data %>%
                  select(-c(PM_Dongsihuan)) %>%
                  scale() %>%
                  as.data.frame()
pca_obj <- prcomp(train_data_PCA, scale = TRUE, center = TRUE, retx = T)

# Summary
summary(pca_obj)
pca_obj
var_explained_df <- data.frame(PC= paste0("PC",1:15),
                               var_explained=(pca_obj$sdev)^2/sum((pca_obj$sdev)^2))
p <- var_explained_df %>%
  ggplot(aes(x=reorder(PC, -var_explained),y=var_explained, group=1))+
  geom_point(size=4)+
  geom_line()+
  labs(title="Scree plot: PCA on scaled data")
p + aes(x = fct_inorder(PC))
p


# Retrain model with Cross validation and PCA
regressControl  <- trainControl(method="repeatedcv",
                                search="grid",
                                number = 5,
                                repeats = 5)  
regress         <- caret::train(PM_Dongsihuan ~ .,
                                data = train_data,
                                method  = "lm",
                                metric="RMSE",
                                trControl = regressControl,
                                preProcess=c("center", "scale", "pca"),
                                tuneGrid = expand.grid(intercept = TRUE))
summary(regress)

# Make predictions on testing set
predictions <- predict(regress, newdata = test_data)
test_data$predictions <- predictions

# Evaluate performance using MSE and RMSE metrics
mse <- mean((test_data$predictions - test_data$PM_Dongsihuan)^2)
rmse <- sqrt(mse)
rsquared <- rsq(test_data$predictions, test_data$PM_Dongsihuan)

#RMSE, RSquared evaluation
cat("MSE: ", paste0(mse), "\n")
cat("RMSE: ", paste0(rmse), "\n")
cat("R Squared: ", paste0(rsquared), "\n")
cat("PM_Dongsihuan range: ", paste0(range(test_data$PM_Dongsihuan)), "\n")
regress


#Graph
# Create a list of line plots for each continuous variable
print(ggplot(test_data, aes(x = PM_Dongsihuan, y = predictions)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE))


# Retrain model with Cross validation only
regressControl  <- trainControl(method="repeatedcv",
                                search="grid",
                                number = 5,
                                repeats = 5)  
regress         <- caret::train(PM_Dongsihuan ~ .,
                                data = train_data,
                                method  = "lm",
                                metric="RMSE",
                                trControl = regressControl,
                                preProcess=c("center", "scale"),
                                tuneGrid = expand.grid(intercept = TRUE))
summary(regress)

# Make predictions on testing set
predictions <- predict(regress, newdata = test_data)
test_data$predictions <- predictions

# Evaluate performance using MSE and RMSE metrics
mse <- mean((test_data$predictions - test_data$PM_Dongsihuan)^2)
rmse <- sqrt(mse)
rsquared <- rsq(test_data$predictions, test_data$PM_Dongsihuan)

#RMSE, RSquared evaluation
cat("MSE: ", paste0(mse), "\n")
cat("RMSE: ", paste0(rmse), "\n")
cat("R Squared: ", paste0(rsquared), "\n")
cat("PM_Dongsihuan range: ", paste0(range(test_data$PM_Dongsihuan)), "\n")
regress


#Graph
# Create a list of line plots for each continuous variable
print(ggplot(test_data, aes(x = PM_Dongsihuan, y = predictions)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE))