library(randomForest)
library(ggplot2)
library(dplyr)

#Before tuning
rf_model <- randomForest(PM_Dongsihuan ~ ., data=train_data, importance=TRUE)

rsq <- function(x,y) cor(x,y)^2
predictions <- predict(rf_model, newdata=test_data)
RMSE(predictions, test_data$PM_Dongsihuan)
plot(predictions, test_data$PM_Dongsihuan) 
rsq(predictions, test_data$PM_Dongsihuan)


#After tuning
tuneRF(x=train_data[,-17], y=train_data[,17], ntreeTry=500, stepFactor=2, improve=0.2, trace=TRUE, plot=TRUE, doBest=FALSE)


#Best mtry = 10 ,OOBError lowest then predict
rf_model_final <- randomForest(PM_Dongsihuan ~ ., data=train_data, importance=TRUE, 
mtry= 10, ntree=500, stepFactor=2, improve=0.2)

predictions_final <- predict(rf_model_final, newdata=test_data)
RMSE(predictions_final, test_data$PM_Dongsihuan)
plot(predictions_final, test_data$PM_Dongsihuan)
rsq(predictions, test_data$PM_Dongsihuan)

# Print feature importance
varImpPlot(rf_model_final, type = 2, main = "Random Forest Feature Importance")

print(ggplot(test_data, aes(x = predictions_final, y = PM_Dongsihuan)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE))


