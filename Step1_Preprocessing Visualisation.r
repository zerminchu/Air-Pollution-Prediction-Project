# Load the libraries
library(dummy)
library(Hmisc)
library(ggplot2)
library(GGally)
library(sp)
library(maptools)
library(reshape2)
library(rgeos)
library(caret)
library(corrplot)
library(ggcorrplot)
library(xtable)
# library(psych)
library(cluster)
library(scales)
library(RSNNS)
# library(e1071)
# library(tree)
# library(randomForest)
# library(arules)
# library(arulesViz)
#source('./coolBlueHotRed.R')

data <- read.csv("./BeijingPM20100101_20151231.csv")

# Drop unrelated columns
data[, c("No", "PM_Dongsi", "PM_Nongzhanguan", "PM_US.Post")] <- NULL

# Analyse the data and identify missing values
Hmisc::describe(data)

# combine the year, month, day columns into a single date column
data$date <- as.Date(paste(data$year, data$month, data$day, sep = "-"))

# Starting
# 1 is Fri
# 2 is Sat 3 is Sun
# 4 is Mon (weekday)
# We use the as.POSIXlt function to convert the date values to a list of date-time components,
# which includes the day of the week as an integer from 0 (Sunday) to 6 (Saturday).
# We use the weekdays function to get the day of the week as a character string and check if the day is a weekend using the %in% operator.
# If the day is a weekend, we assign the value "weekend" to the day_type variable using the ifelse function.
# Otherwise, we assign the value "weekday".
# create a new variable that indicates whether each day is a weekday or a weekend
data$day_type <- ifelse(as.POSIXlt(data$date)$wday %in% c(0, 6), "weekend", "weekday")

# Create new column for working hours, if within working hours will be yes, otherwise will be no
data$time_type <- ifelse(data$hour >= 8 & data$hour <= 17, "working hours", "non working hours")

# New column for corrtable (May have higher correlation with PM index)
# data$during_work_hours_on_weekday <- ifelse(data$day_type & data$during_work_hours, 1, 0)

# Drop unneeded variables
data[, c("year", "month", "day", "hour")] <- NULL

# Impute missing values, mean/median for numeric
# mode for categorical before one hot encode
# Imputing values
# classified_data[, sapply(classified_data, is.numeric)] <- Hmisc::impute(classified_data[, sapply(classified_data, is.numeric)], fun = mean) # nolint
# classified_data[, !sapply(classified_data, is.numeric)] <- apply(classified_data[, !sapply(classified_data, is.numeric)], 2, function(x) {ifelse(is.na(x), mode(x, na.rm = TRUE), x)}) # nolint
for (col in names(data[, !(colnames(data) %in% c("date"))])) {
  if (col == "PM_Dongsihuan") {
    next
  }
  if (is.numeric(data[[col]])) {
    data[[col]] <- Hmisc::impute(data[[col]], fun = mean)
  } else {
    data[[col]] <- Hmisc::impute(data[[col]])
  }
}

# Inspect PM_Dongsihuan
Hmisc::describe(data)

# apply encoding to data frame
# encoded_columns <- dummy::dummy(x = data[, !(colnames(data) %in% c("date", "cbwd", "day_type", "time_type"))], int = TRUE)

# Append encoded columns to dataframe
# encoded_data <- cbind(data, encoded_columns)
# encoded_data[, c("cbwd", "day_type", "time_type")] <- NULL

# Classify data into classified and unknown data.
# Where unknown data = NA values
classified_data <- subset(data, !is.na(data[, "PM_Dongsihuan"]))
unknown_data <- subset(data, is.na(data[, "PM_Dongsihuan"]) | data[, "PM_Dongsihuan"] == 0)

# Explore relationship between PM_Dongsihuan meteorological variables,the time/type of day, season and year
# generate a correlation table
correlation_table <- abs(cor(classified_data[, !(colnames(classified_data) %in% c("date", "cbwd", "day_type", "time_type"))]))
# corr table between features and class label
correlation_table <- abs(cor(classified_data[, !(colnames(classified_data) %in% c("date", "cbwd", "day_type", "time_type"))], y = classified_data$PM_Dongsihuan, use = "pairwise.complete.obs"))

# Order table by highest correlation and display top 5 and bottom 5 results
corTable <- correlation_table[order(correlation_table, decreasing = TRUE), , drop = FALSE]
View(corTable)

Hmisc::describe(classified_data[, !(colnames(classified_data) %in% c("date", "cbwd", "day_type", "time_type"))])

# Explain relationships, strong/weak positive/negative correlations

explain_relationships <- function(df, target_col = "PM_Dongsihuan") {
  correlation_table <- cor(df)
  correlations <- correlation_table[target_col, ]
  abs_correlations <- abs(correlations)
  sorted_correlations <- sort(abs_correlations, decreasing = TRUE)
  for (i in seq_along(sorted_correlations)) {
    var_name <- names(sorted_correlations)[i]
    correlation <- correlations[var_name]
    abs_correlation <- abs(correlation)
    if (var_name == target_col) next
    if (abs_correlation < 0.1) next
    if (correlation > 0) {
      strength <- if (abs_correlation > 0.5) "strong" else "weak"
      direction <- "positive"
    } else {
      strength <- if (abs_correlation > 0.5) "strong" else "weak"
      direction <- "negative"
    }
    message(paste0(var_name, " has a ", strength, " ", direction, " correlation with ", target_col))
  }
}

# correlation_table <- cor(classified_data, use = "pairwise.complete.obs")
# rownames(correlation_table) <- colnames(classified_data)
# colnames(correlation_table) <- colnames(classified_data)


# relationship_explanation <- explain_relationships(correlation_table, "PM_Dongsihuan")
# print(relationship_explanation)

lapply(classified_data, class)

cat_vars <- c("time_type", "day_type", "cbwd", "season")

# Box and Violin plots for categorical variables
for (var in cat_vars) {
  # loop through the categorical variables and create a plot for each one
  print(ggplot(classified_data, aes(x = factor(.data[[var]]), y = PM_Dongsihuan)) +
    geom_violin() + geom_boxplot(width=.1))
}

# Density plot by categorical variables
for (var in cat_vars) {
  print(ggplot(classified_data, aes(x = PM_Dongsihuan, fill = factor(.data[[var]]))) +
    geom_density(alpha = 0.5) +
    ggtitle(paste("Density of PM_Dongsihuan by ", var)))
}
library(lubridate)
library(dplyr)

# Assuming your date column is called "date", create year and month columns
classified_data <- classified_data %>%
  mutate(year = year(date), month = month(date))

print(day(classified_data$date))

# Aggregate by month
monthly_data <- classified_data %>%
  group_by(year, month) %>%
  summarise(mean_PM_Dongsihuan = mean(PM_Dongsihuan))

library(ggplot2)

ggplot(monthly_data, aes(x = ymd(paste0(year, "-", month, "-01")), y = mean_PM_Dongsihuan)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  labs(x = "Month", y = "Value", title = "Monthly Data") +
  ggtitle("Average PM_Dongsihuan by Month")


num_vars <- c("DEWP", "HUMI", "PRES", "TEMP", "Iws", "Iprec", "precipitation")
for (var in num_vars) {
  min_x <- min(classified_data[[var]])
  max_x <- max(classified_data[[var]])
  print(ggplot(classified_data, aes(x = .data[[var]], y = PM_Dongsihuan)) +
    geom_point() +
    xlim(min_x, max_x) +
    geom_smooth(method = "lm", se = FALSE))
}

# Correlation matrix for continuous features
install.packages("ggpubr")
library(ggplot2)
library(dplyr)
library(reshape2)
library(ggpubr)


# Select the relevant columns from the dataset
num_vars <- c("DEWP", "HUMI", "PRES", "TEMP", "Iws", "Iprec", "precipitation", "PM_Dongsihuan")
data_subset <- classified_data %>%
  select(num_vars)

# Calculate correlation matrix
corr_matrix <- cor(data_subset)

# Convert matrix to long format
corr_data <- reshape2::melt(corr_matrix)

# Plot heatmap
ggplot(corr_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "yellow") +
  #theme_minimal() +
  labs(x = "", y = "", title = "Correlation Heatmap")




# Create line plot for continuous features
library(ggplot2)
library(dplyr)

# Select the relevant columns from the dataset
num_vars <- c("DEWP", "HUMI", "PRES", "TEMP", "Iws", "Iprec", "precipitation", "PM_Dongsihuan")
data_subset <- classified_data %>%
  select(num_vars)

# Create a list of line plots for each continuous variable
plots_list <- lapply(num_vars, function(var) {
  ggplot(data_subset, aes_string(x = var, y = "PM_Dongsihuan")) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = var, y = "PM_Dongsihuan")
})

# Display each plot one at a time
for (plot in plots_list) {
  print(plot)
}

# colnames(classified_data)
# dim(classified_data)
# subset_data <- classified_data[, !(colnames(classified_data) %in% c("date", "cbwd", "day_type", "time_type"))]

# apply encoding to data frame
#encoded_columns <- dummy::dummy(x = classified_data, int = TRUE)

#View(encoded_columns)

# Append encoded columns to dataframe
#encoded_data <- cbind(classified_data, encoded_columns)

# Remove original columns that were encoded
#encoded_data <- select(encoded_data, -cbwd, -day_type, -time_type)

#View(encoded_data)

# Scales data
# Select only encoded columns to scale
#cols_to_scale <- select(encoded_data, starts_with("col_"))

# Scale encoded columns
#scaled_cols <- as.data.frame(scale(cols_to_scale))

# Add unscaled columns and target variable to scaled data
#scaled_data <- cbind(classified_data[, !(names(classified_data) %in% names(cols_to_scale))], scaled_cols, PM_Dongsihuan = classified_data$PM_Dongsihuan)

#View(scaled_data)





