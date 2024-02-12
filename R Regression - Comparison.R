#Load the packages in
library(Metrics)
library(janitor)
library(tidyverse)
library(caret)
library(Rcmdr)
library(leaps)


#Save the data, 'complex_retail_sales_dataset' as data
data <- complex_retail_sales_dataset

#Save the dataset as data
set.seed(123)

#Check for null values
print(colSums(is.na(data)))

#Check for duplicated values
print(sum(duplicated(data)))

print(colnames(data))

#Create a list of columns that are going to be rounded to 2 decimal places
col_2_round <- c('PromotionSpend', 'CompetitorSpend', 'Sales')
col_1_round <- c('AvgTemperature')
col_0_round <- c('FootTraffic', 'OnlineTraffic')

#Perform the following rounding to each columns that were listed above
data[, col_2_round] <- round(data[, col_2_round], digits = 2)
data[, col_1_round] <- round(data[, col_1_round], digits = 1)
data[, col_0_round] <- round(data[, col_0_round], digits = 0)

#Create correlation matrix (using r commander)
#Exclude sales which is the output
cor(complex_retail_sales_dataset[,c("AvgTemperature","CompetitorSpend", 
                                    "FootTraffic","Holiday","OnlineTraffic",
                                    "PromotionSpend","Weekday")], 
                                    use="complete")

#Create a BIC graph
plot(regsubsets(Sales ~ AvgTemperature + CompetitorSpend + FootTraffic + 
Holiday + OnlineTraffic + PromotionSpend + Weekday, 
data=complex_retail_sales_dataset, nbest=1, nvmax=8), scale='bic')

#Create a linear model
lm(formula = Sales ~ AvgTemperature + CompetitorSpend + FootTraffic + 
     Holiday + OnlineTraffic + PromotionSpend + Weekday, data = complex_retail_sales_dataset)

#The finalised model is the following:
#210.58841 – 0.16613*CompetitorSpend + 0.31591*FootTraffic + 0.47014*OnlineTraffic + 1.52156*PromotionSpend

#Create a test and training split to test the effectivenss of the model
rand_sample <- createDataPartition(data$Sales, p = 0.50, list = FALSE)

#Create the training and testing sets
train_set <- data[rand_sample, ]
test_set <- data[-rand_sample, ]

#make the model 
model <- lm(Sales ~ PromotionSpend + CompetitorSpend + FootTraffic + OnlineTraffic, data = train_set)

#Generate the predictions
predictions <- predict(model, newdata = test_set)

#find the R^2 value
R2(predictions, test_set$Sales)

#find the MAE value
MAE(predictions, test_set$Sales)

# Calculate the mean and median of Sales in the training set
mean_sales <- mean(train_set$Sales)
median_sales <- median(train_set$Sales)

# Create vectors of mean and median predictions for the test set
mean_predictions <- rep(mean_sales, nrow(test_set))
median_predictions <- rep(median_sales, nrow(test_set))

# Calculate the MAE for the mean and median predictions
mae_mean <- MAE(mean_predictions, test_set$Sales)
mae_median <- MAE(median_predictions, test_set$Sales)

# Print the MAE for the mean and median predictions
print(paste("MAE for mean prediction:", mae_mean))
print(paste("MAE for median prediction:", mae_median))

#Use the k-fold method
train_control <- trainControl(method = "cv",   number = 10)

#implement the method
model <- train(Sales ~ PromotionSpend + CompetitorSpend + FootTraffic + OnlineTraffic, 
               data = train_set, method = "lm", trControl = train_control)

print(model)

