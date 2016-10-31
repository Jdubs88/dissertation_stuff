setwd("C:/Users/JW/Documents/Iowa papers")
library(randomForest)


load("rforest_test.RData")

rfdata <- read.csv('C:/Users/JW/Documents/Iowa papers/rf_data_tops_indic_nocounty.csv',header=T) #rf_dataset as constructed in SQL
rfdata$month <- as.factor(rfdata$month)
rfdata$store_num <- as.factor(rfdata$store_num)
varImpPlot(rforest_test)
plot(rforest_test)
predictions <- predict(rforest_test,rfdata)
rfdata <- cbind(rfdata, predictions)
rfdata$error <- rfdata$volume_liters - rfdata$predictions
rfdata2 <- rfdata
rfdata2$state_cost <- rfdata$state_cost - (.01 * rfdata$state_cost)
rfdata2$predictions <- predict(rforest_test, rfdata2)
rfdata$elasticity_volume <- rfdata2$predictions
elasticity <- 100 * sum(rfdata$predictions - rfdata$elasticity_volume)/sum(rfdata$predictions)
