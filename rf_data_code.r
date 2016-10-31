setwd("C:/Users/JW/Documents/Iowa papers")
#This code outputs information for each trial for each person into a row (long) or column (wide) format.  
require(ggplot2)
require(stargazer)
require(plyr)
require(stats)
library(dummy)
library(party)
#rfdata <- read.csv('C:/Users/JW/Documents/Iowa papers/rf_data_tops_indic.csv',header=T) #rf_dataset as constructed in SQL

rfdata <- read.csv('C:/Users/JW/Documents/Iowa papers/rf_data_tops_indic_nocounty.csv',header=T) #rf_dataset as constructed in SQL

set.seed(72812)
memory.limit(size = 24000)
rfdata <- rfdata[sample(1:nrow(rfdata), 500000, replace=FALSE),]

#choice_data$reduced_item_code <- as.factor(choice_data$reduced_item_code)
rfdata$month <- as.factor(rfdata$month)
rfdata$store_num <- as.factor(rfdata$store_num)

#party package
choice_data_cforest_test <- subset(choice_data,select = c(year,month,volume_liters, square_footage_whole, state_cost))
cforest_test <- cforest(volume_liters ~ ., data = choice_data_cforest_test, 
                        controls = cforest_control(ntree = 50, trace = TRUE, mtry = 2))
varimp(cforest_test)
table(choice_data_cforest_test$volume_liters, predict(cforest_test, type = "response"))

#randomForest package
library(randomForest)
#store_num_dum <- model.matrix(as.numeric(choice_data$store_num) ~ choice_data$store_num)
rfdata <- subset(rfdata, select = -store_num)
rforest_test <- randomForest(volume_liters ~ ., data = rfdata, ntree = 200, importance = TRUE, do.trace = TRUE)
plot(rforest_test)
varImpPlot(rforest_test)
save(rforest_test,file = "rforest_test.Rdata")
