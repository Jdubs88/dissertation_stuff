setwd("C:/Users/JW/Documents/Iowa papers")
#This code outputs information for each trial for each person into a row (long) or column (wide) format.  
require(ggplot2)
require(stargazer)
#require(MASS)
#require(KernSmooth)
require(plyr)
require(stats)
#require(RSGHB)
#require(lubridate)
#require(randomForest)
#require(party)
library(dummy)
library(partykit)
available=read.csv('C:/Users/JW/Documents/Iowa papers/available_product_data.csv',header=T) #product availability
state_cost=read.csv('C:/Users/JW/Documents/Iowa papers/discrete_price_matrix_monthly_0001_filled_state.csv',header=T) #state cost
bottle_cost=read.csv('C:/Users/JW/Documents/Iowa papers/discrete_price_matrix_monthly_0001_filled.csv',header=T) #bottle cost
products=read.csv('C:/Users/JW/Documents/Iowa papers/discrete_data_0001_products_final.csv',header=T) #product information
choice_data=read.csv('C:/Users/JW/Documents/Iowa papers/discrete_data_0001_month_long_for_rf.csv',header=T) #discrete choice data
store_info=read.csv('C:/Users/JW/Documents/Iowa papers/store_info.csv',header=T) #store info
set.seed(72812)

#require(mlbench)
#data("PimaIndiansDiabetes2", package = "mlbench")
#PimaIndiansDiabetes <- na.omit(PimaIndiansDiabetes2[,-c(4, 5)])
#fmPID <- mob(diabetes ~ glucose | pregnant + pressure + mass + pedigree + age, data = PimaIndiansDiabetes, control = mob_control(verbose = TRUE), model = glinearModel, family = binomial())
#plot(fmPID)


choice_data <- choice_data[sample(1:nrow(choice_data), 500000, replace=FALSE),]

#choice_data <- join(choice_data, state_cost, type='left', match='all')

choice_data$store_num_abd <- choice_data$store_num
choice_data <- join(choice_data, store_info, type='left', match='all')

choice_data <- join(choice_data, products, type='left', match='all')

choice_data <-merge(choice_data,store_info, by.x="store_num", by.y = "store_num_abd")

mob_test <- mob(choice_data$volume_liters ~ reduced_cat_num + reduced_vendor_num| choice_data$year + choice_data$month + as.factor(store_type), data = choice_data, control = mob_control(verbose = TRUE), model = linearModel)


choice_data_cforest_test <- subset(choice_data,select = c(year,month,volume_liters, square_footage_whole.x))
cities <- dummy(choice_data$city_abd_proper.x,verbose = TRUE)
sellgas <- dummy(choice_data$sell_gasoline.x,verbose = TRUE)
storetype <- dummy(choice_data$store_type.x, verbose = TRUE)
products <- dummy(as.factor(choice_data$reduced_item_code), verbose = TRUE)
choice_data_cforest_test <- cbind(choice_data_cforest_test, cities, sellgas, storetype, products)
choice_data_cforest_test <- merge(choice_data_cforest_test, state_cost, type = 'left', match = 'all')

cforest_test <- cforest(volume_liters ~ ., data = choice_data_cforest_test)
table(choice_data_cforest_test$volume_liters, predict(cforest_test, type = "response"))

set.seed(290875)
### honest (i.e., out-of-bag) cross-classification of
### true vs. predicted classes
data("mammoexp", package = "TH.data")
table(mammoexp$ME, predict(cforest(ME ~ ., data = mammoexp,
                                   control = cforest_unbiased(ntree = 50)),
                           OOB = TRUE))
### fit forest to censored response
if (require("TH.data") && require("survival")) {
  data("GBSG2", package = "TH.data")
  bst <- cforest(Surv(time, cens) ~ ., data = GBSG2,
                 control = cforest_unbiased(ntree = 50))
  ### estimate conditional Kaplan-Meier curves
  treeresponse(bst, newdata = GBSG2[1:2,], OOB = TRUE)
  ### if you can't resist to look at individual trees ...
  party:::prettytree(bst@ensemble[[1]], names(bst@data@get("input")))
}
### proximity, see ?randomForest
iris.cf <- cforest(Species ~ ., data = iris,
                   control = cforest_unbiased(mtry = 2))
iris.mds <- cmdscale(1 - proximity(iris.cf), eig = TRUE)
op <- par(pty="s")
pairs(cbind(iris[,1:4], iris.mds$points), cex = 0.6, gap = 0,
      col = c("red", "green", "blue")[as.numeric(iris$Species)],
      main = "Iris Data: Predictors and MDS of Proximity Based on cforest")
par(op)