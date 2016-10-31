setwd("C:/Users/JW/Documents/Iowa papers")
#This code outputs information for each trial for each person into a row (long) or column (wide) format.  
require(ggplot2)
require(stargazer)
require(MASS)
require(KernSmooth)
require(plyr)
require(stats)
require(RSGHB)
require(lubridate)
available=read.csv('C:/Users/JW/Documents/Iowa papers/available_product_data.csv',header=T) #product availability
state_cost=read.csv('C:/Users/JW/Documents/Iowa papers/discrete_price_matrix_monthly_0001_filled_state.csv',header=T) #state cost
bottle_cost=read.csv('C:/Users/JW/Documents/Iowa papers/discrete_price_matrix_monthly_0001_filled.csv',header=T) #bottle cost
products=read.csv('C:/Users/JW/Documents/Iowa papers/discrete_data_0001_products_final.csv',header=T) #product information
choice_data=read.csv('C:/Users/JW/Documents/Iowa papers/discrete_data_001_monthly_wide_maxvol.csv',header=T) #discrete choice data
store_info=read.csv('C:/Users/JW/Documents/Iowa papers/store_info.csv',header=T) #store info

#choice_data = subset(choice_data,choice_data$year >= 2015)
#choice_data = subset(choice_data,choice_data$month >= 10)
#choice_data = subset(choice_data,choice_data$store_num <= 2600)
choice_data[is.na(choice_data)] <- 0

#available$monthyear = paste(available$month, available$year)
state_cost$monthyear = paste(state_cost$month, state_cost$year)
#bottle_cost$monthyear = paste(bottle_cost$month, bottle_cost$year)
choice_data$monthyear = paste(choice_data$month, choice_data$year)

choice_data <-merge(choice_data,state_cost, by.x="monthyear", by.y = "monthyear")
choice_data <-merge(choice_data,store_info, by.x="store_num", by.y = "store_num_abd")
choice_data <-merge(choice_data,available, by.x="monthyear", by.y = "monthyear")

choice_data$countm <-   (as.numeric(choice_data$volume_liters_9999  > 0) +
                         as.numeric(choice_data$volume_liters_10550695  > 0) +
                         as.numeric(choice_data$volume_liters_11586693  > 0) +
                         as.numeric(choice_data$volume_liters_11773688  > 0) +
                         as.numeric(choice_data$volume_liters_11774687  > 0) +
                         as.numeric(choice_data$volume_liters_11776686  > 0) +
                         as.numeric(choice_data$volume_liters_11777685  > 0) +
                         as.numeric(choice_data$volume_liters_11786699  > 0) +
                         as.numeric(choice_data$volume_liters_11788689  > 0) +
                         as.numeric(choice_data$volume_liters_16518268  > 0) +
                         as.numeric(choice_data$volume_liters_17991132  > 0) +
                         as.numeric(choice_data$volume_liters_22788573  > 0) +
                         as.numeric(choice_data$volume_liters_29287548  > 0) +
                         as.numeric(choice_data$volume_liters_34006175  > 0) +
                         as.numeric(choice_data$volume_liters_34007174  > 0) +
                         as.numeric(choice_data$volume_liters_34008178  > 0) +
                         as.numeric(choice_data$volume_liters_34029146  > 0) +
                         as.numeric(choice_data$volume_liters_35317555  > 0) +
                         as.numeric(choice_data$volume_liters_35318559  > 0) +
                         as.numeric(choice_data$volume_liters_35416952  > 0) +
                         as.numeric(choice_data$volume_liters_35418953  > 0) +
                         as.numeric(choice_data$volume_liters_43026214  > 0) +
                         as.numeric(choice_data$volume_liters_43027213  > 0) +
                         as.numeric(choice_data$volume_liters_43028216  > 0) +
                         as.numeric(choice_data$volume_liters_43038436  > 0) +
                         as.numeric(choice_data$volume_liters_43126479  > 0) +
                         as.numeric(choice_data$volume_liters_43127478  > 0) +
                         as.numeric(choice_data$volume_liters_43128482  > 0) +
                         as.numeric(choice_data$volume_liters_43136446  > 0) +
                         as.numeric(choice_data$volume_liters_43137443  > 0) +
                         as.numeric(choice_data$volume_liters_44217554  > 0))

choice_data = subset(choice_data,choice_data$countm > 0)

testvar = ifelse(choice_data$chosen_9999     == 1,exp(0   + 0 * choice_data$state_cost_10550695   + log(.5) + (.5 - 1) * log(choice_data$volume_liters_10550695  + .5)), 1)
####################################################
## Useful code example for reading columns in loops
####################################################

# data$col1 = rep(1,952)
# data$col2 = rep(1,952)
# data$col3 = rep(1,952)
# data$col4 = rep(1,952)
# data$col5 = rep(1,952)
# data$col6 = rep(1,952)
# data$col7 = rep(1,952)
# data$col8 = rep(1,952)
# data$col9 = rep(1,952)
# attach(data)
# for(i in 1:9){
#   col = paste('col',i,sep="")
#   data[,col] = 3
# }
# detach(data)
####################################################



####################################################
## The RSGHB Code
####################################################

choice_data$available_9999       <- 1
choice_data$available_10550695   <- as.numeric(choice_data$state_cost_10550695   > 0)
choice_data$available_11586693   <- as.numeric(choice_data$state_cost_11586693   > 0)
choice_data$available_11773688   <- as.numeric(choice_data$state_cost_11773688   > 0)
choice_data$available_11774687   <- as.numeric(choice_data$state_cost_11774687   > 0)
choice_data$available_11776686   <- as.numeric(choice_data$state_cost_11776686   > 0)
choice_data$available_11777685   <- as.numeric(choice_data$state_cost_11777685   > 0)
choice_data$available_11786699   <- as.numeric(choice_data$state_cost_11786699   > 0)
choice_data$available_11788689   <- as.numeric(choice_data$state_cost_11788689   > 0)
choice_data$available_16518268   <- as.numeric(choice_data$state_cost_16518268   > 0)
choice_data$available_17991132   <- as.numeric(choice_data$state_cost_17991132   > 0)
choice_data$available_22788573   <- as.numeric(choice_data$state_cost_22788573   > 0)
choice_data$available_29287548   <- as.numeric(choice_data$state_cost_29287548   > 0)
choice_data$available_34006175   <- as.numeric(choice_data$state_cost_34006175   > 0)
choice_data$available_34007174   <- as.numeric(choice_data$state_cost_34007174   > 0)
choice_data$available_34008178   <- as.numeric(choice_data$state_cost_34008178   > 0)
choice_data$available_34029146   <- as.numeric(choice_data$state_cost_34029146   > 0)
choice_data$available_35317555   <- as.numeric(choice_data$state_cost_35317555   > 0)
choice_data$available_35318559   <- as.numeric(choice_data$state_cost_35318559   > 0)
choice_data$available_35416952   <- as.numeric(choice_data$state_cost_35416952   > 0)
choice_data$available_35418953   <- as.numeric(choice_data$state_cost_35418953   > 0)
choice_data$available_43026214   <- as.numeric(choice_data$state_cost_43026214   > 0)
choice_data$available_43027213   <- as.numeric(choice_data$state_cost_43027213   > 0)
choice_data$available_43028216   <- as.numeric(choice_data$state_cost_43028216   > 0)
choice_data$available_43038436   <- as.numeric(choice_data$state_cost_43038436   > 0)
choice_data$available_43126479   <- as.numeric(choice_data$state_cost_43126479   > 0)
choice_data$available_43127478   <- as.numeric(choice_data$state_cost_43127478   > 0)
choice_data$available_43128482   <- as.numeric(choice_data$state_cost_43128482   > 0)
choice_data$available_43136446   <- as.numeric(choice_data$state_cost_43136446   > 0)
choice_data$available_43137443   <- as.numeric(choice_data$state_cost_43137443   > 0)
choice_data$available_44217554   <- as.numeric(choice_data$state_cost_44217554   > 0)

choice_data$chosen_9999       <- as.numeric(choice_data$volume_liters_9999       > 0)
choice_data$chosen_10550695   <- as.numeric(choice_data$volume_liters_10550695   > 0)
choice_data$chosen_11586693   <- as.numeric(choice_data$volume_liters_11586693   > 0)
choice_data$chosen_11773688   <- as.numeric(choice_data$volume_liters_11773688   > 0)
choice_data$chosen_11774687   <- as.numeric(choice_data$volume_liters_11774687   > 0)
choice_data$chosen_11776686   <- as.numeric(choice_data$volume_liters_11776686   > 0)
choice_data$chosen_11777685   <- as.numeric(choice_data$volume_liters_11777685   > 0)
choice_data$chosen_11786699   <- as.numeric(choice_data$volume_liters_11786699   > 0)
choice_data$chosen_11788689   <- as.numeric(choice_data$volume_liters_11788689   > 0)
choice_data$chosen_16518268   <- as.numeric(choice_data$volume_liters_16518268   > 0)
choice_data$chosen_17991132   <- as.numeric(choice_data$volume_liters_17991132   > 0)
choice_data$chosen_22788573   <- as.numeric(choice_data$volume_liters_22788573   > 0)
choice_data$chosen_29287548   <- as.numeric(choice_data$volume_liters_29287548   > 0)
choice_data$chosen_34006175   <- as.numeric(choice_data$volume_liters_34006175   > 0)
choice_data$chosen_34007174   <- as.numeric(choice_data$volume_liters_34007174   > 0)
choice_data$chosen_34008178   <- as.numeric(choice_data$volume_liters_34008178   > 0)
choice_data$chosen_34029146   <- as.numeric(choice_data$volume_liters_34029146   > 0)
choice_data$chosen_35317555   <- as.numeric(choice_data$volume_liters_35317555   > 0)
choice_data$chosen_35318559   <- as.numeric(choice_data$volume_liters_35318559   > 0)
choice_data$chosen_35416952   <- as.numeric(choice_data$volume_liters_35416952   > 0)
choice_data$chosen_35418953   <- as.numeric(choice_data$volume_liters_35418953   > 0)
choice_data$chosen_43026214   <- as.numeric(choice_data$volume_liters_43026214   > 0)
choice_data$chosen_43027213   <- as.numeric(choice_data$volume_liters_43027213   > 0)
choice_data$chosen_43028216   <- as.numeric(choice_data$volume_liters_43028216   > 0)
choice_data$chosen_43038436   <- as.numeric(choice_data$volume_liters_43038436   > 0)
choice_data$chosen_43126479   <- as.numeric(choice_data$volume_liters_43126479   > 0)
choice_data$chosen_43127478   <- as.numeric(choice_data$volume_liters_43127478   > 0)
choice_data$chosen_43128482   <- as.numeric(choice_data$volume_liters_43128482   > 0)
choice_data$chosen_43136446   <- as.numeric(choice_data$volume_liters_43136446   > 0)
choice_data$chosen_43137443   <- as.numeric(choice_data$volume_liters_43137443   > 0)
choice_data$chosen_44217554   <- as.numeric(choice_data$volume_liters_44217554   > 0)

# The likelihood function
likelihood <- function(fc, b) {
  # Assign fixed parameters to named variables for convenience
  cc <- 1
  PRICE1 <- b[, cc]; cc <- cc + 1
  ASC9999       <- b[, cc]; cc <- cc + 1    
  ASC10550695   <- b[, cc]; cc <- cc + 1
  ASC11586693   <- b[, cc]; cc <- cc + 1
  ASC11773688   <- b[, cc]; cc <- cc + 1
  ASC11774687   <- b[, cc]; cc <- cc + 1
  ASC11776686   <- b[, cc]; cc <- cc + 1
  ASC11777685   <- b[, cc]; cc <- cc + 1
  ASC11786699   <- b[, cc]; cc <- cc + 1
  ASC11788689   <- b[, cc]; cc <- cc + 1
  ASC16518268   <- b[, cc]; cc <- cc + 1
  ASC17991132   <- b[, cc]; cc <- cc + 1
  ASC22788573   <- b[, cc]; cc <- cc + 1
  ASC29287548   <- b[, cc]; cc <- cc + 1
  ASC34006175   <- b[, cc]; cc <- cc + 1
  ASC34007174   <- b[, cc]; cc <- cc + 1
  ASC34008178   <- b[, cc]; cc <- cc + 1
  ASC34029146   <- b[, cc]; cc <- cc + 1
  ASC35317555   <- b[, cc]; cc <- cc + 1
  ASC35318559   <- b[, cc]; cc <- cc + 1
  ASC35416952   <- b[, cc]; cc <- cc + 1
  ASC35418953   <- b[, cc]; cc <- cc + 1
  ASC43026214   <- b[, cc]; cc <- cc + 1
  ASC43027213   <- b[, cc]; cc <- cc + 1
  ASC43028216   <- b[, cc]; cc <- cc + 1
  ASC43038436   <- b[, cc]; cc <- cc + 1
  ASC43126479   <- b[, cc]; cc <- cc + 1
  ASC43127478   <- b[, cc]; cc <- cc + 1
  ASC43128482   <- b[, cc]; cc <- cc + 1
  ASC43136446   <- b[, cc]; cc <- cc + 1
  ASC43137443   <- b[, cc]; cc <- cc + 1
  ASC44217554   <- b[, cc]; cc <- cc + 1
  ALPHA_9999       <- b[, cc]; cc <- cc + 1    
  ALPHA_10550695   <- b[, cc]; cc <- cc + 1
  ALPHA_11586693   <- b[, cc]; cc <- cc + 1
  ALPHA_11773688   <- b[, cc]; cc <- cc + 1
  ALPHA_11774687   <- b[, cc]; cc <- cc + 1
  ALPHA_11776686   <- b[, cc]; cc <- cc + 1
  ALPHA_11777685   <- b[, cc]; cc <- cc + 1
  ALPHA_11786699   <- b[, cc]; cc <- cc + 1
  ALPHA_11788689   <- b[, cc]; cc <- cc + 1
  ALPHA_16518268   <- b[, cc]; cc <- cc + 1
  ALPHA_17991132   <- b[, cc]; cc <- cc + 1
  ALPHA_22788573   <- b[, cc]; cc <- cc + 1
  ALPHA_29287548   <- b[, cc]; cc <- cc + 1
  ALPHA_34006175   <- b[, cc]; cc <- cc + 1
  ALPHA_34007174   <- b[, cc]; cc <- cc + 1
  ALPHA_34008178   <- b[, cc]; cc <- cc + 1
  ALPHA_34029146   <- b[, cc]; cc <- cc + 1
  ALPHA_35317555   <- b[, cc]; cc <- cc + 1
  ALPHA_35318559   <- b[, cc]; cc <- cc + 1
  ALPHA_35416952   <- b[, cc]; cc <- cc + 1
  ALPHA_35418953   <- b[, cc]; cc <- cc + 1
  ALPHA_43026214   <- b[, cc]; cc <- cc + 1
  ALPHA_43027213   <- b[, cc]; cc <- cc + 1
  ALPHA_43028216   <- b[, cc]; cc <- cc + 1
  ALPHA_43038436   <- b[, cc]; cc <- cc + 1
  ALPHA_43126479   <- b[, cc]; cc <- cc + 1
  ALPHA_43127478   <- b[, cc]; cc <- cc + 1
  ALPHA_43128482   <- b[, cc]; cc <- cc + 1
  ALPHA_43136446   <- b[, cc]; cc <- cc + 1
  ALPHA_43137443   <- b[, cc]; cc <- cc + 1
  ALPHA_44217554   <- b[, cc]; cc <- cc + 1
  GAMMA_9999       <- b[, cc]; cc <- cc + 1    
  GAMMA_10550695   <- b[, cc]; cc <- cc + 1
  GAMMA_11586693   <- b[, cc]; cc <- cc + 1
  GAMMA_11773688   <- b[, cc]; cc <- cc + 1
  GAMMA_11774687   <- b[, cc]; cc <- cc + 1
  GAMMA_11776686   <- b[, cc]; cc <- cc + 1
  GAMMA_11777685   <- b[, cc]; cc <- cc + 1
  GAMMA_11786699   <- b[, cc]; cc <- cc + 1
  GAMMA_11788689   <- b[, cc]; cc <- cc + 1
  GAMMA_16518268   <- b[, cc]; cc <- cc + 1
  GAMMA_17991132   <- b[, cc]; cc <- cc + 1
  GAMMA_22788573   <- b[, cc]; cc <- cc + 1
  GAMMA_29287548   <- b[, cc]; cc <- cc + 1
  GAMMA_34006175   <- b[, cc]; cc <- cc + 1
  GAMMA_34007174   <- b[, cc]; cc <- cc + 1
  GAMMA_34008178   <- b[, cc]; cc <- cc + 1
  GAMMA_34029146   <- b[, cc]; cc <- cc + 1
  GAMMA_35317555   <- b[, cc]; cc <- cc + 1
  GAMMA_35318559   <- b[, cc]; cc <- cc + 1
  GAMMA_35416952   <- b[, cc]; cc <- cc + 1
  GAMMA_35418953   <- b[, cc]; cc <- cc + 1
  GAMMA_43026214   <- b[, cc]; cc <- cc + 1
  GAMMA_43027213   <- b[, cc]; cc <- cc + 1
  GAMMA_43028216   <- b[, cc]; cc <- cc + 1
  GAMMA_43038436   <- b[, cc]; cc <- cc + 1
  GAMMA_43126479   <- b[, cc]; cc <- cc + 1
  GAMMA_43127478   <- b[, cc]; cc <- cc + 1
  GAMMA_43128482   <- b[, cc]; cc <- cc + 1
  GAMMA_43136446   <- b[, cc]; cc <- cc + 1
  GAMMA_43137443   <- b[, cc]; cc <- cc + 1
  GAMMA_44217554   <- b[, cc];
  
  v9999       <- 0
  v10550695   <- ASC10550695 + PRICE1 * choice_data$state_cost_10550695 + log(1/(1 + exp(-ALPHA_10550695))) + ((1/(1 + exp(-ALPHA_10550695))) - 1) * log(choice_data$volume_liters_10550695  + 1)
  v11586693   <- ASC11586693 + PRICE1 * choice_data$state_cost_11586693 + log(1/(1 + exp(-ALPHA_11586693))) + ((1/(1 + exp(-ALPHA_11586693))) - 1) * log(choice_data$volume_liters_11586693  + 1)
  v11773688   <- ASC11773688 + PRICE1 * choice_data$state_cost_11773688 + log(1/(1 + exp(-ALPHA_11773688))) + ((1/(1 + exp(-ALPHA_11773688))) - 1) * log(choice_data$volume_liters_11773688  + 1)
  v11774687   <- ASC11774687 + PRICE1 * choice_data$state_cost_11774687 + log(1/(1 + exp(-ALPHA_11774687))) + ((1/(1 + exp(-ALPHA_11774687))) - 1) * log(choice_data$volume_liters_11774687  + 1)
  v11776686   <- ASC11776686 + PRICE1 * choice_data$state_cost_11776686 + log(1/(1 + exp(-ALPHA_11776686))) + ((1/(1 + exp(-ALPHA_11776686))) - 1) * log(choice_data$volume_liters_11776686  + 1)
  v11777685   <- ASC11777685 + PRICE1 * choice_data$state_cost_11777685 + log(1/(1 + exp(-ALPHA_11777685))) + ((1/(1 + exp(-ALPHA_11777685))) - 1) * log(choice_data$volume_liters_11777685  + 1)
  v11786699   <- ASC11786699 + PRICE1 * choice_data$state_cost_11786699 + log(1/(1 + exp(-ALPHA_11786699))) + ((1/(1 + exp(-ALPHA_11786699))) - 1) * log(choice_data$volume_liters_11786699  + 1)
  v11788689   <- ASC11788689 + PRICE1 * choice_data$state_cost_11788689 + log(1/(1 + exp(-ALPHA_11788689))) + ((1/(1 + exp(-ALPHA_11788689))) - 1) * log(choice_data$volume_liters_11788689  + 1)
  v16518268   <- ASC16518268 + PRICE1 * choice_data$state_cost_16518268 + log(1/(1 + exp(-ALPHA_16518268))) + ((1/(1 + exp(-ALPHA_16518268))) - 1) * log(choice_data$volume_liters_16518268  + 1)
  v17991132   <- ASC17991132 + PRICE1 * choice_data$state_cost_17991132 + log(1/(1 + exp(-ALPHA_17991132))) + ((1/(1 + exp(-ALPHA_17991132))) - 1) * log(choice_data$volume_liters_17991132  + 1)
  v22788573   <- ASC22788573 + PRICE1 * choice_data$state_cost_22788573 + log(1/(1 + exp(-ALPHA_22788573))) + ((1/(1 + exp(-ALPHA_22788573))) - 1) * log(choice_data$volume_liters_22788573  + 1)
  v29287548   <- ASC29287548 + PRICE1 * choice_data$state_cost_29287548 + log(1/(1 + exp(-ALPHA_29287548))) + ((1/(1 + exp(-ALPHA_29287548))) - 1) * log(choice_data$volume_liters_29287548  + 1)
  v34006175   <- ASC34006175 + PRICE1 * choice_data$state_cost_34006175 + log(1/(1 + exp(-ALPHA_34006175))) + ((1/(1 + exp(-ALPHA_34006175))) - 1) * log(choice_data$volume_liters_34006175  + 1)
  v34007174   <- ASC34007174 + PRICE1 * choice_data$state_cost_34007174 + log(1/(1 + exp(-ALPHA_34007174))) + ((1/(1 + exp(-ALPHA_34007174))) - 1) * log(choice_data$volume_liters_34007174  + 1)
  v34008178   <- ASC34008178 + PRICE1 * choice_data$state_cost_34008178 + log(1/(1 + exp(-ALPHA_34008178))) + ((1/(1 + exp(-ALPHA_34008178))) - 1) * log(choice_data$volume_liters_34008178  + 1)
  v34029146   <- ASC34029146 + PRICE1 * choice_data$state_cost_34029146 + log(1/(1 + exp(-ALPHA_34029146))) + ((1/(1 + exp(-ALPHA_34029146))) - 1) * log(choice_data$volume_liters_34029146  + 1)
  v35317555   <- ASC35317555 + PRICE1 * choice_data$state_cost_35317555 + log(1/(1 + exp(-ALPHA_35317555))) + ((1/(1 + exp(-ALPHA_35317555))) - 1) * log(choice_data$volume_liters_35317555  + 1)
  v35318559   <- ASC35318559 + PRICE1 * choice_data$state_cost_35318559 + log(1/(1 + exp(-ALPHA_35318559))) + ((1/(1 + exp(-ALPHA_35318559))) - 1) * log(choice_data$volume_liters_35318559  + 1)
  v35416952   <- ASC35416952 + PRICE1 * choice_data$state_cost_35416952 + log(1/(1 + exp(-ALPHA_35416952))) + ((1/(1 + exp(-ALPHA_35416952))) - 1) * log(choice_data$volume_liters_35416952  + 1)
  v35418953   <- ASC35418953 + PRICE1 * choice_data$state_cost_35418953 + log(1/(1 + exp(-ALPHA_35418953))) + ((1/(1 + exp(-ALPHA_35418953))) - 1) * log(choice_data$volume_liters_35418953  + 1)
  v43026214   <- ASC43026214 + PRICE1 * choice_data$state_cost_43026214 + log(1/(1 + exp(-ALPHA_43026214))) + ((1/(1 + exp(-ALPHA_43026214))) - 1) * log(choice_data$volume_liters_43026214  + 1)
  v43027213   <- ASC43027213 + PRICE1 * choice_data$state_cost_43027213 + log(1/(1 + exp(-ALPHA_43027213))) + ((1/(1 + exp(-ALPHA_43027213))) - 1) * log(choice_data$volume_liters_43027213  + 1)
  v43028216   <- ASC43028216 + PRICE1 * choice_data$state_cost_43028216 + log(1/(1 + exp(-ALPHA_43028216))) + ((1/(1 + exp(-ALPHA_43028216))) - 1) * log(choice_data$volume_liters_43028216  + 1)
  v43038436   <- ASC43038436 + PRICE1 * choice_data$state_cost_43038436 + log(1/(1 + exp(-ALPHA_43038436))) + ((1/(1 + exp(-ALPHA_43038436))) - 1) * log(choice_data$volume_liters_43038436  + 1)
  v43126479   <- ASC43126479 + PRICE1 * choice_data$state_cost_43126479 + log(1/(1 + exp(-ALPHA_43126479))) + ((1/(1 + exp(-ALPHA_43126479))) - 1) * log(choice_data$volume_liters_43126479  + 1)
  v43127478   <- ASC43127478 + PRICE1 * choice_data$state_cost_43127478 + log(1/(1 + exp(-ALPHA_43127478))) + ((1/(1 + exp(-ALPHA_43127478))) - 1) * log(choice_data$volume_liters_43127478  + 1)
  v43128482   <- ASC43128482 + PRICE1 * choice_data$state_cost_43128482 + log(1/(1 + exp(-ALPHA_43128482))) + ((1/(1 + exp(-ALPHA_43128482))) - 1) * log(choice_data$volume_liters_43128482  + 1)
  v43136446   <- ASC43136446 + PRICE1 * choice_data$state_cost_43136446 + log(1/(1 + exp(-ALPHA_43136446))) + ((1/(1 + exp(-ALPHA_43136446))) - 1) * log(choice_data$volume_liters_43136446  + 1)
  v43137443   <- ASC43137443 + PRICE1 * choice_data$state_cost_43137443 + log(1/(1 + exp(-ALPHA_43137443))) + ((1/(1 + exp(-ALPHA_43137443))) - 1) * log(choice_data$volume_liters_43137443  + 1)
  v44217554   <- ASC44217554 + PRICE1 * choice_data$state_cost_44217554 + log(1/(1 + exp(-ALPHA_44217554))) + ((1/(1 + exp(-ALPHA_44217554))) - 1) * log(choice_data$volume_liters_44217554  + 1)
  
  
  p <-exp(
    log(    (1- 1/(1 + exp(-ALPHA_9999    )))/(choice_data$volume_liters_9999        + 1) *
            (1- 1/(1 + exp(-ALPHA_10550695)))/(choice_data$volume_liters_10550695    + 1) *
            (1- 1/(1 + exp(-ALPHA_11586693)))/(choice_data$volume_liters_11586693    + 1) *
            (1- 1/(1 + exp(-ALPHA_11773688)))/(choice_data$volume_liters_11773688    + 1) *
            (1- 1/(1 + exp(-ALPHA_11774687)))/(choice_data$volume_liters_11774687    + 1) *
            (1- 1/(1 + exp(-ALPHA_11776686)))/(choice_data$volume_liters_11776686    + 1) *
            (1- 1/(1 + exp(-ALPHA_11777685)))/(choice_data$volume_liters_11777685    + 1) *
            (1- 1/(1 + exp(-ALPHA_11786699)))/(choice_data$volume_liters_11786699    + 1) *
            (1- 1/(1 + exp(-ALPHA_11788689)))/(choice_data$volume_liters_11788689    + 1) *
            (1- 1/(1 + exp(-ALPHA_16518268)))/(choice_data$volume_liters_16518268    + 1) *
            (1- 1/(1 + exp(-ALPHA_17991132)))/(choice_data$volume_liters_17991132    + 1) *
            (1- 1/(1 + exp(-ALPHA_22788573)))/(choice_data$volume_liters_22788573    + 1) *
            (1- 1/(1 + exp(-ALPHA_29287548)))/(choice_data$volume_liters_29287548    + 1) *
            (1- 1/(1 + exp(-ALPHA_34006175)))/(choice_data$volume_liters_34006175    + 1) *
            (1- 1/(1 + exp(-ALPHA_34007174)))/(choice_data$volume_liters_34007174    + 1) *
            (1- 1/(1 + exp(-ALPHA_34008178)))/(choice_data$volume_liters_34008178    + 1) *
            (1- 1/(1 + exp(-ALPHA_34029146)))/(choice_data$volume_liters_34029146    + 1) *
            (1- 1/(1 + exp(-ALPHA_35317555)))/(choice_data$volume_liters_35317555    + 1) *
            (1- 1/(1 + exp(-ALPHA_35318559)))/(choice_data$volume_liters_35318559    + 1) *
            (1- 1/(1 + exp(-ALPHA_35416952)))/(choice_data$volume_liters_35416952    + 1) *
            (1- 1/(1 + exp(-ALPHA_35418953)))/(choice_data$volume_liters_35418953    + 1) *
            (1- 1/(1 + exp(-ALPHA_43026214)))/(choice_data$volume_liters_43026214    + 1) *
            (1- 1/(1 + exp(-ALPHA_43027213)))/(choice_data$volume_liters_43027213    + 1) *
            (1- 1/(1 + exp(-ALPHA_43028216)))/(choice_data$volume_liters_43028216    + 1) *
            (1- 1/(1 + exp(-ALPHA_43038436)))/(choice_data$volume_liters_43038436    + 1) *
            (1- 1/(1 + exp(-ALPHA_43126479)))/(choice_data$volume_liters_43126479    + 1) *
            (1- 1/(1 + exp(-ALPHA_43127478)))/(choice_data$volume_liters_43127478    + 1) *
            (1- 1/(1 + exp(-ALPHA_43128482)))/(choice_data$volume_liters_43128482    + 1) *
            (1- 1/(1 + exp(-ALPHA_43136446)))/(choice_data$volume_liters_43136446    + 1) *
            (1- 1/(1 + exp(-ALPHA_43137443)))/(choice_data$volume_liters_43137443    + 1) *
            (1- 1/(1 + exp(-ALPHA_44217554)))/(choice_data$volume_liters_44217554    + 1)) +
      log(    (choice_data$volume_liters_9999      + 1)/(1- 1/(1 + exp(-ALPHA_9999    ))) +
              (choice_data$volume_liters_10550695  + 1)/(1- 1/(1 + exp(-ALPHA_10550695))) +
              (choice_data$volume_liters_11586693  + 1)/(1- 1/(1 + exp(-ALPHA_11586693))) +
              (choice_data$volume_liters_11773688  + 1)/(1- 1/(1 + exp(-ALPHA_11773688))) +
              (choice_data$volume_liters_11774687  + 1)/(1- 1/(1 + exp(-ALPHA_11774687))) +
              (choice_data$volume_liters_11776686  + 1)/(1- 1/(1 + exp(-ALPHA_11776686))) +
              (choice_data$volume_liters_11777685  + 1)/(1- 1/(1 + exp(-ALPHA_11777685))) +
              (choice_data$volume_liters_11786699  + 1)/(1- 1/(1 + exp(-ALPHA_11786699))) +
              (choice_data$volume_liters_11788689  + 1)/(1- 1/(1 + exp(-ALPHA_11788689))) +
              (choice_data$volume_liters_16518268  + 1)/(1- 1/(1 + exp(-ALPHA_16518268))) +
              (choice_data$volume_liters_17991132  + 1)/(1- 1/(1 + exp(-ALPHA_17991132))) +
              (choice_data$volume_liters_22788573  + 1)/(1- 1/(1 + exp(-ALPHA_22788573))) +
              (choice_data$volume_liters_29287548  + 1)/(1- 1/(1 + exp(-ALPHA_29287548))) +
              (choice_data$volume_liters_34006175  + 1)/(1- 1/(1 + exp(-ALPHA_34006175))) +
              (choice_data$volume_liters_34007174  + 1)/(1- 1/(1 + exp(-ALPHA_34007174))) +
              (choice_data$volume_liters_34008178  + 1)/(1- 1/(1 + exp(-ALPHA_34008178))) +
              (choice_data$volume_liters_34029146  + 1)/(1- 1/(1 + exp(-ALPHA_34029146))) +
              (choice_data$volume_liters_35317555  + 1)/(1- 1/(1 + exp(-ALPHA_35317555))) +
              (choice_data$volume_liters_35318559  + 1)/(1- 1/(1 + exp(-ALPHA_35318559))) +
              (choice_data$volume_liters_35416952  + 1)/(1- 1/(1 + exp(-ALPHA_35416952))) +
              (choice_data$volume_liters_35418953  + 1)/(1- 1/(1 + exp(-ALPHA_35418953))) +
              (choice_data$volume_liters_43026214  + 1)/(1- 1/(1 + exp(-ALPHA_43026214))) +
              (choice_data$volume_liters_43027213  + 1)/(1- 1/(1 + exp(-ALPHA_43027213))) +
              (choice_data$volume_liters_43028216  + 1)/(1- 1/(1 + exp(-ALPHA_43028216))) +
              (choice_data$volume_liters_43038436  + 1)/(1- 1/(1 + exp(-ALPHA_43038436))) +
              (choice_data$volume_liters_43126479  + 1)/(1- 1/(1 + exp(-ALPHA_43126479))) +
              (choice_data$volume_liters_43127478  + 1)/(1- 1/(1 + exp(-ALPHA_43127478))) +
              (choice_data$volume_liters_43128482  + 1)/(1- 1/(1 + exp(-ALPHA_43128482))) +
              (choice_data$volume_liters_43136446  + 1)/(1- 1/(1 + exp(-ALPHA_43136446))) +
              (choice_data$volume_liters_43137443  + 1)/(1- 1/(1 + exp(-ALPHA_43137443))) +
              (choice_data$volume_liters_44217554  + 1)/(1- 1/(1 + exp(-ALPHA_44217554)))) + 
      log(    ifelse(choice_data$chosen_9999     == 1,exp(v9999     ), 1) *
              ifelse(choice_data$chosen_10550695 == 1,exp(v10550695 ), 1) *
              ifelse(choice_data$chosen_11586693 == 1,exp(v11586693 ), 1) *
              ifelse(choice_data$chosen_11773688 == 1,exp(v11773688 ), 1) *
              ifelse(choice_data$chosen_11774687 == 1,exp(v11774687 ), 1) *
              ifelse(choice_data$chosen_11776686 == 1,exp(v11776686 ), 1) *
              ifelse(choice_data$chosen_11777685 == 1,exp(v11777685 ), 1) *
              ifelse(choice_data$chosen_11786699 == 1,exp(v11786699 ), 1) *
              ifelse(choice_data$chosen_11788689 == 1,exp(v11788689 ), 1) *
              ifelse(choice_data$chosen_16518268 == 1,exp(v16518268 ), 1) *
              ifelse(choice_data$chosen_17991132 == 1,exp(v17991132 ), 1) *
              ifelse(choice_data$chosen_22788573 == 1,exp(v22788573 ), 1) *
              ifelse(choice_data$chosen_29287548 == 1,exp(v29287548 ), 1) *
              ifelse(choice_data$chosen_34006175 == 1,exp(v34006175 ), 1) *
              ifelse(choice_data$chosen_34007174 == 1,exp(v34007174 ), 1) *
              ifelse(choice_data$chosen_34008178 == 1,exp(v34008178 ), 1) *
              ifelse(choice_data$chosen_34029146 == 1,exp(v34029146 ), 1) *
              ifelse(choice_data$chosen_35317555 == 1,exp(v35317555 ), 1) *
              ifelse(choice_data$chosen_35318559 == 1,exp(v35318559 ), 1) *
              ifelse(choice_data$chosen_35416952 == 1,exp(v35416952 ), 1) *
              ifelse(choice_data$chosen_35418953 == 1,exp(v35418953 ), 1) *
              ifelse(choice_data$chosen_43026214 == 1,exp(v43026214 ), 1) *
              ifelse(choice_data$chosen_43027213 == 1,exp(v43027213 ), 1) *
              ifelse(choice_data$chosen_43028216 == 1,exp(v43028216 ), 1) *
              ifelse(choice_data$chosen_43038436 == 1,exp(v43038436 ), 1) *
              ifelse(choice_data$chosen_43126479 == 1,exp(v43126479 ), 1) *
              ifelse(choice_data$chosen_43127478 == 1,exp(v43127478 ), 1) *
              ifelse(choice_data$chosen_43128482 == 1,exp(v43128482 ), 1) *
              ifelse(choice_data$chosen_43136446 == 1,exp(v43136446 ), 1) *
              ifelse(choice_data$chosen_43137443 == 1,exp(v43137443 ), 1) *
              ifelse(choice_data$chosen_44217554 == 1,exp(v44217554 ), 1)) -
      choice_data$countm *
      log(    exp(v9999     ) +
              exp(v10550695 ) +
              exp(v11586693 ) +
              exp(v11773688 ) +
              exp(v11774687 ) +
              exp(v11776686 ) +
              exp(v11777685 ) +
              exp(v11786699 ) +
              exp(v11788689 ) +
              exp(v16518268 ) +
              exp(v17991132 ) +
              exp(v22788573 ) +
              exp(v29287548 ) +
              exp(v34006175 ) +
              exp(v34007174 ) +
              exp(v34008178 ) +
              exp(v34029146 ) +
              exp(v35317555 ) +
              exp(v35318559 ) +
              exp(v35416952 ) +
              exp(v35418953 ) +
              exp(v43026214 ) +
              exp(v43027213 ) +
              exp(v43028216 ) +
              exp(v43038436 ) +
              exp(v43126479 ) +
              exp(v43127478 ) +
              exp(v43128482 ) +
              exp(v43136446 ) +
              exp(v43137443 ) +
              exp(v44217554 )) +
      lfactorial(choice_data$countm - 1))
  return(p)
}	


gVarNamesNormal <- c(
  "PRICE1"       ,
  "ASC9999"     , 
  "ASC10550695" , 
  "ASC11586693" , 
  "ASC11773688" , 
  "ASC11774687" , 
  "ASC11776686" , 
  "ASC11777685" , 
  "ASC11786699" , 
  "ASC11788689" , 
  "ASC16518268" , 
  "ASC17991132" , 
  "ASC22788573" , 
  "ASC29287548" , 
  "ASC34006175" , 
  "ASC34007174" , 
  "ASC34008178" , 
  "ASC34029146" , 
  "ASC35317555" , 
  "ASC35318559" , 
  "ASC35416952" , 
  "ASC35418953" , 
  "ASC43026214" , 
  "ASC43027213" , 
  "ASC43028216" , 
  "ASC43038436" , 
  "ASC43126479" , 
  "ASC43127478" , 
  "ASC43128482" , 
  "ASC43136446" , 
  "ASC43137443" , 
  "ASC44217554" , 
  "ALPHA_9999"     , 
  "ALPHA_10550695" , 
  "ALPHA_11586693" , 
  "ALPHA_11773688" , 
  "ALPHA_11774687" , 
  "ALPHA_11776686" , 
  "ALPHA_11777685" , 
  "ALPHA_11786699" , 
  "ALPHA_11788689" , 
  "ALPHA_16518268" , 
  "ALPHA_17991132" , 
  "ALPHA_22788573" , 
  "ALPHA_29287548" , 
  "ALPHA_34006175" , 
  "ALPHA_34007174" , 
  "ALPHA_34008178" , 
  "ALPHA_34029146" , 
  "ALPHA_35317555" , 
  "ALPHA_35318559" , 
  "ALPHA_35416952" , 
  "ALPHA_35418953" , 
  "ALPHA_43026214" , 
  "ALPHA_43027213" , 
  "ALPHA_43028216" , 
  "ALPHA_43038436" , 
  "ALPHA_43126479" , 
  "ALPHA_43127478" , 
  "ALPHA_43128482" , 
  "ALPHA_43136446" , 
  "ALPHA_43137443" , 
  "ALPHA_44217554" , 
  "GAMMA_9999"      , 
  "GAMMA_10550695"  , 
  "GAMMA_11586693"  , 
  "GAMMA_11773688"  , 
  "GAMMA_11774687"  , 
  "GAMMA_11776686"  , 
  "GAMMA_11777685"  , 
  "GAMMA_11786699"  , 
  "GAMMA_11788689"  , 
  "GAMMA_16518268"  , 
  "GAMMA_17991132"  , 
  "GAMMA_22788573"  , 
  "GAMMA_29287548"  , 
  "GAMMA_34006175"  , 
  "GAMMA_34007174"  , 
  "GAMMA_34008178"  , 
  "GAMMA_34029146"  , 
  "GAMMA_35317555"  , 
  "GAMMA_35318559"  , 
  "GAMMA_35416952"  , 
  "GAMMA_35418953"  , 
  "GAMMA_43026214"  , 
  "GAMMA_43027213"  , 
  "GAMMA_43028216"  , 
  "GAMMA_43038436"  , 
  "GAMMA_43126479"  , 
  "GAMMA_43127478"  , 
  "GAMMA_43128482"  , 
  "GAMMA_43136446"  , 
  "GAMMA_43137443"  , 
  "GAMMA_44217554")

# For each variable, specify the distribution for its coefficient
# The options are:
# 1. normal
# 2. log-nomal
# 3. negative log-normal
# 4. normal with all values below zero massed at zero
# 5. Johnson SB with a specified min and max
# gDIST must have an entry for each value in gVarNamesNormal

gDIST <- c(3, rep(1,31), rep(1,31), rep(2,31))

# STARTING VALUES
svN <- c(-.1, rep(0,31), rep(0,31), rep(.5,31))
# ITERATION SETTINGS
gNCREP    <- 15000     # Number of iterations to use prior to convergence
gNEREP    <- 5000          # Number of iterations to keep for averaging after convergence has been reached
gNSKIP    <- 1			  # Number of iterations to do in between retaining draws for averaging
gINFOSKIP <- 100         # How frequently to print info about the iteration process
modelname = "test1"
control <- list(
  modelname = modelname,
  gVarNamesNormal = gVarNamesNormal,
  svN = svN,
  gDIST = gDIST,
  gNCREP = gNCREP,
  gNEREP = gNEREP,
  gNSKIP = gNSKIP,
  gINFOSKIP = gINFOSKIP,
  write.results = TRUE,
  gSeed = 1987,
  nodiagnostics = TRUE, # Set this to FALSE to see initial model diagnostics
  verbose = TRUE       # Set this to TRUE to see real-time progress printed and plotted
)
choice_data$ID = choice_data$store_num
choice_data = choice_data[order(choice_data$ID),]
ptime <- proc.time()
model <- doHB(likelihood, choice_data, control)
proc.time() - ptime








