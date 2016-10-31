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
choice_data=read.csv('C:/Users/JW/Documents/Iowa papers/discrete_data_tops_monthly_wide_maxvol.csv',header=T) #discrete choice data
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

choice_data$countm <-     (as.numeric(choice_data$volume_liters_433361110 > 0) +
                           as.numeric(choice_data$volume_liters_648672039 > 0) +
                           as.numeric(choice_data$volume_liters_433381135 > 0) +
                           as.numeric(choice_data$volume_liters_11788689  > 0) +
                           as.numeric(choice_data$volume_liters_35318559  > 0) +
                           as.numeric(choice_data$volume_liters_268272787 > 0) +
                           as.numeric(choice_data$volume_liters_373484017 > 0) +
                           as.numeric(choice_data$volume_liters_359182079 > 0) +
                           as.numeric(choice_data$volume_liters_363052474 > 0) +
                           as.numeric(choice_data$volume_liters_363082477 > 0) +
                           as.numeric(choice_data$volume_liters_433371133 > 0) +
                           as.numeric(choice_data$volume_liters_124081067 > 0) +
                           as.numeric(choice_data$volume_liters_43028216  > 0) +
                           as.numeric(choice_data$volume_liters_35317555  > 0) +
                           as.numeric(choice_data$volume_liters_11777685  > 0) +
                           as.numeric(choice_data$volume_liters_112971485 > 0) +
                           as.numeric(choice_data$volume_liters_9999      > 0) +
                           as.numeric(choice_data$volume_liters_256084476 > 0) )

choice_data = subset(choice_data,choice_data$countm > 0)
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
choice_data$available_433361110 <- as.numeric(choice_data$state_cost_433361110 > 0)
choice_data$available_648672039 <- as.numeric(choice_data$state_cost_648672039 > 0)
choice_data$available_433381135 <- as.numeric(choice_data$state_cost_433381135 > 0)
choice_data$available_11788689  <- as.numeric(choice_data$state_cost_11788689  > 0)
choice_data$available_35318559  <- as.numeric(choice_data$state_cost_35318559  > 0)
choice_data$available_268272787 <- as.numeric(choice_data$state_cost_268272787 > 0)
choice_data$available_373484017 <- as.numeric(choice_data$state_cost_373484017 > 0)
choice_data$available_359182079 <- as.numeric(choice_data$state_cost_359182079 > 0)
choice_data$available_363052474 <- as.numeric(choice_data$state_cost_363052474 > 0)
choice_data$available_363082477 <- as.numeric(choice_data$state_cost_363082477 > 0)
choice_data$available_433371133 <- as.numeric(choice_data$state_cost_433371133 > 0)
choice_data$available_124081067 <- as.numeric(choice_data$state_cost_124081067 > 0)
choice_data$available_43028216  <- as.numeric(choice_data$state_cost_43028216  > 0)
choice_data$available_35317555  <- as.numeric(choice_data$state_cost_35317555  > 0)
choice_data$available_11777685  <- as.numeric(choice_data$state_cost_11777685  > 0)
choice_data$available_112971485 <- as.numeric(choice_data$state_cost_112971485 > 0)
choice_data$available_256084476 <- as.numeric(choice_data$state_cost_256084476 > 0)

choice_data$chosen_9999      <- as.numeric(choice_data$volume_liters_9999      > 0)
choice_data$chosen_433361110 <- as.numeric(choice_data$volume_liters_433361110 > 0)
choice_data$chosen_648672039 <- as.numeric(choice_data$volume_liters_648672039 > 0)
choice_data$chosen_433381135 <- as.numeric(choice_data$volume_liters_433381135 > 0)
choice_data$chosen_11788689  <- as.numeric(choice_data$volume_liters_11788689  > 0)
choice_data$chosen_35318559  <- as.numeric(choice_data$volume_liters_35318559  > 0)
choice_data$chosen_268272787 <- as.numeric(choice_data$volume_liters_268272787 > 0)
choice_data$chosen_373484017 <- as.numeric(choice_data$volume_liters_373484017 > 0)
choice_data$chosen_359182079 <- as.numeric(choice_data$volume_liters_359182079 > 0)
choice_data$chosen_363052474 <- as.numeric(choice_data$volume_liters_363052474 > 0)
choice_data$chosen_363082477 <- as.numeric(choice_data$volume_liters_363082477 > 0)
choice_data$chosen_433371133 <- as.numeric(choice_data$volume_liters_433371133 > 0)
choice_data$chosen_124081067 <- as.numeric(choice_data$volume_liters_124081067 > 0)
choice_data$chosen_43028216  <- as.numeric(choice_data$volume_liters_43028216  > 0)
choice_data$chosen_35317555  <- as.numeric(choice_data$volume_liters_35317555  > 0)
choice_data$chosen_11777685  <- as.numeric(choice_data$volume_liters_11777685  > 0)
choice_data$chosen_112971485 <- as.numeric(choice_data$volume_liters_112971485 > 0)
choice_data$chosen_256084476 <- as.numeric(choice_data$volume_liters_256084476 > 0)

# The likelihood function
likelihood <- function(fc, b) {
  # Assign fixed parameters to named variables for convenience
  cc <- 1
  PRICE9999      <- b[, cc]; cc <- cc + 1
  PRICE433361110 <- b[, cc]; cc <- cc + 1
  PRICE648672039 <- b[, cc]; cc <- cc + 1
  PRICE433381135 <- b[, cc]; cc <- cc + 1
  PRICE11788689  <- b[, cc]; cc <- cc + 1
  PRICE35318559  <- b[, cc]; cc <- cc + 1
  PRICE268272787 <- b[, cc]; cc <- cc + 1
  PRICE373484017 <- b[, cc]; cc <- cc + 1
  PRICE359182079 <- b[, cc]; cc <- cc + 1
  PRICE363052474 <- b[, cc]; cc <- cc + 1
  PRICE363082477 <- b[, cc]; cc <- cc + 1
  PRICE433371133 <- b[, cc]; cc <- cc + 1
  PRICE124081067 <- b[, cc]; cc <- cc + 1
  PRICE43028216  <- b[, cc]; cc <- cc + 1
  PRICE35317555  <- b[, cc]; cc <- cc + 1
  PRICE11777685  <- b[, cc]; cc <- cc + 1
  PRICE112971485 <- b[, cc]; cc <- cc + 1
  PRICE256084476 <- b[, cc]; cc <- cc + 1
  ASC9999      <- b[, cc]; cc <- cc + 1
  ASC433361110 <- b[, cc]; cc <- cc + 1    
  ASC648672039 <- b[, cc]; cc <- cc + 1
  ASC433381135 <- b[, cc]; cc <- cc + 1
  ASC11788689  <- b[, cc]; cc <- cc + 1
  ASC35318559  <- b[, cc]; cc <- cc + 1
  ASC268272787 <- b[, cc]; cc <- cc + 1
  ASC373484017 <- b[, cc]; cc <- cc + 1
  ASC359182079 <- b[, cc]; cc <- cc + 1
  ASC363052474 <- b[, cc]; cc <- cc + 1
  ASC363082477 <- b[, cc]; cc <- cc + 1
  ASC433371133 <- b[, cc]; cc <- cc + 1
  ASC124081067 <- b[, cc]; cc <- cc + 1
  ASC43028216  <- b[, cc]; cc <- cc + 1
  ASC35317555  <- b[, cc]; cc <- cc + 1
  ASC11777685  <- b[, cc]; cc <- cc + 1
  ASC112971485 <- b[, cc]; cc <- cc + 1
  ASC256084476 <- b[, cc]; cc <- cc + 1
  
  ALPHA_9999      <- b[, cc]; cc <- cc + 1    
  ALPHA_433361110 <- b[, cc]; cc <- cc + 1
  ALPHA_648672039 <- b[, cc]; cc <- cc + 1
  ALPHA_433381135 <- b[, cc]; cc <- cc + 1
  ALPHA_11788689  <- b[, cc]; cc <- cc + 1
  ALPHA_35318559  <- b[, cc]; cc <- cc + 1
  ALPHA_268272787 <- b[, cc]; cc <- cc + 1
  ALPHA_373484017 <- b[, cc]; cc <- cc + 1
  ALPHA_359182079 <- b[, cc]; cc <- cc + 1
  ALPHA_363052474 <- b[, cc]; cc <- cc + 1
  ALPHA_363082477 <- b[, cc]; cc <- cc + 1
  ALPHA_433371133 <- b[, cc]; cc <- cc + 1
  ALPHA_124081067 <- b[, cc]; cc <- cc + 1
  ALPHA_43028216  <- b[, cc]; cc <- cc + 1
  ALPHA_35317555  <- b[, cc]; cc <- cc + 1
  ALPHA_11777685  <- b[, cc]; cc <- cc + 1
  ALPHA_112971485 <- b[, cc]; cc <- cc + 1
  ALPHA_256084476 <- b[, cc]; cc <- cc + 1
 
  # GAMMA_9999      <- b[, cc]; cc <- cc + 1    
  # GAMMA_433361110 <- b[, cc]; cc <- cc + 1
  # GAMMA_648672039 <- b[, cc]; cc <- cc + 1
  # GAMMA_433381135 <- b[, cc]; cc <- cc + 1
  # GAMMA_11788689  <- b[, cc]; cc <- cc + 1
  # GAMMA_35318559  <- b[, cc]; cc <- cc + 1
  # GAMMA_268272787 <- b[, cc]; cc <- cc + 1
  # GAMMA_373484017 <- b[, cc]; cc <- cc + 1
  # GAMMA_359182079 <- b[, cc]; cc <- cc + 1
  # GAMMA_363052474 <- b[, cc]; cc <- cc + 1
  # GAMMA_363082477 <- b[, cc]; cc <- cc + 1
  # GAMMA_433371133 <- b[, cc]; cc <- cc + 1
  # GAMMA_124081067 <- b[, cc]; cc <- cc + 1
  # GAMMA_43028216  <- b[, cc]; cc <- cc + 1
  # GAMMA_35317555  <- b[, cc]; cc <- cc + 1
  # GAMMA_11777685  <- b[, cc]; cc <- cc + 1
  # GAMMA_112971485 <- b[, cc]; cc <- cc + 1
  # GAMMA_256084476 <- b[, cc];
  
  v9999      <- 0
  v433361110 <- ASC433361110 + PRICE433361110 * choice_data$state_cost_433361110 + log(1/(1+exp(-ALPHA_433361110 ))) + ((1/(1+exp(-ALPHA_433361110 ))) - 1) * log(choice_data$volume_liters_433361110 + 1 )
  v648672039 <- ASC648672039 + PRICE648672039 * choice_data$state_cost_648672039 + log(1/(1+exp(-ALPHA_648672039 ))) + ((1/(1+exp(-ALPHA_648672039 ))) - 1) * log(choice_data$volume_liters_648672039 + 1 )
  v433381135 <- ASC433381135 + PRICE433381135 * choice_data$state_cost_433381135 + log(1/(1+exp(-ALPHA_433381135 ))) + ((1/(1+exp(-ALPHA_433381135 ))) - 1) * log(choice_data$volume_liters_433381135 + 1 )
  v11788689  <- ASC11788689  + PRICE11788689  * choice_data$state_cost_11788689  + log(1/(1+exp(-ALPHA_11788689  ))) + ((1/(1+exp(-ALPHA_11788689  ))) - 1) * log(choice_data$volume_liters_11788689  + 1 )
  v35318559  <- ASC35318559  + PRICE35318559  * choice_data$state_cost_35318559  + log(1/(1+exp(-ALPHA_35318559  ))) + ((1/(1+exp(-ALPHA_35318559  ))) - 1) * log(choice_data$volume_liters_35318559  + 1 )
  v268272787 <- ASC268272787 + PRICE268272787 * choice_data$state_cost_268272787 + log(1/(1+exp(-ALPHA_268272787 ))) + ((1/(1+exp(-ALPHA_268272787 ))) - 1) * log(choice_data$volume_liters_268272787 + 1 )
  v373484017 <- ASC373484017 + PRICE373484017 * choice_data$state_cost_373484017 + log(1/(1+exp(-ALPHA_373484017 ))) + ((1/(1+exp(-ALPHA_373484017 ))) - 1) * log(choice_data$volume_liters_373484017 + 1 )
  v359182079 <- ASC359182079 + PRICE359182079 * choice_data$state_cost_359182079 + log(1/(1+exp(-ALPHA_359182079 ))) + ((1/(1+exp(-ALPHA_359182079 ))) - 1) * log(choice_data$volume_liters_359182079 + 1 )
  v363052474 <- ASC363052474 + PRICE363052474 * choice_data$state_cost_363052474 + log(1/(1+exp(-ALPHA_363052474 ))) + ((1/(1+exp(-ALPHA_363052474 ))) - 1) * log(choice_data$volume_liters_363052474 + 1 )
  v363082477 <- ASC363082477 + PRICE363082477 * choice_data$state_cost_363082477 + log(1/(1+exp(-ALPHA_363082477 ))) + ((1/(1+exp(-ALPHA_363082477 ))) - 1) * log(choice_data$volume_liters_363082477 + 1 )
  v433371133 <- ASC433371133 + PRICE433371133 * choice_data$state_cost_433371133 + log(1/(1+exp(-ALPHA_433371133 ))) + ((1/(1+exp(-ALPHA_433371133 ))) - 1) * log(choice_data$volume_liters_433371133 + 1 )
  v124081067 <- ASC124081067 + PRICE124081067 * choice_data$state_cost_124081067 + log(1/(1+exp(-ALPHA_124081067 ))) + ((1/(1+exp(-ALPHA_124081067 ))) - 1) * log(choice_data$volume_liters_124081067 + 1 )
  v43028216  <- ASC43028216  + PRICE43028216  * choice_data$state_cost_43028216  + log(1/(1+exp(-ALPHA_43028216  ))) + ((1/(1+exp(-ALPHA_43028216  ))) - 1) * log(choice_data$volume_liters_43028216  + 1 )
  v35317555  <- ASC35317555  + PRICE35317555  * choice_data$state_cost_35317555  + log(1/(1+exp(-ALPHA_35317555  ))) + ((1/(1+exp(-ALPHA_35317555  ))) - 1) * log(choice_data$volume_liters_35317555  + 1 )
  v11777685  <- ASC11777685  + PRICE11777685  * choice_data$state_cost_11777685  + log(1/(1+exp(-ALPHA_11777685  ))) + ((1/(1+exp(-ALPHA_11777685  ))) - 1) * log(choice_data$volume_liters_11777685  + 1 )
  v112971485 <- ASC112971485 + PRICE112971485 * choice_data$state_cost_112971485 + log(1/(1+exp(-ALPHA_112971485 ))) + ((1/(1+exp(-ALPHA_112971485 ))) - 1) * log(choice_data$volume_liters_112971485 + 1 )
  v256084476 <- ASC256084476 + PRICE256084476 * choice_data$state_cost_256084476 + log(1/(1+exp(-ALPHA_256084476 ))) + ((1/(1+exp(-ALPHA_256084476 ))) - 1) * log(choice_data$volume_liters_256084476 + 1 )
  
  p <-exp(
    .01*(log((1-1/(1 +exp(-ALPHA_9999      )))/(choice_data$volume_liters_9999      + 1) *
            (1- 1/(1 +exp(-ALPHA_433361110 )))/(choice_data$volume_liters_433361110 + 1) *
            (1- 1/(1 +exp(-ALPHA_648672039 )))/(choice_data$volume_liters_648672039 + 1) *
            (1- 1/(1 +exp(-ALPHA_433381135 )))/(choice_data$volume_liters_433381135 + 1) *
            (1- 1/(1 +exp(-ALPHA_11788689  )))/(choice_data$volume_liters_11788689  + 1) *
            (1- 1/(1 +exp(-ALPHA_35318559  )))/(choice_data$volume_liters_35318559  + 1) *
            (1- 1/(1 +exp(-ALPHA_268272787 )))/(choice_data$volume_liters_268272787 + 1) *
            (1- 1/(1 +exp(-ALPHA_373484017 )))/(choice_data$volume_liters_373484017 + 1) *
            (1- 1/(1 +exp(-ALPHA_359182079 )))/(choice_data$volume_liters_359182079 + 1) *
            (1- 1/(1 +exp(-ALPHA_363052474 )))/(choice_data$volume_liters_363052474 + 1) *
            (1- 1/(1 +exp(-ALPHA_363082477 )))/(choice_data$volume_liters_363082477 + 1) *
            (1- 1/(1 +exp(-ALPHA_433371133 )))/(choice_data$volume_liters_433371133 + 1) *
            (1- 1/(1 +exp(-ALPHA_124081067 )))/(choice_data$volume_liters_124081067 + 1) *
            (1- 1/(1 +exp(-ALPHA_43028216  )))/(choice_data$volume_liters_43028216  + 1) *
            (1- 1/(1 +exp(-ALPHA_35317555  )))/(choice_data$volume_liters_35317555  + 1) *
            (1- 1/(1 +exp(-ALPHA_11777685  )))/(choice_data$volume_liters_11777685  + 1) *
            (1- 1/(1 +exp(-ALPHA_112971485 )))/(choice_data$volume_liters_112971485 + 1) *
            (1- 1/(1 +exp(-ALPHA_256084476 )))/(choice_data$volume_liters_256084476 + 1)) +
      log(    (choice_data$volume_liters_9999      + 1 )/(1-1/(1+exp(-ALPHA_9999  ))) +
              (choice_data$volume_liters_433361110 + 1 )/(1-1/(1+exp(-ALPHA_433361110 ))) +
              (choice_data$volume_liters_648672039 + 1 )/(1-1/(1+exp(-ALPHA_648672039 ))) +
              (choice_data$volume_liters_433381135 + 1 )/(1-1/(1+exp(-ALPHA_433381135 ))) +
              (choice_data$volume_liters_11788689  + 1 )/(1-1/(1+exp(-ALPHA_11788689  ))) +
              (choice_data$volume_liters_35318559  + 1 )/(1-1/(1+exp(-ALPHA_35318559  ))) +
              (choice_data$volume_liters_268272787 + 1 )/(1-1/(1+exp(-ALPHA_268272787 ))) +
              (choice_data$volume_liters_373484017 + 1 )/(1-1/(1+exp(-ALPHA_373484017 ))) +
              (choice_data$volume_liters_359182079 + 1 )/(1-1/(1+exp(-ALPHA_359182079 ))) +
              (choice_data$volume_liters_363052474 + 1 )/(1-1/(1+exp(-ALPHA_363052474 ))) +
              (choice_data$volume_liters_363082477 + 1 )/(1-1/(1+exp(-ALPHA_363082477 ))) +
              (choice_data$volume_liters_433371133 + 1 )/(1-1/(1+exp(-ALPHA_433371133 ))) +
              (choice_data$volume_liters_124081067 + 1 )/(1-1/(1+exp(-ALPHA_124081067 ))) +
              (choice_data$volume_liters_43028216  + 1 )/(1-1/(1+exp(-ALPHA_43028216  ))) +
              (choice_data$volume_liters_35317555  + 1 )/(1-1/(1+exp(-ALPHA_35317555  ))) +
              (choice_data$volume_liters_11777685  + 1 )/(1-1/(1+exp(-ALPHA_11777685  ))) +
              (choice_data$volume_liters_112971485 + 1 )/(1-1/(1+exp(-ALPHA_112971485 ))) +
              (choice_data$volume_liters_256084476 + 1 )/(1-1/(1+exp(-ALPHA_256084476 )))) + 
      log(    ifelse(choice_data$chosen_9999      == 1, exp(v9999     ), 1) *
              ifelse(choice_data$chosen_433361110 == 1, exp(v433361110 ), 1) *
              ifelse(choice_data$chosen_648672039 == 1, exp(v648672039 ), 1) *
              ifelse(choice_data$chosen_433381135 == 1, exp(v433381135 ), 1) *
              ifelse(choice_data$chosen_11788689  == 1, exp(v11788689  ), 1) *
              ifelse(choice_data$chosen_35318559  == 1, exp(v35318559  ), 1) *
              ifelse(choice_data$chosen_268272787 == 1, exp(v268272787 ), 1) *
              ifelse(choice_data$chosen_373484017 == 1, exp(v373484017 ), 1) *
              ifelse(choice_data$chosen_359182079 == 1, exp(v359182079 ), 1) *
              ifelse(choice_data$chosen_363052474 == 1, exp(v363052474 ), 1) *
              ifelse(choice_data$chosen_363082477 == 1, exp(v363082477 ), 1) *
              ifelse(choice_data$chosen_433371133 == 1, exp(v433371133 ), 1) *
              ifelse(choice_data$chosen_124081067 == 1, exp(v124081067 ), 1) *
              ifelse(choice_data$chosen_43028216  == 1, exp(v43028216  ), 1) *
              ifelse(choice_data$chosen_35317555  == 1, exp(v35317555  ), 1) *
              ifelse(choice_data$chosen_11777685  == 1, exp(v11777685  ), 1) *
              ifelse(choice_data$chosen_112971485 == 1, exp(v112971485 ), 1) *
              ifelse(choice_data$chosen_256084476 == 1, exp(v256084476 ), 1)) -
      choice_data$countm *
      log(    exp(v9999     ) +
              exp(v433361110 ) +
              exp(v648672039 ) +
              exp(v433381135 ) +
              exp(v11788689  ) +
              exp(v35318559  ) +
              exp(v268272787 ) +
              exp(v373484017 ) +
              exp(v359182079 ) +
              exp(v363052474 ) +
              exp(v363082477 ) +
              exp(v433371133 ) +
              exp(v124081067 ) +
              exp(v43028216  ) +
              exp(v35317555  ) +
              exp(v11777685  ) +
              exp(v112971485 ) +
              exp(v256084476 )) +
      lfactorial(choice_data$countm - 1)))
  return(p)
}	


gVarNamesNormal <- c(
  "PRICE1"       ,
  "ASC9999"     , 
  "ASC433361110 " , 
  "ASC648672039 " , 
  "ASC433381135 " , 
  "ASC11788689  " , 
  "ASC35318559  " , 
  "ASC268272787 " , 
  "ASC373484017 " , 
  "ASC359182079 " , 
  "ASC363052474 " , 
  "ASC363082477 " , 
  "ASC433371133 " , 
  "ASC124081067 " , 
  "ASC43028216  " , 
  "ASC35317555  " , 
  "ASC11777685  " , 
  "ASC112971485 " , 
  "ASC256084476 " ,
  "ALPHA_9999"     , 
  "ALPHA_433361110 " , 
  "ALPHA_648672039 " , 
  "ALPHA_433381135 " , 
  "ALPHA_11788689  " , 
  "ALPHA_35318559  " , 
  "ALPHA_268272787 " , 
  "ALPHA_373484017 " , 
  "ALPHA_359182079 " , 
  "ALPHA_363052474 " , 
  "ALPHA_363082477 " , 
  "ALPHA_433371133 " , 
  "ALPHA_124081067 " , 
  "ALPHA_43028216  " , 
  "ALPHA_35317555  " , 
  "ALPHA_11777685  " , 
  "ALPHA_112971485 " , 
  "ALPHA_256084476 " )
  # "GAMMA_9999"      , 
  # "GAMMA_433361110 "  , 
  # "GAMMA_648672039 "  , 
  # "GAMMA_433381135 "  , 
  # "GAMMA_11788689  "  , 
  # "GAMMA_35318559  "  , 
  # "GAMMA_268272787 "  , 
  # "GAMMA_373484017 "  , 
  # "GAMMA_359182079 "  , 
  # "GAMMA_363052474 "  , 
  # "GAMMA_363082477 "  , 
  # "GAMMA_433371133 "  , 
  # "GAMMA_124081067 "  , 
  # "GAMMA_43028216  "  , 
  # "GAMMA_35317555  "  , 
  # "GAMMA_11777685  "  , 
  # "GAMMA_112971485 "  , 
  # "GAMMA_256084476 "  )

# For each variable, specify the distribution for its coefficient
# The options are:
# 1. normal
# 2. log-nomal
# 3. negative log-normal
# 4. normal with all values below zero massed at zero
# 5. Johnson SB with a specified min and max
# gDIST must have an entry for each value in gVarNamesNormal

gDIST <- c(rep(3,18), rep(1,18), rep(1,18))

# STARTING VALUES
svN <- c(rep(-.1,18), rep(0,18), rep(0,18))
# ITERATION SETTINGS
gNCREP    <- 40000     # Number of iterations to use prior to convergence
gNEREP    <- 4000          # Number of iterations to keep for averaging after convergence has been reached
gNSKIP    <- 2			  # Number of iterations to do in between retaining draws for averaging
gINFOSKIP <- 500         # How frequently to print info about the iteration process
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
write.csv(model[["C"]],"model_output.csv")







