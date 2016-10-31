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

for (i in 1:5){
	choice_data[[paste("nest_",i,"9999     ",sep="_")]] <- 0
	choice_data[[paste("nest_",i,"433361110",sep="_")]] <- 0
	choice_data[[paste("nest_",i,"648672039",sep="_")]] <- 0
	choice_data[[paste("nest_",i,"433381135",sep="_")]] <- 0
	choice_data[[paste("nest_",i,"11788689 ",sep="_")]] <- 0
	choice_data[[paste("nest_",i,"35318559 ",sep="_")]] <- 0
	choice_data[[paste("nest_",i,"268272787",sep="_")]] <- 0
	choice_data[[paste("nest_",i,"373484017",sep="_")]] <- 0
	choice_data[[paste("nest_",i,"359182079",sep="_")]] <- 0
	choice_data[[paste("nest_",i,"363052474",sep="_")]] <- 0
	choice_data[[paste("nest_",i,"363082477",sep="_")]] <- 0
	choice_data[[paste("nest_",i,"433371133",sep="_")]] <- 0
	choice_data[[paste("nest_",i,"124081067",sep="_")]] <- 0
	choice_data[[paste("nest_",i,"43028216 ",sep="_")]] <- 0
	choice_data[[paste("nest_",i,"35317555 ",sep="_")]] <- 0
	choice_data[[paste("nest_",i,"11777685 ",sep="_")]] <- 0
	choice_data[[paste("nest_",i,"112971485",sep="_")]] <- 0
	choice_data[[paste("nest_",i,"256084476",sep="_")]] <- 0
	}

	choice_data$nest_1_433361110 = 1
	choice_data$nest_2_648672039 = 1
	choice_data$nest_3_433381135 = 1
	choice_data$nest_4_11788689  = 1
	choice_data$nest_1_35318559  = 1
	choice_data$nest_2_268272787 = 1
	choice_data$nest_3_373484017 = 1
	choice_data$nest_4_359182079 = 1
	choice_data$nest_5_363052474 = 1
	choice_data$nest_5_363082477 = 1
	choice_data$nest_1_433371133 = 1
	choice_data$nest_2_124081067 = 1
	choice_data$nest_3_43028216  = 1
	choice_data$nest_4_35317555  = 1
	choice_data$nest_5_11777685  = 1
	choice_data$nest_3_112971485 = 1
  choice_data$nest_4_256084476 = 1

# The likelihood function
likelihood <- function(fc, b) {
  # Assign fixed parameters to named variables for convenience
  cc <- 1
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
  
  
  cc <- 1
  lambda_unset_1 <- fc[cc]; cc <- cc + 1
  lambda_unset_2 <- fc[cc]; cc <- cc + 1
  lambda_unset_3 <- fc[cc]; cc <- cc + 1
  lambda_unset_4 <- fc[cc]; cc <- cc + 1
  lambda_unset_5 <- fc[cc];
  
  lambda1 <- 1/(1+exp(-lambda_unset_1))
  lambda2 <- 1/(1+exp(-lambda_unset_2))
  lambda3 <- 1/(1+exp(-lambda_unset_3))
  lambda4 <- 1/(1+exp(-lambda_unset_4))
  lambda5 <- 1/(1+exp(-lambda_unset_5))
  
  v9999      <- 0
  v433361110 <- ASC433361110 + PRICE433361110 * choice_data$state_cost_433361110
  v648672039 <- ASC648672039 + PRICE648672039 * choice_data$state_cost_648672039
  v433381135 <- ASC433381135 + PRICE433381135 * choice_data$state_cost_433381135
  v11788689  <- ASC11788689  + PRICE11788689  * choice_data$state_cost_11788689 
  v35318559  <- ASC35318559  + PRICE35318559  * choice_data$state_cost_35318559 
  v268272787 <- ASC268272787 + PRICE268272787 * choice_data$state_cost_268272787
  v373484017 <- ASC373484017 + PRICE373484017 * choice_data$state_cost_373484017
  v359182079 <- ASC359182079 + PRICE359182079 * choice_data$state_cost_359182079
  v363052474 <- ASC363052474 + PRICE363052474 * choice_data$state_cost_363052474
  v363082477 <- ASC363082477 + PRICE363082477 * choice_data$state_cost_363082477
  v433371133 <- ASC433371133 + PRICE433371133 * choice_data$state_cost_433371133
  v124081067 <- ASC124081067 + PRICE124081067 * choice_data$state_cost_124081067
  v43028216  <- ASC43028216  + PRICE43028216  * choice_data$state_cost_43028216 
  v35317555  <- ASC35317555  + PRICE35317555  * choice_data$state_cost_35317555 
  v11777685  <- ASC11777685  + PRICE11777685  * choice_data$state_cost_11777685 
  v112971485 <- ASC112971485 + PRICE112971485 * choice_data$state_cost_112971485
  v256084476 <- ASC256084476 + PRICE256084476 * choice_data$state_cost_256084476
  
  sum1 <- choice_data$available_433361110 * exp(v433361110/lambda1) * choice_data$nest_1_433361110 + 
		      choice_data$available_648672039 * exp(v648672039/lambda1) * choice_data$nest_1_648672039 + 
		      choice_data$available_433381135 * exp(v433381135/lambda1) * choice_data$nest_1_433381135 + 
		      choice_data$available_11788689  * exp(v11788689 /lambda1) * choice_data$nest_1_11788689  + 
		      choice_data$available_35318559  * exp(v35318559 /lambda1) * choice_data$nest_1_35318559  + 
		      choice_data$available_268272787 * exp(v268272787/lambda1) * choice_data$nest_1_268272787 + 
		      choice_data$available_373484017 * exp(v373484017/lambda1) * choice_data$nest_1_373484017 + 
		      choice_data$available_359182079 * exp(v359182079/lambda1) * choice_data$nest_1_359182079 + 
		      choice_data$available_363052474 * exp(v363052474/lambda1) * choice_data$nest_1_363052474 + 
		      choice_data$available_363082477 * exp(v363082477/lambda1) * choice_data$nest_1_363082477 + 
		      choice_data$available_433371133 * exp(v433371133/lambda1) * choice_data$nest_1_433371133 + 
		      choice_data$available_124081067 * exp(v124081067/lambda1) * choice_data$nest_1_124081067 + 
		      choice_data$available_43028216  * exp(v43028216 /lambda1) * choice_data$nest_1_43028216  + 
		      choice_data$available_35317555  * exp(v35317555 /lambda1) * choice_data$nest_1_35317555  + 
		      choice_data$available_11777685  * exp(v11777685 /lambda1) * choice_data$nest_1_11777685  + 
		      choice_data$available_112971485 * exp(v112971485/lambda1) * choice_data$nest_1_112971485 + 
		      choice_data$available_256084476 * exp(v256084476/lambda1) * choice_data$nest_1_256084476 
		  
  sum2 <- choice_data$available_433361110 * exp(v433361110/lambda2) * choice_data$nest_2_433361110 + 
		      choice_data$available_648672039 * exp(v648672039/lambda2) * choice_data$nest_2_648672039 + 
		      choice_data$available_433381135 * exp(v433381135/lambda2) * choice_data$nest_2_433381135 + 
		      choice_data$available_11788689  * exp(v11788689 /lambda2) * choice_data$nest_2_11788689  + 
		      choice_data$available_35318559  * exp(v35318559 /lambda2) * choice_data$nest_2_35318559  + 
		      choice_data$available_268272787 * exp(v268272787/lambda2) * choice_data$nest_2_268272787 + 
		      choice_data$available_373484017 * exp(v373484017/lambda2) * choice_data$nest_2_373484017 + 
		      choice_data$available_359182079 * exp(v359182079/lambda2) * choice_data$nest_2_359182079 + 
		      choice_data$available_363052474 * exp(v363052474/lambda2) * choice_data$nest_2_363052474 + 
		      choice_data$available_363082477 * exp(v363082477/lambda2) * choice_data$nest_2_363082477 + 
		      choice_data$available_433371133 * exp(v433371133/lambda2) * choice_data$nest_2_433371133 + 
		      choice_data$available_124081067 * exp(v124081067/lambda2) * choice_data$nest_2_124081067 + 
		      choice_data$available_43028216  * exp(v43028216 /lambda2) * choice_data$nest_2_43028216  + 
		      choice_data$available_35317555  * exp(v35317555 /lambda2) * choice_data$nest_2_35317555  + 
		      choice_data$available_11777685  * exp(v11777685 /lambda2) * choice_data$nest_2_11777685  + 
		      choice_data$available_112971485 * exp(v112971485/lambda2) * choice_data$nest_2_112971485 + 
		      choice_data$available_256084476 * exp(v256084476/lambda2) * choice_data$nest_2_256084476 
		  
  sum3 <- choice_data$available_433361110 * exp(v433361110/lambda3) * choice_data$nest_3_433361110 + 
		      choice_data$available_648672039 * exp(v648672039/lambda3) * choice_data$nest_3_648672039 + 
		      choice_data$available_433381135 * exp(v433381135/lambda3) * choice_data$nest_3_433381135 + 
		      choice_data$available_11788689  * exp(v11788689 /lambda3) * choice_data$nest_3_11788689  + 
		      choice_data$available_35318559  * exp(v35318559 /lambda3) * choice_data$nest_3_35318559  + 
		      choice_data$available_268272787 * exp(v268272787/lambda3) * choice_data$nest_3_268272787 + 
		      choice_data$available_373484017 * exp(v373484017/lambda3) * choice_data$nest_3_373484017 + 
		      choice_data$available_359182079 * exp(v359182079/lambda3) * choice_data$nest_3_359182079 + 
		      choice_data$available_363052474 * exp(v363052474/lambda3) * choice_data$nest_3_363052474 + 
		      choice_data$available_363082477 * exp(v363082477/lambda3) * choice_data$nest_3_363082477 + 
		      choice_data$available_433371133 * exp(v433371133/lambda3) * choice_data$nest_3_433371133 + 
		      choice_data$available_124081067 * exp(v124081067/lambda3) * choice_data$nest_3_124081067 + 
		      choice_data$available_43028216  * exp(v43028216 /lambda3) * choice_data$nest_3_43028216  + 
		      choice_data$available_35317555  * exp(v35317555 /lambda3) * choice_data$nest_3_35317555  + 
		      choice_data$available_11777685  * exp(v11777685 /lambda3) * choice_data$nest_3_11777685  + 
		      choice_data$available_112971485 * exp(v112971485/lambda3) * choice_data$nest_3_112971485 + 
		      choice_data$available_256084476 * exp(v256084476/lambda3) * choice_data$nest_3_256084476 
		  
  sum4 <- choice_data$available_433361110 * exp(v433361110/lambda4) * choice_data$nest_4_433361110 + 
		      choice_data$available_648672039 * exp(v648672039/lambda4) * choice_data$nest_4_648672039 + 
		      choice_data$available_433381135 * exp(v433381135/lambda4) * choice_data$nest_4_433381135 + 
		      choice_data$available_11788689  * exp(v11788689 /lambda4) * choice_data$nest_4_11788689  + 
		      choice_data$available_35318559  * exp(v35318559 /lambda4) * choice_data$nest_4_35318559  + 
		      choice_data$available_268272787 * exp(v268272787/lambda4) * choice_data$nest_4_268272787 + 
		      choice_data$available_373484017 * exp(v373484017/lambda4) * choice_data$nest_4_373484017 + 
		      choice_data$available_359182079 * exp(v359182079/lambda4) * choice_data$nest_4_359182079 + 
		      choice_data$available_363052474 * exp(v363052474/lambda4) * choice_data$nest_4_363052474 + 
		      choice_data$available_363082477 * exp(v363082477/lambda4) * choice_data$nest_4_363082477 + 
		      choice_data$available_433371133 * exp(v433371133/lambda4) * choice_data$nest_4_433371133 + 
		      choice_data$available_124081067 * exp(v124081067/lambda4) * choice_data$nest_4_124081067 + 
		      choice_data$available_43028216  * exp(v43028216 /lambda4) * choice_data$nest_4_43028216  + 
		      choice_data$available_35317555  * exp(v35317555 /lambda4) * choice_data$nest_4_35317555  + 
		      choice_data$available_11777685  * exp(v11777685 /lambda4) * choice_data$nest_4_11777685  + 
		      choice_data$available_112971485 * exp(v112971485/lambda4) * choice_data$nest_4_112971485 + 
		      choice_data$available_256084476 * exp(v256084476/lambda4) * choice_data$nest_4_256084476 
		  
  sum5 <- choice_data$available_433361110 * exp(v433361110/lambda5) * choice_data$nest_5_433361110 + 
		      choice_data$available_648672039 * exp(v648672039/lambda5) * choice_data$nest_5_648672039 + 
		      choice_data$available_433381135 * exp(v433381135/lambda5) * choice_data$nest_5_433381135 + 
		      choice_data$available_11788689  * exp(v11788689 /lambda5) * choice_data$nest_5_11788689  + 
		      choice_data$available_35318559  * exp(v35318559 /lambda5) * choice_data$nest_5_35318559  + 
		      choice_data$available_268272787 * exp(v268272787/lambda5) * choice_data$nest_5_268272787 + 
		      choice_data$available_373484017 * exp(v373484017/lambda5) * choice_data$nest_5_373484017 + 
		      choice_data$available_359182079 * exp(v359182079/lambda5) * choice_data$nest_5_359182079 + 
		      choice_data$available_363052474 * exp(v363052474/lambda5) * choice_data$nest_5_363052474 + 
		      choice_data$available_363082477 * exp(v363082477/lambda5) * choice_data$nest_5_363082477 + 
		      choice_data$available_433371133 * exp(v433371133/lambda5) * choice_data$nest_5_433371133 + 
		      choice_data$available_124081067 * exp(v124081067/lambda5) * choice_data$nest_5_124081067 + 
		      choice_data$available_43028216  * exp(v43028216 /lambda5) * choice_data$nest_5_43028216  + 
		      choice_data$available_35317555  * exp(v35317555 /lambda5) * choice_data$nest_5_35317555  + 
		      choice_data$available_11777685  * exp(v11777685 /lambda5) * choice_data$nest_5_11777685  + 
		      choice_data$available_112971485 * exp(v112971485/lambda5) * choice_data$nest_5_112971485 + 
		      choice_data$available_256084476 * exp(v256084476/lambda5) * choice_data$nest_5_256084476 
  
  choicesum1 <- choice_data$available_433361110 * choice_data$chosen_433361110 * exp(v433361110/lambda1) * choice_data$nest_1_433361110 + 
				        choice_data$available_648672039 * choice_data$chosen_648672039 * exp(v648672039/lambda1) * choice_data$nest_1_648672039 + 
				        choice_data$available_433381135 * choice_data$chosen_433381135 * exp(v433381135/lambda1) * choice_data$nest_1_433381135 + 
				        choice_data$available_11788689  * choice_data$chosen_11788689  * exp(v11788689 /lambda1) * choice_data$nest_1_11788689  + 
				        choice_data$available_35318559  * choice_data$chosen_35318559  * exp(v35318559 /lambda1) * choice_data$nest_1_35318559  + 
				        choice_data$available_268272787 * choice_data$chosen_268272787 * exp(v268272787/lambda1) * choice_data$nest_1_268272787 + 
				        choice_data$available_373484017 * choice_data$chosen_373484017 * exp(v373484017/lambda1) * choice_data$nest_1_373484017 + 
				        choice_data$available_359182079 * choice_data$chosen_359182079 * exp(v359182079/lambda1) * choice_data$nest_1_359182079 + 
				        choice_data$available_363052474 * choice_data$chosen_363052474 * exp(v363052474/lambda1) * choice_data$nest_1_363052474 + 
				        choice_data$available_363082477 * choice_data$chosen_363082477 * exp(v363082477/lambda1) * choice_data$nest_1_363082477 + 
				        choice_data$available_433371133 * choice_data$chosen_433371133 * exp(v433371133/lambda1) * choice_data$nest_1_433371133 + 
				        choice_data$available_124081067 * choice_data$chosen_124081067 * exp(v124081067/lambda1) * choice_data$nest_1_124081067 + 
				        choice_data$available_43028216  * choice_data$chosen_43028216  * exp(v43028216 /lambda1) * choice_data$nest_1_43028216  + 
				        choice_data$available_35317555  * choice_data$chosen_35317555  * exp(v35317555 /lambda1) * choice_data$nest_1_35317555  + 
				        choice_data$available_11777685  * choice_data$chosen_11777685  * exp(v11777685 /lambda1) * choice_data$nest_1_11777685  + 
				        choice_data$available_112971485 * choice_data$chosen_112971485 * exp(v112971485/lambda1) * choice_data$nest_1_112971485 + 
				        choice_data$available_256084476 * choice_data$chosen_256084476 * exp(v256084476/lambda1) * choice_data$nest_1_256084476 
               
  choicesum2 <- choice_data$available_433361110 * choice_data$chosen_433361110 * exp(v433361110/lambda2) * choice_data$nest_2_433361110 + 
				        choice_data$available_648672039 * choice_data$chosen_648672039 * exp(v648672039/lambda2) * choice_data$nest_2_648672039 + 
				        choice_data$available_433381135 * choice_data$chosen_433381135 * exp(v433381135/lambda2) * choice_data$nest_2_433381135 + 
				        choice_data$available_11788689  * choice_data$chosen_11788689  * exp(v11788689 /lambda2) * choice_data$nest_2_11788689  + 
				        choice_data$available_35318559  * choice_data$chosen_35318559  * exp(v35318559 /lambda2) * choice_data$nest_2_35318559  + 
				        choice_data$available_268272787 * choice_data$chosen_268272787 * exp(v268272787/lambda2) * choice_data$nest_2_268272787 + 
				        choice_data$available_373484017 * choice_data$chosen_373484017 * exp(v373484017/lambda2) * choice_data$nest_2_373484017 + 
				        choice_data$available_359182079 * choice_data$chosen_359182079 * exp(v359182079/lambda2) * choice_data$nest_2_359182079 + 
				        choice_data$available_363052474 * choice_data$chosen_363052474 * exp(v363052474/lambda2) * choice_data$nest_2_363052474 + 
				        choice_data$available_363082477 * choice_data$chosen_363082477 * exp(v363082477/lambda2) * choice_data$nest_2_363082477 + 
				        choice_data$available_433371133 * choice_data$chosen_433371133 * exp(v433371133/lambda2) * choice_data$nest_2_433371133 + 
				        choice_data$available_124081067 * choice_data$chosen_124081067 * exp(v124081067/lambda2) * choice_data$nest_2_124081067 + 
				        choice_data$available_43028216  * choice_data$chosen_43028216  * exp(v43028216 /lambda2) * choice_data$nest_2_43028216  + 
				        choice_data$available_35317555  * choice_data$chosen_35317555  * exp(v35317555 /lambda2) * choice_data$nest_2_35317555  + 
				        choice_data$available_11777685  * choice_data$chosen_11777685  * exp(v11777685 /lambda2) * choice_data$nest_2_11777685  + 
				        choice_data$available_112971485 * choice_data$chosen_112971485 * exp(v112971485/lambda2) * choice_data$nest_2_112971485 + 
				        choice_data$available_256084476 * choice_data$chosen_256084476 * exp(v256084476/lambda2) * choice_data$nest_2_256084476 
               
  choicesum3 <- choice_data$available_433361110 * choice_data$chosen_433361110 * exp(v433361110/lambda3) * choice_data$nest_3_433361110 + 
				        choice_data$available_648672039 * choice_data$chosen_648672039 * exp(v648672039/lambda3) * choice_data$nest_3_648672039 + 
				        choice_data$available_433381135 * choice_data$chosen_433381135 * exp(v433381135/lambda3) * choice_data$nest_3_433381135 + 
				        choice_data$available_11788689  * choice_data$chosen_11788689  * exp(v11788689 /lambda3) * choice_data$nest_3_11788689  + 
				        choice_data$available_35318559  * choice_data$chosen_35318559  * exp(v35318559 /lambda3) * choice_data$nest_3_35318559  + 
				        choice_data$available_268272787 * choice_data$chosen_268272787 * exp(v268272787/lambda3) * choice_data$nest_3_268272787 + 
				        choice_data$available_373484017 * choice_data$chosen_373484017 * exp(v373484017/lambda3) * choice_data$nest_3_373484017 + 
				        choice_data$available_359182079 * choice_data$chosen_359182079 * exp(v359182079/lambda3) * choice_data$nest_3_359182079 + 
				        choice_data$available_363052474 * choice_data$chosen_363052474 * exp(v363052474/lambda3) * choice_data$nest_3_363052474 + 
				        choice_data$available_363082477 * choice_data$chosen_363082477 * exp(v363082477/lambda3) * choice_data$nest_3_363082477 + 
				        choice_data$available_433371133 * choice_data$chosen_433371133 * exp(v433371133/lambda3) * choice_data$nest_3_433371133 + 
				        choice_data$available_124081067 * choice_data$chosen_124081067 * exp(v124081067/lambda3) * choice_data$nest_3_124081067 + 
				        choice_data$available_43028216  * choice_data$chosen_43028216  * exp(v43028216 /lambda3) * choice_data$nest_3_43028216  + 
				        choice_data$available_35317555  * choice_data$chosen_35317555  * exp(v35317555 /lambda3) * choice_data$nest_3_35317555  + 
				        choice_data$available_11777685  * choice_data$chosen_11777685  * exp(v11777685 /lambda3) * choice_data$nest_3_11777685  + 
				        choice_data$available_112971485 * choice_data$chosen_112971485 * exp(v112971485/lambda3) * choice_data$nest_3_112971485 + 
				        choice_data$available_256084476 * choice_data$chosen_256084476 * exp(v256084476/lambda3) * choice_data$nest_3_256084476 
               
  choicesum4 <- choice_data$available_433361110 * choice_data$chosen_433361110 * exp(v433361110/lambda4) * choice_data$nest_4_433361110 + 
				        choice_data$available_648672039 * choice_data$chosen_648672039 * exp(v648672039/lambda4) * choice_data$nest_4_648672039 + 
				        choice_data$available_433381135 * choice_data$chosen_433381135 * exp(v433381135/lambda4) * choice_data$nest_4_433381135 + 
				        choice_data$available_11788689  * choice_data$chosen_11788689  * exp(v11788689 /lambda4) * choice_data$nest_4_11788689  + 
				        choice_data$available_35318559  * choice_data$chosen_35318559  * exp(v35318559 /lambda4) * choice_data$nest_4_35318559  + 
				        choice_data$available_268272787 * choice_data$chosen_268272787 * exp(v268272787/lambda4) * choice_data$nest_4_268272787 + 
				        choice_data$available_373484017 * choice_data$chosen_373484017 * exp(v373484017/lambda4) * choice_data$nest_4_373484017 + 
				        choice_data$available_359182079 * choice_data$chosen_359182079 * exp(v359182079/lambda4) * choice_data$nest_4_359182079 + 
				        choice_data$available_363052474 * choice_data$chosen_363052474 * exp(v363052474/lambda4) * choice_data$nest_4_363052474 + 
				        choice_data$available_363082477 * choice_data$chosen_363082477 * exp(v363082477/lambda4) * choice_data$nest_4_363082477 + 
				        choice_data$available_433371133 * choice_data$chosen_433371133 * exp(v433371133/lambda4) * choice_data$nest_4_433371133 + 
				        choice_data$available_124081067 * choice_data$chosen_124081067 * exp(v124081067/lambda4) * choice_data$nest_4_124081067 + 
				        choice_data$available_43028216  * choice_data$chosen_43028216  * exp(v43028216 /lambda4) * choice_data$nest_4_43028216  + 
				        choice_data$available_35317555  * choice_data$chosen_35317555  * exp(v35317555 /lambda4) * choice_data$nest_4_35317555  + 
				        choice_data$available_11777685  * choice_data$chosen_11777685  * exp(v11777685 /lambda4) * choice_data$nest_4_11777685  + 
				        choice_data$available_112971485 * choice_data$chosen_112971485 * exp(v112971485/lambda4) * choice_data$nest_4_112971485 + 
				        choice_data$available_256084476 * choice_data$chosen_256084476 * exp(v256084476/lambda4) * choice_data$nest_4_256084476 
               
  choicesum5 <- choice_data$available_433361110 * choice_data$chosen_433361110 * exp(v433361110/lambda5) * choice_data$nest_5_433361110 + 
				        choice_data$available_648672039 * choice_data$chosen_648672039 * exp(v648672039/lambda5) * choice_data$nest_5_648672039 + 
				        choice_data$available_433381135 * choice_data$chosen_433381135 * exp(v433381135/lambda5) * choice_data$nest_5_433381135 + 
				        choice_data$available_11788689  * choice_data$chosen_11788689  * exp(v11788689 /lambda5) * choice_data$nest_5_11788689  + 
				        choice_data$available_35318559  * choice_data$chosen_35318559  * exp(v35318559 /lambda5) * choice_data$nest_5_35318559  + 
				        choice_data$available_268272787 * choice_data$chosen_268272787 * exp(v268272787/lambda5) * choice_data$nest_5_268272787 + 
				        choice_data$available_373484017 * choice_data$chosen_373484017 * exp(v373484017/lambda5) * choice_data$nest_5_373484017 + 
				        choice_data$available_359182079 * choice_data$chosen_359182079 * exp(v359182079/lambda5) * choice_data$nest_5_359182079 + 
				        choice_data$available_363052474 * choice_data$chosen_363052474 * exp(v363052474/lambda5) * choice_data$nest_5_363052474 + 
				        choice_data$available_363082477 * choice_data$chosen_363082477 * exp(v363082477/lambda5) * choice_data$nest_5_363082477 + 
				        choice_data$available_433371133 * choice_data$chosen_433371133 * exp(v433371133/lambda5) * choice_data$nest_5_433371133 + 
				        choice_data$available_124081067 * choice_data$chosen_124081067 * exp(v124081067/lambda5) * choice_data$nest_5_124081067 + 
				        choice_data$available_43028216  * choice_data$chosen_43028216  * exp(v43028216 /lambda5) * choice_data$nest_5_43028216  + 
				        choice_data$available_35317555  * choice_data$chosen_35317555  * exp(v35317555 /lambda5) * choice_data$nest_5_35317555  + 
				        choice_data$available_11777685  * choice_data$chosen_11777685  * exp(v11777685 /lambda5) * choice_data$nest_5_11777685  + 
				        choice_data$available_112971485 * choice_data$chosen_112971485 * exp(v112971485/lambda5) * choice_data$nest_5_112971485 + 
				        choice_data$available_256084476 * choice_data$chosen_256084476 * exp(v256084476/lambda5) * choice_data$nest_5_256084476 
		  
		  
		  logsum1  <- ifelse(sum1  == 0,0,log(sum1 ))
		  logsum2  <- ifelse(sum2  == 0,0,log(sum2 ))  
		  logsum3  <- ifelse(sum3  == 0,0,log(sum3 ))  
		  logsum4  <- ifelse(sum4  == 0,0,log(sum4 ))  
		  logsum5  <- ifelse(sum5  == 0,0,log(sum5 ))  
		  
		  prob_nest <-  
                (exp(lambda1 * logsum1) * (choice_data$chosen_433361110 * choice_data$nest_1_433361110 + 
											                      choice_data$chosen_648672039 * choice_data$nest_1_648672039 + 
											                      choice_data$chosen_433381135 * choice_data$nest_1_433381135 + 
											                      choice_data$chosen_11788689  * choice_data$nest_1_11788689  + 
											                      choice_data$chosen_35318559  * choice_data$nest_1_35318559  + 
											                      choice_data$chosen_268272787 * choice_data$nest_1_268272787 + 
											                      choice_data$chosen_373484017 * choice_data$nest_1_373484017 + 
											                      choice_data$chosen_359182079 * choice_data$nest_1_359182079 + 
											                      choice_data$chosen_363052474 * choice_data$nest_1_363052474 + 
											                      choice_data$chosen_363082477 * choice_data$nest_1_363082477 + 
											                      choice_data$chosen_433371133 * choice_data$nest_1_433371133 + 
											                      choice_data$chosen_124081067 * choice_data$nest_1_124081067 + 
											                      choice_data$chosen_43028216  * choice_data$nest_1_43028216  + 
											                      choice_data$chosen_35317555  * choice_data$nest_1_35317555  + 
											                      choice_data$chosen_11777685  * choice_data$nest_1_11777685  + 
											                      choice_data$chosen_112971485 * choice_data$nest_1_112971485 + 
											                      choice_data$chosen_256084476 * choice_data$nest_1_256084476) + 
                  exp(lambda2 * logsum2) * (choice_data$chosen_433361110 * choice_data$nest_2_433361110 + 
											                      choice_data$chosen_648672039 * choice_data$nest_2_648672039 + 
											                      choice_data$chosen_433381135 * choice_data$nest_2_433381135 + 
											                      choice_data$chosen_11788689  * choice_data$nest_2_11788689  + 
											                      choice_data$chosen_35318559  * choice_data$nest_2_35318559  + 
											                      choice_data$chosen_268272787 * choice_data$nest_2_268272787 + 
											                      choice_data$chosen_373484017 * choice_data$nest_2_373484017 + 
											                      choice_data$chosen_359182079 * choice_data$nest_2_359182079 + 
											                      choice_data$chosen_363052474 * choice_data$nest_2_363052474 + 
											                      choice_data$chosen_363082477 * choice_data$nest_2_363082477 + 
											                      choice_data$chosen_433371133 * choice_data$nest_2_433371133 + 
											                      choice_data$chosen_124081067 * choice_data$nest_2_124081067 + 
											                      choice_data$chosen_43028216  * choice_data$nest_2_43028216  + 
											                      choice_data$chosen_35317555  * choice_data$nest_2_35317555  + 
											                      choice_data$chosen_11777685  * choice_data$nest_2_11777685  + 
											                      choice_data$chosen_112971485 * choice_data$nest_2_112971485 + 
											                      choice_data$chosen_256084476 * choice_data$nest_2_256084476) + 
                  exp(lambda3 * logsum3) * (choice_data$chosen_433361110 * choice_data$nest_3_433361110 + 
											                      choice_data$chosen_648672039 * choice_data$nest_3_648672039 + 
											                      choice_data$chosen_433381135 * choice_data$nest_3_433381135 + 
											                      choice_data$chosen_11788689  * choice_data$nest_3_11788689  + 
											                      choice_data$chosen_35318559  * choice_data$nest_3_35318559  + 
											                      choice_data$chosen_268272787 * choice_data$nest_3_268272787 + 
											                      choice_data$chosen_373484017 * choice_data$nest_3_373484017 + 
											                      choice_data$chosen_359182079 * choice_data$nest_3_359182079 + 
											                      choice_data$chosen_363052474 * choice_data$nest_3_363052474 + 
											                      choice_data$chosen_363082477 * choice_data$nest_3_363082477 + 
											                      choice_data$chosen_433371133 * choice_data$nest_3_433371133 + 
											                      choice_data$chosen_124081067 * choice_data$nest_3_124081067 + 
											                      choice_data$chosen_43028216  * choice_data$nest_3_43028216  + 
											                      choice_data$chosen_35317555  * choice_data$nest_3_35317555  + 
											                      choice_data$chosen_11777685  * choice_data$nest_3_11777685  + 
											                      choice_data$chosen_112971485 * choice_data$nest_3_112971485 + 
											                      choice_data$chosen_256084476 * choice_data$nest_3_256084476) + 
                  exp(lambda4 * logsum4) * (choice_data$chosen_433361110 * choice_data$nest_4_433361110 + 
											                      choice_data$chosen_648672039 * choice_data$nest_4_648672039 + 
											                      choice_data$chosen_433381135 * choice_data$nest_4_433381135 + 
											                      choice_data$chosen_11788689  * choice_data$nest_4_11788689  + 
											                      choice_data$chosen_35318559  * choice_data$nest_4_35318559  + 
											                      choice_data$chosen_268272787 * choice_data$nest_4_268272787 + 
											                      choice_data$chosen_373484017 * choice_data$nest_4_373484017 + 
											                      choice_data$chosen_359182079 * choice_data$nest_4_359182079 + 
											                      choice_data$chosen_363052474 * choice_data$nest_4_363052474 + 
											                      choice_data$chosen_363082477 * choice_data$nest_4_363082477 + 
											                      choice_data$chosen_433371133 * choice_data$nest_4_433371133 + 
											                      choice_data$chosen_124081067 * choice_data$nest_4_124081067 + 
											                      choice_data$chosen_43028216  * choice_data$nest_4_43028216  + 
											                      choice_data$chosen_35317555  * choice_data$nest_4_35317555  + 
											                      choice_data$chosen_11777685  * choice_data$nest_4_11777685  + 
											                      choice_data$chosen_112971485 * choice_data$nest_4_112971485 + 
											                      choice_data$chosen_256084476 * choice_data$nest_4_256084476) + 
                  exp(lambda5 * logsum5) * (choice_data$chosen_433361110 * choice_data$nest_5_433361110 + 
											                      choice_data$chosen_648672039 * choice_data$nest_5_648672039 + 
											                      choice_data$chosen_433381135 * choice_data$nest_5_433381135 + 
											                      choice_data$chosen_11788689  * choice_data$nest_5_11788689  + 
											                      choice_data$chosen_35318559  * choice_data$nest_5_35318559  + 
											                      choice_data$chosen_268272787 * choice_data$nest_5_268272787 + 
											                      choice_data$chosen_373484017 * choice_data$nest_5_373484017 + 
											                      choice_data$chosen_359182079 * choice_data$nest_5_359182079 + 
											                      choice_data$chosen_363052474 * choice_data$nest_5_363052474 + 
											                      choice_data$chosen_363082477 * choice_data$nest_5_363082477 + 
											                      choice_data$chosen_433371133 * choice_data$nest_5_433371133 + 
											                      choice_data$chosen_124081067 * choice_data$nest_5_124081067 + 
											                      choice_data$chosen_43028216  * choice_data$nest_5_43028216  + 
											                      choice_data$chosen_35317555  * choice_data$nest_5_35317555  + 
											                      choice_data$chosen_11777685  * choice_data$nest_5_11777685  + 
											                      choice_data$chosen_112971485 * choice_data$nest_5_112971485 + 
											                      choice_data$chosen_256084476 * choice_data$nest_5_256084476))/ 
				  (exp(lambda1  * logsum1 ) +
				   exp(lambda2  * logsum2 ) +
				   exp(lambda3  * logsum3 ) +
				   exp(lambda4  * logsum4 ) +
				   exp(lambda5  * logsum5 ))
		  
  prob_win_nest <- 
				   ifelse(sum1  == 0, 0, choicesum1 /sum1 ) +
				   ifelse(sum2  == 0, 0, choicesum2 /sum2 ) +
				   ifelse(sum3  == 0, 0, choicesum3 /sum3 ) +
				   ifelse(sum4  == 0, 0, choicesum4 /sum4 ) +
				   ifelse(sum5  == 0, 0, choicesum5 /sum5 )
  
  p <- prob_nest * prob_win_nest

  return(p)
}	


gVarNamesNormal <- c(
  "PRICE433361110"       ,
  "PRICE648672039"       ,
  "PRICE433381135"       ,
  "PRICE11788689 "       ,
  "PRICE35318559 "       ,
  "PRICE268272787"       ,
  "PRICE373484017"       ,
  "PRICE359182079"       ,
  "PRICE363052474"       ,
  "PRICE363082477"       ,
  "PRICE433371133"       ,
  "PRICE124081067"       ,
  "PRICE43028216 "       ,
  "PRICE35317555 "       ,
  "PRICE11777685 "       ,
  "PRICE112971485"       ,
  "PRICE256084476"       ,
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
  "ASC256084476 " )

# Names for the fixed variables
gVarNamesFixed <- c("lambda_unset_1", "lambda_unset_2", "lambda_unset_3", "lambda_unset_4", "lambda_unset_5")
# For each variable, specify the distribution for its coefficient
# The options are:
# 1. normal
# 2. log-nomal
# 3. negative log-normal
# 4. normal with all values below zero massed at zero
# 5. Johnson SB with a specified min and max
# gDIST must have an entry for each value in gVarNamesNormal

gDIST <- c(rep(3,17), rep(1,17))

# STARTING VALUES
svN <- c(rep(-.1,17), rep(0,17))
FC  <- c(rep(.5,5))             # for the fixed coefficients
# ITERATION SETTINGS
gNCREP    <- 40000     # Number of iterations to use prior to convergence
gNEREP    <- 4000          # Number of iterations to keep for averaging after convergence has been reached
gNSKIP    <- 2			  # Number of iterations to do in between retaining draws for averaging
gINFOSKIP <- 500         # How frequently to print info about the iteration process
modelname = "test1"
control <- list(
  modelname = modelname,
  gVarNamesNormal = gVarNamesNormal,
  gVarNamesFixed = gVarNamesFixed,
  svN = svN,
  FC = FC,
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







