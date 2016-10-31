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

choice_data$countm <-     (as.numeric(choice_data$volume_liters_9999  > 0) +
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
                           as.numeric(choice_data$volume_liters_44217554  > 0) +
                           as.numeric(choice_data$volume_liters_68036500  > 0) +
                           as.numeric(choice_data$volume_liters_68037498  > 0) +
                           as.numeric(choice_data$volume_liters_88283243  > 0) +
                           as.numeric(choice_data$volume_liters_100084433  > 0) +
                           as.numeric(choice_data$volume_liters_106271048  > 0) +
                           as.numeric(choice_data$volume_liters_106281053  > 0) +
                           as.numeric(choice_data$volume_liters_108071511  > 0) +
                           as.numeric(choice_data$volume_liters_112961476  > 0) +
                           as.numeric(choice_data$volume_liters_112971485  > 0) +
                           as.numeric(choice_data$volume_liters_112981488  > 0) +
                           as.numeric(choice_data$volume_liters_124071064  > 0) +
                           as.numeric(choice_data$volume_liters_124081067  > 0) +
                           as.numeric(choice_data$volume_liters_124781072  > 0) +
                           as.numeric(choice_data$volume_liters_130381081  > 0) +
                           as.numeric(choice_data$volume_liters_152485359  > 0) +
                           as.numeric(choice_data$volume_liters_156262825  > 0) +
                           as.numeric(choice_data$volume_liters_156272824  > 0) +
                           as.numeric(choice_data$volume_liters_179561971  > 0) +
                           as.numeric(choice_data$volume_liters_179581973  > 0) +
                           as.numeric(choice_data$volume_liters_190662873  > 0) +
                           as.numeric(choice_data$volume_liters_190672872  > 0) +
                           as.numeric(choice_data$volume_liters_190682877  > 0) +
                           as.numeric(choice_data$volume_liters_194763335  > 0) +
                           as.numeric(choice_data$volume_liters_194773333  > 0) +
                           as.numeric(choice_data$volume_liters_202483690  > 0) +
                           as.numeric(choice_data$volume_liters_215974940  > 0) +
                           as.numeric(choice_data$volume_liters_215984943  > 0) +
                           as.numeric(choice_data$volume_liters_238242082  > 0) +
                           as.numeric(choice_data$volume_liters_238262081  > 0) +
                           as.numeric(choice_data$volume_liters_238272080  > 0) +
                           as.numeric(choice_data$volume_liters_238282084  > 0) +
                           as.numeric(choice_data$volume_liters_241582464  > 0) +
                           as.numeric(choice_data$volume_liters_244573083  > 0) +
                           as.numeric(choice_data$volume_liters_244583087  > 0) +
                           as.numeric(choice_data$volume_liters_256064473  > 0) +
                           as.numeric(choice_data$volume_liters_256074472  > 0) +
                           as.numeric(choice_data$volume_liters_256084476  > 0) +
                           as.numeric(choice_data$volume_liters_268262788  > 0) +
                           as.numeric(choice_data$volume_liters_268272787  > 0) +
                           as.numeric(choice_data$volume_liters_268282790  > 0) +
                           as.numeric(choice_data$volume_liters_271024938  > 0) +
                           as.numeric(choice_data$volume_liters_288664915  > 0) +
                           as.numeric(choice_data$volume_liters_288674914  > 0) +
                           as.numeric(choice_data$volume_liters_316583822  > 0) +
                           as.numeric(choice_data$volume_liters_322364483  > 0) +
                           as.numeric(choice_data$volume_liters_322384487  > 0) +
                           as.numeric(choice_data$volume_liters_343682147  > 0) +
                           as.numeric(choice_data$volume_liters_344222417  > 0) +
                           as.numeric(choice_data$volume_liters_344332416  > 0) +
                           as.numeric(choice_data$volume_liters_344573094  > 0) +
                           as.numeric(choice_data$volume_liters_345784107  > 0) +
                           as.numeric(choice_data$volume_liters_347474844  > 0) +
                           as.numeric(choice_data$volume_liters_348204895  > 0) +
                           as.numeric(choice_data$volume_liters_348214898  > 0) +
                           as.numeric(choice_data$volume_liters_359142077  > 0) +
                           as.numeric(choice_data$volume_liters_359162076  > 0) +
                           as.numeric(choice_data$volume_liters_359172075  > 0) +
                           as.numeric(choice_data$volume_liters_359182079  > 0) +
                           as.numeric(choice_data$volume_liters_359262073  > 0) +
                           as.numeric(choice_data$volume_liters_359482096  > 0) +
                           as.numeric(choice_data$volume_liters_363042475  > 0) +
                           as.numeric(choice_data$volume_liters_363052474  > 0) +
                           as.numeric(choice_data$volume_liters_363072473  > 0) +
                           as.numeric(choice_data$volume_liters_363082477  > 0) +
                           as.numeric(choice_data$volume_liters_368863465  > 0) +
                           as.numeric(choice_data$volume_liters_369033466  > 0) +
                           as.numeric(choice_data$volume_liters_369043469  > 0) +
                           as.numeric(choice_data$volume_liters_369073471  > 0) +
                           as.numeric(choice_data$volume_liters_369083470  > 0) +
                           as.numeric(choice_data$volume_liters_369693633  > 0) +
                           as.numeric(choice_data$volume_liters_369713637  > 0) +
                           as.numeric(choice_data$volume_liters_369763644  > 0) +
                           as.numeric(choice_data$volume_liters_369783647  > 0) +
                           as.numeric(choice_data$volume_liters_373365237  > 0) +
                           as.numeric(choice_data$volume_liters_373385241  > 0) +
                           as.numeric(choice_data$volume_liters_373464016  > 0) +
                           as.numeric(choice_data$volume_liters_373474014  > 0) +
                           as.numeric(choice_data$volume_liters_373484017  > 0) +
                           as.numeric(choice_data$volume_liters_374174133  > 0) +
                           as.numeric(choice_data$volume_liters_374184137  > 0) +
                           as.numeric(choice_data$volume_liters_379384566  > 0) +
                           as.numeric(choice_data$volume_liters_379944707  > 0) +
                           as.numeric(choice_data$volume_liters_379964706  > 0) +
                           as.numeric(choice_data$volume_liters_379974705  > 0) +
                           as.numeric(choice_data$volume_liters_379984709  > 0) +
                           as.numeric(choice_data$volume_liters_380064712  > 0) +
                           as.numeric(choice_data$volume_liters_380084711  > 0) +
                           as.numeric(choice_data$volume_liters_380884122  > 0) +
                           as.numeric(choice_data$volume_liters_381765067  > 0) +
                           as.numeric(choice_data$volume_liters_381775064  > 0) +
                           as.numeric(choice_data$volume_liters_381785066  > 0) +
                           as.numeric(choice_data$volume_liters_410762864  > 0) +
                           as.numeric(choice_data$volume_liters_416925185  > 0) +
                           as.numeric(choice_data$volume_liters_416935184  > 0) +
                           as.numeric(choice_data$volume_liters_416945182  > 0) +
                           as.numeric(choice_data$volume_liters_417045220  > 0) +
                           as.numeric(choice_data$volume_liters_418461979  > 0) +
                           as.numeric(choice_data$volume_liters_419895193  > 0) +
                           as.numeric(choice_data$volume_liters_427163350  > 0) +
                           as.numeric(choice_data$volume_liters_427173349  > 0) +
                           as.numeric(choice_data$volume_liters_427183352  > 0) +
                           as.numeric(choice_data$volume_liters_432851114  > 0) +
                           as.numeric(choice_data$volume_liters_433341134  > 0) +
                           as.numeric(choice_data$volume_liters_433351138  > 0) +
                           as.numeric(choice_data$volume_liters_433361110  > 0) +
                           as.numeric(choice_data$volume_liters_433371133  > 0) +
                           as.numeric(choice_data$volume_liters_433381135  > 0) +
                           as.numeric(choice_data$volume_liters_433871168  > 0) +
                           as.numeric(choice_data$volume_liters_446583206  > 0) +
                           as.numeric(choice_data$volume_liters_452483827  > 0) +
                           as.numeric(choice_data$volume_liters_452763855  > 0) +
                           as.numeric(choice_data$volume_liters_452773854  > 0) +
                           as.numeric(choice_data$volume_liters_452783857  > 0) +
                           as.numeric(choice_data$volume_liters_458864376  > 0) +
                           as.numeric(choice_data$volume_liters_463502472  > 0) +
                           as.numeric(choice_data$volume_liters_481052499  > 0) +
                           as.numeric(choice_data$volume_liters_481062503  > 0) +
                           as.numeric(choice_data$volume_liters_523161282  > 0) +
                           as.numeric(choice_data$volume_liters_525961862  > 0) +
                           as.numeric(choice_data$volume_liters_525981863  > 0) +
                           as.numeric(choice_data$volume_liters_532143913  > 0) +
                           as.numeric(choice_data$volume_liters_550863803  > 0) +
                           as.numeric(choice_data$volume_liters_571251241  > 0) +
                           as.numeric(choice_data$volume_liters_571481243  > 0) +
                           as.numeric(choice_data$volume_liters_588382962  > 0) +
                           as.numeric(choice_data$volume_liters_588682969  > 0) +
                           as.numeric(choice_data$volume_liters_588722960  > 0) +
                           as.numeric(choice_data$volume_liters_588752994  > 0) +
                           as.numeric(choice_data$volume_liters_590371737  > 0) +
                           as.numeric(choice_data$volume_liters_648582044  > 0) +
                           as.numeric(choice_data$volume_liters_648652045  > 0) +
                           as.numeric(choice_data$volume_liters_648662041  > 0) +
                           as.numeric(choice_data$volume_liters_648672039  > 0) +
                           as.numeric(choice_data$volume_liters_648682043  > 0) +
                           as.numeric(choice_data$volume_liters_652562809  > 0) +
                           as.numeric(choice_data$volume_liters_652572808  > 0) +
                           as.numeric(choice_data$volume_liters_675263042  > 0) +
                           as.numeric(choice_data$volume_liters_675273041  > 0) +
                           as.numeric(choice_data$volume_liters_696361815  > 0) +
                           as.numeric(choice_data$volume_liters_696371814  > 0) +
                           as.numeric(choice_data$volume_liters_696381817  > 0) +
                           as.numeric(choice_data$volume_liters_699474354  > 0) +
                           as.numeric(choice_data$volume_liters_730504347  > 0) +
                           as.numeric(choice_data$volume_liters_730534345  > 0) +
                           as.numeric(choice_data$volume_liters_730554344  > 0) +
                           as.numeric(choice_data$volume_liters_750873008  > 0) +
                           as.numeric(choice_data$volume_liters_752103114  > 0) +
                           as.numeric(choice_data$volume_liters_752143108  > 0) +
                           as.numeric(choice_data$volume_liters_764863795  > 0) +
                           as.numeric(choice_data$volume_liters_764873794  > 0) +
                           as.numeric(choice_data$volume_liters_774875094  > 0) +
                           as.numeric(choice_data$volume_liters_812063836  > 0) +
                           as.numeric(choice_data$volume_liters_812073835  > 0) +
                           as.numeric(choice_data$volume_liters_812083838  > 0) +
                           as.numeric(choice_data$volume_liters_826071715  > 0) +
                           as.numeric(choice_data$volume_liters_826371666  > 0) +
                           as.numeric(choice_data$volume_liters_827871639  > 0) +
                           as.numeric(choice_data$volume_liters_828461681  > 0) +
                           as.numeric(choice_data$volume_liters_828471697  > 0) +
                           as.numeric(choice_data$volume_liters_828671728  > 0) +
                           as.numeric(choice_data$volume_liters_861121724  > 0) +
                           as.numeric(choice_data$volume_liters_862513015  > 0) +
                           as.numeric(choice_data$volume_liters_865073846  > 0) +
                           as.numeric(choice_data$volume_liters_866702776  > 0) +
                           as.numeric(choice_data$volume_liters_868864757  > 0) +
                           as.numeric(choice_data$volume_liters_868874756  > 0) +
                           as.numeric(choice_data$volume_liters_868884776  > 0) +
                           as.numeric(choice_data$volume_liters_879373012  > 0) +
                           as.numeric(choice_data$volume_liters_882963898  > 0) +
                           as.numeric(choice_data$volume_liters_891962980  > 0) +
                           as.numeric(choice_data$volume_liters_891972979  > 0) +
                           as.numeric(choice_data$volume_liters_891982982  > 0) +
                           as.numeric(choice_data$volume_liters_893873009  > 0) +
                           as.numeric(choice_data$volume_liters_895773544  > 0))

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
choice_data$available_68036500   <- as.numeric(choice_data$state_cost_68036500   > 0)
choice_data$available_68037498   <- as.numeric(choice_data$state_cost_68037498   > 0)
choice_data$available_88283243   <- as.numeric(choice_data$state_cost_88283243   > 0)
choice_data$available_100084433  <- as.numeric(choice_data$state_cost_100084433  > 0)
choice_data$available_106271048  <- as.numeric(choice_data$state_cost_106271048  > 0)
choice_data$available_106281053  <- as.numeric(choice_data$state_cost_106281053  > 0)
choice_data$available_108071511  <- as.numeric(choice_data$state_cost_108071511  > 0)
choice_data$available_112961476  <- as.numeric(choice_data$state_cost_112961476  > 0)
choice_data$available_112971485  <- as.numeric(choice_data$state_cost_112971485  > 0)
choice_data$available_112981488  <- as.numeric(choice_data$state_cost_112981488  > 0)
choice_data$available_124071064  <- as.numeric(choice_data$state_cost_124071064  > 0)
choice_data$available_124081067  <- as.numeric(choice_data$state_cost_124081067  > 0)
choice_data$available_124781072  <- as.numeric(choice_data$state_cost_124781072  > 0)
choice_data$available_130381081  <- as.numeric(choice_data$state_cost_130381081  > 0)
choice_data$available_152485359  <- as.numeric(choice_data$state_cost_152485359  > 0)
choice_data$available_156262825  <- as.numeric(choice_data$state_cost_156262825  > 0)
choice_data$available_156272824  <- as.numeric(choice_data$state_cost_156272824  > 0)
choice_data$available_179561971  <- as.numeric(choice_data$state_cost_179561971  > 0)
choice_data$available_179581973  <- as.numeric(choice_data$state_cost_179581973  > 0)
choice_data$available_190662873  <- as.numeric(choice_data$state_cost_190662873  > 0)
choice_data$available_190672872  <- as.numeric(choice_data$state_cost_190672872  > 0)
choice_data$available_190682877  <- as.numeric(choice_data$state_cost_190682877  > 0)
choice_data$available_194763335  <- as.numeric(choice_data$state_cost_194763335  > 0)
choice_data$available_194773333  <- as.numeric(choice_data$state_cost_194773333  > 0)
choice_data$available_202483690  <- as.numeric(choice_data$state_cost_202483690  > 0)
choice_data$available_215974940  <- as.numeric(choice_data$state_cost_215974940  > 0)
choice_data$available_215984943  <- as.numeric(choice_data$state_cost_215984943  > 0)
choice_data$available_238242082  <- as.numeric(choice_data$state_cost_238242082  > 0)
choice_data$available_238262081  <- as.numeric(choice_data$state_cost_238262081  > 0)
choice_data$available_238272080  <- as.numeric(choice_data$state_cost_238272080  > 0)
choice_data$available_238282084  <- as.numeric(choice_data$state_cost_238282084  > 0)
choice_data$available_241582464  <- as.numeric(choice_data$state_cost_241582464  > 0)
choice_data$available_244573083  <- as.numeric(choice_data$state_cost_244573083  > 0)
choice_data$available_244583087  <- as.numeric(choice_data$state_cost_244583087  > 0)
choice_data$available_256064473  <- as.numeric(choice_data$state_cost_256064473  > 0)
choice_data$available_256074472  <- as.numeric(choice_data$state_cost_256074472  > 0)
choice_data$available_256084476  <- as.numeric(choice_data$state_cost_256084476  > 0)
choice_data$available_268262788  <- as.numeric(choice_data$state_cost_268262788  > 0)
choice_data$available_268272787  <- as.numeric(choice_data$state_cost_268272787  > 0)
choice_data$available_268282790  <- as.numeric(choice_data$state_cost_268282790  > 0)
choice_data$available_271024938  <- as.numeric(choice_data$state_cost_271024938  > 0)
choice_data$available_288664915  <- as.numeric(choice_data$state_cost_288664915  > 0)
choice_data$available_288674914  <- as.numeric(choice_data$state_cost_288674914  > 0)
choice_data$available_316583822  <- as.numeric(choice_data$state_cost_316583822  > 0)
choice_data$available_322364483  <- as.numeric(choice_data$state_cost_322364483  > 0)
choice_data$available_322384487  <- as.numeric(choice_data$state_cost_322384487  > 0)
choice_data$available_343682147  <- as.numeric(choice_data$state_cost_343682147  > 0)
choice_data$available_344222417  <- as.numeric(choice_data$state_cost_344222417  > 0)
choice_data$available_344332416  <- as.numeric(choice_data$state_cost_344332416  > 0)
choice_data$available_344573094  <- as.numeric(choice_data$state_cost_344573094  > 0)
choice_data$available_345784107  <- as.numeric(choice_data$state_cost_345784107  > 0)
choice_data$available_347474844  <- as.numeric(choice_data$state_cost_347474844  > 0)
choice_data$available_348204895  <- as.numeric(choice_data$state_cost_348204895  > 0)
choice_data$available_348214898  <- as.numeric(choice_data$state_cost_348214898  > 0)
choice_data$available_359142077  <- as.numeric(choice_data$state_cost_359142077  > 0)
choice_data$available_359162076  <- as.numeric(choice_data$state_cost_359162076  > 0)
choice_data$available_359172075  <- as.numeric(choice_data$state_cost_359172075  > 0)
choice_data$available_359182079  <- as.numeric(choice_data$state_cost_359182079  > 0)
choice_data$available_359262073  <- as.numeric(choice_data$state_cost_359262073  > 0)
choice_data$available_359482096  <- as.numeric(choice_data$state_cost_359482096  > 0)
choice_data$available_363042475  <- as.numeric(choice_data$state_cost_363042475  > 0)
choice_data$available_363052474  <- as.numeric(choice_data$state_cost_363052474  > 0)
choice_data$available_363072473  <- as.numeric(choice_data$state_cost_363072473  > 0)
choice_data$available_363082477  <- as.numeric(choice_data$state_cost_363082477  > 0)
choice_data$available_368863465  <- as.numeric(choice_data$state_cost_368863465  > 0)
choice_data$available_369033466  <- as.numeric(choice_data$state_cost_369033466  > 0)
choice_data$available_369043469  <- as.numeric(choice_data$state_cost_369043469  > 0)
choice_data$available_369073471  <- as.numeric(choice_data$state_cost_369073471  > 0)
choice_data$available_369083470  <- as.numeric(choice_data$state_cost_369083470  > 0)
choice_data$available_369693633  <- as.numeric(choice_data$state_cost_369693633  > 0)
choice_data$available_369713637  <- as.numeric(choice_data$state_cost_369713637  > 0)
choice_data$available_369763644  <- as.numeric(choice_data$state_cost_369763644  > 0)
choice_data$available_369783647  <- as.numeric(choice_data$state_cost_369783647  > 0)
choice_data$available_373365237  <- as.numeric(choice_data$state_cost_373365237  > 0)
choice_data$available_373385241  <- as.numeric(choice_data$state_cost_373385241  > 0)
choice_data$available_373464016  <- as.numeric(choice_data$state_cost_373464016  > 0)
choice_data$available_373474014  <- as.numeric(choice_data$state_cost_373474014  > 0)
choice_data$available_373484017  <- as.numeric(choice_data$state_cost_373484017  > 0)
choice_data$available_374174133  <- as.numeric(choice_data$state_cost_374174133  > 0)
choice_data$available_374184137  <- as.numeric(choice_data$state_cost_374184137  > 0)
choice_data$available_379384566  <- as.numeric(choice_data$state_cost_379384566  > 0)
choice_data$available_379944707  <- as.numeric(choice_data$state_cost_379944707  > 0)
choice_data$available_379964706  <- as.numeric(choice_data$state_cost_379964706  > 0)
choice_data$available_379974705  <- as.numeric(choice_data$state_cost_379974705  > 0)
choice_data$available_379984709  <- as.numeric(choice_data$state_cost_379984709  > 0)
choice_data$available_380064712  <- as.numeric(choice_data$state_cost_380064712  > 0)
choice_data$available_380084711  <- as.numeric(choice_data$state_cost_380084711  > 0)
choice_data$available_380884122  <- as.numeric(choice_data$state_cost_380884122  > 0)
choice_data$available_381765067  <- as.numeric(choice_data$state_cost_381765067  > 0)
choice_data$available_381775064  <- as.numeric(choice_data$state_cost_381775064  > 0)
choice_data$available_381785066  <- as.numeric(choice_data$state_cost_381785066  > 0)
choice_data$available_410762864  <- as.numeric(choice_data$state_cost_410762864  > 0)
choice_data$available_416925185  <- as.numeric(choice_data$state_cost_416925185  > 0)
choice_data$available_416935184  <- as.numeric(choice_data$state_cost_416935184  > 0)
choice_data$available_416945182  <- as.numeric(choice_data$state_cost_416945182  > 0)
choice_data$available_417045220  <- as.numeric(choice_data$state_cost_417045220  > 0)
choice_data$available_418461979  <- as.numeric(choice_data$state_cost_418461979  > 0)
choice_data$available_419895193  <- as.numeric(choice_data$state_cost_419895193  > 0)
choice_data$available_427163350  <- as.numeric(choice_data$state_cost_427163350  > 0)
choice_data$available_427173349  <- as.numeric(choice_data$state_cost_427173349  > 0)
choice_data$available_427183352  <- as.numeric(choice_data$state_cost_427183352  > 0)
choice_data$available_432851114  <- as.numeric(choice_data$state_cost_432851114  > 0)
choice_data$available_433341134  <- as.numeric(choice_data$state_cost_433341134  > 0)
choice_data$available_433351138  <- as.numeric(choice_data$state_cost_433351138  > 0)
choice_data$available_433361110  <- as.numeric(choice_data$state_cost_433361110  > 0)
choice_data$available_433371133  <- as.numeric(choice_data$state_cost_433371133  > 0)
choice_data$available_433381135  <- as.numeric(choice_data$state_cost_433381135  > 0)
choice_data$available_433871168  <- as.numeric(choice_data$state_cost_433871168  > 0)
choice_data$available_446583206  <- as.numeric(choice_data$state_cost_446583206  > 0)
choice_data$available_452483827  <- as.numeric(choice_data$state_cost_452483827  > 0)
choice_data$available_452763855  <- as.numeric(choice_data$state_cost_452763855  > 0)
choice_data$available_452773854  <- as.numeric(choice_data$state_cost_452773854  > 0)
choice_data$available_452783857  <- as.numeric(choice_data$state_cost_452783857  > 0)
choice_data$available_458864376  <- as.numeric(choice_data$state_cost_458864376  > 0)
choice_data$available_463502472  <- as.numeric(choice_data$state_cost_463502472  > 0)
choice_data$available_481052499  <- as.numeric(choice_data$state_cost_481052499  > 0)
choice_data$available_481062503  <- as.numeric(choice_data$state_cost_481062503  > 0)
choice_data$available_523161282  <- as.numeric(choice_data$state_cost_523161282  > 0)
choice_data$available_525961862  <- as.numeric(choice_data$state_cost_525961862  > 0)
choice_data$available_525981863  <- as.numeric(choice_data$state_cost_525981863  > 0)
choice_data$available_532143913  <- as.numeric(choice_data$state_cost_532143913  > 0)
choice_data$available_550863803  <- as.numeric(choice_data$state_cost_550863803  > 0)
choice_data$available_571251241  <- as.numeric(choice_data$state_cost_571251241  > 0)
choice_data$available_571481243  <- as.numeric(choice_data$state_cost_571481243  > 0)
choice_data$available_588382962  <- as.numeric(choice_data$state_cost_588382962  > 0)
choice_data$available_588682969  <- as.numeric(choice_data$state_cost_588682969  > 0)
choice_data$available_588722960  <- as.numeric(choice_data$state_cost_588722960  > 0)
choice_data$available_588752994  <- as.numeric(choice_data$state_cost_588752994  > 0)
choice_data$available_590371737  <- as.numeric(choice_data$state_cost_590371737  > 0)
choice_data$available_648582044  <- as.numeric(choice_data$state_cost_648582044  > 0)
choice_data$available_648652045  <- as.numeric(choice_data$state_cost_648652045  > 0)
choice_data$available_648662041  <- as.numeric(choice_data$state_cost_648662041  > 0)
choice_data$available_648672039  <- as.numeric(choice_data$state_cost_648672039  > 0)
choice_data$available_648682043  <- as.numeric(choice_data$state_cost_648682043  > 0)
choice_data$available_652562809  <- as.numeric(choice_data$state_cost_652562809  > 0)
choice_data$available_652572808  <- as.numeric(choice_data$state_cost_652572808  > 0)
choice_data$available_675263042  <- as.numeric(choice_data$state_cost_675263042  > 0)
choice_data$available_675273041  <- as.numeric(choice_data$state_cost_675273041  > 0)
choice_data$available_696361815  <- as.numeric(choice_data$state_cost_696361815  > 0)
choice_data$available_696371814  <- as.numeric(choice_data$state_cost_696371814  > 0)
choice_data$available_696381817  <- as.numeric(choice_data$state_cost_696381817  > 0)
choice_data$available_699474354  <- as.numeric(choice_data$state_cost_699474354  > 0)
choice_data$available_730504347  <- as.numeric(choice_data$state_cost_730504347  > 0)
choice_data$available_730534345  <- as.numeric(choice_data$state_cost_730534345  > 0)
choice_data$available_730554344  <- as.numeric(choice_data$state_cost_730554344  > 0)
choice_data$available_750873008  <- as.numeric(choice_data$state_cost_750873008  > 0)
choice_data$available_752103114  <- as.numeric(choice_data$state_cost_752103114  > 0)
choice_data$available_752143108  <- as.numeric(choice_data$state_cost_752143108  > 0)
choice_data$available_764863795  <- as.numeric(choice_data$state_cost_764863795  > 0)
choice_data$available_764873794  <- as.numeric(choice_data$state_cost_764873794  > 0)
choice_data$available_774875094  <- as.numeric(choice_data$state_cost_774875094  > 0)
choice_data$available_812063836  <- as.numeric(choice_data$state_cost_812063836  > 0)
choice_data$available_812073835  <- as.numeric(choice_data$state_cost_812073835  > 0)
choice_data$available_812083838  <- as.numeric(choice_data$state_cost_812083838  > 0)
choice_data$available_826071715  <- as.numeric(choice_data$state_cost_826071715  > 0)
choice_data$available_826371666  <- as.numeric(choice_data$state_cost_826371666  > 0)
choice_data$available_827871639  <- as.numeric(choice_data$state_cost_827871639  > 0)
choice_data$available_828461681  <- as.numeric(choice_data$state_cost_828461681  > 0)
choice_data$available_828471697  <- as.numeric(choice_data$state_cost_828471697  > 0)
choice_data$available_828671728  <- as.numeric(choice_data$state_cost_828671728  > 0)
choice_data$available_861121724  <- as.numeric(choice_data$state_cost_861121724  > 0)
choice_data$available_862513015  <- as.numeric(choice_data$state_cost_862513015  > 0)
choice_data$available_865073846  <- as.numeric(choice_data$state_cost_865073846  > 0)
choice_data$available_866702776  <- as.numeric(choice_data$state_cost_866702776  > 0)
choice_data$available_868864757  <- as.numeric(choice_data$state_cost_868864757  > 0)
choice_data$available_868874756  <- as.numeric(choice_data$state_cost_868874756  > 0)
choice_data$available_868884776  <- as.numeric(choice_data$state_cost_868884776  > 0)
choice_data$available_879373012  <- as.numeric(choice_data$state_cost_879373012  > 0)
choice_data$available_882963898  <- as.numeric(choice_data$state_cost_882963898  > 0)
choice_data$available_891962980  <- as.numeric(choice_data$state_cost_891962980  > 0)
choice_data$available_891972979  <- as.numeric(choice_data$state_cost_891972979  > 0)
choice_data$available_891982982  <- as.numeric(choice_data$state_cost_891982982  > 0)
choice_data$available_893873009  <- as.numeric(choice_data$state_cost_893873009  > 0)
choice_data$available_895773544  <- as.numeric(choice_data$state_cost_895773544  > 0)



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
choice_data$chosen_68036500   <- as.numeric(choice_data$volume_liters_68036500   > 0)
choice_data$chosen_68037498   <- as.numeric(choice_data$volume_liters_68037498   > 0)
choice_data$chosen_88283243   <- as.numeric(choice_data$volume_liters_88283243   > 0)
choice_data$chosen_100084433  <- as.numeric(choice_data$volume_liters_100084433  > 0)
choice_data$chosen_106271048  <- as.numeric(choice_data$volume_liters_106271048  > 0)
choice_data$chosen_106281053  <- as.numeric(choice_data$volume_liters_106281053  > 0)
choice_data$chosen_108071511  <- as.numeric(choice_data$volume_liters_108071511  > 0)
choice_data$chosen_112961476  <- as.numeric(choice_data$volume_liters_112961476  > 0)
choice_data$chosen_112971485  <- as.numeric(choice_data$volume_liters_112971485  > 0)
choice_data$chosen_112981488  <- as.numeric(choice_data$volume_liters_112981488  > 0)
choice_data$chosen_124071064  <- as.numeric(choice_data$volume_liters_124071064  > 0)
choice_data$chosen_124081067  <- as.numeric(choice_data$volume_liters_124081067  > 0)
choice_data$chosen_124781072  <- as.numeric(choice_data$volume_liters_124781072  > 0)
choice_data$chosen_130381081  <- as.numeric(choice_data$volume_liters_130381081  > 0)
choice_data$chosen_152485359  <- as.numeric(choice_data$volume_liters_152485359  > 0)
choice_data$chosen_156262825  <- as.numeric(choice_data$volume_liters_156262825  > 0)
choice_data$chosen_156272824  <- as.numeric(choice_data$volume_liters_156272824  > 0)
choice_data$chosen_179561971  <- as.numeric(choice_data$volume_liters_179561971  > 0)
choice_data$chosen_179581973  <- as.numeric(choice_data$volume_liters_179581973  > 0)
choice_data$chosen_190662873  <- as.numeric(choice_data$volume_liters_190662873  > 0)
choice_data$chosen_190672872  <- as.numeric(choice_data$volume_liters_190672872  > 0)
choice_data$chosen_190682877  <- as.numeric(choice_data$volume_liters_190682877  > 0)
choice_data$chosen_194763335  <- as.numeric(choice_data$volume_liters_194763335  > 0)
choice_data$chosen_194773333  <- as.numeric(choice_data$volume_liters_194773333  > 0)
choice_data$chosen_202483690  <- as.numeric(choice_data$volume_liters_202483690  > 0)
choice_data$chosen_215974940  <- as.numeric(choice_data$volume_liters_215974940  > 0)
choice_data$chosen_215984943  <- as.numeric(choice_data$volume_liters_215984943  > 0)
choice_data$chosen_238242082  <- as.numeric(choice_data$volume_liters_238242082  > 0)
choice_data$chosen_238262081  <- as.numeric(choice_data$volume_liters_238262081  > 0)
choice_data$chosen_238272080  <- as.numeric(choice_data$volume_liters_238272080  > 0)
choice_data$chosen_238282084  <- as.numeric(choice_data$volume_liters_238282084  > 0)
choice_data$chosen_241582464  <- as.numeric(choice_data$volume_liters_241582464  > 0)
choice_data$chosen_244573083  <- as.numeric(choice_data$volume_liters_244573083  > 0)
choice_data$chosen_244583087  <- as.numeric(choice_data$volume_liters_244583087  > 0)
choice_data$chosen_256064473  <- as.numeric(choice_data$volume_liters_256064473  > 0)
choice_data$chosen_256074472  <- as.numeric(choice_data$volume_liters_256074472  > 0)
choice_data$chosen_256084476  <- as.numeric(choice_data$volume_liters_256084476  > 0)
choice_data$chosen_268262788  <- as.numeric(choice_data$volume_liters_268262788  > 0)
choice_data$chosen_268272787  <- as.numeric(choice_data$volume_liters_268272787  > 0)
choice_data$chosen_268282790  <- as.numeric(choice_data$volume_liters_268282790  > 0)
choice_data$chosen_271024938  <- as.numeric(choice_data$volume_liters_271024938  > 0)
choice_data$chosen_288664915  <- as.numeric(choice_data$volume_liters_288664915  > 0)
choice_data$chosen_288674914  <- as.numeric(choice_data$volume_liters_288674914  > 0)
choice_data$chosen_316583822  <- as.numeric(choice_data$volume_liters_316583822  > 0)
choice_data$chosen_322364483  <- as.numeric(choice_data$volume_liters_322364483  > 0)
choice_data$chosen_322384487  <- as.numeric(choice_data$volume_liters_322384487  > 0)
choice_data$chosen_343682147  <- as.numeric(choice_data$volume_liters_343682147  > 0)
choice_data$chosen_344222417  <- as.numeric(choice_data$volume_liters_344222417  > 0)
choice_data$chosen_344332416  <- as.numeric(choice_data$volume_liters_344332416  > 0)
choice_data$chosen_344573094  <- as.numeric(choice_data$volume_liters_344573094  > 0)
choice_data$chosen_345784107  <- as.numeric(choice_data$volume_liters_345784107  > 0)
choice_data$chosen_347474844  <- as.numeric(choice_data$volume_liters_347474844  > 0)
choice_data$chosen_348204895  <- as.numeric(choice_data$volume_liters_348204895  > 0)
choice_data$chosen_348214898  <- as.numeric(choice_data$volume_liters_348214898  > 0)
choice_data$chosen_359142077  <- as.numeric(choice_data$volume_liters_359142077  > 0)
choice_data$chosen_359162076  <- as.numeric(choice_data$volume_liters_359162076  > 0)
choice_data$chosen_359172075  <- as.numeric(choice_data$volume_liters_359172075  > 0)
choice_data$chosen_359182079  <- as.numeric(choice_data$volume_liters_359182079  > 0)
choice_data$chosen_359262073  <- as.numeric(choice_data$volume_liters_359262073  > 0)
choice_data$chosen_359482096  <- as.numeric(choice_data$volume_liters_359482096  > 0)
choice_data$chosen_363042475  <- as.numeric(choice_data$volume_liters_363042475  > 0)
choice_data$chosen_363052474  <- as.numeric(choice_data$volume_liters_363052474  > 0)
choice_data$chosen_363072473  <- as.numeric(choice_data$volume_liters_363072473  > 0)
choice_data$chosen_363082477  <- as.numeric(choice_data$volume_liters_363082477  > 0)
choice_data$chosen_368863465  <- as.numeric(choice_data$volume_liters_368863465  > 0)
choice_data$chosen_369033466  <- as.numeric(choice_data$volume_liters_369033466  > 0)
choice_data$chosen_369043469  <- as.numeric(choice_data$volume_liters_369043469  > 0)
choice_data$chosen_369073471  <- as.numeric(choice_data$volume_liters_369073471  > 0)
choice_data$chosen_369083470  <- as.numeric(choice_data$volume_liters_369083470  > 0)
choice_data$chosen_369693633  <- as.numeric(choice_data$volume_liters_369693633  > 0)
choice_data$chosen_369713637  <- as.numeric(choice_data$volume_liters_369713637  > 0)
choice_data$chosen_369763644  <- as.numeric(choice_data$volume_liters_369763644  > 0)
choice_data$chosen_369783647  <- as.numeric(choice_data$volume_liters_369783647  > 0)
choice_data$chosen_373365237  <- as.numeric(choice_data$volume_liters_373365237  > 0)
choice_data$chosen_373385241  <- as.numeric(choice_data$volume_liters_373385241  > 0)
choice_data$chosen_373464016  <- as.numeric(choice_data$volume_liters_373464016  > 0)
choice_data$chosen_373474014  <- as.numeric(choice_data$volume_liters_373474014  > 0)
choice_data$chosen_373484017  <- as.numeric(choice_data$volume_liters_373484017  > 0)
choice_data$chosen_374174133  <- as.numeric(choice_data$volume_liters_374174133  > 0)
choice_data$chosen_374184137  <- as.numeric(choice_data$volume_liters_374184137  > 0)
choice_data$chosen_379384566  <- as.numeric(choice_data$volume_liters_379384566  > 0)
choice_data$chosen_379944707  <- as.numeric(choice_data$volume_liters_379944707  > 0)
choice_data$chosen_379964706  <- as.numeric(choice_data$volume_liters_379964706  > 0)
choice_data$chosen_379974705  <- as.numeric(choice_data$volume_liters_379974705  > 0)
choice_data$chosen_379984709  <- as.numeric(choice_data$volume_liters_379984709  > 0)
choice_data$chosen_380064712  <- as.numeric(choice_data$volume_liters_380064712  > 0)
choice_data$chosen_380084711  <- as.numeric(choice_data$volume_liters_380084711  > 0)
choice_data$chosen_380884122  <- as.numeric(choice_data$volume_liters_380884122  > 0)
choice_data$chosen_381765067  <- as.numeric(choice_data$volume_liters_381765067  > 0)
choice_data$chosen_381775064  <- as.numeric(choice_data$volume_liters_381775064  > 0)
choice_data$chosen_381785066  <- as.numeric(choice_data$volume_liters_381785066  > 0)
choice_data$chosen_410762864  <- as.numeric(choice_data$volume_liters_410762864  > 0)
choice_data$chosen_416925185  <- as.numeric(choice_data$volume_liters_416925185  > 0)
choice_data$chosen_416935184  <- as.numeric(choice_data$volume_liters_416935184  > 0)
choice_data$chosen_416945182  <- as.numeric(choice_data$volume_liters_416945182  > 0)
choice_data$chosen_417045220  <- as.numeric(choice_data$volume_liters_417045220  > 0)
choice_data$chosen_418461979  <- as.numeric(choice_data$volume_liters_418461979  > 0)
choice_data$chosen_419895193  <- as.numeric(choice_data$volume_liters_419895193  > 0)
choice_data$chosen_427163350  <- as.numeric(choice_data$volume_liters_427163350  > 0)
choice_data$chosen_427173349  <- as.numeric(choice_data$volume_liters_427173349  > 0)
choice_data$chosen_427183352  <- as.numeric(choice_data$volume_liters_427183352  > 0)
choice_data$chosen_432851114  <- as.numeric(choice_data$volume_liters_432851114  > 0)
choice_data$chosen_433341134  <- as.numeric(choice_data$volume_liters_433341134  > 0)
choice_data$chosen_433351138  <- as.numeric(choice_data$volume_liters_433351138  > 0)
choice_data$chosen_433361110  <- as.numeric(choice_data$volume_liters_433361110  > 0)
choice_data$chosen_433371133  <- as.numeric(choice_data$volume_liters_433371133  > 0)
choice_data$chosen_433381135  <- as.numeric(choice_data$volume_liters_433381135  > 0)
choice_data$chosen_433871168  <- as.numeric(choice_data$volume_liters_433871168  > 0)
choice_data$chosen_446583206  <- as.numeric(choice_data$volume_liters_446583206  > 0)
choice_data$chosen_452483827  <- as.numeric(choice_data$volume_liters_452483827  > 0)
choice_data$chosen_452763855  <- as.numeric(choice_data$volume_liters_452763855  > 0)
choice_data$chosen_452773854  <- as.numeric(choice_data$volume_liters_452773854  > 0)
choice_data$chosen_452783857  <- as.numeric(choice_data$volume_liters_452783857  > 0)
choice_data$chosen_458864376  <- as.numeric(choice_data$volume_liters_458864376  > 0)
choice_data$chosen_463502472  <- as.numeric(choice_data$volume_liters_463502472  > 0)
choice_data$chosen_481052499  <- as.numeric(choice_data$volume_liters_481052499  > 0)
choice_data$chosen_481062503  <- as.numeric(choice_data$volume_liters_481062503  > 0)
choice_data$chosen_523161282  <- as.numeric(choice_data$volume_liters_523161282  > 0)
choice_data$chosen_525961862  <- as.numeric(choice_data$volume_liters_525961862  > 0)
choice_data$chosen_525981863  <- as.numeric(choice_data$volume_liters_525981863  > 0)
choice_data$chosen_532143913  <- as.numeric(choice_data$volume_liters_532143913  > 0)
choice_data$chosen_550863803  <- as.numeric(choice_data$volume_liters_550863803  > 0)
choice_data$chosen_571251241  <- as.numeric(choice_data$volume_liters_571251241  > 0)
choice_data$chosen_571481243  <- as.numeric(choice_data$volume_liters_571481243  > 0)
choice_data$chosen_588382962  <- as.numeric(choice_data$volume_liters_588382962  > 0)
choice_data$chosen_588682969  <- as.numeric(choice_data$volume_liters_588682969  > 0)
choice_data$chosen_588722960  <- as.numeric(choice_data$volume_liters_588722960  > 0)
choice_data$chosen_588752994  <- as.numeric(choice_data$volume_liters_588752994  > 0)
choice_data$chosen_590371737  <- as.numeric(choice_data$volume_liters_590371737  > 0)
choice_data$chosen_648582044  <- as.numeric(choice_data$volume_liters_648582044  > 0)
choice_data$chosen_648652045  <- as.numeric(choice_data$volume_liters_648652045  > 0)
choice_data$chosen_648662041  <- as.numeric(choice_data$volume_liters_648662041  > 0)
choice_data$chosen_648672039  <- as.numeric(choice_data$volume_liters_648672039  > 0)
choice_data$chosen_648682043  <- as.numeric(choice_data$volume_liters_648682043  > 0)
choice_data$chosen_652562809  <- as.numeric(choice_data$volume_liters_652562809  > 0)
choice_data$chosen_652572808  <- as.numeric(choice_data$volume_liters_652572808  > 0)
choice_data$chosen_675263042  <- as.numeric(choice_data$volume_liters_675263042  > 0)
choice_data$chosen_675273041  <- as.numeric(choice_data$volume_liters_675273041  > 0)
choice_data$chosen_696361815  <- as.numeric(choice_data$volume_liters_696361815  > 0)
choice_data$chosen_696371814  <- as.numeric(choice_data$volume_liters_696371814  > 0)
choice_data$chosen_696381817  <- as.numeric(choice_data$volume_liters_696381817  > 0)
choice_data$chosen_699474354  <- as.numeric(choice_data$volume_liters_699474354  > 0)
choice_data$chosen_730504347  <- as.numeric(choice_data$volume_liters_730504347  > 0)
choice_data$chosen_730534345  <- as.numeric(choice_data$volume_liters_730534345  > 0)
choice_data$chosen_730554344  <- as.numeric(choice_data$volume_liters_730554344  > 0)
choice_data$chosen_750873008  <- as.numeric(choice_data$volume_liters_750873008  > 0)
choice_data$chosen_752103114  <- as.numeric(choice_data$volume_liters_752103114  > 0)
choice_data$chosen_752143108  <- as.numeric(choice_data$volume_liters_752143108  > 0)
choice_data$chosen_764863795  <- as.numeric(choice_data$volume_liters_764863795  > 0)
choice_data$chosen_764873794  <- as.numeric(choice_data$volume_liters_764873794  > 0)
choice_data$chosen_774875094  <- as.numeric(choice_data$volume_liters_774875094  > 0)
choice_data$chosen_812063836  <- as.numeric(choice_data$volume_liters_812063836  > 0)
choice_data$chosen_812073835  <- as.numeric(choice_data$volume_liters_812073835  > 0)
choice_data$chosen_812083838  <- as.numeric(choice_data$volume_liters_812083838  > 0)
choice_data$chosen_826071715  <- as.numeric(choice_data$volume_liters_826071715  > 0)
choice_data$chosen_826371666  <- as.numeric(choice_data$volume_liters_826371666  > 0)
choice_data$chosen_827871639  <- as.numeric(choice_data$volume_liters_827871639  > 0)
choice_data$chosen_828461681  <- as.numeric(choice_data$volume_liters_828461681  > 0)
choice_data$chosen_828471697  <- as.numeric(choice_data$volume_liters_828471697  > 0)
choice_data$chosen_828671728  <- as.numeric(choice_data$volume_liters_828671728  > 0)
choice_data$chosen_861121724  <- as.numeric(choice_data$volume_liters_861121724  > 0)
choice_data$chosen_862513015  <- as.numeric(choice_data$volume_liters_862513015  > 0)
choice_data$chosen_865073846  <- as.numeric(choice_data$volume_liters_865073846  > 0)
choice_data$chosen_866702776  <- as.numeric(choice_data$volume_liters_866702776  > 0)
choice_data$chosen_868864757  <- as.numeric(choice_data$volume_liters_868864757  > 0)
choice_data$chosen_868874756  <- as.numeric(choice_data$volume_liters_868874756  > 0)
choice_data$chosen_868884776  <- as.numeric(choice_data$volume_liters_868884776  > 0)
choice_data$chosen_879373012  <- as.numeric(choice_data$volume_liters_879373012  > 0)
choice_data$chosen_882963898  <- as.numeric(choice_data$volume_liters_882963898  > 0)
choice_data$chosen_891962980  <- as.numeric(choice_data$volume_liters_891962980  > 0)
choice_data$chosen_891972979  <- as.numeric(choice_data$volume_liters_891972979  > 0)
choice_data$chosen_891982982  <- as.numeric(choice_data$volume_liters_891982982  > 0)
choice_data$chosen_893873009  <- as.numeric(choice_data$volume_liters_893873009  > 0)
choice_data$chosen_895773544  <- as.numeric(choice_data$volume_liters_895773544  > 0)

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
  ASC68036500   <- b[, cc]; cc <- cc + 1
  ASC68037498   <- b[, cc]; cc <- cc + 1
  ASC88283243   <- b[, cc]; cc <- cc + 1
  ASC100084433  <- b[, cc]; cc <- cc + 1
  ASC106271048  <- b[, cc]; cc <- cc + 1
  ASC106281053  <- b[, cc]; cc <- cc + 1
  ASC108071511  <- b[, cc]; cc <- cc + 1
  ASC112961476  <- b[, cc]; cc <- cc + 1
  ASC112971485  <- b[, cc]; cc <- cc + 1
  ASC112981488  <- b[, cc]; cc <- cc + 1
  ASC124071064  <- b[, cc]; cc <- cc + 1
  ASC124081067  <- b[, cc]; cc <- cc + 1
  ASC124781072  <- b[, cc]; cc <- cc + 1
  ASC130381081  <- b[, cc]; cc <- cc + 1
  ASC152485359  <- b[, cc]; cc <- cc + 1
  ASC156262825  <- b[, cc]; cc <- cc + 1
  ASC156272824  <- b[, cc]; cc <- cc + 1
  ASC179561971  <- b[, cc]; cc <- cc + 1
  ASC179581973  <- b[, cc]; cc <- cc + 1
  ASC190662873  <- b[, cc]; cc <- cc + 1
  ASC190672872  <- b[, cc]; cc <- cc + 1
  ASC190682877  <- b[, cc]; cc <- cc + 1
  ASC194763335  <- b[, cc]; cc <- cc + 1
  ASC194773333  <- b[, cc]; cc <- cc + 1
  ASC202483690  <- b[, cc]; cc <- cc + 1
  ASC215974940  <- b[, cc]; cc <- cc + 1
  ASC215984943  <- b[, cc]; cc <- cc + 1
  ASC238242082  <- b[, cc]; cc <- cc + 1
  ASC238262081  <- b[, cc]; cc <- cc + 1
  ASC238272080  <- b[, cc]; cc <- cc + 1
  ASC238282084  <- b[, cc]; cc <- cc + 1
  ASC241582464  <- b[, cc]; cc <- cc + 1
  ASC244573083  <- b[, cc]; cc <- cc + 1
  ASC244583087  <- b[, cc]; cc <- cc + 1
  ASC256064473  <- b[, cc]; cc <- cc + 1
  ASC256074472  <- b[, cc]; cc <- cc + 1
  ASC256084476  <- b[, cc]; cc <- cc + 1
  ASC268262788  <- b[, cc]; cc <- cc + 1
  ASC268272787  <- b[, cc]; cc <- cc + 1    
  ASC268282790  <- b[, cc]; cc <- cc + 1
  ASC271024938  <- b[, cc]; cc <- cc + 1
  ASC288664915  <- b[, cc]; cc <- cc + 1
  ASC288674914  <- b[, cc]; cc <- cc + 1
  ASC316583822  <- b[, cc]; cc <- cc + 1
  ASC322364483  <- b[, cc]; cc <- cc + 1
  ASC322384487  <- b[, cc]; cc <- cc + 1
  ASC343682147  <- b[, cc]; cc <- cc + 1
  ASC344222417  <- b[, cc]; cc <- cc + 1
  ASC344332416  <- b[, cc]; cc <- cc + 1
  ASC344573094  <- b[, cc]; cc <- cc + 1
  ASC345784107  <- b[, cc]; cc <- cc + 1
  ASC347474844  <- b[, cc]; cc <- cc + 1
  ASC348204895  <- b[, cc]; cc <- cc + 1
  ASC348214898  <- b[, cc]; cc <- cc + 1
  ASC359142077  <- b[, cc]; cc <- cc + 1
  ASC359162076  <- b[, cc]; cc <- cc + 1
  ASC359172075  <- b[, cc]; cc <- cc + 1
  ASC359182079  <- b[, cc]; cc <- cc + 1
  ASC359262073  <- b[, cc]; cc <- cc + 1
  ASC359482096  <- b[, cc]; cc <- cc + 1
  ASC363042475  <- b[, cc]; cc <- cc + 1
  ASC363052474  <- b[, cc]; cc <- cc + 1
  ASC363072473  <- b[, cc]; cc <- cc + 1
  ASC363082477  <- b[, cc]; cc <- cc + 1
  ASC368863465  <- b[, cc]; cc <- cc + 1
  ASC369033466  <- b[, cc]; cc <- cc + 1
  ASC369043469  <- b[, cc]; cc <- cc + 1
  ASC369073471  <- b[, cc]; cc <- cc + 1
  ASC369083470  <- b[, cc]; cc <- cc + 1
  ASC369693633  <- b[, cc]; cc <- cc + 1
  ASC369713637  <- b[, cc]; cc <- cc + 1
  ASC369763644  <- b[, cc]; cc <- cc + 1
  ASC369783647  <- b[, cc]; cc <- cc + 1
  ASC373365237  <- b[, cc]; cc <- cc + 1
  ASC373385241  <- b[, cc]; cc <- cc + 1
  ASC373464016  <- b[, cc]; cc <- cc + 1
  ASC373474014  <- b[, cc]; cc <- cc + 1
  ASC373484017  <- b[, cc]; cc <- cc + 1
  ASC374174133  <- b[, cc]; cc <- cc + 1
  ASC374184137  <- b[, cc]; cc <- cc + 1
  ASC379384566  <- b[, cc]; cc <- cc + 1
  ASC379944707  <- b[, cc]; cc <- cc + 1
  ASC379964706  <- b[, cc]; cc <- cc + 1
  ASC379974705  <- b[, cc]; cc <- cc + 1
  ASC379984709  <- b[, cc]; cc <- cc + 1
  ASC380064712  <- b[, cc]; cc <- cc + 1
  ASC380084711  <- b[, cc]; cc <- cc + 1
  ASC380884122  <- b[, cc]; cc <- cc + 1
  ASC381765067  <- b[, cc]; cc <- cc + 1
  ASC381775064  <- b[, cc]; cc <- cc + 1
  ASC381785066  <- b[, cc]; cc <- cc + 1
  ASC410762864  <- b[, cc]; cc <- cc + 1
  ASC416925185  <- b[, cc]; cc <- cc + 1
  ASC416935184  <- b[, cc]; cc <- cc + 1
  ASC416945182  <- b[, cc]; cc <- cc + 1
  ASC417045220  <- b[, cc]; cc <- cc + 1
  ASC418461979  <- b[, cc]; cc <- cc + 1
  ASC419895193  <- b[, cc]; cc <- cc + 1
  ASC427163350  <- b[, cc]; cc <- cc + 1
  ASC427173349  <- b[, cc]; cc <- cc + 1
  ASC427183352  <- b[, cc]; cc <- cc + 1
  ASC432851114  <- b[, cc]; cc <- cc + 1
  ASC433341134  <- b[, cc]; cc <- cc + 1
  ASC433351138  <- b[, cc]; cc <- cc + 1
  ASC433361110  <- b[, cc]; cc <- cc + 1
  ASC433371133  <- b[, cc]; cc <- cc + 1
  ASC433381135  <- b[, cc]; cc <- cc + 1
  ASC433871168  <- b[, cc]; cc <- cc + 1    
  ASC446583206  <- b[, cc]; cc <- cc + 1
  ASC452483827  <- b[, cc]; cc <- cc + 1
  ASC452763855  <- b[, cc]; cc <- cc + 1
  ASC452773854  <- b[, cc]; cc <- cc + 1
  ASC452783857  <- b[, cc]; cc <- cc + 1
  ASC458864376  <- b[, cc]; cc <- cc + 1
  ASC463502472  <- b[, cc]; cc <- cc + 1
  ASC481052499  <- b[, cc]; cc <- cc + 1
  ASC481062503  <- b[, cc]; cc <- cc + 1
  ASC523161282  <- b[, cc]; cc <- cc + 1
  ASC525961862  <- b[, cc]; cc <- cc + 1
  ASC525981863  <- b[, cc]; cc <- cc + 1
  ASC532143913  <- b[, cc]; cc <- cc + 1
  ASC550863803  <- b[, cc]; cc <- cc + 1
  ASC571251241  <- b[, cc]; cc <- cc + 1
  ASC571481243  <- b[, cc]; cc <- cc + 1
  ASC588382962  <- b[, cc]; cc <- cc + 1
  ASC588682969  <- b[, cc]; cc <- cc + 1
  ASC588722960  <- b[, cc]; cc <- cc + 1
  ASC588752994  <- b[, cc]; cc <- cc + 1
  ASC590371737  <- b[, cc]; cc <- cc + 1
  ASC648582044  <- b[, cc]; cc <- cc + 1
  ASC648652045  <- b[, cc]; cc <- cc + 1
  ASC648662041  <- b[, cc]; cc <- cc + 1
  ASC648672039  <- b[, cc]; cc <- cc + 1
  ASC648682043  <- b[, cc]; cc <- cc + 1
  ASC652562809  <- b[, cc]; cc <- cc + 1
  ASC652572808  <- b[, cc]; cc <- cc + 1
  ASC675263042  <- b[, cc]; cc <- cc + 1
  ASC675273041  <- b[, cc]; cc <- cc + 1
  ASC696361815  <- b[, cc]; cc <- cc + 1
  ASC696371814  <- b[, cc]; cc <- cc + 1
  ASC696381817  <- b[, cc]; cc <- cc + 1
  ASC699474354  <- b[, cc]; cc <- cc + 1
  ASC730504347  <- b[, cc]; cc <- cc + 1
  ASC730534345  <- b[, cc]; cc <- cc + 1
  ASC730554344  <- b[, cc]; cc <- cc + 1
  ASC750873008  <- b[, cc]; cc <- cc + 1
  ASC752103114  <- b[, cc]; cc <- cc + 1
  ASC752143108  <- b[, cc]; cc <- cc + 1
  ASC764863795  <- b[, cc]; cc <- cc + 1
  ASC764873794  <- b[, cc]; cc <- cc + 1
  ASC774875094  <- b[, cc]; cc <- cc + 1
  ASC812063836  <- b[, cc]; cc <- cc + 1
  ASC812073835  <- b[, cc]; cc <- cc + 1
  ASC812083838  <- b[, cc]; cc <- cc + 1
  ASC826071715  <- b[, cc]; cc <- cc + 1
  ASC826371666  <- b[, cc]; cc <- cc + 1
  ASC827871639  <- b[, cc]; cc <- cc + 1
  ASC828461681  <- b[, cc]; cc <- cc + 1
  ASC828471697  <- b[, cc]; cc <- cc + 1
  ASC828671728  <- b[, cc]; cc <- cc + 1
  ASC861121724  <- b[, cc]; cc <- cc + 1
  ASC862513015  <- b[, cc]; cc <- cc + 1
  ASC865073846  <- b[, cc]; cc <- cc + 1
  ASC866702776  <- b[, cc]; cc <- cc + 1
  ASC868864757  <- b[, cc]; cc <- cc + 1
  ASC868874756  <- b[, cc]; cc <- cc + 1
  ASC868884776  <- b[, cc]; cc <- cc + 1
  ASC879373012  <- b[, cc]; cc <- cc + 1
  ASC882963898  <- b[, cc]; cc <- cc + 1
  ASC891962980  <- b[, cc]; cc <- cc + 1
  ASC891972979  <- b[, cc]; cc <- cc + 1
  ASC891982982  <- b[, cc]; cc <- cc + 1
  ASC893873009  <- b[, cc]; cc <- cc + 1
  ASC895773544  <- b[, cc]; cc <- cc + 1
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
  ALPHA_68036500   <- b[, cc]; cc <- cc + 1
  ALPHA_68037498   <- b[, cc]; cc <- cc + 1
  ALPHA_88283243   <- b[, cc]; cc <- cc + 1
  ALPHA_100084433  <- b[, cc]; cc <- cc + 1
  ALPHA_106271048  <- b[, cc]; cc <- cc + 1
  ALPHA_106281053  <- b[, cc]; cc <- cc + 1
  ALPHA_108071511  <- b[, cc]; cc <- cc + 1
  ALPHA_112961476  <- b[, cc]; cc <- cc + 1
  ALPHA_112971485  <- b[, cc]; cc <- cc + 1
  ALPHA_112981488  <- b[, cc]; cc <- cc + 1
  ALPHA_124071064  <- b[, cc]; cc <- cc + 1
  ALPHA_124081067  <- b[, cc]; cc <- cc + 1
  ALPHA_124781072  <- b[, cc]; cc <- cc + 1
  ALPHA_130381081  <- b[, cc]; cc <- cc + 1
  ALPHA_152485359  <- b[, cc]; cc <- cc + 1
  ALPHA_156262825  <- b[, cc]; cc <- cc + 1
  ALPHA_156272824  <- b[, cc]; cc <- cc + 1
  ALPHA_179561971  <- b[, cc]; cc <- cc + 1
  ALPHA_179581973  <- b[, cc]; cc <- cc + 1
  ALPHA_190662873  <- b[, cc]; cc <- cc + 1
  ALPHA_190672872  <- b[, cc]; cc <- cc + 1
  ALPHA_190682877  <- b[, cc]; cc <- cc + 1
  ALPHA_194763335  <- b[, cc]; cc <- cc + 1
  ALPHA_194773333  <- b[, cc]; cc <- cc + 1
  ALPHA_202483690  <- b[, cc]; cc <- cc + 1
  ALPHA_215974940  <- b[, cc]; cc <- cc + 1
  ALPHA_215984943  <- b[, cc]; cc <- cc + 1
  ALPHA_238242082  <- b[, cc]; cc <- cc + 1
  ALPHA_238262081  <- b[, cc]; cc <- cc + 1
  ALPHA_238272080  <- b[, cc]; cc <- cc + 1
  ALPHA_238282084  <- b[, cc]; cc <- cc + 1
  ALPHA_241582464  <- b[, cc]; cc <- cc + 1
  ALPHA_244573083  <- b[, cc]; cc <- cc + 1
  ALPHA_244583087  <- b[, cc]; cc <- cc + 1
  ALPHA_256064473  <- b[, cc]; cc <- cc + 1
  ALPHA_256074472  <- b[, cc]; cc <- cc + 1
  ALPHA_256084476  <- b[, cc]; cc <- cc + 1
  ALPHA_268262788  <- b[, cc]; cc <- cc + 1
  ALPHA_268272787  <- b[, cc]; cc <- cc + 1    
  ALPHA_268282790  <- b[, cc]; cc <- cc + 1
  ALPHA_271024938  <- b[, cc]; cc <- cc + 1
  ALPHA_288664915  <- b[, cc]; cc <- cc + 1
  ALPHA_288674914  <- b[, cc]; cc <- cc + 1
  ALPHA_316583822  <- b[, cc]; cc <- cc + 1
  ALPHA_322364483  <- b[, cc]; cc <- cc + 1
  ALPHA_322384487  <- b[, cc]; cc <- cc + 1
  ALPHA_343682147  <- b[, cc]; cc <- cc + 1
  ALPHA_344222417  <- b[, cc]; cc <- cc + 1
  ALPHA_344332416  <- b[, cc]; cc <- cc + 1
  ALPHA_344573094  <- b[, cc]; cc <- cc + 1
  ALPHA_345784107  <- b[, cc]; cc <- cc + 1
  ALPHA_347474844  <- b[, cc]; cc <- cc + 1
  ALPHA_348204895  <- b[, cc]; cc <- cc + 1
  ALPHA_348214898  <- b[, cc]; cc <- cc + 1
  ALPHA_359142077  <- b[, cc]; cc <- cc + 1
  ALPHA_359162076  <- b[, cc]; cc <- cc + 1
  ALPHA_359172075  <- b[, cc]; cc <- cc + 1
  ALPHA_359182079  <- b[, cc]; cc <- cc + 1
  ALPHA_359262073  <- b[, cc]; cc <- cc + 1
  ALPHA_359482096  <- b[, cc]; cc <- cc + 1
  ALPHA_363042475  <- b[, cc]; cc <- cc + 1
  ALPHA_363052474  <- b[, cc]; cc <- cc + 1
  ALPHA_363072473  <- b[, cc]; cc <- cc + 1
  ALPHA_363082477  <- b[, cc]; cc <- cc + 1
  ALPHA_368863465  <- b[, cc]; cc <- cc + 1
  ALPHA_369033466  <- b[, cc]; cc <- cc + 1
  ALPHA_369043469  <- b[, cc]; cc <- cc + 1
  ALPHA_369073471  <- b[, cc]; cc <- cc + 1
  ALPHA_369083470  <- b[, cc]; cc <- cc + 1
  ALPHA_369693633  <- b[, cc]; cc <- cc + 1
  ALPHA_369713637  <- b[, cc]; cc <- cc + 1
  ALPHA_369763644  <- b[, cc]; cc <- cc + 1
  ALPHA_369783647  <- b[, cc]; cc <- cc + 1
  ALPHA_373365237  <- b[, cc]; cc <- cc + 1
  ALPHA_373385241  <- b[, cc]; cc <- cc + 1
  ALPHA_373464016  <- b[, cc]; cc <- cc + 1
  ALPHA_373474014  <- b[, cc]; cc <- cc + 1
  ALPHA_373484017  <- b[, cc]; cc <- cc + 1
  ALPHA_374174133  <- b[, cc]; cc <- cc + 1
  ALPHA_374184137  <- b[, cc]; cc <- cc + 1
  ALPHA_379384566  <- b[, cc]; cc <- cc + 1
  ALPHA_379944707  <- b[, cc]; cc <- cc + 1
  ALPHA_379964706  <- b[, cc]; cc <- cc + 1
  ALPHA_379974705  <- b[, cc]; cc <- cc + 1
  ALPHA_379984709  <- b[, cc]; cc <- cc + 1
  ALPHA_380064712  <- b[, cc]; cc <- cc + 1
  ALPHA_380084711  <- b[, cc]; cc <- cc + 1
  ALPHA_380884122  <- b[, cc]; cc <- cc + 1
  ALPHA_381765067  <- b[, cc]; cc <- cc + 1
  ALPHA_381775064  <- b[, cc]; cc <- cc + 1
  ALPHA_381785066  <- b[, cc]; cc <- cc + 1
  ALPHA_410762864  <- b[, cc]; cc <- cc + 1
  ALPHA_416925185  <- b[, cc]; cc <- cc + 1
  ALPHA_416935184  <- b[, cc]; cc <- cc + 1
  ALPHA_416945182  <- b[, cc]; cc <- cc + 1
  ALPHA_417045220  <- b[, cc]; cc <- cc + 1
  ALPHA_418461979  <- b[, cc]; cc <- cc + 1
  ALPHA_419895193  <- b[, cc]; cc <- cc + 1
  ALPHA_427163350  <- b[, cc]; cc <- cc + 1
  ALPHA_427173349  <- b[, cc]; cc <- cc + 1
  ALPHA_427183352  <- b[, cc]; cc <- cc + 1
  ALPHA_432851114  <- b[, cc]; cc <- cc + 1
  ALPHA_433341134  <- b[, cc]; cc <- cc + 1
  ALPHA_433351138  <- b[, cc]; cc <- cc + 1
  ALPHA_433361110  <- b[, cc]; cc <- cc + 1
  ALPHA_433371133  <- b[, cc]; cc <- cc + 1
  ALPHA_433381135  <- b[, cc]; cc <- cc + 1
  ALPHA_433871168  <- b[, cc]; cc <- cc + 1    
  ALPHA_446583206  <- b[, cc]; cc <- cc + 1
  ALPHA_452483827  <- b[, cc]; cc <- cc + 1
  ALPHA_452763855  <- b[, cc]; cc <- cc + 1
  ALPHA_452773854  <- b[, cc]; cc <- cc + 1
  ALPHA_452783857  <- b[, cc]; cc <- cc + 1
  ALPHA_458864376  <- b[, cc]; cc <- cc + 1
  ALPHA_463502472  <- b[, cc]; cc <- cc + 1
  ALPHA_481052499  <- b[, cc]; cc <- cc + 1
  ALPHA_481062503  <- b[, cc]; cc <- cc + 1
  ALPHA_523161282  <- b[, cc]; cc <- cc + 1
  ALPHA_525961862  <- b[, cc]; cc <- cc + 1
  ALPHA_525981863  <- b[, cc]; cc <- cc + 1
  ALPHA_532143913  <- b[, cc]; cc <- cc + 1
  ALPHA_550863803  <- b[, cc]; cc <- cc + 1
  ALPHA_571251241  <- b[, cc]; cc <- cc + 1
  ALPHA_571481243  <- b[, cc]; cc <- cc + 1
  ALPHA_588382962  <- b[, cc]; cc <- cc + 1
  ALPHA_588682969  <- b[, cc]; cc <- cc + 1
  ALPHA_588722960  <- b[, cc]; cc <- cc + 1
  ALPHA_588752994  <- b[, cc]; cc <- cc + 1
  ALPHA_590371737  <- b[, cc]; cc <- cc + 1
  ALPHA_648582044  <- b[, cc]; cc <- cc + 1
  ALPHA_648652045  <- b[, cc]; cc <- cc + 1
  ALPHA_648662041  <- b[, cc]; cc <- cc + 1
  ALPHA_648672039  <- b[, cc]; cc <- cc + 1
  ALPHA_648682043  <- b[, cc]; cc <- cc + 1
  ALPHA_652562809  <- b[, cc]; cc <- cc + 1
  ALPHA_652572808  <- b[, cc]; cc <- cc + 1
  ALPHA_675263042  <- b[, cc]; cc <- cc + 1
  ALPHA_675273041  <- b[, cc]; cc <- cc + 1
  ALPHA_696361815  <- b[, cc]; cc <- cc + 1
  ALPHA_696371814  <- b[, cc]; cc <- cc + 1
  ALPHA_696381817  <- b[, cc]; cc <- cc + 1
  ALPHA_699474354  <- b[, cc]; cc <- cc + 1
  ALPHA_730504347  <- b[, cc]; cc <- cc + 1
  ALPHA_730534345  <- b[, cc]; cc <- cc + 1
  ALPHA_730554344  <- b[, cc]; cc <- cc + 1
  ALPHA_750873008  <- b[, cc]; cc <- cc + 1
  ALPHA_752103114  <- b[, cc]; cc <- cc + 1
  ALPHA_752143108  <- b[, cc]; cc <- cc + 1
  ALPHA_764863795  <- b[, cc]; cc <- cc + 1
  ALPHA_764873794  <- b[, cc]; cc <- cc + 1
  ALPHA_774875094  <- b[, cc]; cc <- cc + 1
  ALPHA_812063836  <- b[, cc]; cc <- cc + 1
  ALPHA_812073835  <- b[, cc]; cc <- cc + 1
  ALPHA_812083838  <- b[, cc]; cc <- cc + 1
  ALPHA_826071715  <- b[, cc]; cc <- cc + 1
  ALPHA_826371666  <- b[, cc]; cc <- cc + 1
  ALPHA_827871639  <- b[, cc]; cc <- cc + 1
  ALPHA_828461681  <- b[, cc]; cc <- cc + 1
  ALPHA_828471697  <- b[, cc]; cc <- cc + 1
  ALPHA_828671728  <- b[, cc]; cc <- cc + 1
  ALPHA_861121724  <- b[, cc]; cc <- cc + 1
  ALPHA_862513015  <- b[, cc]; cc <- cc + 1
  ALPHA_865073846  <- b[, cc]; cc <- cc + 1
  ALPHA_866702776  <- b[, cc]; cc <- cc + 1
  ALPHA_868864757  <- b[, cc]; cc <- cc + 1
  ALPHA_868874756  <- b[, cc]; cc <- cc + 1
  ALPHA_868884776  <- b[, cc]; cc <- cc + 1
  ALPHA_879373012  <- b[, cc]; cc <- cc + 1
  ALPHA_882963898  <- b[, cc]; cc <- cc + 1
  ALPHA_891962980  <- b[, cc]; cc <- cc + 1
  ALPHA_891972979  <- b[, cc]; cc <- cc + 1
  ALPHA_891982982  <- b[, cc]; cc <- cc + 1
  ALPHA_893873009  <- b[, cc]; cc <- cc + 1
  ALPHA_895773544  <- b[, cc]; cc <- cc + 1
  # GAMMA_9999       <- b[, cc]; cc <- cc + 1    
  # GAMMA_10550695   <- b[, cc]; cc <- cc + 1
  # GAMMA_11586693   <- b[, cc]; cc <- cc + 1
  # GAMMA_11773688   <- b[, cc]; cc <- cc + 1
  # GAMMA_11774687   <- b[, cc]; cc <- cc + 1
  # GAMMA_11776686   <- b[, cc]; cc <- cc + 1
  # GAMMA_11777685   <- b[, cc]; cc <- cc + 1
  # GAMMA_11786699   <- b[, cc]; cc <- cc + 1
  # GAMMA_11788689   <- b[, cc]; cc <- cc + 1
  # GAMMA_16518268   <- b[, cc]; cc <- cc + 1
  # GAMMA_17991132   <- b[, cc]; cc <- cc + 1
  # GAMMA_22788573   <- b[, cc]; cc <- cc + 1
  # GAMMA_29287548   <- b[, cc]; cc <- cc + 1
  # GAMMA_34006175   <- b[, cc]; cc <- cc + 1
  # GAMMA_34007174   <- b[, cc]; cc <- cc + 1
  # GAMMA_34008178   <- b[, cc]; cc <- cc + 1
  # GAMMA_34029146   <- b[, cc]; cc <- cc + 1
  # GAMMA_35317555   <- b[, cc]; cc <- cc + 1
  # GAMMA_35318559   <- b[, cc]; cc <- cc + 1
  # GAMMA_35416952   <- b[, cc]; cc <- cc + 1
  # GAMMA_35418953   <- b[, cc]; cc <- cc + 1
  # GAMMA_43026214   <- b[, cc]; cc <- cc + 1
  # GAMMA_43027213   <- b[, cc]; cc <- cc + 1
  # GAMMA_43028216   <- b[, cc]; cc <- cc + 1
  # GAMMA_43038436   <- b[, cc]; cc <- cc + 1
  # GAMMA_43126479   <- b[, cc]; cc <- cc + 1
  # GAMMA_43127478   <- b[, cc]; cc <- cc + 1
  # GAMMA_43128482   <- b[, cc]; cc <- cc + 1
  # GAMMA_43136446   <- b[, cc]; cc <- cc + 1
  # GAMMA_43137443   <- b[, cc]; cc <- cc + 1
  # GAMMA_44217554   <- b[, cc]; cc <- cc + 1
  # GAMMA_68036500   <- b[, cc]; cc <- cc + 1
  # GAMMA_68037498   <- b[, cc]; cc <- cc + 1
  # GAMMA_88283243   <- b[, cc]; cc <- cc + 1
  # GAMMA_100084433  <- b[, cc]; cc <- cc + 1
  # GAMMA_106271048  <- b[, cc]; cc <- cc + 1
  # GAMMA_106281053  <- b[, cc]; cc <- cc + 1
  # GAMMA_108071511  <- b[, cc]; cc <- cc + 1
  # GAMMA_112961476  <- b[, cc]; cc <- cc + 1
  # GAMMA_112971485  <- b[, cc]; cc <- cc + 1
  # GAMMA_112981488  <- b[, cc]; cc <- cc + 1
  # GAMMA_124071064  <- b[, cc]; cc <- cc + 1
  # GAMMA_124081067  <- b[, cc]; cc <- cc + 1
  # GAMMA_124781072  <- b[, cc]; cc <- cc + 1
  # GAMMA_130381081  <- b[, cc]; cc <- cc + 1
  # GAMMA_152485359  <- b[, cc]; cc <- cc + 1
  # GAMMA_156262825  <- b[, cc]; cc <- cc + 1
  # GAMMA_156272824  <- b[, cc]; cc <- cc + 1
  # GAMMA_179561971  <- b[, cc]; cc <- cc + 1
  # GAMMA_179581973  <- b[, cc]; cc <- cc + 1
  # GAMMA_190662873  <- b[, cc]; cc <- cc + 1
  # GAMMA_190672872  <- b[, cc]; cc <- cc + 1
  # GAMMA_190682877  <- b[, cc]; cc <- cc + 1
  # GAMMA_194763335  <- b[, cc]; cc <- cc + 1
  # GAMMA_194773333  <- b[, cc]; cc <- cc + 1
  # GAMMA_202483690  <- b[, cc]; cc <- cc + 1
  # GAMMA_215974940  <- b[, cc]; cc <- cc + 1
  # GAMMA_215984943  <- b[, cc]; cc <- cc + 1
  # GAMMA_238242082  <- b[, cc]; cc <- cc + 1
  # GAMMA_238262081  <- b[, cc]; cc <- cc + 1
  # GAMMA_238272080  <- b[, cc]; cc <- cc + 1
  # GAMMA_238282084  <- b[, cc]; cc <- cc + 1
  # GAMMA_241582464  <- b[, cc]; cc <- cc + 1
  # GAMMA_244573083  <- b[, cc]; cc <- cc + 1
  # GAMMA_244583087  <- b[, cc]; cc <- cc + 1
  # GAMMA_256064473  <- b[, cc]; cc <- cc + 1
  # GAMMA_256074472  <- b[, cc]; cc <- cc + 1
  # GAMMA_256084476  <- b[, cc]; cc <- cc + 1
  # GAMMA_268262788  <- b[, cc]; cc <- cc + 1
  # GAMMA_268272787  <- b[, cc]; cc <- cc + 1    
  # GAMMA_268282790  <- b[, cc]; cc <- cc + 1
  # GAMMA_271024938  <- b[, cc]; cc <- cc + 1
  # GAMMA_288664915  <- b[, cc]; cc <- cc + 1
  # GAMMA_288674914  <- b[, cc]; cc <- cc + 1
  # GAMMA_316583822  <- b[, cc]; cc <- cc + 1
  # GAMMA_322364483  <- b[, cc]; cc <- cc + 1
  # GAMMA_322384487  <- b[, cc]; cc <- cc + 1
  # GAMMA_343682147  <- b[, cc]; cc <- cc + 1
  # GAMMA_344222417  <- b[, cc]; cc <- cc + 1
  # GAMMA_344332416  <- b[, cc]; cc <- cc + 1
  # GAMMA_344573094  <- b[, cc]; cc <- cc + 1
  # GAMMA_345784107  <- b[, cc]; cc <- cc + 1
  # GAMMA_347474844  <- b[, cc]; cc <- cc + 1
  # GAMMA_348204895  <- b[, cc]; cc <- cc + 1
  # GAMMA_348214898  <- b[, cc]; cc <- cc + 1
  # GAMMA_359142077  <- b[, cc]; cc <- cc + 1
  # GAMMA_359162076  <- b[, cc]; cc <- cc + 1
  # GAMMA_359172075  <- b[, cc]; cc <- cc + 1
  # GAMMA_359182079  <- b[, cc]; cc <- cc + 1
  # GAMMA_359262073  <- b[, cc]; cc <- cc + 1
  # GAMMA_359482096  <- b[, cc]; cc <- cc + 1
  # GAMMA_363042475  <- b[, cc]; cc <- cc + 1
  # GAMMA_363052474  <- b[, cc]; cc <- cc + 1
  # GAMMA_363072473  <- b[, cc]; cc <- cc + 1
  # GAMMA_363082477  <- b[, cc]; cc <- cc + 1
  # GAMMA_368863465  <- b[, cc]; cc <- cc + 1
  # GAMMA_369033466  <- b[, cc]; cc <- cc + 1
  # GAMMA_369043469  <- b[, cc]; cc <- cc + 1
  # GAMMA_369073471  <- b[, cc]; cc <- cc + 1
  # GAMMA_369083470  <- b[, cc]; cc <- cc + 1
  # GAMMA_369693633  <- b[, cc]; cc <- cc + 1
  # GAMMA_369713637  <- b[, cc]; cc <- cc + 1
  # GAMMA_369763644  <- b[, cc]; cc <- cc + 1
  # GAMMA_369783647  <- b[, cc]; cc <- cc + 1
  # GAMMA_373365237  <- b[, cc]; cc <- cc + 1
  # GAMMA_373385241  <- b[, cc]; cc <- cc + 1
  # GAMMA_373464016  <- b[, cc]; cc <- cc + 1
  # GAMMA_373474014  <- b[, cc]; cc <- cc + 1
  # GAMMA_373484017  <- b[, cc]; cc <- cc + 1
  # GAMMA_374174133  <- b[, cc]; cc <- cc + 1
  # GAMMA_374184137  <- b[, cc]; cc <- cc + 1
  # GAMMA_379384566  <- b[, cc]; cc <- cc + 1
  # GAMMA_379944707  <- b[, cc]; cc <- cc + 1
  # GAMMA_379964706  <- b[, cc]; cc <- cc + 1
  # GAMMA_379974705  <- b[, cc]; cc <- cc + 1
  # GAMMA_379984709  <- b[, cc]; cc <- cc + 1
  # GAMMA_380064712  <- b[, cc]; cc <- cc + 1
  # GAMMA_380084711  <- b[, cc]; cc <- cc + 1
  # GAMMA_380884122  <- b[, cc]; cc <- cc + 1
  # GAMMA_381765067  <- b[, cc]; cc <- cc + 1
  # GAMMA_381775064  <- b[, cc]; cc <- cc + 1
  # GAMMA_381785066  <- b[, cc]; cc <- cc + 1
  # GAMMA_410762864  <- b[, cc]; cc <- cc + 1
  # GAMMA_416925185  <- b[, cc]; cc <- cc + 1
  # GAMMA_416935184  <- b[, cc]; cc <- cc + 1
  # GAMMA_416945182  <- b[, cc]; cc <- cc + 1
  # GAMMA_417045220  <- b[, cc]; cc <- cc + 1
  # GAMMA_418461979  <- b[, cc]; cc <- cc + 1
  # GAMMA_419895193  <- b[, cc]; cc <- cc + 1
  # GAMMA_427163350  <- b[, cc]; cc <- cc + 1
  # GAMMA_427173349  <- b[, cc]; cc <- cc + 1
  # GAMMA_427183352  <- b[, cc]; cc <- cc + 1
  # GAMMA_432851114  <- b[, cc]; cc <- cc + 1
  # GAMMA_433341134  <- b[, cc]; cc <- cc + 1
  # GAMMA_433351138  <- b[, cc]; cc <- cc + 1
  # GAMMA_433361110  <- b[, cc]; cc <- cc + 1
  # GAMMA_433371133  <- b[, cc]; cc <- cc + 1
  # GAMMA_433381135  <- b[, cc]; cc <- cc + 1
  # GAMMA_433871168  <- b[, cc]; cc <- cc + 1    
  # GAMMA_446583206  <- b[, cc]; cc <- cc + 1
  # GAMMA_452483827  <- b[, cc]; cc <- cc + 1
  # GAMMA_452763855  <- b[, cc]; cc <- cc + 1
  # GAMMA_452773854  <- b[, cc]; cc <- cc + 1
  # GAMMA_452783857  <- b[, cc]; cc <- cc + 1
  # GAMMA_458864376  <- b[, cc]; cc <- cc + 1
  # GAMMA_463502472  <- b[, cc]; cc <- cc + 1
  # GAMMA_481052499  <- b[, cc]; cc <- cc + 1
  # GAMMA_481062503  <- b[, cc]; cc <- cc + 1
  # GAMMA_523161282  <- b[, cc]; cc <- cc + 1
  # GAMMA_525961862  <- b[, cc]; cc <- cc + 1
  # GAMMA_525981863  <- b[, cc]; cc <- cc + 1
  # GAMMA_532143913  <- b[, cc]; cc <- cc + 1
  # GAMMA_550863803  <- b[, cc]; cc <- cc + 1
  # GAMMA_571251241  <- b[, cc]; cc <- cc + 1
  # GAMMA_571481243  <- b[, cc]; cc <- cc + 1
  # GAMMA_588382962  <- b[, cc]; cc <- cc + 1
  # GAMMA_588682969  <- b[, cc]; cc <- cc + 1
  # GAMMA_588722960  <- b[, cc]; cc <- cc + 1
  # GAMMA_588752994  <- b[, cc]; cc <- cc + 1
  # GAMMA_590371737  <- b[, cc]; cc <- cc + 1
  # GAMMA_648582044  <- b[, cc]; cc <- cc + 1
  # GAMMA_648652045  <- b[, cc]; cc <- cc + 1
  # GAMMA_648662041  <- b[, cc]; cc <- cc + 1
  # GAMMA_648672039  <- b[, cc]; cc <- cc + 1
  # GAMMA_648682043  <- b[, cc]; cc <- cc + 1
  # GAMMA_652562809  <- b[, cc]; cc <- cc + 1
  # GAMMA_652572808  <- b[, cc]; cc <- cc + 1
  # GAMMA_675263042  <- b[, cc]; cc <- cc + 1
  # GAMMA_675273041  <- b[, cc]; cc <- cc + 1
  # GAMMA_696361815  <- b[, cc]; cc <- cc + 1
  # GAMMA_696371814  <- b[, cc]; cc <- cc + 1
  # GAMMA_696381817  <- b[, cc]; cc <- cc + 1
  # GAMMA_699474354  <- b[, cc]; cc <- cc + 1
  # GAMMA_730504347  <- b[, cc]; cc <- cc + 1
  # GAMMA_730534345  <- b[, cc]; cc <- cc + 1
  # GAMMA_730554344  <- b[, cc]; cc <- cc + 1
  # GAMMA_750873008  <- b[, cc]; cc <- cc + 1
  # GAMMA_752103114  <- b[, cc]; cc <- cc + 1
  # GAMMA_752143108  <- b[, cc]; cc <- cc + 1
  # GAMMA_764863795  <- b[, cc]; cc <- cc + 1
  # GAMMA_764873794  <- b[, cc]; cc <- cc + 1
  # GAMMA_774875094  <- b[, cc]; cc <- cc + 1
  # GAMMA_812063836  <- b[, cc]; cc <- cc + 1
  # GAMMA_812073835  <- b[, cc]; cc <- cc + 1
  # GAMMA_812083838  <- b[, cc]; cc <- cc + 1
  # GAMMA_826071715  <- b[, cc]; cc <- cc + 1
  # GAMMA_826371666  <- b[, cc]; cc <- cc + 1
  # GAMMA_827871639  <- b[, cc]; cc <- cc + 1
  # GAMMA_828461681  <- b[, cc]; cc <- cc + 1
  # GAMMA_828471697  <- b[, cc]; cc <- cc + 1
  # GAMMA_828671728  <- b[, cc]; cc <- cc + 1
  # GAMMA_861121724  <- b[, cc]; cc <- cc + 1
  # GAMMA_862513015  <- b[, cc]; cc <- cc + 1
  # GAMMA_865073846  <- b[, cc]; cc <- cc + 1
  # GAMMA_866702776  <- b[, cc]; cc <- cc + 1
  # GAMMA_868864757  <- b[, cc]; cc <- cc + 1
  # GAMMA_868874756  <- b[, cc]; cc <- cc + 1
  # GAMMA_868884776  <- b[, cc]; cc <- cc + 1
  # GAMMA_879373012  <- b[, cc]; cc <- cc + 1
  # GAMMA_882963898  <- b[, cc]; cc <- cc + 1
  # GAMMA_891962980  <- b[, cc]; cc <- cc + 1
  # GAMMA_891972979  <- b[, cc]; cc <- cc + 1
  # GAMMA_891982982  <- b[, cc]; cc <- cc + 1
  # GAMMA_893873009  <- b[, cc]; cc <- cc + 1
  # GAMMA_895773544  <- b[, cc];
  
  v9999       <- 0
  v10550695   <- ASC10550695   + PRICE1 * choice_data$state_cost_10550695   + log(1/(1+exp(-ALPHA_10550695  ))) + ((1/(1+exp(-ALPHA_10550695  ))) - 1) * log(choice_data$volume_liters_10550695  + 1 )
  v11586693   <- ASC11586693   + PRICE1 * choice_data$state_cost_11586693   + log(1/(1+exp(-ALPHA_11586693  ))) + ((1/(1+exp(-ALPHA_11586693  ))) - 1) * log(choice_data$volume_liters_11586693  + 1 )
  v11773688   <- ASC11773688   + PRICE1 * choice_data$state_cost_11773688   + log(1/(1+exp(-ALPHA_11773688  ))) + ((1/(1+exp(-ALPHA_11773688  ))) - 1) * log(choice_data$volume_liters_11773688  + 1 )
  v11774687   <- ASC11774687   + PRICE1 * choice_data$state_cost_11774687   + log(1/(1+exp(-ALPHA_11774687  ))) + ((1/(1+exp(-ALPHA_11774687  ))) - 1) * log(choice_data$volume_liters_11774687  + 1 )
  v11776686   <- ASC11776686   + PRICE1 * choice_data$state_cost_11776686   + log(1/(1+exp(-ALPHA_11776686  ))) + ((1/(1+exp(-ALPHA_11776686  ))) - 1) * log(choice_data$volume_liters_11776686  + 1 )
  v11777685   <- ASC11777685   + PRICE1 * choice_data$state_cost_11777685   + log(1/(1+exp(-ALPHA_11777685  ))) + ((1/(1+exp(-ALPHA_11777685  ))) - 1) * log(choice_data$volume_liters_11777685  + 1 )
  v11786699   <- ASC11786699   + PRICE1 * choice_data$state_cost_11786699   + log(1/(1+exp(-ALPHA_11786699  ))) + ((1/(1+exp(-ALPHA_11786699  ))) - 1) * log(choice_data$volume_liters_11786699  + 1 )
  v11788689   <- ASC11788689   + PRICE1 * choice_data$state_cost_11788689   + log(1/(1+exp(-ALPHA_11788689  ))) + ((1/(1+exp(-ALPHA_11788689  ))) - 1) * log(choice_data$volume_liters_11788689  + 1 )
  v16518268   <- ASC16518268   + PRICE1 * choice_data$state_cost_16518268   + log(1/(1+exp(-ALPHA_16518268  ))) + ((1/(1+exp(-ALPHA_16518268  ))) - 1) * log(choice_data$volume_liters_16518268  + 1 )
  v17991132   <- ASC17991132   + PRICE1 * choice_data$state_cost_17991132   + log(1/(1+exp(-ALPHA_17991132  ))) + ((1/(1+exp(-ALPHA_17991132  ))) - 1) * log(choice_data$volume_liters_17991132  + 1 )
  v22788573   <- ASC22788573   + PRICE1 * choice_data$state_cost_22788573   + log(1/(1+exp(-ALPHA_22788573  ))) + ((1/(1+exp(-ALPHA_22788573  ))) - 1) * log(choice_data$volume_liters_22788573  + 1 )
  v29287548   <- ASC29287548   + PRICE1 * choice_data$state_cost_29287548   + log(1/(1+exp(-ALPHA_29287548  ))) + ((1/(1+exp(-ALPHA_29287548  ))) - 1) * log(choice_data$volume_liters_29287548  + 1 )
  v34006175   <- ASC34006175   + PRICE1 * choice_data$state_cost_34006175   + log(1/(1+exp(-ALPHA_34006175  ))) + ((1/(1+exp(-ALPHA_34006175  ))) - 1) * log(choice_data$volume_liters_34006175  + 1 )
  v34007174   <- ASC34007174   + PRICE1 * choice_data$state_cost_34007174   + log(1/(1+exp(-ALPHA_34007174  ))) + ((1/(1+exp(-ALPHA_34007174  ))) - 1) * log(choice_data$volume_liters_34007174  + 1 )
  v34008178   <- ASC34008178   + PRICE1 * choice_data$state_cost_34008178   + log(1/(1+exp(-ALPHA_34008178  ))) + ((1/(1+exp(-ALPHA_34008178  ))) - 1) * log(choice_data$volume_liters_34008178  + 1 )
  v34029146   <- ASC34029146   + PRICE1 * choice_data$state_cost_34029146   + log(1/(1+exp(-ALPHA_34029146  ))) + ((1/(1+exp(-ALPHA_34029146  ))) - 1) * log(choice_data$volume_liters_34029146  + 1 )
  v35317555   <- ASC35317555   + PRICE1 * choice_data$state_cost_35317555   + log(1/(1+exp(-ALPHA_35317555  ))) + ((1/(1+exp(-ALPHA_35317555  ))) - 1) * log(choice_data$volume_liters_35317555  + 1 )
  v35318559   <- ASC35318559   + PRICE1 * choice_data$state_cost_35318559   + log(1/(1+exp(-ALPHA_35318559  ))) + ((1/(1+exp(-ALPHA_35318559  ))) - 1) * log(choice_data$volume_liters_35318559  + 1 )
  v35416952   <- ASC35416952   + PRICE1 * choice_data$state_cost_35416952   + log(1/(1+exp(-ALPHA_35416952  ))) + ((1/(1+exp(-ALPHA_35416952  ))) - 1) * log(choice_data$volume_liters_35416952  + 1 )
  v35418953   <- ASC35418953   + PRICE1 * choice_data$state_cost_35418953   + log(1/(1+exp(-ALPHA_35418953  ))) + ((1/(1+exp(-ALPHA_35418953  ))) - 1) * log(choice_data$volume_liters_35418953  + 1 )
  v43026214   <- ASC43026214   + PRICE1 * choice_data$state_cost_43026214   + log(1/(1+exp(-ALPHA_43026214  ))) + ((1/(1+exp(-ALPHA_43026214  ))) - 1) * log(choice_data$volume_liters_43026214  + 1 )
  v43027213   <- ASC43027213   + PRICE1 * choice_data$state_cost_43027213   + log(1/(1+exp(-ALPHA_43027213  ))) + ((1/(1+exp(-ALPHA_43027213  ))) - 1) * log(choice_data$volume_liters_43027213  + 1 )
  v43028216   <- ASC43028216   + PRICE1 * choice_data$state_cost_43028216   + log(1/(1+exp(-ALPHA_43028216  ))) + ((1/(1+exp(-ALPHA_43028216  ))) - 1) * log(choice_data$volume_liters_43028216  + 1 )
  v43038436   <- ASC43038436   + PRICE1 * choice_data$state_cost_43038436   + log(1/(1+exp(-ALPHA_43038436  ))) + ((1/(1+exp(-ALPHA_43038436  ))) - 1) * log(choice_data$volume_liters_43038436  + 1 )
  v43126479   <- ASC43126479   + PRICE1 * choice_data$state_cost_43126479   + log(1/(1+exp(-ALPHA_43126479  ))) + ((1/(1+exp(-ALPHA_43126479  ))) - 1) * log(choice_data$volume_liters_43126479  + 1 )
  v43127478   <- ASC43127478   + PRICE1 * choice_data$state_cost_43127478   + log(1/(1+exp(-ALPHA_43127478  ))) + ((1/(1+exp(-ALPHA_43127478  ))) - 1) * log(choice_data$volume_liters_43127478  + 1 )
  v43128482   <- ASC43128482   + PRICE1 * choice_data$state_cost_43128482   + log(1/(1+exp(-ALPHA_43128482  ))) + ((1/(1+exp(-ALPHA_43128482  ))) - 1) * log(choice_data$volume_liters_43128482  + 1 )
  v43136446   <- ASC43136446   + PRICE1 * choice_data$state_cost_43136446   + log(1/(1+exp(-ALPHA_43136446  ))) + ((1/(1+exp(-ALPHA_43136446  ))) - 1) * log(choice_data$volume_liters_43136446  + 1 )
  v43137443   <- ASC43137443   + PRICE1 * choice_data$state_cost_43137443   + log(1/(1+exp(-ALPHA_43137443  ))) + ((1/(1+exp(-ALPHA_43137443  ))) - 1) * log(choice_data$volume_liters_43137443  + 1 )
  v44217554   <- ASC44217554   + PRICE1 * choice_data$state_cost_44217554   + log(1/(1+exp(-ALPHA_44217554  ))) + ((1/(1+exp(-ALPHA_44217554  ))) - 1) * log(choice_data$volume_liters_44217554  + 1 )
  v68036500   <- ASC68036500   + PRICE1 * choice_data$state_cost_68036500   + log(1/(1+exp(-ALPHA_68036500  ))) + ((1/(1+exp(-ALPHA_68036500  ))) - 1) * log(choice_data$volume_liters_68036500  + 1 )
  v68037498   <- ASC68037498   + PRICE1 * choice_data$state_cost_68037498   + log(1/(1+exp(-ALPHA_68037498  ))) + ((1/(1+exp(-ALPHA_68037498  ))) - 1) * log(choice_data$volume_liters_68037498  + 1 )
  v88283243   <- ASC88283243   + PRICE1 * choice_data$state_cost_88283243   + log(1/(1+exp(-ALPHA_88283243  ))) + ((1/(1+exp(-ALPHA_88283243  ))) - 1) * log(choice_data$volume_liters_88283243  + 1 )
  v100084433  <- ASC100084433  + PRICE1 * choice_data$state_cost_100084433  + log(1/(1+exp(-ALPHA_100084433 ))) + ((1/(1+exp(-ALPHA_100084433 ))) - 1) * log(choice_data$volume_liters_100084433 + 1 )
  v106271048  <- ASC106271048  + PRICE1 * choice_data$state_cost_106271048  + log(1/(1+exp(-ALPHA_106271048 ))) + ((1/(1+exp(-ALPHA_106271048 ))) - 1) * log(choice_data$volume_liters_106271048 + 1 )
  v106281053  <- ASC106281053  + PRICE1 * choice_data$state_cost_106281053  + log(1/(1+exp(-ALPHA_106281053 ))) + ((1/(1+exp(-ALPHA_106281053 ))) - 1) * log(choice_data$volume_liters_106281053 + 1 )
  v108071511  <- ASC108071511  + PRICE1 * choice_data$state_cost_108071511  + log(1/(1+exp(-ALPHA_108071511 ))) + ((1/(1+exp(-ALPHA_108071511 ))) - 1) * log(choice_data$volume_liters_108071511 + 1 )
  v112961476  <- ASC112961476  + PRICE1 * choice_data$state_cost_112961476  + log(1/(1+exp(-ALPHA_112961476 ))) + ((1/(1+exp(-ALPHA_112961476 ))) - 1) * log(choice_data$volume_liters_112961476 + 1 )
  v112971485  <- ASC112971485  + PRICE1 * choice_data$state_cost_112971485  + log(1/(1+exp(-ALPHA_112971485 ))) + ((1/(1+exp(-ALPHA_112971485 ))) - 1) * log(choice_data$volume_liters_112971485 + 1 )
  v112981488  <- ASC112981488  + PRICE1 * choice_data$state_cost_112981488  + log(1/(1+exp(-ALPHA_112981488 ))) + ((1/(1+exp(-ALPHA_112981488 ))) - 1) * log(choice_data$volume_liters_112981488 + 1 )
  v124071064  <- ASC124071064  + PRICE1 * choice_data$state_cost_124071064  + log(1/(1+exp(-ALPHA_124071064 ))) + ((1/(1+exp(-ALPHA_124071064 ))) - 1) * log(choice_data$volume_liters_124071064 + 1 )
  v124081067  <- ASC124081067  + PRICE1 * choice_data$state_cost_124081067  + log(1/(1+exp(-ALPHA_124081067 ))) + ((1/(1+exp(-ALPHA_124081067 ))) - 1) * log(choice_data$volume_liters_124081067 + 1 )
  v124781072  <- ASC124781072  + PRICE1 * choice_data$state_cost_124781072  + log(1/(1+exp(-ALPHA_124781072 ))) + ((1/(1+exp(-ALPHA_124781072 ))) - 1) * log(choice_data$volume_liters_124781072 + 1 )
  v130381081  <- ASC130381081  + PRICE1 * choice_data$state_cost_130381081  + log(1/(1+exp(-ALPHA_130381081 ))) + ((1/(1+exp(-ALPHA_130381081 ))) - 1) * log(choice_data$volume_liters_130381081 + 1 )
  v152485359  <- ASC152485359  + PRICE1 * choice_data$state_cost_152485359  + log(1/(1+exp(-ALPHA_152485359 ))) + ((1/(1+exp(-ALPHA_152485359 ))) - 1) * log(choice_data$volume_liters_152485359 + 1 )
  v156262825  <- ASC156262825  + PRICE1 * choice_data$state_cost_156262825  + log(1/(1+exp(-ALPHA_156262825 ))) + ((1/(1+exp(-ALPHA_156262825 ))) - 1) * log(choice_data$volume_liters_156262825 + 1 )
  v156272824  <- ASC156272824  + PRICE1 * choice_data$state_cost_156272824  + log(1/(1+exp(-ALPHA_156272824 ))) + ((1/(1+exp(-ALPHA_156272824 ))) - 1) * log(choice_data$volume_liters_156272824 + 1 )
  v179561971  <- ASC179561971  + PRICE1 * choice_data$state_cost_179561971  + log(1/(1+exp(-ALPHA_179561971 ))) + ((1/(1+exp(-ALPHA_179561971 ))) - 1) * log(choice_data$volume_liters_179561971 + 1 )
  v179581973  <- ASC179581973  + PRICE1 * choice_data$state_cost_179581973  + log(1/(1+exp(-ALPHA_179581973 ))) + ((1/(1+exp(-ALPHA_179581973 ))) - 1) * log(choice_data$volume_liters_179581973 + 1 )
  v190662873  <- ASC190662873  + PRICE1 * choice_data$state_cost_190662873  + log(1/(1+exp(-ALPHA_190662873 ))) + ((1/(1+exp(-ALPHA_190662873 ))) - 1) * log(choice_data$volume_liters_190662873 + 1 )
  v190672872  <- ASC190672872  + PRICE1 * choice_data$state_cost_190672872  + log(1/(1+exp(-ALPHA_190672872 ))) + ((1/(1+exp(-ALPHA_190672872 ))) - 1) * log(choice_data$volume_liters_190672872 + 1 )
  v190682877  <- ASC190682877  + PRICE1 * choice_data$state_cost_190682877  + log(1/(1+exp(-ALPHA_190682877 ))) + ((1/(1+exp(-ALPHA_190682877 ))) - 1) * log(choice_data$volume_liters_190682877 + 1 )
  v194763335  <- ASC194763335  + PRICE1 * choice_data$state_cost_194763335  + log(1/(1+exp(-ALPHA_194763335 ))) + ((1/(1+exp(-ALPHA_194763335 ))) - 1) * log(choice_data$volume_liters_194763335 + 1 )
  v194773333  <- ASC194773333  + PRICE1 * choice_data$state_cost_194773333  + log(1/(1+exp(-ALPHA_194773333 ))) + ((1/(1+exp(-ALPHA_194773333 ))) - 1) * log(choice_data$volume_liters_194773333 + 1 )
  v202483690  <- ASC202483690  + PRICE1 * choice_data$state_cost_202483690  + log(1/(1+exp(-ALPHA_202483690 ))) + ((1/(1+exp(-ALPHA_202483690 ))) - 1) * log(choice_data$volume_liters_202483690 + 1 )
  v215974940  <- ASC215974940  + PRICE1 * choice_data$state_cost_215974940  + log(1/(1+exp(-ALPHA_215974940 ))) + ((1/(1+exp(-ALPHA_215974940 ))) - 1) * log(choice_data$volume_liters_215974940 + 1 )
  v215984943  <- ASC215984943  + PRICE1 * choice_data$state_cost_215984943  + log(1/(1+exp(-ALPHA_215984943 ))) + ((1/(1+exp(-ALPHA_215984943 ))) - 1) * log(choice_data$volume_liters_215984943 + 1 )
  v238242082  <- ASC238242082  + PRICE1 * choice_data$state_cost_238242082  + log(1/(1+exp(-ALPHA_238242082 ))) + ((1/(1+exp(-ALPHA_238242082 ))) - 1) * log(choice_data$volume_liters_238242082 + 1 )
  v238262081  <- ASC238262081  + PRICE1 * choice_data$state_cost_238262081  + log(1/(1+exp(-ALPHA_238262081 ))) + ((1/(1+exp(-ALPHA_238262081 ))) - 1) * log(choice_data$volume_liters_238262081 + 1 )
  v238272080  <- ASC238272080  + PRICE1 * choice_data$state_cost_238272080  + log(1/(1+exp(-ALPHA_238272080 ))) + ((1/(1+exp(-ALPHA_238272080 ))) - 1) * log(choice_data$volume_liters_238272080 + 1 )
  v238282084  <- ASC238282084  + PRICE1 * choice_data$state_cost_238282084  + log(1/(1+exp(-ALPHA_238282084 ))) + ((1/(1+exp(-ALPHA_238282084 ))) - 1) * log(choice_data$volume_liters_238282084 + 1 )
  v241582464  <- ASC241582464  + PRICE1 * choice_data$state_cost_241582464  + log(1/(1+exp(-ALPHA_241582464 ))) + ((1/(1+exp(-ALPHA_241582464 ))) - 1) * log(choice_data$volume_liters_241582464 + 1 )
  v244573083  <- ASC244573083  + PRICE1 * choice_data$state_cost_244573083  + log(1/(1+exp(-ALPHA_244573083 ))) + ((1/(1+exp(-ALPHA_244573083 ))) - 1) * log(choice_data$volume_liters_244573083 + 1 )
  v244583087  <- ASC244583087  + PRICE1 * choice_data$state_cost_244583087  + log(1/(1+exp(-ALPHA_244583087 ))) + ((1/(1+exp(-ALPHA_244583087 ))) - 1) * log(choice_data$volume_liters_244583087 + 1 )
  v256064473  <- ASC256064473  + PRICE1 * choice_data$state_cost_256064473  + log(1/(1+exp(-ALPHA_256064473 ))) + ((1/(1+exp(-ALPHA_256064473 ))) - 1) * log(choice_data$volume_liters_256064473 + 1 )
  v256074472  <- ASC256074472  + PRICE1 * choice_data$state_cost_256074472  + log(1/(1+exp(-ALPHA_256074472 ))) + ((1/(1+exp(-ALPHA_256074472 ))) - 1) * log(choice_data$volume_liters_256074472 + 1 )
  v256084476  <- ASC256084476  + PRICE1 * choice_data$state_cost_256084476  + log(1/(1+exp(-ALPHA_256084476 ))) + ((1/(1+exp(-ALPHA_256084476 ))) - 1) * log(choice_data$volume_liters_256084476 + 1 )
  v268262788  <- ASC268262788  + PRICE1 * choice_data$state_cost_268262788  + log(1/(1+exp(-ALPHA_268262788 ))) + ((1/(1+exp(-ALPHA_268262788 ))) - 1) * log(choice_data$volume_liters_268262788 + 1 )
  v268272787  <- ASC268272787  + PRICE1 * choice_data$state_cost_268272787  + log(1/(1+exp(-ALPHA_268272787 ))) + ((1/(1+exp(-ALPHA_268272787 ))) - 1) * log(choice_data$volume_liters_268272787 + 1 )
  v268282790  <- ASC268282790  + PRICE1 * choice_data$state_cost_268282790  + log(1/(1+exp(-ALPHA_268282790 ))) + ((1/(1+exp(-ALPHA_268282790 ))) - 1) * log(choice_data$volume_liters_268282790 + 1 )
  v271024938  <- ASC271024938  + PRICE1 * choice_data$state_cost_271024938  + log(1/(1+exp(-ALPHA_271024938 ))) + ((1/(1+exp(-ALPHA_271024938 ))) - 1) * log(choice_data$volume_liters_271024938 + 1 )
  v288664915  <- ASC288664915  + PRICE1 * choice_data$state_cost_288664915  + log(1/(1+exp(-ALPHA_288664915 ))) + ((1/(1+exp(-ALPHA_288664915 ))) - 1) * log(choice_data$volume_liters_288664915 + 1 )
  v288674914  <- ASC288674914  + PRICE1 * choice_data$state_cost_288674914  + log(1/(1+exp(-ALPHA_288674914 ))) + ((1/(1+exp(-ALPHA_288674914 ))) - 1) * log(choice_data$volume_liters_288674914 + 1 )
  v316583822  <- ASC316583822  + PRICE1 * choice_data$state_cost_316583822  + log(1/(1+exp(-ALPHA_316583822 ))) + ((1/(1+exp(-ALPHA_316583822 ))) - 1) * log(choice_data$volume_liters_316583822 + 1 )
  v322364483  <- ASC322364483  + PRICE1 * choice_data$state_cost_322364483  + log(1/(1+exp(-ALPHA_322364483 ))) + ((1/(1+exp(-ALPHA_322364483 ))) - 1) * log(choice_data$volume_liters_322364483 + 1 )
  v322384487  <- ASC322384487  + PRICE1 * choice_data$state_cost_322384487  + log(1/(1+exp(-ALPHA_322384487 ))) + ((1/(1+exp(-ALPHA_322384487 ))) - 1) * log(choice_data$volume_liters_322384487 + 1 )
  v343682147  <- ASC343682147  + PRICE1 * choice_data$state_cost_343682147  + log(1/(1+exp(-ALPHA_343682147 ))) + ((1/(1+exp(-ALPHA_343682147 ))) - 1) * log(choice_data$volume_liters_343682147 + 1 )
  v344222417  <- ASC344222417  + PRICE1 * choice_data$state_cost_344222417  + log(1/(1+exp(-ALPHA_344222417 ))) + ((1/(1+exp(-ALPHA_344222417 ))) - 1) * log(choice_data$volume_liters_344222417 + 1 )
  v344332416  <- ASC344332416  + PRICE1 * choice_data$state_cost_344332416  + log(1/(1+exp(-ALPHA_344332416 ))) + ((1/(1+exp(-ALPHA_344332416 ))) - 1) * log(choice_data$volume_liters_344332416 + 1 )
  v344573094  <- ASC344573094  + PRICE1 * choice_data$state_cost_344573094  + log(1/(1+exp(-ALPHA_344573094 ))) + ((1/(1+exp(-ALPHA_344573094 ))) - 1) * log(choice_data$volume_liters_344573094 + 1 )
  v345784107  <- ASC345784107  + PRICE1 * choice_data$state_cost_345784107  + log(1/(1+exp(-ALPHA_345784107 ))) + ((1/(1+exp(-ALPHA_345784107 ))) - 1) * log(choice_data$volume_liters_345784107 + 1 )
  v347474844  <- ASC347474844  + PRICE1 * choice_data$state_cost_347474844  + log(1/(1+exp(-ALPHA_347474844 ))) + ((1/(1+exp(-ALPHA_347474844 ))) - 1) * log(choice_data$volume_liters_347474844 + 1 )
  v348204895  <- ASC348204895  + PRICE1 * choice_data$state_cost_348204895  + log(1/(1+exp(-ALPHA_348204895 ))) + ((1/(1+exp(-ALPHA_348204895 ))) - 1) * log(choice_data$volume_liters_348204895 + 1 )
  v348214898  <- ASC348214898  + PRICE1 * choice_data$state_cost_348214898  + log(1/(1+exp(-ALPHA_348214898 ))) + ((1/(1+exp(-ALPHA_348214898 ))) - 1) * log(choice_data$volume_liters_348214898 + 1 )
  v359142077  <- ASC359142077  + PRICE1 * choice_data$state_cost_359142077  + log(1/(1+exp(-ALPHA_359142077 ))) + ((1/(1+exp(-ALPHA_359142077 ))) - 1) * log(choice_data$volume_liters_359142077 + 1 )
  v359162076  <- ASC359162076  + PRICE1 * choice_data$state_cost_359162076  + log(1/(1+exp(-ALPHA_359162076 ))) + ((1/(1+exp(-ALPHA_359162076 ))) - 1) * log(choice_data$volume_liters_359162076 + 1 )
  v359172075  <- ASC359172075  + PRICE1 * choice_data$state_cost_359172075  + log(1/(1+exp(-ALPHA_359172075 ))) + ((1/(1+exp(-ALPHA_359172075 ))) - 1) * log(choice_data$volume_liters_359172075 + 1 )
  v359182079  <- ASC359182079  + PRICE1 * choice_data$state_cost_359182079  + log(1/(1+exp(-ALPHA_359182079 ))) + ((1/(1+exp(-ALPHA_359182079 ))) - 1) * log(choice_data$volume_liters_359182079 + 1 )
  v359262073  <- ASC359262073  + PRICE1 * choice_data$state_cost_359262073  + log(1/(1+exp(-ALPHA_359262073 ))) + ((1/(1+exp(-ALPHA_359262073 ))) - 1) * log(choice_data$volume_liters_359262073 + 1 )
  v359482096  <- ASC359482096  + PRICE1 * choice_data$state_cost_359482096  + log(1/(1+exp(-ALPHA_359482096 ))) + ((1/(1+exp(-ALPHA_359482096 ))) - 1) * log(choice_data$volume_liters_359482096 + 1 )
  v363042475  <- ASC363042475  + PRICE1 * choice_data$state_cost_363042475  + log(1/(1+exp(-ALPHA_363042475 ))) + ((1/(1+exp(-ALPHA_363042475 ))) - 1) * log(choice_data$volume_liters_363042475 + 1 )
  v363052474  <- ASC363052474  + PRICE1 * choice_data$state_cost_363052474  + log(1/(1+exp(-ALPHA_363052474 ))) + ((1/(1+exp(-ALPHA_363052474 ))) - 1) * log(choice_data$volume_liters_363052474 + 1 )
  v363072473  <- ASC363072473  + PRICE1 * choice_data$state_cost_363072473  + log(1/(1+exp(-ALPHA_363072473 ))) + ((1/(1+exp(-ALPHA_363072473 ))) - 1) * log(choice_data$volume_liters_363072473 + 1 )
  v363082477  <- ASC363082477  + PRICE1 * choice_data$state_cost_363082477  + log(1/(1+exp(-ALPHA_363082477 ))) + ((1/(1+exp(-ALPHA_363082477 ))) - 1) * log(choice_data$volume_liters_363082477 + 1 )
  v368863465  <- ASC368863465  + PRICE1 * choice_data$state_cost_368863465  + log(1/(1+exp(-ALPHA_368863465 ))) + ((1/(1+exp(-ALPHA_368863465 ))) - 1) * log(choice_data$volume_liters_368863465 + 1 )
  v369033466  <- ASC369033466  + PRICE1 * choice_data$state_cost_369033466  + log(1/(1+exp(-ALPHA_369033466 ))) + ((1/(1+exp(-ALPHA_369033466 ))) - 1) * log(choice_data$volume_liters_369033466 + 1 )
  v369043469  <- ASC369043469  + PRICE1 * choice_data$state_cost_369043469  + log(1/(1+exp(-ALPHA_369043469 ))) + ((1/(1+exp(-ALPHA_369043469 ))) - 1) * log(choice_data$volume_liters_369043469 + 1 )
  v369073471  <- ASC369073471  + PRICE1 * choice_data$state_cost_369073471  + log(1/(1+exp(-ALPHA_369073471 ))) + ((1/(1+exp(-ALPHA_369073471 ))) - 1) * log(choice_data$volume_liters_369073471 + 1 )
  v369083470  <- ASC369083470  + PRICE1 * choice_data$state_cost_369083470  + log(1/(1+exp(-ALPHA_369083470 ))) + ((1/(1+exp(-ALPHA_369083470 ))) - 1) * log(choice_data$volume_liters_369083470 + 1 )
  v369693633  <- ASC369693633  + PRICE1 * choice_data$state_cost_369693633  + log(1/(1+exp(-ALPHA_369693633 ))) + ((1/(1+exp(-ALPHA_369693633 ))) - 1) * log(choice_data$volume_liters_369693633 + 1 )
  v369713637  <- ASC369713637  + PRICE1 * choice_data$state_cost_369713637  + log(1/(1+exp(-ALPHA_369713637 ))) + ((1/(1+exp(-ALPHA_369713637 ))) - 1) * log(choice_data$volume_liters_369713637 + 1 )
  v369763644  <- ASC369763644  + PRICE1 * choice_data$state_cost_369763644  + log(1/(1+exp(-ALPHA_369763644 ))) + ((1/(1+exp(-ALPHA_369763644 ))) - 1) * log(choice_data$volume_liters_369763644 + 1 )
  v369783647  <- ASC369783647  + PRICE1 * choice_data$state_cost_369783647  + log(1/(1+exp(-ALPHA_369783647 ))) + ((1/(1+exp(-ALPHA_369783647 ))) - 1) * log(choice_data$volume_liters_369783647 + 1 )
  v373365237  <- ASC373365237  + PRICE1 * choice_data$state_cost_373365237  + log(1/(1+exp(-ALPHA_373365237 ))) + ((1/(1+exp(-ALPHA_373365237 ))) - 1) * log(choice_data$volume_liters_373365237 + 1 )
  v373385241  <- ASC373385241  + PRICE1 * choice_data$state_cost_373385241  + log(1/(1+exp(-ALPHA_373385241 ))) + ((1/(1+exp(-ALPHA_373385241 ))) - 1) * log(choice_data$volume_liters_373385241 + 1 )
  v373464016  <- ASC373464016  + PRICE1 * choice_data$state_cost_373464016  + log(1/(1+exp(-ALPHA_373464016 ))) + ((1/(1+exp(-ALPHA_373464016 ))) - 1) * log(choice_data$volume_liters_373464016 + 1 )
  v373474014  <- ASC373474014  + PRICE1 * choice_data$state_cost_373474014  + log(1/(1+exp(-ALPHA_373474014 ))) + ((1/(1+exp(-ALPHA_373474014 ))) - 1) * log(choice_data$volume_liters_373474014 + 1 )
  v373484017  <- ASC373484017  + PRICE1 * choice_data$state_cost_373484017  + log(1/(1+exp(-ALPHA_373484017 ))) + ((1/(1+exp(-ALPHA_373484017 ))) - 1) * log(choice_data$volume_liters_373484017 + 1 )
  v374174133  <- ASC374174133  + PRICE1 * choice_data$state_cost_374174133  + log(1/(1+exp(-ALPHA_374174133 ))) + ((1/(1+exp(-ALPHA_374174133 ))) - 1) * log(choice_data$volume_liters_374174133 + 1 )
  v374184137  <- ASC374184137  + PRICE1 * choice_data$state_cost_374184137  + log(1/(1+exp(-ALPHA_374184137 ))) + ((1/(1+exp(-ALPHA_374184137 ))) - 1) * log(choice_data$volume_liters_374184137 + 1 )
  v379384566  <- ASC379384566  + PRICE1 * choice_data$state_cost_379384566  + log(1/(1+exp(-ALPHA_379384566 ))) + ((1/(1+exp(-ALPHA_379384566 ))) - 1) * log(choice_data$volume_liters_379384566 + 1 )
  v379944707  <- ASC379944707  + PRICE1 * choice_data$state_cost_379944707  + log(1/(1+exp(-ALPHA_379944707 ))) + ((1/(1+exp(-ALPHA_379944707 ))) - 1) * log(choice_data$volume_liters_379944707 + 1 )
  v379964706  <- ASC379964706  + PRICE1 * choice_data$state_cost_379964706  + log(1/(1+exp(-ALPHA_379964706 ))) + ((1/(1+exp(-ALPHA_379964706 ))) - 1) * log(choice_data$volume_liters_379964706 + 1 )
  v379974705  <- ASC379974705  + PRICE1 * choice_data$state_cost_379974705  + log(1/(1+exp(-ALPHA_379974705 ))) + ((1/(1+exp(-ALPHA_379974705 ))) - 1) * log(choice_data$volume_liters_379974705 + 1 )
  v379984709  <- ASC379984709  + PRICE1 * choice_data$state_cost_379984709  + log(1/(1+exp(-ALPHA_379984709 ))) + ((1/(1+exp(-ALPHA_379984709 ))) - 1) * log(choice_data$volume_liters_379984709 + 1 )
  v380064712  <- ASC380064712  + PRICE1 * choice_data$state_cost_380064712  + log(1/(1+exp(-ALPHA_380064712 ))) + ((1/(1+exp(-ALPHA_380064712 ))) - 1) * log(choice_data$volume_liters_380064712 + 1 )
  v380084711  <- ASC380084711  + PRICE1 * choice_data$state_cost_380084711  + log(1/(1+exp(-ALPHA_380084711 ))) + ((1/(1+exp(-ALPHA_380084711 ))) - 1) * log(choice_data$volume_liters_380084711 + 1 )
  v380884122  <- ASC380884122  + PRICE1 * choice_data$state_cost_380884122  + log(1/(1+exp(-ALPHA_380884122 ))) + ((1/(1+exp(-ALPHA_380884122 ))) - 1) * log(choice_data$volume_liters_380884122 + 1 )
  v381765067  <- ASC381765067  + PRICE1 * choice_data$state_cost_381765067  + log(1/(1+exp(-ALPHA_381765067 ))) + ((1/(1+exp(-ALPHA_381765067 ))) - 1) * log(choice_data$volume_liters_381765067 + 1 )
  v381775064  <- ASC381775064  + PRICE1 * choice_data$state_cost_381775064  + log(1/(1+exp(-ALPHA_381775064 ))) + ((1/(1+exp(-ALPHA_381775064 ))) - 1) * log(choice_data$volume_liters_381775064 + 1 )
  v381785066  <- ASC381785066  + PRICE1 * choice_data$state_cost_381785066  + log(1/(1+exp(-ALPHA_381785066 ))) + ((1/(1+exp(-ALPHA_381785066 ))) - 1) * log(choice_data$volume_liters_381785066 + 1 )
  v410762864  <- ASC410762864  + PRICE1 * choice_data$state_cost_410762864  + log(1/(1+exp(-ALPHA_410762864 ))) + ((1/(1+exp(-ALPHA_410762864 ))) - 1) * log(choice_data$volume_liters_410762864 + 1 )
  v416925185  <- ASC416925185  + PRICE1 * choice_data$state_cost_416925185  + log(1/(1+exp(-ALPHA_416925185 ))) + ((1/(1+exp(-ALPHA_416925185 ))) - 1) * log(choice_data$volume_liters_416925185 + 1 )
  v416935184  <- ASC416935184  + PRICE1 * choice_data$state_cost_416935184  + log(1/(1+exp(-ALPHA_416935184 ))) + ((1/(1+exp(-ALPHA_416935184 ))) - 1) * log(choice_data$volume_liters_416935184 + 1 )
  v416945182  <- ASC416945182  + PRICE1 * choice_data$state_cost_416945182  + log(1/(1+exp(-ALPHA_416945182 ))) + ((1/(1+exp(-ALPHA_416945182 ))) - 1) * log(choice_data$volume_liters_416945182 + 1 )
  v417045220  <- ASC417045220  + PRICE1 * choice_data$state_cost_417045220  + log(1/(1+exp(-ALPHA_417045220 ))) + ((1/(1+exp(-ALPHA_417045220 ))) - 1) * log(choice_data$volume_liters_417045220 + 1 )
  v418461979  <- ASC418461979  + PRICE1 * choice_data$state_cost_418461979  + log(1/(1+exp(-ALPHA_418461979 ))) + ((1/(1+exp(-ALPHA_418461979 ))) - 1) * log(choice_data$volume_liters_418461979 + 1 )
  v419895193  <- ASC419895193  + PRICE1 * choice_data$state_cost_419895193  + log(1/(1+exp(-ALPHA_419895193 ))) + ((1/(1+exp(-ALPHA_419895193 ))) - 1) * log(choice_data$volume_liters_419895193 + 1 )
  v427163350  <- ASC427163350  + PRICE1 * choice_data$state_cost_427163350  + log(1/(1+exp(-ALPHA_427163350 ))) + ((1/(1+exp(-ALPHA_427163350 ))) - 1) * log(choice_data$volume_liters_427163350 + 1 )
  v427173349  <- ASC427173349  + PRICE1 * choice_data$state_cost_427173349  + log(1/(1+exp(-ALPHA_427173349 ))) + ((1/(1+exp(-ALPHA_427173349 ))) - 1) * log(choice_data$volume_liters_427173349 + 1 )
  v427183352  <- ASC427183352  + PRICE1 * choice_data$state_cost_427183352  + log(1/(1+exp(-ALPHA_427183352 ))) + ((1/(1+exp(-ALPHA_427183352 ))) - 1) * log(choice_data$volume_liters_427183352 + 1 )
  v432851114  <- ASC432851114  + PRICE1 * choice_data$state_cost_432851114  + log(1/(1+exp(-ALPHA_432851114 ))) + ((1/(1+exp(-ALPHA_432851114 ))) - 1) * log(choice_data$volume_liters_432851114 + 1 )
  v433341134  <- ASC433341134  + PRICE1 * choice_data$state_cost_433341134  + log(1/(1+exp(-ALPHA_433341134 ))) + ((1/(1+exp(-ALPHA_433341134 ))) - 1) * log(choice_data$volume_liters_433341134 + 1 )
  v433351138  <- ASC433351138  + PRICE1 * choice_data$state_cost_433351138  + log(1/(1+exp(-ALPHA_433351138 ))) + ((1/(1+exp(-ALPHA_433351138 ))) - 1) * log(choice_data$volume_liters_433351138 + 1 )
  v433361110  <- ASC433361110  + PRICE1 * choice_data$state_cost_433361110  + log(1/(1+exp(-ALPHA_433361110 ))) + ((1/(1+exp(-ALPHA_433361110 ))) - 1) * log(choice_data$volume_liters_433361110 + 1 )
  v433371133  <- ASC433371133  + PRICE1 * choice_data$state_cost_433371133  + log(1/(1+exp(-ALPHA_433371133 ))) + ((1/(1+exp(-ALPHA_433371133 ))) - 1) * log(choice_data$volume_liters_433371133 + 1 )
  v433381135  <- ASC433381135  + PRICE1 * choice_data$state_cost_433381135  + log(1/(1+exp(-ALPHA_433381135 ))) + ((1/(1+exp(-ALPHA_433381135 ))) - 1) * log(choice_data$volume_liters_433381135 + 1 )
  v433871168  <- ASC433871168  + PRICE1 * choice_data$state_cost_433871168  + log(1/(1+exp(-ALPHA_433871168 ))) + ((1/(1+exp(-ALPHA_433871168 ))) - 1) * log(choice_data$volume_liters_433871168 + 1 )
  v446583206  <- ASC446583206  + PRICE1 * choice_data$state_cost_446583206  + log(1/(1+exp(-ALPHA_446583206 ))) + ((1/(1+exp(-ALPHA_446583206 ))) - 1) * log(choice_data$volume_liters_446583206 + 1 )
  v452483827  <- ASC452483827  + PRICE1 * choice_data$state_cost_452483827  + log(1/(1+exp(-ALPHA_452483827 ))) + ((1/(1+exp(-ALPHA_452483827 ))) - 1) * log(choice_data$volume_liters_452483827 + 1 )
  v452763855  <- ASC452763855  + PRICE1 * choice_data$state_cost_452763855  + log(1/(1+exp(-ALPHA_452763855 ))) + ((1/(1+exp(-ALPHA_452763855 ))) - 1) * log(choice_data$volume_liters_452763855 + 1 )
  v452773854  <- ASC452773854  + PRICE1 * choice_data$state_cost_452773854  + log(1/(1+exp(-ALPHA_452773854 ))) + ((1/(1+exp(-ALPHA_452773854 ))) - 1) * log(choice_data$volume_liters_452773854 + 1 )
  v452783857  <- ASC452783857  + PRICE1 * choice_data$state_cost_452783857  + log(1/(1+exp(-ALPHA_452783857 ))) + ((1/(1+exp(-ALPHA_452783857 ))) - 1) * log(choice_data$volume_liters_452783857 + 1 )
  v458864376  <- ASC458864376  + PRICE1 * choice_data$state_cost_458864376  + log(1/(1+exp(-ALPHA_458864376 ))) + ((1/(1+exp(-ALPHA_458864376 ))) - 1) * log(choice_data$volume_liters_458864376 + 1 )
  v463502472  <- ASC463502472  + PRICE1 * choice_data$state_cost_463502472  + log(1/(1+exp(-ALPHA_463502472 ))) + ((1/(1+exp(-ALPHA_463502472 ))) - 1) * log(choice_data$volume_liters_463502472 + 1 )
  v481052499  <- ASC481052499  + PRICE1 * choice_data$state_cost_481052499  + log(1/(1+exp(-ALPHA_481052499 ))) + ((1/(1+exp(-ALPHA_481052499 ))) - 1) * log(choice_data$volume_liters_481052499 + 1 )
  v481062503  <- ASC481062503  + PRICE1 * choice_data$state_cost_481062503  + log(1/(1+exp(-ALPHA_481062503 ))) + ((1/(1+exp(-ALPHA_481062503 ))) - 1) * log(choice_data$volume_liters_481062503 + 1 )
  v523161282  <- ASC523161282  + PRICE1 * choice_data$state_cost_523161282  + log(1/(1+exp(-ALPHA_523161282 ))) + ((1/(1+exp(-ALPHA_523161282 ))) - 1) * log(choice_data$volume_liters_523161282 + 1 )
  v525961862  <- ASC525961862  + PRICE1 * choice_data$state_cost_525961862  + log(1/(1+exp(-ALPHA_525961862 ))) + ((1/(1+exp(-ALPHA_525961862 ))) - 1) * log(choice_data$volume_liters_525961862 + 1 )
  v525981863  <- ASC525981863  + PRICE1 * choice_data$state_cost_525981863  + log(1/(1+exp(-ALPHA_525981863 ))) + ((1/(1+exp(-ALPHA_525981863 ))) - 1) * log(choice_data$volume_liters_525981863 + 1 )
  v532143913  <- ASC532143913  + PRICE1 * choice_data$state_cost_532143913  + log(1/(1+exp(-ALPHA_532143913 ))) + ((1/(1+exp(-ALPHA_532143913 ))) - 1) * log(choice_data$volume_liters_532143913 + 1 )
  v550863803  <- ASC550863803  + PRICE1 * choice_data$state_cost_550863803  + log(1/(1+exp(-ALPHA_550863803 ))) + ((1/(1+exp(-ALPHA_550863803 ))) - 1) * log(choice_data$volume_liters_550863803 + 1 )
  v571251241  <- ASC571251241  + PRICE1 * choice_data$state_cost_571251241  + log(1/(1+exp(-ALPHA_571251241 ))) + ((1/(1+exp(-ALPHA_571251241 ))) - 1) * log(choice_data$volume_liters_571251241 + 1 )
  v571481243  <- ASC571481243  + PRICE1 * choice_data$state_cost_571481243  + log(1/(1+exp(-ALPHA_571481243 ))) + ((1/(1+exp(-ALPHA_571481243 ))) - 1) * log(choice_data$volume_liters_571481243 + 1 )
  v588382962  <- ASC588382962  + PRICE1 * choice_data$state_cost_588382962  + log(1/(1+exp(-ALPHA_588382962 ))) + ((1/(1+exp(-ALPHA_588382962 ))) - 1) * log(choice_data$volume_liters_588382962 + 1 )
  v588682969  <- ASC588682969  + PRICE1 * choice_data$state_cost_588682969  + log(1/(1+exp(-ALPHA_588682969 ))) + ((1/(1+exp(-ALPHA_588682969 ))) - 1) * log(choice_data$volume_liters_588682969 + 1 )
  v588722960  <- ASC588722960  + PRICE1 * choice_data$state_cost_588722960  + log(1/(1+exp(-ALPHA_588722960 ))) + ((1/(1+exp(-ALPHA_588722960 ))) - 1) * log(choice_data$volume_liters_588722960 + 1 )
  v588752994  <- ASC588752994  + PRICE1 * choice_data$state_cost_588752994  + log(1/(1+exp(-ALPHA_588752994 ))) + ((1/(1+exp(-ALPHA_588752994 ))) - 1) * log(choice_data$volume_liters_588752994 + 1 )
  v590371737  <- ASC590371737  + PRICE1 * choice_data$state_cost_590371737  + log(1/(1+exp(-ALPHA_590371737 ))) + ((1/(1+exp(-ALPHA_590371737 ))) - 1) * log(choice_data$volume_liters_590371737 + 1 )
  v648582044  <- ASC648582044  + PRICE1 * choice_data$state_cost_648582044  + log(1/(1+exp(-ALPHA_648582044 ))) + ((1/(1+exp(-ALPHA_648582044 ))) - 1) * log(choice_data$volume_liters_648582044 + 1 )
  v648652045  <- ASC648652045  + PRICE1 * choice_data$state_cost_648652045  + log(1/(1+exp(-ALPHA_648652045 ))) + ((1/(1+exp(-ALPHA_648652045 ))) - 1) * log(choice_data$volume_liters_648652045 + 1 )
  v648662041  <- ASC648662041  + PRICE1 * choice_data$state_cost_648662041  + log(1/(1+exp(-ALPHA_648662041 ))) + ((1/(1+exp(-ALPHA_648662041 ))) - 1) * log(choice_data$volume_liters_648662041 + 1 )
  v648672039  <- ASC648672039  + PRICE1 * choice_data$state_cost_648672039  + log(1/(1+exp(-ALPHA_648672039 ))) + ((1/(1+exp(-ALPHA_648672039 ))) - 1) * log(choice_data$volume_liters_648672039 + 1 )
  v648682043  <- ASC648682043  + PRICE1 * choice_data$state_cost_648682043  + log(1/(1+exp(-ALPHA_648682043 ))) + ((1/(1+exp(-ALPHA_648682043 ))) - 1) * log(choice_data$volume_liters_648682043 + 1 )
  v652562809  <- ASC652562809  + PRICE1 * choice_data$state_cost_652562809  + log(1/(1+exp(-ALPHA_652562809 ))) + ((1/(1+exp(-ALPHA_652562809 ))) - 1) * log(choice_data$volume_liters_652562809 + 1 )
  v652572808  <- ASC652572808  + PRICE1 * choice_data$state_cost_652572808  + log(1/(1+exp(-ALPHA_652572808 ))) + ((1/(1+exp(-ALPHA_652572808 ))) - 1) * log(choice_data$volume_liters_652572808 + 1 )
  v675263042  <- ASC675263042  + PRICE1 * choice_data$state_cost_675263042  + log(1/(1+exp(-ALPHA_675263042 ))) + ((1/(1+exp(-ALPHA_675263042 ))) - 1) * log(choice_data$volume_liters_675263042 + 1 )
  v675273041  <- ASC675273041  + PRICE1 * choice_data$state_cost_675273041  + log(1/(1+exp(-ALPHA_675273041 ))) + ((1/(1+exp(-ALPHA_675273041 ))) - 1) * log(choice_data$volume_liters_675273041 + 1 )
  v696361815  <- ASC696361815  + PRICE1 * choice_data$state_cost_696361815  + log(1/(1+exp(-ALPHA_696361815 ))) + ((1/(1+exp(-ALPHA_696361815 ))) - 1) * log(choice_data$volume_liters_696361815 + 1 )
  v696371814  <- ASC696371814  + PRICE1 * choice_data$state_cost_696371814  + log(1/(1+exp(-ALPHA_696371814 ))) + ((1/(1+exp(-ALPHA_696371814 ))) - 1) * log(choice_data$volume_liters_696371814 + 1 )
  v696381817  <- ASC696381817  + PRICE1 * choice_data$state_cost_696381817  + log(1/(1+exp(-ALPHA_696381817 ))) + ((1/(1+exp(-ALPHA_696381817 ))) - 1) * log(choice_data$volume_liters_696381817 + 1 )
  v699474354  <- ASC699474354  + PRICE1 * choice_data$state_cost_699474354  + log(1/(1+exp(-ALPHA_699474354 ))) + ((1/(1+exp(-ALPHA_699474354 ))) - 1) * log(choice_data$volume_liters_699474354 + 1 )
  v730504347  <- ASC730504347  + PRICE1 * choice_data$state_cost_730504347  + log(1/(1+exp(-ALPHA_730504347 ))) + ((1/(1+exp(-ALPHA_730504347 ))) - 1) * log(choice_data$volume_liters_730504347 + 1 )
  v730534345  <- ASC730534345  + PRICE1 * choice_data$state_cost_730534345  + log(1/(1+exp(-ALPHA_730534345 ))) + ((1/(1+exp(-ALPHA_730534345 ))) - 1) * log(choice_data$volume_liters_730534345 + 1 )
  v730554344  <- ASC730554344  + PRICE1 * choice_data$state_cost_730554344  + log(1/(1+exp(-ALPHA_730554344 ))) + ((1/(1+exp(-ALPHA_730554344 ))) - 1) * log(choice_data$volume_liters_730554344 + 1 )
  v750873008  <- ASC750873008  + PRICE1 * choice_data$state_cost_750873008  + log(1/(1+exp(-ALPHA_750873008 ))) + ((1/(1+exp(-ALPHA_750873008 ))) - 1) * log(choice_data$volume_liters_750873008 + 1 )
  v752103114  <- ASC752103114  + PRICE1 * choice_data$state_cost_752103114  + log(1/(1+exp(-ALPHA_752103114 ))) + ((1/(1+exp(-ALPHA_752103114 ))) - 1) * log(choice_data$volume_liters_752103114 + 1 )
  v752143108  <- ASC752143108  + PRICE1 * choice_data$state_cost_752143108  + log(1/(1+exp(-ALPHA_752143108 ))) + ((1/(1+exp(-ALPHA_752143108 ))) - 1) * log(choice_data$volume_liters_752143108 + 1 )
  v764863795  <- ASC764863795  + PRICE1 * choice_data$state_cost_764863795  + log(1/(1+exp(-ALPHA_764863795 ))) + ((1/(1+exp(-ALPHA_764863795 ))) - 1) * log(choice_data$volume_liters_764863795 + 1 )
  v764873794  <- ASC764873794  + PRICE1 * choice_data$state_cost_764873794  + log(1/(1+exp(-ALPHA_764873794 ))) + ((1/(1+exp(-ALPHA_764873794 ))) - 1) * log(choice_data$volume_liters_764873794 + 1 )
  v774875094  <- ASC774875094  + PRICE1 * choice_data$state_cost_774875094  + log(1/(1+exp(-ALPHA_774875094 ))) + ((1/(1+exp(-ALPHA_774875094 ))) - 1) * log(choice_data$volume_liters_774875094 + 1 )
  v812063836  <- ASC812063836  + PRICE1 * choice_data$state_cost_812063836  + log(1/(1+exp(-ALPHA_812063836 ))) + ((1/(1+exp(-ALPHA_812063836 ))) - 1) * log(choice_data$volume_liters_812063836 + 1 )
  v812073835  <- ASC812073835  + PRICE1 * choice_data$state_cost_812073835  + log(1/(1+exp(-ALPHA_812073835 ))) + ((1/(1+exp(-ALPHA_812073835 ))) - 1) * log(choice_data$volume_liters_812073835 + 1 )
  v812083838  <- ASC812083838  + PRICE1 * choice_data$state_cost_812083838  + log(1/(1+exp(-ALPHA_812083838 ))) + ((1/(1+exp(-ALPHA_812083838 ))) - 1) * log(choice_data$volume_liters_812083838 + 1 )
  v826071715  <- ASC826071715  + PRICE1 * choice_data$state_cost_826071715  + log(1/(1+exp(-ALPHA_826071715 ))) + ((1/(1+exp(-ALPHA_826071715 ))) - 1) * log(choice_data$volume_liters_826071715 + 1 )
  v826371666  <- ASC826371666  + PRICE1 * choice_data$state_cost_826371666  + log(1/(1+exp(-ALPHA_826371666 ))) + ((1/(1+exp(-ALPHA_826371666 ))) - 1) * log(choice_data$volume_liters_826371666 + 1 )
  v827871639  <- ASC827871639  + PRICE1 * choice_data$state_cost_827871639  + log(1/(1+exp(-ALPHA_827871639 ))) + ((1/(1+exp(-ALPHA_827871639 ))) - 1) * log(choice_data$volume_liters_827871639 + 1 )
  v828461681  <- ASC828461681  + PRICE1 * choice_data$state_cost_828461681  + log(1/(1+exp(-ALPHA_828461681 ))) + ((1/(1+exp(-ALPHA_828461681 ))) - 1) * log(choice_data$volume_liters_828461681 + 1 )
  v828471697  <- ASC828471697  + PRICE1 * choice_data$state_cost_828471697  + log(1/(1+exp(-ALPHA_828471697 ))) + ((1/(1+exp(-ALPHA_828471697 ))) - 1) * log(choice_data$volume_liters_828471697 + 1 )
  v828671728  <- ASC828671728  + PRICE1 * choice_data$state_cost_828671728  + log(1/(1+exp(-ALPHA_828671728 ))) + ((1/(1+exp(-ALPHA_828671728 ))) - 1) * log(choice_data$volume_liters_828671728 + 1 )
  v861121724  <- ASC861121724  + PRICE1 * choice_data$state_cost_861121724  + log(1/(1+exp(-ALPHA_861121724 ))) + ((1/(1+exp(-ALPHA_861121724 ))) - 1) * log(choice_data$volume_liters_861121724 + 1 )
  v862513015  <- ASC862513015  + PRICE1 * choice_data$state_cost_862513015  + log(1/(1+exp(-ALPHA_862513015 ))) + ((1/(1+exp(-ALPHA_862513015 ))) - 1) * log(choice_data$volume_liters_862513015 + 1 )
  v865073846  <- ASC865073846  + PRICE1 * choice_data$state_cost_865073846  + log(1/(1+exp(-ALPHA_865073846 ))) + ((1/(1+exp(-ALPHA_865073846 ))) - 1) * log(choice_data$volume_liters_865073846 + 1 )
  v866702776  <- ASC866702776  + PRICE1 * choice_data$state_cost_866702776  + log(1/(1+exp(-ALPHA_866702776 ))) + ((1/(1+exp(-ALPHA_866702776 ))) - 1) * log(choice_data$volume_liters_866702776 + 1 )
  v868864757  <- ASC868864757  + PRICE1 * choice_data$state_cost_868864757  + log(1/(1+exp(-ALPHA_868864757 ))) + ((1/(1+exp(-ALPHA_868864757 ))) - 1) * log(choice_data$volume_liters_868864757 + 1 )
  v868874756  <- ASC868874756  + PRICE1 * choice_data$state_cost_868874756  + log(1/(1+exp(-ALPHA_868874756 ))) + ((1/(1+exp(-ALPHA_868874756 ))) - 1) * log(choice_data$volume_liters_868874756 + 1 )
  v868884776  <- ASC868884776  + PRICE1 * choice_data$state_cost_868884776  + log(1/(1+exp(-ALPHA_868884776 ))) + ((1/(1+exp(-ALPHA_868884776 ))) - 1) * log(choice_data$volume_liters_868884776 + 1 )
  v879373012  <- ASC879373012  + PRICE1 * choice_data$state_cost_879373012  + log(1/(1+exp(-ALPHA_879373012 ))) + ((1/(1+exp(-ALPHA_879373012 ))) - 1) * log(choice_data$volume_liters_879373012 + 1 )
  v882963898  <- ASC882963898  + PRICE1 * choice_data$state_cost_882963898  + log(1/(1+exp(-ALPHA_882963898 ))) + ((1/(1+exp(-ALPHA_882963898 ))) - 1) * log(choice_data$volume_liters_882963898 + 1 )
  v891962980  <- ASC891962980  + PRICE1 * choice_data$state_cost_891962980  + log(1/(1+exp(-ALPHA_891962980 ))) + ((1/(1+exp(-ALPHA_891962980 ))) - 1) * log(choice_data$volume_liters_891962980 + 1 )
  v891972979  <- ASC891972979  + PRICE1 * choice_data$state_cost_891972979  + log(1/(1+exp(-ALPHA_891972979 ))) + ((1/(1+exp(-ALPHA_891972979 ))) - 1) * log(choice_data$volume_liters_891972979 + 1 )
  v891982982  <- ASC891982982  + PRICE1 * choice_data$state_cost_891982982  + log(1/(1+exp(-ALPHA_891982982 ))) + ((1/(1+exp(-ALPHA_891982982 ))) - 1) * log(choice_data$volume_liters_891982982 + 1 )
  v893873009  <- ASC893873009  + PRICE1 * choice_data$state_cost_893873009  + log(1/(1+exp(-ALPHA_893873009 ))) + ((1/(1+exp(-ALPHA_893873009 ))) - 1) * log(choice_data$volume_liters_893873009 + 1 )
  v895773544  <- ASC895773544  + PRICE1 * choice_data$state_cost_895773544  + log(1/(1+exp(-ALPHA_895773544 ))) + ((1/(1+exp(-ALPHA_895773544 ))) - 1) * log(choice_data$volume_liters_895773544 + 1 )
  
  
  p <-exp(
    .01 * (log(    (1- 1/(1 +exp(-ALPHA_9999      )))/(choice_data$volume_liters_9999        + 1) *
            (1- 1/(1 +exp(-ALPHA_10550695  )))/(choice_data$volume_liters_10550695    + 1) *
            (1- 1/(1 +exp(-ALPHA_11586693  )))/(choice_data$volume_liters_11586693    + 1) *
            (1- 1/(1 +exp(-ALPHA_11773688  )))/(choice_data$volume_liters_11773688    + 1) *
            (1- 1/(1 +exp(-ALPHA_11774687  )))/(choice_data$volume_liters_11774687    + 1) *
            (1- 1/(1 +exp(-ALPHA_11776686  )))/(choice_data$volume_liters_11776686    + 1) *
            (1- 1/(1 +exp(-ALPHA_11777685  )))/(choice_data$volume_liters_11777685    + 1) *
            (1- 1/(1 +exp(-ALPHA_11786699  )))/(choice_data$volume_liters_11786699    + 1) *
            (1- 1/(1 +exp(-ALPHA_11788689  )))/(choice_data$volume_liters_11788689    + 1) *
            (1- 1/(1 +exp(-ALPHA_16518268  )))/(choice_data$volume_liters_16518268    + 1) *
            (1- 1/(1 +exp(-ALPHA_17991132  )))/(choice_data$volume_liters_17991132    + 1) *
            (1- 1/(1 +exp(-ALPHA_22788573  )))/(choice_data$volume_liters_22788573    + 1) *
            (1- 1/(1 +exp(-ALPHA_29287548  )))/(choice_data$volume_liters_29287548    + 1) *
            (1- 1/(1 +exp(-ALPHA_34006175  )))/(choice_data$volume_liters_34006175    + 1) *
            (1- 1/(1 +exp(-ALPHA_34007174  )))/(choice_data$volume_liters_34007174    + 1) *
            (1- 1/(1 +exp(-ALPHA_34008178  )))/(choice_data$volume_liters_34008178    + 1) *
            (1- 1/(1 +exp(-ALPHA_34029146  )))/(choice_data$volume_liters_34029146    + 1) *
            (1- 1/(1 +exp(-ALPHA_35317555  )))/(choice_data$volume_liters_35317555    + 1) *
            (1- 1/(1 +exp(-ALPHA_35318559  )))/(choice_data$volume_liters_35318559    + 1) *
            (1- 1/(1 +exp(-ALPHA_35416952  )))/(choice_data$volume_liters_35416952    + 1) *
            (1- 1/(1 +exp(-ALPHA_35418953  )))/(choice_data$volume_liters_35418953    + 1) *
            (1- 1/(1 +exp(-ALPHA_43026214  )))/(choice_data$volume_liters_43026214    + 1) *
            (1- 1/(1 +exp(-ALPHA_43027213  )))/(choice_data$volume_liters_43027213    + 1) *
            (1- 1/(1 +exp(-ALPHA_43028216  )))/(choice_data$volume_liters_43028216    + 1) *
            (1- 1/(1 +exp(-ALPHA_43038436  )))/(choice_data$volume_liters_43038436    + 1) *
            (1- 1/(1 +exp(-ALPHA_43126479  )))/(choice_data$volume_liters_43126479    + 1) *
            (1- 1/(1 +exp(-ALPHA_43127478  )))/(choice_data$volume_liters_43127478    + 1) *
            (1- 1/(1 +exp(-ALPHA_43128482  )))/(choice_data$volume_liters_43128482    + 1) *
            (1- 1/(1 +exp(-ALPHA_43136446  )))/(choice_data$volume_liters_43136446    + 1) *
            (1- 1/(1 +exp(-ALPHA_43137443  )))/(choice_data$volume_liters_43137443    + 1) *
            (1- 1/(1 +exp(-ALPHA_44217554  )))/(choice_data$volume_liters_44217554    + 1) *
            (1- 1/(1 +exp(-ALPHA_68036500  )))/(choice_data$volume_liters_68036500    + 1) *
            (1- 1/(1 +exp(-ALPHA_68037498  )))/(choice_data$volume_liters_68037498    + 1) *
            (1- 1/(1 +exp(-ALPHA_88283243  )))/(choice_data$volume_liters_88283243    + 1) *
            (1- 1/(1 +exp(-ALPHA_100084433 )))/(choice_data$volume_liters_100084433   + 1) *
            (1- 1/(1 +exp(-ALPHA_106271048 )))/(choice_data$volume_liters_106271048   + 1) *
            (1- 1/(1 +exp(-ALPHA_106281053 )))/(choice_data$volume_liters_106281053   + 1) *
            (1- 1/(1 +exp(-ALPHA_108071511 )))/(choice_data$volume_liters_108071511   + 1) *
            (1- 1/(1 +exp(-ALPHA_112961476 )))/(choice_data$volume_liters_112961476   + 1) *
            (1- 1/(1 +exp(-ALPHA_112971485 )))/(choice_data$volume_liters_112971485   + 1) *
            (1- 1/(1 +exp(-ALPHA_112981488 )))/(choice_data$volume_liters_112981488   + 1) *
            (1- 1/(1 +exp(-ALPHA_124071064 )))/(choice_data$volume_liters_124071064   + 1) *
            (1- 1/(1 +exp(-ALPHA_124081067 )))/(choice_data$volume_liters_124081067   + 1) *
            (1- 1/(1 +exp(-ALPHA_124781072 )))/(choice_data$volume_liters_124781072   + 1) *
            (1- 1/(1 +exp(-ALPHA_130381081 )))/(choice_data$volume_liters_130381081   + 1) *
            (1- 1/(1 +exp(-ALPHA_152485359 )))/(choice_data$volume_liters_152485359   + 1) *
            (1- 1/(1 +exp(-ALPHA_156262825 )))/(choice_data$volume_liters_156262825   + 1) *
            (1- 1/(1 +exp(-ALPHA_156272824 )))/(choice_data$volume_liters_156272824   + 1) *
            (1- 1/(1 +exp(-ALPHA_179561971 )))/(choice_data$volume_liters_179561971   + 1) *
            (1- 1/(1 +exp(-ALPHA_179581973 )))/(choice_data$volume_liters_179581973   + 1) *
            (1- 1/(1 +exp(-ALPHA_190662873 )))/(choice_data$volume_liters_190662873   + 1) *
            (1- 1/(1 +exp(-ALPHA_190672872 )))/(choice_data$volume_liters_190672872   + 1) *
            (1- 1/(1 +exp(-ALPHA_190682877 )))/(choice_data$volume_liters_190682877   + 1) *
            (1- 1/(1 +exp(-ALPHA_194763335 )))/(choice_data$volume_liters_194763335   + 1) *
            (1- 1/(1 +exp(-ALPHA_194773333 )))/(choice_data$volume_liters_194773333   + 1) *
            (1- 1/(1 +exp(-ALPHA_202483690 )))/(choice_data$volume_liters_202483690   + 1) *
            (1- 1/(1 +exp(-ALPHA_215974940 )))/(choice_data$volume_liters_215974940   + 1) *
            (1- 1/(1 +exp(-ALPHA_215984943 )))/(choice_data$volume_liters_215984943   + 1) *
            (1- 1/(1 +exp(-ALPHA_238242082 )))/(choice_data$volume_liters_238242082   + 1) *
            (1- 1/(1 +exp(-ALPHA_238262081 )))/(choice_data$volume_liters_238262081   + 1) *
            (1- 1/(1 +exp(-ALPHA_238272080 )))/(choice_data$volume_liters_238272080   + 1) *
            (1- 1/(1 +exp(-ALPHA_238282084 )))/(choice_data$volume_liters_238282084   + 1) *
            (1- 1/(1 +exp(-ALPHA_241582464 )))/(choice_data$volume_liters_241582464   + 1) *
            (1- 1/(1 +exp(-ALPHA_244573083 )))/(choice_data$volume_liters_244573083   + 1) *
            (1- 1/(1 +exp(-ALPHA_244583087 )))/(choice_data$volume_liters_244583087   + 1) *
            (1- 1/(1 +exp(-ALPHA_256064473 )))/(choice_data$volume_liters_256064473   + 1) *
            (1- 1/(1 +exp(-ALPHA_256074472 )))/(choice_data$volume_liters_256074472   + 1) *
            (1- 1/(1 +exp(-ALPHA_256084476 )))/(choice_data$volume_liters_256084476   + 1) *
            (1- 1/(1 +exp(-ALPHA_268262788 )))/(choice_data$volume_liters_268262788   + 1) *
            (1- 1/(1 +exp(-ALPHA_268272787 )))/(choice_data$volume_liters_268272787   + 1) *
            (1- 1/(1 +exp(-ALPHA_268282790 )))/(choice_data$volume_liters_268282790   + 1) *
            (1- 1/(1 +exp(-ALPHA_271024938 )))/(choice_data$volume_liters_271024938   + 1) *
            (1- 1/(1 +exp(-ALPHA_288664915 )))/(choice_data$volume_liters_288664915   + 1) *
            (1- 1/(1 +exp(-ALPHA_288674914 )))/(choice_data$volume_liters_288674914   + 1) *
            (1- 1/(1 +exp(-ALPHA_316583822 )))/(choice_data$volume_liters_316583822   + 1) *
            (1- 1/(1 +exp(-ALPHA_322364483 )))/(choice_data$volume_liters_322364483   + 1) *
            (1- 1/(1 +exp(-ALPHA_322384487 )))/(choice_data$volume_liters_322384487   + 1) *
            (1- 1/(1 +exp(-ALPHA_343682147 )))/(choice_data$volume_liters_343682147   + 1) *
            (1- 1/(1 +exp(-ALPHA_344222417 )))/(choice_data$volume_liters_344222417   + 1) *
            (1- 1/(1 +exp(-ALPHA_344332416 )))/(choice_data$volume_liters_344332416   + 1) *
            (1- 1/(1 +exp(-ALPHA_344573094 )))/(choice_data$volume_liters_344573094   + 1) *
            (1- 1/(1 +exp(-ALPHA_345784107 )))/(choice_data$volume_liters_345784107   + 1) *
            (1- 1/(1 +exp(-ALPHA_347474844 )))/(choice_data$volume_liters_347474844   + 1) *
            (1- 1/(1 +exp(-ALPHA_348204895 )))/(choice_data$volume_liters_348204895   + 1) *
            (1- 1/(1 +exp(-ALPHA_348214898 )))/(choice_data$volume_liters_348214898   + 1) *
            (1- 1/(1 +exp(-ALPHA_359142077 )))/(choice_data$volume_liters_359142077   + 1) *
            (1- 1/(1 +exp(-ALPHA_359162076 )))/(choice_data$volume_liters_359162076   + 1) *
            (1- 1/(1 +exp(-ALPHA_359172075 )))/(choice_data$volume_liters_359172075   + 1) *
            (1- 1/(1 +exp(-ALPHA_359182079 )))/(choice_data$volume_liters_359182079   + 1) *
            (1- 1/(1 +exp(-ALPHA_359262073 )))/(choice_data$volume_liters_359262073   + 1) *
            (1- 1/(1 +exp(-ALPHA_359482096 )))/(choice_data$volume_liters_359482096   + 1) *
            (1- 1/(1 +exp(-ALPHA_363042475 )))/(choice_data$volume_liters_363042475   + 1) *
            (1- 1/(1 +exp(-ALPHA_363052474 )))/(choice_data$volume_liters_363052474   + 1) *
            (1- 1/(1 +exp(-ALPHA_363072473 )))/(choice_data$volume_liters_363072473   + 1) *
            (1- 1/(1 +exp(-ALPHA_363082477 )))/(choice_data$volume_liters_363082477   + 1) *
            (1- 1/(1 +exp(-ALPHA_368863465 )))/(choice_data$volume_liters_368863465   + 1) *
            (1- 1/(1 +exp(-ALPHA_369033466 )))/(choice_data$volume_liters_369033466   + 1) *
            (1- 1/(1 +exp(-ALPHA_369043469 )))/(choice_data$volume_liters_369043469   + 1) *
            (1- 1/(1 +exp(-ALPHA_369073471 )))/(choice_data$volume_liters_369073471   + 1) *
            (1- 1/(1 +exp(-ALPHA_369083470 )))/(choice_data$volume_liters_369083470   + 1) *
            (1- 1/(1 +exp(-ALPHA_369693633 )))/(choice_data$volume_liters_369693633   + 1) *
            (1- 1/(1 +exp(-ALPHA_369713637 )))/(choice_data$volume_liters_369713637   + 1) *
            (1- 1/(1 +exp(-ALPHA_369763644 )))/(choice_data$volume_liters_369763644   + 1) *
            (1- 1/(1 +exp(-ALPHA_369783647 )))/(choice_data$volume_liters_369783647   + 1) *
            (1- 1/(1 +exp(-ALPHA_373365237 )))/(choice_data$volume_liters_373365237   + 1) *
            (1- 1/(1 +exp(-ALPHA_373385241 )))/(choice_data$volume_liters_373385241   + 1) *
            (1- 1/(1 +exp(-ALPHA_373464016 )))/(choice_data$volume_liters_373464016   + 1) *
            (1- 1/(1 +exp(-ALPHA_373474014 )))/(choice_data$volume_liters_373474014   + 1) *
            (1- 1/(1 +exp(-ALPHA_373484017 )))/(choice_data$volume_liters_373484017   + 1) *
            (1- 1/(1 +exp(-ALPHA_374174133 )))/(choice_data$volume_liters_374174133   + 1) *
            (1- 1/(1 +exp(-ALPHA_374184137 )))/(choice_data$volume_liters_374184137   + 1) *
            (1- 1/(1 +exp(-ALPHA_379384566 )))/(choice_data$volume_liters_379384566   + 1) *
            (1- 1/(1 +exp(-ALPHA_379944707 )))/(choice_data$volume_liters_379944707   + 1) *
            (1- 1/(1 +exp(-ALPHA_379964706 )))/(choice_data$volume_liters_379964706   + 1) *
            (1- 1/(1 +exp(-ALPHA_379974705 )))/(choice_data$volume_liters_379974705   + 1) *
            (1- 1/(1 +exp(-ALPHA_379984709 )))/(choice_data$volume_liters_379984709   + 1) *
            (1- 1/(1 +exp(-ALPHA_380064712 )))/(choice_data$volume_liters_380064712   + 1) *
            (1- 1/(1 +exp(-ALPHA_380084711 )))/(choice_data$volume_liters_380084711   + 1) *
            (1- 1/(1 +exp(-ALPHA_380884122 )))/(choice_data$volume_liters_380884122   + 1) *
            (1- 1/(1 +exp(-ALPHA_381765067 )))/(choice_data$volume_liters_381765067   + 1) *
            (1- 1/(1 +exp(-ALPHA_381775064 )))/(choice_data$volume_liters_381775064   + 1) *
            (1- 1/(1 +exp(-ALPHA_381785066 )))/(choice_data$volume_liters_381785066   + 1) *
            (1- 1/(1 +exp(-ALPHA_410762864 )))/(choice_data$volume_liters_410762864   + 1) *
            (1- 1/(1 +exp(-ALPHA_416925185 )))/(choice_data$volume_liters_416925185   + 1) *
            (1- 1/(1 +exp(-ALPHA_416935184 )))/(choice_data$volume_liters_416935184   + 1) *
            (1- 1/(1 +exp(-ALPHA_416945182 )))/(choice_data$volume_liters_416945182   + 1) *
            (1- 1/(1 +exp(-ALPHA_417045220 )))/(choice_data$volume_liters_417045220   + 1) *
            (1- 1/(1 +exp(-ALPHA_418461979 )))/(choice_data$volume_liters_418461979   + 1) *
            (1- 1/(1 +exp(-ALPHA_419895193 )))/(choice_data$volume_liters_419895193   + 1) *
            (1- 1/(1 +exp(-ALPHA_427163350 )))/(choice_data$volume_liters_427163350   + 1) *
            (1- 1/(1 +exp(-ALPHA_427173349 )))/(choice_data$volume_liters_427173349   + 1) *
            (1- 1/(1 +exp(-ALPHA_427183352 )))/(choice_data$volume_liters_427183352   + 1) *
            (1- 1/(1 +exp(-ALPHA_432851114 )))/(choice_data$volume_liters_432851114   + 1) *
            (1- 1/(1 +exp(-ALPHA_433341134 )))/(choice_data$volume_liters_433341134   + 1) *
            (1- 1/(1 +exp(-ALPHA_433351138 )))/(choice_data$volume_liters_433351138   + 1) *
            (1- 1/(1 +exp(-ALPHA_433361110 )))/(choice_data$volume_liters_433361110   + 1) *
            (1- 1/(1 +exp(-ALPHA_433371133 )))/(choice_data$volume_liters_433371133   + 1) *
            (1- 1/(1 +exp(-ALPHA_433381135 )))/(choice_data$volume_liters_433381135   + 1) *
            (1- 1/(1 +exp(-ALPHA_433871168 )))/(choice_data$volume_liters_433871168   + 1) *
            (1- 1/(1 +exp(-ALPHA_446583206 )))/(choice_data$volume_liters_446583206   + 1) *
            (1- 1/(1 +exp(-ALPHA_452483827 )))/(choice_data$volume_liters_452483827   + 1) *
            (1- 1/(1 +exp(-ALPHA_452763855 )))/(choice_data$volume_liters_452763855   + 1) *
            (1- 1/(1 +exp(-ALPHA_452773854 )))/(choice_data$volume_liters_452773854   + 1) *
            (1- 1/(1 +exp(-ALPHA_452783857 )))/(choice_data$volume_liters_452783857   + 1) *
            (1- 1/(1 +exp(-ALPHA_458864376 )))/(choice_data$volume_liters_458864376   + 1) *
            (1- 1/(1 +exp(-ALPHA_463502472 )))/(choice_data$volume_liters_463502472   + 1) *
            (1- 1/(1 +exp(-ALPHA_481052499 )))/(choice_data$volume_liters_481052499   + 1) *
            (1- 1/(1 +exp(-ALPHA_481062503 )))/(choice_data$volume_liters_481062503   + 1) *
            (1- 1/(1 +exp(-ALPHA_523161282 )))/(choice_data$volume_liters_523161282   + 1) *
            (1- 1/(1 +exp(-ALPHA_525961862 )))/(choice_data$volume_liters_525961862   + 1) *
            (1- 1/(1 +exp(-ALPHA_525981863 )))/(choice_data$volume_liters_525981863   + 1) *
            (1- 1/(1 +exp(-ALPHA_532143913 )))/(choice_data$volume_liters_532143913   + 1) *
            (1- 1/(1 +exp(-ALPHA_550863803 )))/(choice_data$volume_liters_550863803   + 1) *
            (1- 1/(1 +exp(-ALPHA_571251241 )))/(choice_data$volume_liters_571251241   + 1) *
            (1- 1/(1 +exp(-ALPHA_571481243 )))/(choice_data$volume_liters_571481243   + 1) *
            (1- 1/(1 +exp(-ALPHA_588382962 )))/(choice_data$volume_liters_588382962   + 1) *
            (1- 1/(1 +exp(-ALPHA_588682969 )))/(choice_data$volume_liters_588682969   + 1) *
            (1- 1/(1 +exp(-ALPHA_588722960 )))/(choice_data$volume_liters_588722960   + 1) *
            (1- 1/(1 +exp(-ALPHA_588752994 )))/(choice_data$volume_liters_588752994   + 1) *
            (1- 1/(1 +exp(-ALPHA_590371737 )))/(choice_data$volume_liters_590371737   + 1) *
            (1- 1/(1 +exp(-ALPHA_648582044 )))/(choice_data$volume_liters_648582044   + 1) *
            (1- 1/(1 +exp(-ALPHA_648652045 )))/(choice_data$volume_liters_648652045   + 1) *
            (1- 1/(1 +exp(-ALPHA_648662041 )))/(choice_data$volume_liters_648662041   + 1) *
            (1- 1/(1 +exp(-ALPHA_648672039 )))/(choice_data$volume_liters_648672039   + 1) *
            (1- 1/(1 +exp(-ALPHA_648682043 )))/(choice_data$volume_liters_648682043   + 1) *
            (1- 1/(1 +exp(-ALPHA_652562809 )))/(choice_data$volume_liters_652562809   + 1) *
            (1- 1/(1 +exp(-ALPHA_652572808 )))/(choice_data$volume_liters_652572808   + 1) *
            (1- 1/(1 +exp(-ALPHA_675263042 )))/(choice_data$volume_liters_675263042   + 1) *
            (1- 1/(1 +exp(-ALPHA_675273041 )))/(choice_data$volume_liters_675273041   + 1) *
            (1- 1/(1 +exp(-ALPHA_696361815 )))/(choice_data$volume_liters_696361815   + 1) *
            (1- 1/(1 +exp(-ALPHA_696371814 )))/(choice_data$volume_liters_696371814   + 1) *
            (1- 1/(1 +exp(-ALPHA_696381817 )))/(choice_data$volume_liters_696381817   + 1) *
            (1- 1/(1 +exp(-ALPHA_699474354 )))/(choice_data$volume_liters_699474354   + 1) *
            (1- 1/(1 +exp(-ALPHA_730504347 )))/(choice_data$volume_liters_730504347   + 1) *
            (1- 1/(1 +exp(-ALPHA_730534345 )))/(choice_data$volume_liters_730534345   + 1) *
            (1- 1/(1 +exp(-ALPHA_730554344 )))/(choice_data$volume_liters_730554344   + 1) *
            (1- 1/(1 +exp(-ALPHA_750873008 )))/(choice_data$volume_liters_750873008   + 1) *
            (1- 1/(1 +exp(-ALPHA_752103114 )))/(choice_data$volume_liters_752103114   + 1) *
            (1- 1/(1 +exp(-ALPHA_752143108 )))/(choice_data$volume_liters_752143108   + 1) *
            (1- 1/(1 +exp(-ALPHA_764863795 )))/(choice_data$volume_liters_764863795   + 1) *
            (1- 1/(1 +exp(-ALPHA_764873794 )))/(choice_data$volume_liters_764873794   + 1) *
            (1- 1/(1 +exp(-ALPHA_774875094 )))/(choice_data$volume_liters_774875094   + 1) *
            (1- 1/(1 +exp(-ALPHA_812063836 )))/(choice_data$volume_liters_812063836   + 1) *
            (1- 1/(1 +exp(-ALPHA_812073835 )))/(choice_data$volume_liters_812073835   + 1) *
            (1- 1/(1 +exp(-ALPHA_812083838 )))/(choice_data$volume_liters_812083838   + 1) *
            (1- 1/(1 +exp(-ALPHA_826071715 )))/(choice_data$volume_liters_826071715   + 1) *
            (1- 1/(1 +exp(-ALPHA_826371666 )))/(choice_data$volume_liters_826371666   + 1) *
            (1- 1/(1 +exp(-ALPHA_827871639 )))/(choice_data$volume_liters_827871639   + 1) *
            (1- 1/(1 +exp(-ALPHA_828461681 )))/(choice_data$volume_liters_828461681   + 1) *
            (1- 1/(1 +exp(-ALPHA_828471697 )))/(choice_data$volume_liters_828471697   + 1) *
            (1- 1/(1 +exp(-ALPHA_828671728 )))/(choice_data$volume_liters_828671728   + 1) *
            (1- 1/(1 +exp(-ALPHA_861121724 )))/(choice_data$volume_liters_861121724   + 1) *
            (1- 1/(1 +exp(-ALPHA_862513015 )))/(choice_data$volume_liters_862513015   + 1) *
            (1- 1/(1 +exp(-ALPHA_865073846 )))/(choice_data$volume_liters_865073846   + 1) *
            (1- 1/(1 +exp(-ALPHA_866702776 )))/(choice_data$volume_liters_866702776   + 1) *
            (1- 1/(1 +exp(-ALPHA_868864757 )))/(choice_data$volume_liters_868864757   + 1) *
            (1- 1/(1 +exp(-ALPHA_868874756 )))/(choice_data$volume_liters_868874756   + 1) *
            (1- 1/(1 +exp(-ALPHA_868884776 )))/(choice_data$volume_liters_868884776   + 1) *
            (1- 1/(1 +exp(-ALPHA_879373012 )))/(choice_data$volume_liters_879373012   + 1) *
            (1- 1/(1 +exp(-ALPHA_882963898 )))/(choice_data$volume_liters_882963898   + 1) *
            (1- 1/(1 +exp(-ALPHA_891962980 )))/(choice_data$volume_liters_891962980   + 1) *
            (1- 1/(1 +exp(-ALPHA_891972979 )))/(choice_data$volume_liters_891972979   + 1) *
            (1- 1/(1 +exp(-ALPHA_891982982 )))/(choice_data$volume_liters_891982982   + 1) *
            (1- 1/(1 +exp(-ALPHA_893873009 )))/(choice_data$volume_liters_893873009   + 1) *
            (1- 1/(1 +exp(-ALPHA_895773544 )))/(choice_data$volume_liters_895773544   + 1)) +
      log(    (choice_data$volume_liters_9999      + 1 )/(1-1/(1+exp(-ALPHA_9999  ))) +
              (choice_data$volume_liters_10550695  + 1 )/(1-1/(1+exp(-ALPHA_10550695  ))) +
              (choice_data$volume_liters_11586693  + 1 )/(1-1/(1+exp(-ALPHA_11586693  ))) +
              (choice_data$volume_liters_11773688  + 1 )/(1-1/(1+exp(-ALPHA_11773688  ))) +
              (choice_data$volume_liters_11774687  + 1 )/(1-1/(1+exp(-ALPHA_11774687  ))) +
              (choice_data$volume_liters_11776686  + 1 )/(1-1/(1+exp(-ALPHA_11776686  ))) +
              (choice_data$volume_liters_11777685  + 1 )/(1-1/(1+exp(-ALPHA_11777685  ))) +
              (choice_data$volume_liters_11786699  + 1 )/(1-1/(1+exp(-ALPHA_11786699  ))) +
              (choice_data$volume_liters_11788689  + 1 )/(1-1/(1+exp(-ALPHA_11788689  ))) +
              (choice_data$volume_liters_16518268  + 1 )/(1-1/(1+exp(-ALPHA_16518268  ))) +
              (choice_data$volume_liters_17991132  + 1 )/(1-1/(1+exp(-ALPHA_17991132  ))) +
              (choice_data$volume_liters_22788573  + 1 )/(1-1/(1+exp(-ALPHA_22788573  ))) +
              (choice_data$volume_liters_29287548  + 1 )/(1-1/(1+exp(-ALPHA_29287548  ))) +
              (choice_data$volume_liters_34006175  + 1 )/(1-1/(1+exp(-ALPHA_34006175  ))) +
              (choice_data$volume_liters_34007174  + 1 )/(1-1/(1+exp(-ALPHA_34007174  ))) +
              (choice_data$volume_liters_34008178  + 1 )/(1-1/(1+exp(-ALPHA_34008178  ))) +
              (choice_data$volume_liters_34029146  + 1 )/(1-1/(1+exp(-ALPHA_34029146  ))) +
              (choice_data$volume_liters_35317555  + 1 )/(1-1/(1+exp(-ALPHA_35317555  ))) +
              (choice_data$volume_liters_35318559  + 1 )/(1-1/(1+exp(-ALPHA_35318559  ))) +
              (choice_data$volume_liters_35416952  + 1 )/(1-1/(1+exp(-ALPHA_35416952  ))) +
              (choice_data$volume_liters_35418953  + 1 )/(1-1/(1+exp(-ALPHA_35418953  ))) +
              (choice_data$volume_liters_43026214  + 1 )/(1-1/(1+exp(-ALPHA_43026214  ))) +
              (choice_data$volume_liters_43027213  + 1 )/(1-1/(1+exp(-ALPHA_43027213  ))) +
              (choice_data$volume_liters_43028216  + 1 )/(1-1/(1+exp(-ALPHA_43028216  ))) +
              (choice_data$volume_liters_43038436  + 1 )/(1-1/(1+exp(-ALPHA_43038436  ))) +
              (choice_data$volume_liters_43126479  + 1 )/(1-1/(1+exp(-ALPHA_43126479  ))) +
              (choice_data$volume_liters_43127478  + 1 )/(1-1/(1+exp(-ALPHA_43127478  ))) +
              (choice_data$volume_liters_43128482  + 1 )/(1-1/(1+exp(-ALPHA_43128482  ))) +
              (choice_data$volume_liters_43136446  + 1 )/(1-1/(1+exp(-ALPHA_43136446  ))) +
              (choice_data$volume_liters_43137443  + 1 )/(1-1/(1+exp(-ALPHA_43137443  ))) +
              (choice_data$volume_liters_44217554  + 1 )/(1-1/(1+exp(-ALPHA_44217554  ))) +
              (choice_data$volume_liters_68036500  + 1 )/(1-1/(1+exp(-ALPHA_68036500  ))) +
              (choice_data$volume_liters_68037498  + 1 )/(1-1/(1+exp(-ALPHA_68037498  ))) +
              (choice_data$volume_liters_88283243  + 1 )/(1-1/(1+exp(-ALPHA_88283243  ))) +
              (choice_data$volume_liters_100084433 + 1 )/(1-1/(1+exp(-ALPHA_100084433 ))) +
              (choice_data$volume_liters_106271048 + 1 )/(1-1/(1+exp(-ALPHA_106271048 ))) +
              (choice_data$volume_liters_106281053 + 1 )/(1-1/(1+exp(-ALPHA_106281053 ))) +
              (choice_data$volume_liters_108071511 + 1 )/(1-1/(1+exp(-ALPHA_108071511 ))) +
              (choice_data$volume_liters_112961476 + 1 )/(1-1/(1+exp(-ALPHA_112961476 ))) +
              (choice_data$volume_liters_112971485 + 1 )/(1-1/(1+exp(-ALPHA_112971485 ))) +
              (choice_data$volume_liters_112981488 + 1 )/(1-1/(1+exp(-ALPHA_112981488 ))) +
              (choice_data$volume_liters_124071064 + 1 )/(1-1/(1+exp(-ALPHA_124071064 ))) +
              (choice_data$volume_liters_124081067 + 1 )/(1-1/(1+exp(-ALPHA_124081067 ))) +
              (choice_data$volume_liters_124781072 + 1 )/(1-1/(1+exp(-ALPHA_124781072 ))) +
              (choice_data$volume_liters_130381081 + 1 )/(1-1/(1+exp(-ALPHA_130381081 ))) +
              (choice_data$volume_liters_152485359 + 1 )/(1-1/(1+exp(-ALPHA_152485359 ))) +
              (choice_data$volume_liters_156262825 + 1 )/(1-1/(1+exp(-ALPHA_156262825 ))) +
              (choice_data$volume_liters_156272824 + 1 )/(1-1/(1+exp(-ALPHA_156272824 ))) +
              (choice_data$volume_liters_179561971 + 1 )/(1-1/(1+exp(-ALPHA_179561971 ))) +
              (choice_data$volume_liters_179581973 + 1 )/(1-1/(1+exp(-ALPHA_179581973 ))) +
              (choice_data$volume_liters_190662873 + 1 )/(1-1/(1+exp(-ALPHA_190662873 ))) +
              (choice_data$volume_liters_190672872 + 1 )/(1-1/(1+exp(-ALPHA_190672872 ))) +
              (choice_data$volume_liters_190682877 + 1 )/(1-1/(1+exp(-ALPHA_190682877 ))) +
              (choice_data$volume_liters_194763335 + 1 )/(1-1/(1+exp(-ALPHA_194763335 ))) +
              (choice_data$volume_liters_194773333 + 1 )/(1-1/(1+exp(-ALPHA_194773333 ))) +
              (choice_data$volume_liters_202483690 + 1 )/(1-1/(1+exp(-ALPHA_202483690 ))) +
              (choice_data$volume_liters_215974940 + 1 )/(1-1/(1+exp(-ALPHA_215974940 ))) +
              (choice_data$volume_liters_215984943 + 1 )/(1-1/(1+exp(-ALPHA_215984943 ))) +
              (choice_data$volume_liters_238242082 + 1 )/(1-1/(1+exp(-ALPHA_238242082 ))) +
              (choice_data$volume_liters_238262081 + 1 )/(1-1/(1+exp(-ALPHA_238262081 ))) +
              (choice_data$volume_liters_238272080 + 1 )/(1-1/(1+exp(-ALPHA_238272080 ))) +
              (choice_data$volume_liters_238282084 + 1 )/(1-1/(1+exp(-ALPHA_238282084 ))) +
              (choice_data$volume_liters_241582464 + 1 )/(1-1/(1+exp(-ALPHA_241582464 ))) +
              (choice_data$volume_liters_244573083 + 1 )/(1-1/(1+exp(-ALPHA_244573083 ))) +
              (choice_data$volume_liters_244583087 + 1 )/(1-1/(1+exp(-ALPHA_244583087 ))) +
              (choice_data$volume_liters_256064473 + 1 )/(1-1/(1+exp(-ALPHA_256064473 ))) +
              (choice_data$volume_liters_256074472 + 1 )/(1-1/(1+exp(-ALPHA_256074472 ))) +
              (choice_data$volume_liters_256084476 + 1 )/(1-1/(1+exp(-ALPHA_256084476 ))) +
              (choice_data$volume_liters_268262788 + 1 )/(1-1/(1+exp(-ALPHA_268262788 ))) +
              (choice_data$volume_liters_268272787 + 1 )/(1-1/(1+exp(-ALPHA_268272787 ))) +
              (choice_data$volume_liters_268282790 + 1 )/(1-1/(1+exp(-ALPHA_268282790 ))) +
              (choice_data$volume_liters_271024938 + 1 )/(1-1/(1+exp(-ALPHA_271024938 ))) +
              (choice_data$volume_liters_288664915 + 1 )/(1-1/(1+exp(-ALPHA_288664915 ))) +
              (choice_data$volume_liters_288674914 + 1 )/(1-1/(1+exp(-ALPHA_288674914 ))) +
              (choice_data$volume_liters_316583822 + 1 )/(1-1/(1+exp(-ALPHA_316583822 ))) +
              (choice_data$volume_liters_322364483 + 1 )/(1-1/(1+exp(-ALPHA_322364483 ))) +
              (choice_data$volume_liters_322384487 + 1 )/(1-1/(1+exp(-ALPHA_322384487 ))) +
              (choice_data$volume_liters_343682147 + 1 )/(1-1/(1+exp(-ALPHA_343682147 ))) +
              (choice_data$volume_liters_344222417 + 1 )/(1-1/(1+exp(-ALPHA_344222417 ))) +
              (choice_data$volume_liters_344332416 + 1 )/(1-1/(1+exp(-ALPHA_344332416 ))) +
              (choice_data$volume_liters_344573094 + 1 )/(1-1/(1+exp(-ALPHA_344573094 ))) +
              (choice_data$volume_liters_345784107 + 1 )/(1-1/(1+exp(-ALPHA_345784107 ))) +
              (choice_data$volume_liters_347474844 + 1 )/(1-1/(1+exp(-ALPHA_347474844 ))) +
              (choice_data$volume_liters_348204895 + 1 )/(1-1/(1+exp(-ALPHA_348204895 ))) +
              (choice_data$volume_liters_348214898 + 1 )/(1-1/(1+exp(-ALPHA_348214898 ))) +
              (choice_data$volume_liters_359142077 + 1 )/(1-1/(1+exp(-ALPHA_359142077 ))) +
              (choice_data$volume_liters_359162076 + 1 )/(1-1/(1+exp(-ALPHA_359162076 ))) +
              (choice_data$volume_liters_359172075 + 1 )/(1-1/(1+exp(-ALPHA_359172075 ))) +
              (choice_data$volume_liters_359182079 + 1 )/(1-1/(1+exp(-ALPHA_359182079 ))) +
              (choice_data$volume_liters_359262073 + 1 )/(1-1/(1+exp(-ALPHA_359262073 ))) +
              (choice_data$volume_liters_359482096 + 1 )/(1-1/(1+exp(-ALPHA_359482096 ))) +
              (choice_data$volume_liters_363042475 + 1 )/(1-1/(1+exp(-ALPHA_363042475 ))) +
              (choice_data$volume_liters_363052474 + 1 )/(1-1/(1+exp(-ALPHA_363052474 ))) +
              (choice_data$volume_liters_363072473 + 1 )/(1-1/(1+exp(-ALPHA_363072473 ))) +
              (choice_data$volume_liters_363082477 + 1 )/(1-1/(1+exp(-ALPHA_363082477 ))) +
              (choice_data$volume_liters_368863465 + 1 )/(1-1/(1+exp(-ALPHA_368863465 ))) +
              (choice_data$volume_liters_369033466 + 1 )/(1-1/(1+exp(-ALPHA_369033466 ))) +
              (choice_data$volume_liters_369043469 + 1 )/(1-1/(1+exp(-ALPHA_369043469 ))) +
              (choice_data$volume_liters_369073471 + 1 )/(1-1/(1+exp(-ALPHA_369073471 ))) +
              (choice_data$volume_liters_369083470 + 1 )/(1-1/(1+exp(-ALPHA_369083470 ))) +
              (choice_data$volume_liters_369693633 + 1 )/(1-1/(1+exp(-ALPHA_369693633 ))) +
              (choice_data$volume_liters_369713637 + 1 )/(1-1/(1+exp(-ALPHA_369713637 ))) +
              (choice_data$volume_liters_369763644 + 1 )/(1-1/(1+exp(-ALPHA_369763644 ))) +
              (choice_data$volume_liters_369783647 + 1 )/(1-1/(1+exp(-ALPHA_369783647 ))) +
              (choice_data$volume_liters_373365237 + 1 )/(1-1/(1+exp(-ALPHA_373365237 ))) +
              (choice_data$volume_liters_373385241 + 1 )/(1-1/(1+exp(-ALPHA_373385241 ))) +
              (choice_data$volume_liters_373464016 + 1 )/(1-1/(1+exp(-ALPHA_373464016 ))) +
              (choice_data$volume_liters_373474014 + 1 )/(1-1/(1+exp(-ALPHA_373474014 ))) +
              (choice_data$volume_liters_373484017 + 1 )/(1-1/(1+exp(-ALPHA_373484017 ))) +
              (choice_data$volume_liters_374174133 + 1 )/(1-1/(1+exp(-ALPHA_374174133 ))) +
              (choice_data$volume_liters_374184137 + 1 )/(1-1/(1+exp(-ALPHA_374184137 ))) +
              (choice_data$volume_liters_379384566 + 1 )/(1-1/(1+exp(-ALPHA_379384566 ))) +
              (choice_data$volume_liters_379944707 + 1 )/(1-1/(1+exp(-ALPHA_379944707 ))) +
              (choice_data$volume_liters_379964706 + 1 )/(1-1/(1+exp(-ALPHA_379964706 ))) +
              (choice_data$volume_liters_379974705 + 1 )/(1-1/(1+exp(-ALPHA_379974705 ))) +
              (choice_data$volume_liters_379984709 + 1 )/(1-1/(1+exp(-ALPHA_379984709 ))) +
              (choice_data$volume_liters_380064712 + 1 )/(1-1/(1+exp(-ALPHA_380064712 ))) +
              (choice_data$volume_liters_380084711 + 1 )/(1-1/(1+exp(-ALPHA_380084711 ))) +
              (choice_data$volume_liters_380884122 + 1 )/(1-1/(1+exp(-ALPHA_380884122 ))) +
              (choice_data$volume_liters_381765067 + 1 )/(1-1/(1+exp(-ALPHA_381765067 ))) +
              (choice_data$volume_liters_381775064 + 1 )/(1-1/(1+exp(-ALPHA_381775064 ))) +
              (choice_data$volume_liters_381785066 + 1 )/(1-1/(1+exp(-ALPHA_381785066 ))) +
              (choice_data$volume_liters_410762864 + 1 )/(1-1/(1+exp(-ALPHA_410762864 ))) +
              (choice_data$volume_liters_416925185 + 1 )/(1-1/(1+exp(-ALPHA_416925185 ))) +
              (choice_data$volume_liters_416935184 + 1 )/(1-1/(1+exp(-ALPHA_416935184 ))) +
              (choice_data$volume_liters_416945182 + 1 )/(1-1/(1+exp(-ALPHA_416945182 ))) +
              (choice_data$volume_liters_417045220 + 1 )/(1-1/(1+exp(-ALPHA_417045220 ))) +
              (choice_data$volume_liters_418461979 + 1 )/(1-1/(1+exp(-ALPHA_418461979 ))) +
              (choice_data$volume_liters_419895193 + 1 )/(1-1/(1+exp(-ALPHA_419895193 ))) +
              (choice_data$volume_liters_427163350 + 1 )/(1-1/(1+exp(-ALPHA_427163350 ))) +
              (choice_data$volume_liters_427173349 + 1 )/(1-1/(1+exp(-ALPHA_427173349 ))) +
              (choice_data$volume_liters_427183352 + 1 )/(1-1/(1+exp(-ALPHA_427183352 ))) +
              (choice_data$volume_liters_432851114 + 1 )/(1-1/(1+exp(-ALPHA_432851114 ))) +
              (choice_data$volume_liters_433341134 + 1 )/(1-1/(1+exp(-ALPHA_433341134 ))) +
              (choice_data$volume_liters_433351138 + 1 )/(1-1/(1+exp(-ALPHA_433351138 ))) +
              (choice_data$volume_liters_433361110 + 1 )/(1-1/(1+exp(-ALPHA_433361110 ))) +
              (choice_data$volume_liters_433371133 + 1 )/(1-1/(1+exp(-ALPHA_433371133 ))) +
              (choice_data$volume_liters_433381135 + 1 )/(1-1/(1+exp(-ALPHA_433381135 ))) +
              (choice_data$volume_liters_433871168 + 1 )/(1-1/(1+exp(-ALPHA_433871168 ))) +
              (choice_data$volume_liters_446583206 + 1 )/(1-1/(1+exp(-ALPHA_446583206 ))) +
              (choice_data$volume_liters_452483827 + 1 )/(1-1/(1+exp(-ALPHA_452483827 ))) +
              (choice_data$volume_liters_452763855 + 1 )/(1-1/(1+exp(-ALPHA_452763855 ))) +
              (choice_data$volume_liters_452773854 + 1 )/(1-1/(1+exp(-ALPHA_452773854 ))) +
              (choice_data$volume_liters_452783857 + 1 )/(1-1/(1+exp(-ALPHA_452783857 ))) +
              (choice_data$volume_liters_458864376 + 1 )/(1-1/(1+exp(-ALPHA_458864376 ))) +
              (choice_data$volume_liters_463502472 + 1 )/(1-1/(1+exp(-ALPHA_463502472 ))) +
              (choice_data$volume_liters_481052499 + 1 )/(1-1/(1+exp(-ALPHA_481052499 ))) +
              (choice_data$volume_liters_481062503 + 1 )/(1-1/(1+exp(-ALPHA_481062503 ))) +
              (choice_data$volume_liters_523161282 + 1 )/(1-1/(1+exp(-ALPHA_523161282 ))) +
              (choice_data$volume_liters_525961862 + 1 )/(1-1/(1+exp(-ALPHA_525961862 ))) +
              (choice_data$volume_liters_525981863 + 1 )/(1-1/(1+exp(-ALPHA_525981863 ))) +
              (choice_data$volume_liters_532143913 + 1 )/(1-1/(1+exp(-ALPHA_532143913 ))) +
              (choice_data$volume_liters_550863803 + 1 )/(1-1/(1+exp(-ALPHA_550863803 ))) +
              (choice_data$volume_liters_571251241 + 1 )/(1-1/(1+exp(-ALPHA_571251241 ))) +
              (choice_data$volume_liters_571481243 + 1 )/(1-1/(1+exp(-ALPHA_571481243 ))) +
              (choice_data$volume_liters_588382962 + 1 )/(1-1/(1+exp(-ALPHA_588382962 ))) +
              (choice_data$volume_liters_588682969 + 1 )/(1-1/(1+exp(-ALPHA_588682969 ))) +
              (choice_data$volume_liters_588722960 + 1 )/(1-1/(1+exp(-ALPHA_588722960 ))) +
              (choice_data$volume_liters_588752994 + 1 )/(1-1/(1+exp(-ALPHA_588752994 ))) +
              (choice_data$volume_liters_590371737 + 1 )/(1-1/(1+exp(-ALPHA_590371737 ))) +
              (choice_data$volume_liters_648582044 + 1 )/(1-1/(1+exp(-ALPHA_648582044 ))) +
              (choice_data$volume_liters_648652045 + 1 )/(1-1/(1+exp(-ALPHA_648652045 ))) +
              (choice_data$volume_liters_648662041 + 1 )/(1-1/(1+exp(-ALPHA_648662041 ))) +
              (choice_data$volume_liters_648672039 + 1 )/(1-1/(1+exp(-ALPHA_648672039 ))) +
              (choice_data$volume_liters_648682043 + 1 )/(1-1/(1+exp(-ALPHA_648682043 ))) +
              (choice_data$volume_liters_652562809 + 1 )/(1-1/(1+exp(-ALPHA_652562809 ))) +
              (choice_data$volume_liters_652572808 + 1 )/(1-1/(1+exp(-ALPHA_652572808 ))) +
              (choice_data$volume_liters_675263042 + 1 )/(1-1/(1+exp(-ALPHA_675263042 ))) +
              (choice_data$volume_liters_675273041 + 1 )/(1-1/(1+exp(-ALPHA_675273041 ))) +
              (choice_data$volume_liters_696361815 + 1 )/(1-1/(1+exp(-ALPHA_696361815 ))) +
              (choice_data$volume_liters_696371814 + 1 )/(1-1/(1+exp(-ALPHA_696371814 ))) +
              (choice_data$volume_liters_696381817 + 1 )/(1-1/(1+exp(-ALPHA_696381817 ))) +
              (choice_data$volume_liters_699474354 + 1 )/(1-1/(1+exp(-ALPHA_699474354 ))) +
              (choice_data$volume_liters_730504347 + 1 )/(1-1/(1+exp(-ALPHA_730504347 ))) +
              (choice_data$volume_liters_730534345 + 1 )/(1-1/(1+exp(-ALPHA_730534345 ))) +
              (choice_data$volume_liters_730554344 + 1 )/(1-1/(1+exp(-ALPHA_730554344 ))) +
              (choice_data$volume_liters_750873008 + 1 )/(1-1/(1+exp(-ALPHA_750873008 ))) +
              (choice_data$volume_liters_752103114 + 1 )/(1-1/(1+exp(-ALPHA_752103114 ))) +
              (choice_data$volume_liters_752143108 + 1 )/(1-1/(1+exp(-ALPHA_752143108 ))) +
              (choice_data$volume_liters_764863795 + 1 )/(1-1/(1+exp(-ALPHA_764863795 ))) +
              (choice_data$volume_liters_764873794 + 1 )/(1-1/(1+exp(-ALPHA_764873794 ))) +
              (choice_data$volume_liters_774875094 + 1 )/(1-1/(1+exp(-ALPHA_774875094 ))) +
              (choice_data$volume_liters_812063836 + 1 )/(1-1/(1+exp(-ALPHA_812063836 ))) +
              (choice_data$volume_liters_812073835 + 1 )/(1-1/(1+exp(-ALPHA_812073835 ))) +
              (choice_data$volume_liters_812083838 + 1 )/(1-1/(1+exp(-ALPHA_812083838 ))) +
              (choice_data$volume_liters_826071715 + 1 )/(1-1/(1+exp(-ALPHA_826071715 ))) +
              (choice_data$volume_liters_826371666 + 1 )/(1-1/(1+exp(-ALPHA_826371666 ))) +
              (choice_data$volume_liters_827871639 + 1 )/(1-1/(1+exp(-ALPHA_827871639 ))) +
              (choice_data$volume_liters_828461681 + 1 )/(1-1/(1+exp(-ALPHA_828461681 ))) +
              (choice_data$volume_liters_828471697 + 1 )/(1-1/(1+exp(-ALPHA_828471697 ))) +
              (choice_data$volume_liters_828671728 + 1 )/(1-1/(1+exp(-ALPHA_828671728 ))) +
              (choice_data$volume_liters_861121724 + 1 )/(1-1/(1+exp(-ALPHA_861121724 ))) +
              (choice_data$volume_liters_862513015 + 1 )/(1-1/(1+exp(-ALPHA_862513015 ))) +
              (choice_data$volume_liters_865073846 + 1 )/(1-1/(1+exp(-ALPHA_865073846 ))) +
              (choice_data$volume_liters_866702776 + 1 )/(1-1/(1+exp(-ALPHA_866702776 ))) +
              (choice_data$volume_liters_868864757 + 1 )/(1-1/(1+exp(-ALPHA_868864757 ))) +
              (choice_data$volume_liters_868874756 + 1 )/(1-1/(1+exp(-ALPHA_868874756 ))) +
              (choice_data$volume_liters_868884776 + 1 )/(1-1/(1+exp(-ALPHA_868884776 ))) +
              (choice_data$volume_liters_879373012 + 1 )/(1-1/(1+exp(-ALPHA_879373012 ))) +
              (choice_data$volume_liters_882963898 + 1 )/(1-1/(1+exp(-ALPHA_882963898 ))) +
              (choice_data$volume_liters_891962980 + 1 )/(1-1/(1+exp(-ALPHA_891962980 ))) +
              (choice_data$volume_liters_891972979 + 1 )/(1-1/(1+exp(-ALPHA_891972979 ))) +
              (choice_data$volume_liters_891982982 + 1 )/(1-1/(1+exp(-ALPHA_891982982 ))) +
              (choice_data$volume_liters_893873009 + 1 )/(1-1/(1+exp(-ALPHA_893873009 ))) +
              (choice_data$volume_liters_895773544 + 1 )/(1-1/(1+exp(-ALPHA_895773544 )))) + 
      log(    ifelse(choice_data$chosen_9999      == 1, exp(v9999     ), 1) *
              ifelse(choice_data$chosen_10550695  == 1, exp(v10550695 ), 1) *
              ifelse(choice_data$chosen_11586693  == 1, exp(v11586693 ), 1) *
              ifelse(choice_data$chosen_11773688  == 1, exp(v11773688 ), 1) *
              ifelse(choice_data$chosen_11774687  == 1, exp(v11774687 ), 1) *
              ifelse(choice_data$chosen_11776686  == 1, exp(v11776686 ), 1) *
              ifelse(choice_data$chosen_11777685  == 1, exp(v11777685 ), 1) *
              ifelse(choice_data$chosen_11786699  == 1, exp(v11786699 ), 1) *
              ifelse(choice_data$chosen_11788689  == 1, exp(v11788689 ), 1) *
              ifelse(choice_data$chosen_16518268  == 1, exp(v16518268 ), 1) *
              ifelse(choice_data$chosen_17991132  == 1, exp(v17991132 ), 1) *
              ifelse(choice_data$chosen_22788573  == 1, exp(v22788573 ), 1) *
              ifelse(choice_data$chosen_29287548  == 1, exp(v29287548 ), 1) *
              ifelse(choice_data$chosen_34006175  == 1, exp(v34006175 ), 1) *
              ifelse(choice_data$chosen_34007174  == 1, exp(v34007174 ), 1) *
              ifelse(choice_data$chosen_34008178  == 1, exp(v34008178 ), 1) *
              ifelse(choice_data$chosen_34029146  == 1, exp(v34029146 ), 1) *
              ifelse(choice_data$chosen_35317555  == 1, exp(v35317555 ), 1) *
              ifelse(choice_data$chosen_35318559  == 1, exp(v35318559 ), 1) *
              ifelse(choice_data$chosen_35416952  == 1, exp(v35416952 ), 1) *
              ifelse(choice_data$chosen_35418953  == 1, exp(v35418953 ), 1) *
              ifelse(choice_data$chosen_43026214  == 1, exp(v43026214 ), 1) *
              ifelse(choice_data$chosen_43027213  == 1, exp(v43027213 ), 1) *
              ifelse(choice_data$chosen_43028216  == 1, exp(v43028216 ), 1) *
              ifelse(choice_data$chosen_43038436  == 1, exp(v43038436 ), 1) *
              ifelse(choice_data$chosen_43126479  == 1, exp(v43126479 ), 1) *
              ifelse(choice_data$chosen_43127478  == 1, exp(v43127478 ), 1) *
              ifelse(choice_data$chosen_43128482  == 1, exp(v43128482 ), 1) *
              ifelse(choice_data$chosen_43136446  == 1, exp(v43136446 ), 1) *
              ifelse(choice_data$chosen_43137443  == 1, exp(v43137443 ), 1) *
              ifelse(choice_data$chosen_44217554  == 1, exp(v44217554 ), 1) *
              ifelse(choice_data$chosen_68036500  == 1, exp(v68036500 ), 1) *
              ifelse(choice_data$chosen_68037498  == 1, exp(v68037498 ), 1) *
              ifelse(choice_data$chosen_88283243  == 1, exp(v88283243 ), 1) *
              ifelse(choice_data$chosen_100084433 == 1, exp(v100084433), 1) *
              ifelse(choice_data$chosen_106271048 == 1, exp(v106271048), 1) *
              ifelse(choice_data$chosen_106281053 == 1, exp(v106281053), 1) *
              ifelse(choice_data$chosen_108071511 == 1, exp(v108071511), 1) *
              ifelse(choice_data$chosen_112961476 == 1, exp(v112961476), 1) *
              ifelse(choice_data$chosen_112971485 == 1, exp(v112971485), 1) *
              ifelse(choice_data$chosen_112981488 == 1, exp(v112981488), 1) *
              ifelse(choice_data$chosen_124071064 == 1, exp(v124071064), 1) *
              ifelse(choice_data$chosen_124081067 == 1, exp(v124081067), 1) *
              ifelse(choice_data$chosen_124781072 == 1, exp(v124781072), 1) *
              ifelse(choice_data$chosen_130381081 == 1, exp(v130381081), 1) *
              ifelse(choice_data$chosen_152485359 == 1, exp(v152485359), 1) *
              ifelse(choice_data$chosen_156262825 == 1, exp(v156262825), 1) *
              ifelse(choice_data$chosen_156272824 == 1, exp(v156272824), 1) *
              ifelse(choice_data$chosen_179561971 == 1, exp(v179561971), 1) *
              ifelse(choice_data$chosen_179581973 == 1, exp(v179581973), 1) *
              ifelse(choice_data$chosen_190662873 == 1, exp(v190662873), 1) *
              ifelse(choice_data$chosen_190672872 == 1, exp(v190672872), 1) *
              ifelse(choice_data$chosen_190682877 == 1, exp(v190682877), 1) *
              ifelse(choice_data$chosen_194763335 == 1, exp(v194763335), 1) *
              ifelse(choice_data$chosen_194773333 == 1, exp(v194773333), 1) *
              ifelse(choice_data$chosen_202483690 == 1, exp(v202483690), 1) *
              ifelse(choice_data$chosen_215974940 == 1, exp(v215974940), 1) *
              ifelse(choice_data$chosen_215984943 == 1, exp(v215984943), 1) *
              ifelse(choice_data$chosen_238242082 == 1, exp(v238242082), 1) *
              ifelse(choice_data$chosen_238262081 == 1, exp(v238262081), 1) *
              ifelse(choice_data$chosen_238272080 == 1, exp(v238272080), 1) *
              ifelse(choice_data$chosen_238282084 == 1, exp(v238282084), 1) *
              ifelse(choice_data$chosen_241582464 == 1, exp(v241582464), 1) *
              ifelse(choice_data$chosen_244573083 == 1, exp(v244573083), 1) *
              ifelse(choice_data$chosen_244583087 == 1, exp(v244583087), 1) *
              ifelse(choice_data$chosen_256064473 == 1, exp(v256064473), 1) *
              ifelse(choice_data$chosen_256074472 == 1, exp(v256074472), 1) *
              ifelse(choice_data$chosen_256084476 == 1, exp(v256084476), 1) *
              ifelse(choice_data$chosen_268262788 == 1, exp(v268262788), 1) *
              ifelse(choice_data$chosen_268272787 == 1, exp(v268272787), 1) *
              ifelse(choice_data$chosen_268282790 == 1, exp(v268282790), 1) *
              ifelse(choice_data$chosen_271024938 == 1, exp(v271024938), 1) *
              ifelse(choice_data$chosen_288664915 == 1, exp(v288664915), 1) *
              ifelse(choice_data$chosen_288674914 == 1, exp(v288674914), 1) *
              ifelse(choice_data$chosen_316583822 == 1, exp(v316583822), 1) *
              ifelse(choice_data$chosen_322364483 == 1, exp(v322364483), 1) *
              ifelse(choice_data$chosen_322384487 == 1, exp(v322384487), 1) *
              ifelse(choice_data$chosen_343682147 == 1, exp(v343682147), 1) *
              ifelse(choice_data$chosen_344222417 == 1, exp(v344222417), 1) *
              ifelse(choice_data$chosen_344332416 == 1, exp(v344332416), 1) *
              ifelse(choice_data$chosen_344573094 == 1, exp(v344573094), 1) *
              ifelse(choice_data$chosen_345784107 == 1, exp(v345784107), 1) *
              ifelse(choice_data$chosen_347474844 == 1, exp(v347474844), 1) *
              ifelse(choice_data$chosen_348204895 == 1, exp(v348204895), 1) *
              ifelse(choice_data$chosen_348214898 == 1, exp(v348214898), 1) *
              ifelse(choice_data$chosen_359142077 == 1, exp(v359142077), 1) *
              ifelse(choice_data$chosen_359162076 == 1, exp(v359162076), 1) *
              ifelse(choice_data$chosen_359172075 == 1, exp(v359172075), 1) *
              ifelse(choice_data$chosen_359182079 == 1, exp(v359182079), 1) *
              ifelse(choice_data$chosen_359262073 == 1, exp(v359262073), 1) *
              ifelse(choice_data$chosen_359482096 == 1, exp(v359482096), 1) *
              ifelse(choice_data$chosen_363042475 == 1, exp(v363042475), 1) *
              ifelse(choice_data$chosen_363052474 == 1, exp(v363052474), 1) *
              ifelse(choice_data$chosen_363072473 == 1, exp(v363072473), 1) *
              ifelse(choice_data$chosen_363082477 == 1, exp(v363082477), 1) *
              ifelse(choice_data$chosen_368863465 == 1, exp(v368863465), 1) *
              ifelse(choice_data$chosen_369033466 == 1, exp(v369033466), 1) *
              ifelse(choice_data$chosen_369043469 == 1, exp(v369043469), 1) *
              ifelse(choice_data$chosen_369073471 == 1, exp(v369073471), 1) *
              ifelse(choice_data$chosen_369083470 == 1, exp(v369083470), 1) *
              ifelse(choice_data$chosen_369693633 == 1, exp(v369693633), 1) *
              ifelse(choice_data$chosen_369713637 == 1, exp(v369713637), 1) *
              ifelse(choice_data$chosen_369763644 == 1, exp(v369763644), 1) *
              ifelse(choice_data$chosen_369783647 == 1, exp(v369783647), 1) *
              ifelse(choice_data$chosen_373365237 == 1, exp(v373365237), 1) *
              ifelse(choice_data$chosen_373385241 == 1, exp(v373385241), 1) *
              ifelse(choice_data$chosen_373464016 == 1, exp(v373464016), 1) *
              ifelse(choice_data$chosen_373474014 == 1, exp(v373474014), 1) *
              ifelse(choice_data$chosen_373484017 == 1, exp(v373484017), 1) *
              ifelse(choice_data$chosen_374174133 == 1, exp(v374174133), 1) *
              ifelse(choice_data$chosen_374184137 == 1, exp(v374184137), 1) *
              ifelse(choice_data$chosen_379384566 == 1, exp(v379384566), 1) *
              ifelse(choice_data$chosen_379944707 == 1, exp(v379944707), 1) *
              ifelse(choice_data$chosen_379964706 == 1, exp(v379964706), 1) *
              ifelse(choice_data$chosen_379974705 == 1, exp(v379974705), 1) *
              ifelse(choice_data$chosen_379984709 == 1, exp(v379984709), 1) *
              ifelse(choice_data$chosen_380064712 == 1, exp(v380064712), 1) *
              ifelse(choice_data$chosen_380084711 == 1, exp(v380084711), 1) *
              ifelse(choice_data$chosen_380884122 == 1, exp(v380884122), 1) *
              ifelse(choice_data$chosen_381765067 == 1, exp(v381765067), 1) *
              ifelse(choice_data$chosen_381775064 == 1, exp(v381775064), 1) *
              ifelse(choice_data$chosen_381785066 == 1, exp(v381785066), 1) *
              ifelse(choice_data$chosen_410762864 == 1, exp(v410762864), 1) *
              ifelse(choice_data$chosen_416925185 == 1, exp(v416925185), 1) *
              ifelse(choice_data$chosen_416935184 == 1, exp(v416935184), 1) *
              ifelse(choice_data$chosen_416945182 == 1, exp(v416945182), 1) *
              ifelse(choice_data$chosen_417045220 == 1, exp(v417045220), 1) *
              ifelse(choice_data$chosen_418461979 == 1, exp(v418461979), 1) *
              ifelse(choice_data$chosen_419895193 == 1, exp(v419895193), 1) *
              ifelse(choice_data$chosen_427163350 == 1, exp(v427163350), 1) *
              ifelse(choice_data$chosen_427173349 == 1, exp(v427173349), 1) *
              ifelse(choice_data$chosen_427183352 == 1, exp(v427183352), 1) *
              ifelse(choice_data$chosen_432851114 == 1, exp(v432851114), 1) *
              ifelse(choice_data$chosen_433341134 == 1, exp(v433341134), 1) *
              ifelse(choice_data$chosen_433351138 == 1, exp(v433351138), 1) *
              ifelse(choice_data$chosen_433361110 == 1, exp(v433361110), 1) *
              ifelse(choice_data$chosen_433371133 == 1, exp(v433371133), 1) *
              ifelse(choice_data$chosen_433381135 == 1, exp(v433381135), 1) *
              ifelse(choice_data$chosen_433871168 == 1, exp(v433871168), 1) *
              ifelse(choice_data$chosen_446583206 == 1, exp(v446583206), 1) *
              ifelse(choice_data$chosen_452483827 == 1, exp(v452483827), 1) *
              ifelse(choice_data$chosen_452763855 == 1, exp(v452763855), 1) *
              ifelse(choice_data$chosen_452773854 == 1, exp(v452773854), 1) *
              ifelse(choice_data$chosen_452783857 == 1, exp(v452783857), 1) *
              ifelse(choice_data$chosen_458864376 == 1, exp(v458864376), 1) *
              ifelse(choice_data$chosen_463502472 == 1, exp(v463502472), 1) *
              ifelse(choice_data$chosen_481052499 == 1, exp(v481052499), 1) *
              ifelse(choice_data$chosen_481062503 == 1, exp(v481062503), 1) *
              ifelse(choice_data$chosen_523161282 == 1, exp(v523161282), 1) *
              ifelse(choice_data$chosen_525961862 == 1, exp(v525961862), 1) *
              ifelse(choice_data$chosen_525981863 == 1, exp(v525981863), 1) *
              ifelse(choice_data$chosen_532143913 == 1, exp(v532143913), 1) *
              ifelse(choice_data$chosen_550863803 == 1, exp(v550863803), 1) *
              ifelse(choice_data$chosen_571251241 == 1, exp(v571251241), 1) *
              ifelse(choice_data$chosen_571481243 == 1, exp(v571481243), 1) *
              ifelse(choice_data$chosen_588382962 == 1, exp(v588382962), 1) *
              ifelse(choice_data$chosen_588682969 == 1, exp(v588682969), 1) *
              ifelse(choice_data$chosen_588722960 == 1, exp(v588722960), 1) *
              ifelse(choice_data$chosen_588752994 == 1, exp(v588752994), 1) *
              ifelse(choice_data$chosen_590371737 == 1, exp(v590371737), 1) *
              ifelse(choice_data$chosen_648582044 == 1, exp(v648582044), 1) *
              ifelse(choice_data$chosen_648652045 == 1, exp(v648652045), 1) *
              ifelse(choice_data$chosen_648662041 == 1, exp(v648662041), 1) *
              ifelse(choice_data$chosen_648672039 == 1, exp(v648672039), 1) *
              ifelse(choice_data$chosen_648682043 == 1, exp(v648682043), 1) *
              ifelse(choice_data$chosen_652562809 == 1, exp(v652562809), 1) *
              ifelse(choice_data$chosen_652572808 == 1, exp(v652572808), 1) *
              ifelse(choice_data$chosen_675263042 == 1, exp(v675263042), 1) *
              ifelse(choice_data$chosen_675273041 == 1, exp(v675273041), 1) *
              ifelse(choice_data$chosen_696361815 == 1, exp(v696361815), 1) *
              ifelse(choice_data$chosen_696371814 == 1, exp(v696371814), 1) *
              ifelse(choice_data$chosen_696381817 == 1, exp(v696381817), 1) *
              ifelse(choice_data$chosen_699474354 == 1, exp(v699474354), 1) *
              ifelse(choice_data$chosen_730504347 == 1, exp(v730504347), 1) *
              ifelse(choice_data$chosen_730534345 == 1, exp(v730534345), 1) *
              ifelse(choice_data$chosen_730554344 == 1, exp(v730554344), 1) *
              ifelse(choice_data$chosen_750873008 == 1, exp(v750873008), 1) *
              ifelse(choice_data$chosen_752103114 == 1, exp(v752103114), 1) *
              ifelse(choice_data$chosen_752143108 == 1, exp(v752143108), 1) *
              ifelse(choice_data$chosen_764863795 == 1, exp(v764863795), 1) *
              ifelse(choice_data$chosen_764873794 == 1, exp(v764873794), 1) *
              ifelse(choice_data$chosen_774875094 == 1, exp(v774875094), 1) *
              ifelse(choice_data$chosen_812063836 == 1, exp(v812063836), 1) *
              ifelse(choice_data$chosen_812073835 == 1, exp(v812073835), 1) *
              ifelse(choice_data$chosen_812083838 == 1, exp(v812083838), 1) *
              ifelse(choice_data$chosen_826071715 == 1, exp(v826071715), 1) *
              ifelse(choice_data$chosen_826371666 == 1, exp(v826371666), 1) *
              ifelse(choice_data$chosen_827871639 == 1, exp(v827871639), 1) *
              ifelse(choice_data$chosen_828461681 == 1, exp(v828461681), 1) *
              ifelse(choice_data$chosen_828471697 == 1, exp(v828471697), 1) *
              ifelse(choice_data$chosen_828671728 == 1, exp(v828671728), 1) *
              ifelse(choice_data$chosen_861121724 == 1, exp(v861121724), 1) *
              ifelse(choice_data$chosen_862513015 == 1, exp(v862513015), 1) *
              ifelse(choice_data$chosen_865073846 == 1, exp(v865073846), 1) *
              ifelse(choice_data$chosen_866702776 == 1, exp(v866702776), 1) *
              ifelse(choice_data$chosen_868864757 == 1, exp(v868864757), 1) *
              ifelse(choice_data$chosen_868874756 == 1, exp(v868874756), 1) *
              ifelse(choice_data$chosen_868884776 == 1, exp(v868884776), 1) *
              ifelse(choice_data$chosen_879373012 == 1, exp(v879373012), 1) *
              ifelse(choice_data$chosen_882963898 == 1, exp(v882963898), 1) *
              ifelse(choice_data$chosen_891962980 == 1, exp(v891962980), 1) *
              ifelse(choice_data$chosen_891972979 == 1, exp(v891972979), 1) *
              ifelse(choice_data$chosen_891982982 == 1, exp(v891982982), 1) *
              ifelse(choice_data$chosen_893873009 == 1, exp(v893873009), 1) *
              ifelse(choice_data$chosen_895773544 == 1, exp(v895773544), 1)) -
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
              exp(v44217554 ) +
              exp(v68036500 ) +
              exp(v68037498 ) +
              exp(v88283243 ) +
              exp(v100084433) +
              exp(v106271048) +
              exp(v106281053) +
              exp(v108071511) +
              exp(v112961476) +
              exp(v112971485) +
              exp(v112981488) +
              exp(v124071064) +
              exp(v124081067) +
              exp(v124781072) +
              exp(v130381081) +
              exp(v152485359) +
              exp(v156262825) +
              exp(v156272824) +
              exp(v179561971) +
              exp(v179581973) +
              exp(v190662873) +
              exp(v190672872) +
              exp(v190682877) +
              exp(v194763335) +
              exp(v194773333) +
              exp(v202483690) +
              exp(v215974940) +
              exp(v215984943) +
              exp(v238242082) +
              exp(v238262081) +
              exp(v238272080) +
              exp(v238282084) +
              exp(v241582464) +
              exp(v244573083) +
              exp(v244583087) +
              exp(v256064473) +
              exp(v256074472) +
              exp(v256084476) +
              exp(v268262788) +
              exp(v268272787) +
              exp(v268282790) +
              exp(v271024938) +
              exp(v288664915) +
              exp(v288674914) +
              exp(v316583822) +
              exp(v322364483) +
              exp(v322384487) +
              exp(v343682147) +
              exp(v344222417) +
              exp(v344332416) +
              exp(v344573094) +
              exp(v345784107) +
              exp(v347474844) +
              exp(v348204895) +
              exp(v348214898) +
              exp(v359142077) +
              exp(v359162076) +
              exp(v359172075) +
              exp(v359182079) +
              exp(v359262073) +
              exp(v359482096) +
              exp(v363042475) +
              exp(v363052474) +
              exp(v363072473) +
              exp(v363082477) +
              exp(v368863465) +
              exp(v369033466) +
              exp(v369043469) +
              exp(v369073471) +
              exp(v369083470) +
              exp(v369693633) +
              exp(v369713637) +
              exp(v369763644) +
              exp(v369783647) +
              exp(v373365237) +
              exp(v373385241) +
              exp(v373464016) +
              exp(v373474014) +
              exp(v373484017) +
              exp(v374174133) +
              exp(v374184137) +
              exp(v379384566) +
              exp(v379944707) +
              exp(v379964706) +
              exp(v379974705) +
              exp(v379984709) +
              exp(v380064712) +
              exp(v380084711) +
              exp(v380884122) +
              exp(v381765067) +
              exp(v381775064) +
              exp(v381785066) +
              exp(v410762864) +
              exp(v416925185) +
              exp(v416935184) +
              exp(v416945182) +
              exp(v417045220) +
              exp(v418461979) +
              exp(v419895193) +
              exp(v427163350) +
              exp(v427173349) +
              exp(v427183352) +
              exp(v432851114) +
              exp(v433341134) +
              exp(v433351138) +
              exp(v433361110) +
              exp(v433371133) +
              exp(v433381135) +
              exp(v433871168) +
              exp(v446583206) +
              exp(v452483827) +
              exp(v452763855) +
              exp(v452773854) +
              exp(v452783857) +
              exp(v458864376) +
              exp(v463502472) +
              exp(v481052499) +
              exp(v481062503) +
              exp(v523161282) +
              exp(v525961862) +
              exp(v525981863) +
              exp(v532143913) +
              exp(v550863803) +
              exp(v571251241) +
              exp(v571481243) +
              exp(v588382962) +
              exp(v588682969) +
              exp(v588722960) +
              exp(v588752994) +
              exp(v590371737) +
              exp(v648582044) +
              exp(v648652045) +
              exp(v648662041) +
              exp(v648672039) +
              exp(v648682043) +
              exp(v652562809) +
              exp(v652572808) +
              exp(v675263042) +
              exp(v675273041) +
              exp(v696361815) +
              exp(v696371814) +
              exp(v696381817) +
              exp(v699474354) +
              exp(v730504347) +
              exp(v730534345) +
              exp(v730554344) +
              exp(v750873008) +
              exp(v752103114) +
              exp(v752143108) +
              exp(v764863795) +
              exp(v764873794) +
              exp(v774875094) +
              exp(v812063836) +
              exp(v812073835) +
              exp(v812083838) +
              exp(v826071715) +
              exp(v826371666) +
              exp(v827871639) +
              exp(v828461681) +
              exp(v828471697) +
              exp(v828671728) +
              exp(v861121724) +
              exp(v862513015) +
              exp(v865073846) +
              exp(v866702776) +
              exp(v868864757) +
              exp(v868874756) +
              exp(v868884776) +
              exp(v879373012) +
              exp(v882963898) +
              exp(v891962980) +
              exp(v891972979) +
              exp(v891982982) +
              exp(v893873009) +
              exp(v895773544)) +
      lfactorial(choice_data$countm - 1)))
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
  "ASC68036500" , 
  "ASC68037498" , 
  "ASC88283243" , 
  "ASC100084433", 
  "ASC106271048", 
  "ASC106281053", 
  "ASC108071511", 
  "ASC112961476", 
  "ASC112971485", 
  "ASC112981488", 
  "ASC124071064", 
  "ASC124081067", 
  "ASC124781072", 
  "ASC130381081", 
  "ASC152485359", 
  "ASC156262825", 
  "ASC156272824", 
  "ASC179561971", 
  "ASC179581973", 
  "ASC190662873", 
  "ASC190672872", 
  "ASC190682877", 
  "ASC194763335", 
  "ASC194773333", 
  "ASC202483690", 
  "ASC215974940", 
  "ASC215984943", 
  "ASC238242082", 
  "ASC238262081", 
  "ASC238272080", 
  "ASC238282084", 
  "ASC241582464", 
  "ASC244573083", 
  "ASC244583087", 
  "ASC256064473", 
  "ASC256074472", 
  "ASC256084476", 
  "ASC268262788", 
  "ASC268272787", 
  "ASC268282790", 
  "ASC271024938", 
  "ASC288664915", 
  "ASC288674914", 
  "ASC316583822", 
  "ASC322364483", 
  "ASC322384487", 
  "ASC343682147", 
  "ASC344222417", 
  "ASC344332416", 
  "ASC344573094", 
  "ASC345784107", 
  "ASC347474844", 
  "ASC348204895", 
  "ASC348214898", 
  "ASC359142077", 
  "ASC359162076", 
  "ASC359172075", 
  "ASC359182079", 
  "ASC359262073", 
  "ASC359482096", 
  "ASC363042475", 
  "ASC363052474", 
  "ASC363072473", 
  "ASC363082477", 
  "ASC368863465", 
  "ASC369033466", 
  "ASC369043469", 
  "ASC369073471", 
  "ASC369083470", 
  "ASC369693633", 
  "ASC369713637", 
  "ASC369763644", 
  "ASC369783647", 
  "ASC373365237", 
  "ASC373385241", 
  "ASC373464016", 
  "ASC373474014", 
  "ASC373484017", 
  "ASC374174133", 
  "ASC374184137", 
  "ASC379384566", 
  "ASC379944707", 
  "ASC379964706", 
  "ASC379974705", 
  "ASC379984709", 
  "ASC380064712", 
  "ASC380084711", 
  "ASC380884122", 
  "ASC381765067", 
  "ASC381775064", 
  "ASC381785066", 
  "ASC410762864", 
  "ASC416925185", 
  "ASC416935184", 
  "ASC416945182", 
  "ASC417045220", 
  "ASC418461979", 
  "ASC419895193", 
  "ASC427163350", 
  "ASC427173349", 
  "ASC427183352", 
  "ASC432851114", 
  "ASC433341134", 
  "ASC433351138", 
  "ASC433361110", 
  "ASC433371133", 
  "ASC433381135", 
  "ASC433871168", 
  "ASC446583206", 
  "ASC452483827", 
  "ASC452763855", 
  "ASC452773854", 
  "ASC452783857", 
  "ASC458864376", 
  "ASC463502472", 
  "ASC481052499", 
  "ASC481062503", 
  "ASC523161282", 
  "ASC525961862", 
  "ASC525981863", 
  "ASC532143913", 
  "ASC550863803", 
  "ASC571251241", 
  "ASC571481243", 
  "ASC588382962", 
  "ASC588682969", 
  "ASC588722960", 
  "ASC588752994", 
  "ASC590371737", 
  "ASC648582044", 
  "ASC648652045", 
  "ASC648662041", 
  "ASC648672039", 
  "ASC648682043", 
  "ASC652562809", 
  "ASC652572808", 
  "ASC675263042", 
  "ASC675273041", 
  "ASC696361815", 
  "ASC696371814", 
  "ASC696381817", 
  "ASC699474354", 
  "ASC730504347", 
  "ASC730534345", 
  "ASC730554344", 
  "ASC750873008", 
  "ASC752103114", 
  "ASC752143108", 
  "ASC764863795", 
  "ASC764873794", 
  "ASC774875094", 
  "ASC812063836", 
  "ASC812073835", 
  "ASC812083838", 
  "ASC826071715", 
  "ASC826371666", 
  "ASC827871639", 
  "ASC828461681", 
  "ASC828471697", 
  "ASC828671728", 
  "ASC861121724", 
  "ASC862513015", 
  "ASC865073846", 
  "ASC866702776", 
  "ASC868864757", 
  "ASC868874756", 
  "ASC868884776", 
  "ASC879373012", 
  "ASC882963898", 
  "ASC891962980", 
  "ASC891972979", 
  "ASC891982982", 
  "ASC893873009", 
  "ASC895773544",
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
  "ALPHA_68036500" , 
  "ALPHA_68037498" , 
  "ALPHA_88283243" , 
  "ALPHA_100084433", 
  "ALPHA_106271048", 
  "ALPHA_106281053", 
  "ALPHA_108071511", 
  "ALPHA_112961476", 
  "ALPHA_112971485", 
  "ALPHA_112981488", 
  "ALPHA_124071064", 
  "ALPHA_124081067", 
  "ALPHA_124781072", 
  "ALPHA_130381081", 
  "ALPHA_152485359", 
  "ALPHA_156262825", 
  "ALPHA_156272824", 
  "ALPHA_179561971", 
  "ALPHA_179581973", 
  "ALPHA_190662873", 
  "ALPHA_190672872", 
  "ALPHA_190682877", 
  "ALPHA_194763335", 
  "ALPHA_194773333", 
  "ALPHA_202483690", 
  "ALPHA_215974940", 
  "ALPHA_215984943", 
  "ALPHA_238242082", 
  "ALPHA_238262081", 
  "ALPHA_238272080", 
  "ALPHA_238282084", 
  "ALPHA_241582464", 
  "ALPHA_244573083", 
  "ALPHA_244583087", 
  "ALPHA_256064473", 
  "ALPHA_256074472", 
  "ALPHA_256084476", 
  "ALPHA_268262788", 
  "ALPHA_268272787", 
  "ALPHA_268282790", 
  "ALPHA_271024938", 
  "ALPHA_288664915", 
  "ALPHA_288674914", 
  "ALPHA_316583822", 
  "ALPHA_322364483", 
  "ALPHA_322384487", 
  "ALPHA_343682147", 
  "ALPHA_344222417", 
  "ALPHA_344332416", 
  "ALPHA_344573094", 
  "ALPHA_345784107", 
  "ALPHA_347474844", 
  "ALPHA_348204895", 
  "ALPHA_348214898", 
  "ALPHA_359142077", 
  "ALPHA_359162076", 
  "ALPHA_359172075", 
  "ALPHA_359182079", 
  "ALPHA_359262073", 
  "ALPHA_359482096", 
  "ALPHA_363042475", 
  "ALPHA_363052474", 
  "ALPHA_363072473", 
  "ALPHA_363082477", 
  "ALPHA_368863465", 
  "ALPHA_369033466", 
  "ALPHA_369043469", 
  "ALPHA_369073471", 
  "ALPHA_369083470", 
  "ALPHA_369693633", 
  "ALPHA_369713637", 
  "ALPHA_369763644", 
  "ALPHA_369783647", 
  "ALPHA_373365237", 
  "ALPHA_373385241", 
  "ALPHA_373464016", 
  "ALPHA_373474014", 
  "ALPHA_373484017", 
  "ALPHA_374174133", 
  "ALPHA_374184137", 
  "ALPHA_379384566", 
  "ALPHA_379944707", 
  "ALPHA_379964706", 
  "ALPHA_379974705", 
  "ALPHA_379984709", 
  "ALPHA_380064712", 
  "ALPHA_380084711", 
  "ALPHA_380884122", 
  "ALPHA_381765067", 
  "ALPHA_381775064", 
  "ALPHA_381785066", 
  "ALPHA_410762864", 
  "ALPHA_416925185", 
  "ALPHA_416935184", 
  "ALPHA_416945182", 
  "ALPHA_417045220", 
  "ALPHA_418461979", 
  "ALPHA_419895193", 
  "ALPHA_427163350", 
  "ALPHA_427173349", 
  "ALPHA_427183352", 
  "ALPHA_432851114", 
  "ALPHA_433341134", 
  "ALPHA_433351138", 
  "ALPHA_433361110", 
  "ALPHA_433371133", 
  "ALPHA_433381135", 
  "ALPHA_433871168", 
  "ALPHA_446583206", 
  "ALPHA_452483827", 
  "ALPHA_452763855", 
  "ALPHA_452773854", 
  "ALPHA_452783857", 
  "ALPHA_458864376", 
  "ALPHA_463502472", 
  "ALPHA_481052499", 
  "ALPHA_481062503", 
  "ALPHA_523161282", 
  "ALPHA_525961862", 
  "ALPHA_525981863", 
  "ALPHA_532143913", 
  "ALPHA_550863803", 
  "ALPHA_571251241", 
  "ALPHA_571481243", 
  "ALPHA_588382962", 
  "ALPHA_588682969", 
  "ALPHA_588722960", 
  "ALPHA_588752994", 
  "ALPHA_590371737", 
  "ALPHA_648582044", 
  "ALPHA_648652045", 
  "ALPHA_648662041", 
  "ALPHA_648672039", 
  "ALPHA_648682043", 
  "ALPHA_652562809", 
  "ALPHA_652572808", 
  "ALPHA_675263042", 
  "ALPHA_675273041", 
  "ALPHA_696361815", 
  "ALPHA_696371814", 
  "ALPHA_696381817", 
  "ALPHA_699474354", 
  "ALPHA_730504347", 
  "ALPHA_730534345", 
  "ALPHA_730554344", 
  "ALPHA_750873008", 
  "ALPHA_752103114", 
  "ALPHA_752143108", 
  "ALPHA_764863795", 
  "ALPHA_764873794", 
  "ALPHA_774875094", 
  "ALPHA_812063836", 
  "ALPHA_812073835", 
  "ALPHA_812083838", 
  "ALPHA_826071715", 
  "ALPHA_826371666", 
  "ALPHA_827871639", 
  "ALPHA_828461681", 
  "ALPHA_828471697", 
  "ALPHA_828671728", 
  "ALPHA_861121724", 
  "ALPHA_862513015", 
  "ALPHA_865073846", 
  "ALPHA_866702776", 
  "ALPHA_868864757", 
  "ALPHA_868874756", 
  "ALPHA_868884776", 
  "ALPHA_879373012", 
  "ALPHA_882963898", 
  "ALPHA_891962980", 
  "ALPHA_891972979", 
  "ALPHA_891982982", 
  "ALPHA_893873009", 
  "ALPHA_895773544")
  # "GAMMA_9999"      , 
  # "GAMMA_10550695"  , 
  # "GAMMA_11586693"  , 
  # "GAMMA_11773688"  , 
  # "GAMMA_11774687"  , 
  # "GAMMA_11776686"  , 
  # "GAMMA_11777685"  , 
  # "GAMMA_11786699"  , 
  # "GAMMA_11788689"  , 
  # "GAMMA_16518268"  , 
  # "GAMMA_17991132"  , 
  # "GAMMA_22788573"  , 
  # "GAMMA_29287548"  , 
  # "GAMMA_34006175"  , 
  # "GAMMA_34007174"  , 
  # "GAMMA_34008178"  , 
  # "GAMMA_34029146"  , 
  # "GAMMA_35317555"  , 
  # "GAMMA_35318559"  , 
  # "GAMMA_35416952"  , 
  # "GAMMA_35418953"  , 
  # "GAMMA_43026214"  , 
  # "GAMMA_43027213"  , 
  # "GAMMA_43028216"  , 
  # "GAMMA_43038436"  , 
  # "GAMMA_43126479"  , 
  # "GAMMA_43127478"  , 
  # "GAMMA_43128482"  , 
  # "GAMMA_43136446"  , 
  # "GAMMA_43137443"  , 
  # "GAMMA_44217554"  , 
  # "GAMMA_68036500"  , 
  # "GAMMA_68037498"  , 
  # "GAMMA_88283243"  , 
  # "GAMMA_100084433" , 
  # "GAMMA_106271048" , 
  # "GAMMA_106281053" , 
  # "GAMMA_108071511" , 
  # "GAMMA_112961476" , 
  # "GAMMA_112971485" , 
  # "GAMMA_112981488" , 
  # "GAMMA_124071064" , 
  # "GAMMA_124081067" , 
  # "GAMMA_124781072" , 
  # "GAMMA_130381081" , 
  # "GAMMA_152485359" , 
  # "GAMMA_156262825" , 
  # "GAMMA_156272824" , 
  # "GAMMA_179561971" , 
  # "GAMMA_179581973" , 
  # "GAMMA_190662873" , 
  # "GAMMA_190672872" , 
  # "GAMMA_190682877" , 
  # "GAMMA_194763335" , 
  # "GAMMA_194773333" , 
  # "GAMMA_202483690" , 
  # "GAMMA_215974940" , 
  # "GAMMA_215984943" , 
  # "GAMMA_238242082" , 
  # "GAMMA_238262081" , 
  # "GAMMA_238272080" , 
  # "GAMMA_238282084" , 
  # "GAMMA_241582464" , 
  # "GAMMA_244573083" , 
  # "GAMMA_244583087" , 
  # "GAMMA_256064473" , 
  # "GAMMA_256074472" , 
  # "GAMMA_256084476" , 
  # "GAMMA_268262788" , 
  # "GAMMA_268272787" , 
  # "GAMMA_268282790" , 
  # "GAMMA_271024938" , 
  # "GAMMA_288664915" , 
  # "GAMMA_288674914" , 
  # "GAMMA_316583822" , 
  # "GAMMA_322364483" , 
  # "GAMMA_322384487" , 
  # "GAMMA_343682147" , 
  # "GAMMA_344222417" , 
  # "GAMMA_344332416" , 
  # "GAMMA_344573094" , 
  # "GAMMA_345784107" , 
  # "GAMMA_347474844" , 
  # "GAMMA_348204895" , 
  # "GAMMA_348214898" , 
  # "GAMMA_359142077" , 
  # "GAMMA_359162076" , 
  # "GAMMA_359172075" , 
  # "GAMMA_359182079" , 
  # "GAMMA_359262073" , 
  # "GAMMA_359482096" , 
  # "GAMMA_363042475" , 
  # "GAMMA_363052474" , 
  # "GAMMA_363072473" , 
  # "GAMMA_363082477" , 
  # "GAMMA_368863465" , 
  # "GAMMA_369033466" , 
  # "GAMMA_369043469" , 
  # "GAMMA_369073471" , 
  # "GAMMA_369083470" , 
  # "GAMMA_369693633" , 
  # "GAMMA_369713637" , 
  # "GAMMA_369763644" , 
  # "GAMMA_369783647" , 
  # "GAMMA_373365237" , 
  # "GAMMA_373385241" , 
  # "GAMMA_373464016" , 
  # "GAMMA_373474014" , 
  # "GAMMA_373484017" , 
  # "GAMMA_374174133" , 
  # "GAMMA_374184137" , 
  # "GAMMA_379384566" , 
  # "GAMMA_379944707" , 
  # "GAMMA_379964706" , 
  # "GAMMA_379974705" , 
  # "GAMMA_379984709" , 
  # "GAMMA_380064712" , 
  # "GAMMA_380084711" , 
  # "GAMMA_380884122" , 
  # "GAMMA_381765067" , 
  # "GAMMA_381775064" , 
  # "GAMMA_381785066" , 
  # "GAMMA_410762864" , 
  # "GAMMA_416925185" , 
  # "GAMMA_416935184" , 
  # "GAMMA_416945182" , 
  # "GAMMA_417045220" , 
  # "GAMMA_418461979" , 
  # "GAMMA_419895193" , 
  # "GAMMA_427163350" , 
  # "GAMMA_427173349" , 
  # "GAMMA_427183352" , 
  # "GAMMA_432851114" , 
  # "GAMMA_433341134" , 
  # "GAMMA_433351138" , 
  # "GAMMA_433361110" , 
  # "GAMMA_433371133" , 
  # "GAMMA_433381135" , 
  # "GAMMA_433871168" , 
  # "GAMMA_446583206" , 
  # "GAMMA_452483827" , 
  # "GAMMA_452763855" , 
  # "GAMMA_452773854" , 
  # "GAMMA_452783857" , 
  # "GAMMA_458864376" , 
  # "GAMMA_463502472" , 
  # "GAMMA_481052499" , 
  # "GAMMA_481062503" , 
  # "GAMMA_523161282" , 
  # "GAMMA_525961862" , 
  # "GAMMA_525981863" , 
  # "GAMMA_532143913" , 
  # "GAMMA_550863803" , 
  # "GAMMA_571251241" , 
  # "GAMMA_571481243" , 
  # "GAMMA_588382962" , 
  # "GAMMA_588682969" , 
  # "GAMMA_588722960" , 
  # "GAMMA_588752994" , 
  # "GAMMA_590371737" , 
  # "GAMMA_648582044" , 
  # "GAMMA_648652045" , 
  # "GAMMA_648662041" , 
  # "GAMMA_648672039" , 
  # "GAMMA_648682043" , 
  # "GAMMA_652562809" , 
  # "GAMMA_652572808" , 
  # "GAMMA_675263042" , 
  # "GAMMA_675273041" , 
  # "GAMMA_696361815" , 
  # "GAMMA_696371814" , 
  # "GAMMA_696381817" , 
  # "GAMMA_699474354" , 
  # "GAMMA_730504347" , 
  # "GAMMA_730534345" , 
  # "GAMMA_730554344" , 
  # "GAMMA_750873008" , 
  # "GAMMA_752103114" , 
  # "GAMMA_752143108" , 
  # "GAMMA_764863795" , 
  # "GAMMA_764873794" , 
  # "GAMMA_774875094" , 
  # "GAMMA_812063836" , 
  # "GAMMA_812073835" , 
  # "GAMMA_812083838" , 
  # "GAMMA_826071715" , 
  # "GAMMA_826371666" , 
  # "GAMMA_827871639" , 
  # "GAMMA_828461681" , 
  # "GAMMA_828471697" , 
  # "GAMMA_828671728" , 
  # "GAMMA_861121724" , 
  # "GAMMA_862513015" , 
  # "GAMMA_865073846" , 
  # "GAMMA_866702776" , 
  # "GAMMA_868864757" , 
  # "GAMMA_868874756" , 
  # "GAMMA_868884776" , 
  # "GAMMA_879373012" , 
  # "GAMMA_882963898" , 
  # "GAMMA_891962980" , 
  # "GAMMA_891972979" , 
  # "GAMMA_891982982" , 
  # "GAMMA_893873009" , 
  # "GAMMA_895773544" )

# For each variable, specify the distribution for its coefficient
# The options are:
# 1. normal
# 2. log-nomal
# 3. negative log-normal
# 4. normal with all values below zero massed at zero
# 5. Johnson SB with a specified min and max
# gDIST must have an entry for each value in gVarNamesNormal

gDIST <- c(3, rep(1,205), rep(1,205))

# STARTING VALUES
svN <- c(-.1, rep(0,205), rep(0,205))
# ITERATION SETTINGS
gNCREP    <- 40000     # Number of iterations to use prior to convergence
gNEREP    <- 2000          # Number of iterations to keep for averaging after convergence has been reached
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








