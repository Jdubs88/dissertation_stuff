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
choice_data=read.csv('C:/Users/JW/Documents/Iowa papers/discrete_data_0001_monthly_long.csv',header=T) #discrete choice data
store_info=read.csv('C:/Users/JW/Documents/Iowa papers/store_info.csv',header=T) #store info

choice_data = subset(choice_data,choice_data$year >= 2015)
choice_data = subset(choice_data,choice_data$month >= 10)
choice_data = subset(choice_data,choice_data$store_num <= 2600)


available$monthyear = paste(available$month, available$year)
state_cost$monthyear = paste(state_cost$month, state_cost$year)
bottle_cost$monthyear = paste(bottle_cost$month, bottle_cost$year)
choice_data$monthyear = paste(choice_data$month, choice_data$year)

choice_data <-merge(choice_data,state_cost, by.x="monthyear", by.y = "monthyear")
choice_data <-merge(choice_data,store_info, by.x="store_num", by.y = "store_num_abd")
choice_data <-merge(choice_data,available, by.x="monthyear", by.y = "monthyear")

attach(choice_data)

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

state_cost_9999       <- choice_data$state_cost_9999      
state_cost_734190     <- choice_data$state_cost_734190    
state_cost_1573935    <- choice_data$state_cost_1573935   
state_cost_2870505    <- choice_data$state_cost_2870505   
state_cost_4356521    <- choice_data$state_cost_4356521   
state_cost_4626831    <- choice_data$state_cost_4626831   
state_cost_5915426    <- choice_data$state_cost_5915426   
state_cost_6144352    <- choice_data$state_cost_6144352   
state_cost_8417089    <- choice_data$state_cost_8417089   
state_cost_8751020    <- choice_data$state_cost_8751020   
state_cost_8920617    <- choice_data$state_cost_8920617   
state_cost_10548697   <- choice_data$state_cost_10548697  
state_cost_10549698   <- choice_data$state_cost_10549698  
state_cost_10550695   <- choice_data$state_cost_10550695  
state_cost_10553690   <- choice_data$state_cost_10553690  
state_cost_10555696   <- choice_data$state_cost_10555696  
state_cost_11586693   <- choice_data$state_cost_11586693  
state_cost_11588694   <- choice_data$state_cost_11588694  
state_cost_11771692   <- choice_data$state_cost_11771692  
state_cost_11773688   <- choice_data$state_cost_11773688  
state_cost_11774687   <- choice_data$state_cost_11774687  
state_cost_11776686   <- choice_data$state_cost_11776686  
state_cost_11777685   <- choice_data$state_cost_11777685  
state_cost_11786699   <- choice_data$state_cost_11786699  
state_cost_11788689   <- choice_data$state_cost_11788689  
state_cost_13774351   <- choice_data$state_cost_13774351  
state_cost_14001522   <- choice_data$state_cost_14001522  
state_cost_15776973   <- choice_data$state_cost_15776973  
state_cost_16516267   <- choice_data$state_cost_16516267  
state_cost_16517266   <- choice_data$state_cost_16517266  
state_cost_16518268   <- choice_data$state_cost_16518268  
state_cost_16676562   <- choice_data$state_cost_16676562  
state_cost_16987789   <- choice_data$state_cost_16987789  
state_cost_16988790   <- choice_data$state_cost_16988790  
state_cost_17086850   <- choice_data$state_cost_17086850  
state_cost_17127981   <- choice_data$state_cost_17127981  
state_cost_17991132   <- choice_data$state_cost_17991132  
state_cost_18006841   <- choice_data$state_cost_18006841  
state_cost_21480620   <- choice_data$state_cost_21480620  
state_cost_22291300   <- choice_data$state_cost_22291300  
state_cost_22783572   <- choice_data$state_cost_22783572  
state_cost_22784571   <- choice_data$state_cost_22784571  
state_cost_22786570   <- choice_data$state_cost_22786570  
state_cost_22787569   <- choice_data$state_cost_22787569  
state_cost_22788573   <- choice_data$state_cost_22788573  
state_cost_27025847   <- choice_data$state_cost_27025847  
state_cost_27474650   <- choice_data$state_cost_27474650  
state_cost_28086580   <- choice_data$state_cost_28086580  
state_cost_28087579   <- choice_data$state_cost_28087579  
state_cost_28088582   <- choice_data$state_cost_28088582  
state_cost_28206761   <- choice_data$state_cost_28206761  
state_cost_28233766   <- choice_data$state_cost_28233766  
state_cost_28236769   <- choice_data$state_cost_28236769  
state_cost_29287548   <- choice_data$state_cost_29287548  
state_cost_29288550   <- choice_data$state_cost_29288550  
state_cost_29566892   <- choice_data$state_cost_29566892  
state_cost_29568893   <- choice_data$state_cost_29568893  
state_cost_31352818   <- choice_data$state_cost_31352818  
state_cost_33102819   <- choice_data$state_cost_33102819  
state_cost_34001179   <- choice_data$state_cost_34001179  
state_cost_34004177   <- choice_data$state_cost_34004177  
state_cost_34006175   <- choice_data$state_cost_34006175  
state_cost_34007174   <- choice_data$state_cost_34007174  
state_cost_34008178   <- choice_data$state_cost_34008178  
state_cost_34014137   <- choice_data$state_cost_34014137  
state_cost_34021113   <- choice_data$state_cost_34021113  
state_cost_34026169   <- choice_data$state_cost_34026169  
state_cost_34029146   <- choice_data$state_cost_34029146  
state_cost_34030147   <- choice_data$state_cost_34030147  
state_cost_34032148   <- choice_data$state_cost_34032148  
state_cost_34036168   <- choice_data$state_cost_34036168  
state_cost_34052171   <- choice_data$state_cost_34052171  
state_cost_34061172   <- choice_data$state_cost_34061172  
state_cost_34076184   <- choice_data$state_cost_34076184  
state_cost_34116159   <- choice_data$state_cost_34116159  
state_cost_34117158   <- choice_data$state_cost_34117158  
state_cost_34155616   <- choice_data$state_cost_34155616  
state_cost_34162614   <- choice_data$state_cost_34162614  
state_cost_35315561   <- choice_data$state_cost_35315561  
state_cost_35316556   <- choice_data$state_cost_35316556  
state_cost_35317555   <- choice_data$state_cost_35317555  
state_cost_35318559   <- choice_data$state_cost_35318559  
state_cost_35354162   <- choice_data$state_cost_35354162  
state_cost_35416952   <- choice_data$state_cost_35416952  
state_cost_35418953   <- choice_data$state_cost_35418953  
state_cost_36572532   <- choice_data$state_cost_36572532  
state_cost_40482820   <- choice_data$state_cost_40482820  
state_cost_41271111   <- choice_data$state_cost_41271111  
state_cost_41279896   <- choice_data$state_cost_41279896  
state_cost_41284608   <- choice_data$state_cost_41284608  
state_cost_41299880   <- choice_data$state_cost_41299880  
state_cost_41316966   <- choice_data$state_cost_41316966  
state_cost_41320957   <- choice_data$state_cost_41320957  
state_cost_41413922   <- choice_data$state_cost_41413922  
state_cost_41840902   <- choice_data$state_cost_41840902  
state_cost_41927891   <- choice_data$state_cost_41927891  
state_cost_42006282   <- choice_data$state_cost_42006282  
state_cost_43022218   <- choice_data$state_cost_43022218  
state_cost_43024215   <- choice_data$state_cost_43024215  
state_cost_43025219   <- choice_data$state_cost_43025219  
state_cost_43026214   <- choice_data$state_cost_43026214  
state_cost_43027213   <- choice_data$state_cost_43027213  
state_cost_43028216   <- choice_data$state_cost_43028216  
state_cost_43031437   <- choice_data$state_cost_43031437  
state_cost_43035438   <- choice_data$state_cost_43035438  
state_cost_43036432   <- choice_data$state_cost_43036432  
state_cost_43037431   <- choice_data$state_cost_43037431  
state_cost_43038436   <- choice_data$state_cost_43038436  
state_cost_43046475   <- choice_data$state_cost_43046475  
state_cost_43048476   <- choice_data$state_cost_43048476  
state_cost_43050426   <- choice_data$state_cost_43050426  
state_cost_43051427   <- choice_data$state_cost_43051427  
state_cost_43066467   <- choice_data$state_cost_43066467  
state_cost_43076208   <- choice_data$state_cost_43076208  
state_cost_43077207   <- choice_data$state_cost_43077207  
state_cost_43116455   <- choice_data$state_cost_43116455  
state_cost_43117454   <- choice_data$state_cost_43117454  
state_cost_43120477   <- choice_data$state_cost_43120477  
state_cost_43121483   <- choice_data$state_cost_43121483  
state_cost_43123481   <- choice_data$state_cost_43123481  
state_cost_43124480   <- choice_data$state_cost_43124480  
state_cost_43125484   <- choice_data$state_cost_43125484  
state_cost_43126479   <- choice_data$state_cost_43126479  
state_cost_43127478   <- choice_data$state_cost_43127478  
state_cost_43128482   <- choice_data$state_cost_43128482  
state_cost_43136446   <- choice_data$state_cost_43136446  
state_cost_43137443   <- choice_data$state_cost_43137443  
state_cost_43138448   <- choice_data$state_cost_43138448  
state_cost_43145469   <- choice_data$state_cost_43145469  
state_cost_43156402   <- choice_data$state_cost_43156402  
state_cost_43197488   <- choice_data$state_cost_43197488  
state_cost_43205458   <- choice_data$state_cost_43205458  
state_cost_43209415   <- choice_data$state_cost_43209415  
state_cost_43984204   <- choice_data$state_cost_43984204  
state_cost_43986205   <- choice_data$state_cost_43986205  
state_cost_44217554   <- choice_data$state_cost_44217554  
state_cost_44258701   <- choice_data$state_cost_44258701  
state_cost_47161262   <- choice_data$state_cost_47161262  
state_cost_47171261   <- choice_data$state_cost_47171261  
state_cost_48661746   <- choice_data$state_cost_48661746  
state_cost_48671745   <- choice_data$state_cost_48671745  
state_cost_48681749   <- choice_data$state_cost_48681749  
state_cost_49361993   <- choice_data$state_cost_49361993  
state_cost_50062247   <- choice_data$state_cost_50062247  
state_cost_50362273   <- choice_data$state_cost_50362273  
state_cost_50372272   <- choice_data$state_cost_50372272  
state_cost_52882750   <- choice_data$state_cost_52882750  
state_cost_52892748   <- choice_data$state_cost_52892748  
state_cost_52902749   <- choice_data$state_cost_52902749  
state_cost_53262930   <- choice_data$state_cost_53262930  
state_cost_53272929   <- choice_data$state_cost_53272929  
state_cost_53462949   <- choice_data$state_cost_53462949  
state_cost_53472952   <- choice_data$state_cost_53472952  
state_cost_53492951   <- choice_data$state_cost_53492951  
state_cost_54056332   <- choice_data$state_cost_54056332  
state_cost_54057331   <- choice_data$state_cost_54057331  
state_cost_54646335   <- choice_data$state_cost_54646335  
state_cost_54647334   <- choice_data$state_cost_54647334  
state_cost_54863315   <- choice_data$state_cost_54863315  
state_cost_55246362   <- choice_data$state_cost_55246362  
state_cost_55963447   <- choice_data$state_cost_55963447  
state_cost_56063448   <- choice_data$state_cost_56063448  
state_cost_56828417   <- choice_data$state_cost_56828417  
state_cost_56840464   <- choice_data$state_cost_56840464  
state_cost_56843466   <- choice_data$state_cost_56843466  
state_cost_56846465   <- choice_data$state_cost_56846465  
state_cost_56850463   <- choice_data$state_cost_56850463  
state_cost_56957542   <- choice_data$state_cost_56957542  
state_cost_56958543   <- choice_data$state_cost_56958543  
state_cost_57051553   <- choice_data$state_cost_57051553  
state_cost_57092420   <- choice_data$state_cost_57092420  
state_cost_64336401   <- choice_data$state_cost_64336401  
state_cost_68022508   <- choice_data$state_cost_68022508  
state_cost_68036500   <- choice_data$state_cost_68036500  
state_cost_68037498   <- choice_data$state_cost_68037498  
state_cost_68038502   <- choice_data$state_cost_68038502  
state_cost_68039501   <- choice_data$state_cost_68039501  
state_cost_68049509   <- choice_data$state_cost_68049509  
state_cost_69971349   <- choice_data$state_cost_69971349  
state_cost_69981351   <- choice_data$state_cost_69981351  
state_cost_71886256   <- choice_data$state_cost_71886256  
state_cost_72816991   <- choice_data$state_cost_72816991  
state_cost_79026343   <- choice_data$state_cost_79026343  
state_cost_80571355   <- choice_data$state_cost_80571355  
state_cost_80574352   <- choice_data$state_cost_80574352  
state_cost_80576351   <- choice_data$state_cost_80576351  
state_cost_80577350   <- choice_data$state_cost_80577350  
state_cost_80578354   <- choice_data$state_cost_80578354  
state_cost_81966346   <- choice_data$state_cost_81966346  
state_cost_82082628   <- choice_data$state_cost_82082628  
state_cost_82126349   <- choice_data$state_cost_82126349  
state_cost_82127348   <- choice_data$state_cost_82127348  
state_cost_82147356   <- choice_data$state_cost_82147356  
state_cost_82187359   <- choice_data$state_cost_82187359  
state_cost_88253245   <- choice_data$state_cost_88253245  
state_cost_88263244   <- choice_data$state_cost_88263244  
state_cost_88273241   <- choice_data$state_cost_88273241  
state_cost_88283243   <- choice_data$state_cost_88283243  
state_cost_89139984   <- choice_data$state_cost_89139984  
state_cost_92783722   <- choice_data$state_cost_92783722  
state_cost_94263877   <- choice_data$state_cost_94263877  
state_cost_94283878   <- choice_data$state_cost_94283878  
state_cost_100064432  <- choice_data$state_cost_100064432 
state_cost_100084433  <- choice_data$state_cost_100084433 
state_cost_100094431  <- choice_data$state_cost_100094431 
state_cost_102785180  <- choice_data$state_cost_102785180 
state_cost_106251054  <- choice_data$state_cost_106251054 
state_cost_106261050  <- choice_data$state_cost_106261050 
state_cost_106271048  <- choice_data$state_cost_106271048 
state_cost_106281053  <- choice_data$state_cost_106281053 
state_cost_108051512  <- choice_data$state_cost_108051512 
state_cost_108071511  <- choice_data$state_cost_108071511 
state_cost_108081510  <- choice_data$state_cost_108081510 
state_cost_108181507  <- choice_data$state_cost_108181507 
state_cost_108361481  <- choice_data$state_cost_108361481 
state_cost_108902109  <- choice_data$state_cost_108902109 
state_cost_112901489  <- choice_data$state_cost_112901489 
state_cost_112931487  <- choice_data$state_cost_112931487 
state_cost_112941486  <- choice_data$state_cost_112941486 
state_cost_112961476  <- choice_data$state_cost_112961476 
state_cost_112971485  <- choice_data$state_cost_112971485 
state_cost_112981488  <- choice_data$state_cost_112981488 
state_cost_113464508  <- choice_data$state_cost_113464508 
state_cost_113474507  <- choice_data$state_cost_113474507 
state_cost_113484509  <- choice_data$state_cost_113484509 
state_cost_113661518  <- choice_data$state_cost_113661518 
state_cost_119361068  <- choice_data$state_cost_119361068 
state_cost_124041066  <- choice_data$state_cost_124041066 
state_cost_124061065  <- choice_data$state_cost_124061065 
state_cost_124071064  <- choice_data$state_cost_124071064 
state_cost_124081067  <- choice_data$state_cost_124081067 
state_cost_124641076  <- choice_data$state_cost_124641076 
state_cost_124661075  <- choice_data$state_cost_124661075 
state_cost_124671074  <- choice_data$state_cost_124671074 
state_cost_124761073  <- choice_data$state_cost_124761073 
state_cost_124781072  <- choice_data$state_cost_124781072 
state_cost_124791071  <- choice_data$state_cost_124791071 
state_cost_126681083  <- choice_data$state_cost_126681083 
state_cost_128564285  <- choice_data$state_cost_128564285 
state_cost_128884281  <- choice_data$state_cost_128884281 
state_cost_130361080  <- choice_data$state_cost_130361080 
state_cost_130371079  <- choice_data$state_cost_130371079 
state_cost_130381081  <- choice_data$state_cost_130381081 
state_cost_133883651  <- choice_data$state_cost_133883651 
state_cost_136383284  <- choice_data$state_cost_136383284 
state_cost_141923976  <- choice_data$state_cost_141923976 
state_cost_141993977  <- choice_data$state_cost_141993977 
state_cost_152465358  <- choice_data$state_cost_152465358 
state_cost_152475357  <- choice_data$state_cost_152475357 
state_cost_152485359  <- choice_data$state_cost_152485359 
state_cost_152965362  <- choice_data$state_cost_152965362 
state_cost_156262825  <- choice_data$state_cost_156262825 
state_cost_156272824  <- choice_data$state_cost_156272824 
state_cost_156282827  <- choice_data$state_cost_156282827 
state_cost_156442826  <- choice_data$state_cost_156442826 
state_cost_156774143  <- choice_data$state_cost_156774143 
state_cost_159405139  <- choice_data$state_cost_159405139 
state_cost_172061186  <- choice_data$state_cost_172061186 
state_cost_178261887  <- choice_data$state_cost_178261887 
state_cost_178301888  <- choice_data$state_cost_178301888 
state_cost_179161934  <- choice_data$state_cost_179161934 
state_cost_179561971  <- choice_data$state_cost_179561971 
state_cost_179581973  <- choice_data$state_cost_179581973 
state_cost_181961984  <- choice_data$state_cost_181961984 
state_cost_181981986  <- choice_data$state_cost_181981986 
state_cost_184072158  <- choice_data$state_cost_184072158 
state_cost_184082159  <- choice_data$state_cost_184082159 
state_cost_190262884  <- choice_data$state_cost_190262884 
state_cost_190612905  <- choice_data$state_cost_190612905 
state_cost_190632876  <- choice_data$state_cost_190632876 
state_cost_190642874  <- choice_data$state_cost_190642874 
state_cost_190662873  <- choice_data$state_cost_190662873 
state_cost_190672872  <- choice_data$state_cost_190672872 
state_cost_190682877  <- choice_data$state_cost_190682877 
state_cost_190822906  <- choice_data$state_cost_190822906 
state_cost_190962911  <- choice_data$state_cost_190962911 
state_cost_191122890  <- choice_data$state_cost_191122890 
state_cost_192263131  <- choice_data$state_cost_192263131 
state_cost_194763335  <- choice_data$state_cost_194763335 
state_cost_194773333  <- choice_data$state_cost_194773333 
state_cost_194783336  <- choice_data$state_cost_194783336 
state_cost_194863332  <- choice_data$state_cost_194863332 
state_cost_202463689  <- choice_data$state_cost_202463689 
state_cost_202473688  <- choice_data$state_cost_202473688 
state_cost_202483690  <- choice_data$state_cost_202483690 
state_cost_202863693  <- choice_data$state_cost_202863693 
state_cost_215954944  <- choice_data$state_cost_215954944 
state_cost_215964941  <- choice_data$state_cost_215964941 
state_cost_215974940  <- choice_data$state_cost_215974940 
state_cost_215984943  <- choice_data$state_cost_215984943 
state_cost_221215325  <- choice_data$state_cost_221215325 
state_cost_221555322  <- choice_data$state_cost_221555322 
state_cost_221565319  <- choice_data$state_cost_221565319 
state_cost_221575318  <- choice_data$state_cost_221575318 
state_cost_222135384  <- choice_data$state_cost_222135384 
state_cost_222165385  <- choice_data$state_cost_222165385 
state_cost_232771028  <- choice_data$state_cost_232771028 
state_cost_232781030  <- choice_data$state_cost_232781030 
state_cost_236262086  <- choice_data$state_cost_236262086 
state_cost_237082092  <- choice_data$state_cost_237082092 
state_cost_238232083  <- choice_data$state_cost_238232083 
state_cost_238242082  <- choice_data$state_cost_238242082 
state_cost_238262081  <- choice_data$state_cost_238262081 
state_cost_238272080  <- choice_data$state_cost_238272080 
state_cost_238282084  <- choice_data$state_cost_238282084 
state_cost_241562463  <- choice_data$state_cost_241562463 
state_cost_241572462  <- choice_data$state_cost_241572462 
state_cost_241582464  <- choice_data$state_cost_241582464 
state_cost_244543085  <- choice_data$state_cost_244543085 
state_cost_244563084  <- choice_data$state_cost_244563084 
state_cost_244573083  <- choice_data$state_cost_244573083 
state_cost_244583087  <- choice_data$state_cost_244583087 
state_cost_244663089  <- choice_data$state_cost_244663089 
state_cost_247063452  <- choice_data$state_cost_247063452 
state_cost_247283453  <- choice_data$state_cost_247283453 
state_cost_256034475  <- choice_data$state_cost_256034475 
state_cost_256044474  <- choice_data$state_cost_256044474 
state_cost_256064473  <- choice_data$state_cost_256064473 
state_cost_256074472  <- choice_data$state_cost_256074472 
state_cost_256084476  <- choice_data$state_cost_256084476 
state_cost_256164478  <- choice_data$state_cost_256164478 
state_cost_258764867  <- choice_data$state_cost_258764867 
state_cost_258774866  <- choice_data$state_cost_258774866 
state_cost_258784869  <- choice_data$state_cost_258784869 
state_cost_265862175  <- choice_data$state_cost_265862175 
state_cost_266562181  <- choice_data$state_cost_266562181 
state_cost_268202791  <- choice_data$state_cost_268202791 
state_cost_268212792  <- choice_data$state_cost_268212792 
state_cost_268222784  <- choice_data$state_cost_268222784 
state_cost_268232789  <- choice_data$state_cost_268232789 
state_cost_268242793  <- choice_data$state_cost_268242793 
state_cost_268262788  <- choice_data$state_cost_268262788 
state_cost_268272787  <- choice_data$state_cost_268272787 
state_cost_268282790  <- choice_data$state_cost_268282790 
state_cost_269062795  <- choice_data$state_cost_269062795 
state_cost_270271374  <- choice_data$state_cost_270271374 
state_cost_270562907  <- choice_data$state_cost_270562907 
state_cost_271024938  <- choice_data$state_cost_271024938 
state_cost_273922903  <- choice_data$state_cost_273922903 
state_cost_274102893  <- choice_data$state_cost_274102893 
state_cost_274541302  <- choice_data$state_cost_274541302 
state_cost_274791372  <- choice_data$state_cost_274791372 
state_cost_275444212  <- choice_data$state_cost_275444212 
state_cost_275544501  <- choice_data$state_cost_275544501 
state_cost_275624505  <- choice_data$state_cost_275624505 
state_cost_275852626  <- choice_data$state_cost_275852626 
state_cost_286252492  <- choice_data$state_cost_286252492 
state_cost_287184070  <- choice_data$state_cost_287184070 
state_cost_288664915  <- choice_data$state_cost_288664915 
state_cost_288674914  <- choice_data$state_cost_288674914 
state_cost_288684918  <- choice_data$state_cost_288684918 
state_cost_288904925  <- choice_data$state_cost_288904925 
state_cost_297281031  <- choice_data$state_cost_297281031 
state_cost_299932071  <- choice_data$state_cost_299932071 
state_cost_299942070  <- choice_data$state_cost_299942070 
state_cost_299962069  <- choice_data$state_cost_299962069 
state_cost_299972068  <- choice_data$state_cost_299972068 
state_cost_299982072  <- choice_data$state_cost_299982072 
state_cost_300362067  <- choice_data$state_cost_300362067 
state_cost_300562090  <- choice_data$state_cost_300562090 
state_cost_300572089  <- choice_data$state_cost_300572089 
state_cost_300582091  <- choice_data$state_cost_300582091 
state_cost_302362196  <- choice_data$state_cost_302362196 
state_cost_302382197  <- choice_data$state_cost_302382197 
state_cost_303162321  <- choice_data$state_cost_303162321 
state_cost_303172320  <- choice_data$state_cost_303172320 
state_cost_303182322  <- choice_data$state_cost_303182322 
state_cost_305262468  <- choice_data$state_cost_305262468 
state_cost_305272467  <- choice_data$state_cost_305272467 
state_cost_305282469  <- choice_data$state_cost_305282469 
state_cost_311673458  <- choice_data$state_cost_311673458 
state_cost_312083459  <- choice_data$state_cost_312083459 
state_cost_314723602  <- choice_data$state_cost_314723602 
state_cost_314733605  <- choice_data$state_cost_314733605 
state_cost_314743603  <- choice_data$state_cost_314743603 
state_cost_314753601  <- choice_data$state_cost_314753601 
state_cost_316543821  <- choice_data$state_cost_316543821 
state_cost_316563820  <- choice_data$state_cost_316563820 
state_cost_316573819  <- choice_data$state_cost_316573819 
state_cost_316583822  <- choice_data$state_cost_316583822 
state_cost_316663823  <- choice_data$state_cost_316663823 
state_cost_317183995  <- choice_data$state_cost_317183995 
state_cost_317193994  <- choice_data$state_cost_317193994 
state_cost_322314488  <- choice_data$state_cost_322314488 
state_cost_322324485  <- choice_data$state_cost_322324485 
state_cost_322334486  <- choice_data$state_cost_322334486 
state_cost_322344484  <- choice_data$state_cost_322344484 
state_cost_322354489  <- choice_data$state_cost_322354489 
state_cost_322364483  <- choice_data$state_cost_322364483 
state_cost_322374482  <- choice_data$state_cost_322374482 
state_cost_322384487  <- choice_data$state_cost_322384487 
state_cost_332564494  <- choice_data$state_cost_332564494 
state_cost_336634891  <- choice_data$state_cost_336634891 
state_cost_337163842  <- choice_data$state_cost_337163842 
state_cost_337173841  <- choice_data$state_cost_337173841 
state_cost_341641324  <- choice_data$state_cost_341641324 
state_cost_341971322  <- choice_data$state_cost_341971322 
state_cost_341981344  <- choice_data$state_cost_341981344 
state_cost_343592414  <- choice_data$state_cost_343592414 
state_cost_343662146  <- choice_data$state_cost_343662146 
state_cost_343682147  <- choice_data$state_cost_343682147 
state_cost_344222417  <- choice_data$state_cost_344222417 
state_cost_344232415  <- choice_data$state_cost_344232415 
state_cost_344252418  <- choice_data$state_cost_344252418 
state_cost_344332416  <- choice_data$state_cost_344332416 
state_cost_344493093  <- choice_data$state_cost_344493093 
state_cost_344563096  <- choice_data$state_cost_344563096 
state_cost_344573094  <- choice_data$state_cost_344573094 
state_cost_344583097  <- choice_data$state_cost_344583097 
state_cost_345463959  <- choice_data$state_cost_345463959 
state_cost_345663961  <- choice_data$state_cost_345663961 
state_cost_345784107  <- choice_data$state_cost_345784107 
state_cost_345794105  <- choice_data$state_cost_345794105 
state_cost_346904740  <- choice_data$state_cost_346904740 
state_cost_347464848  <- choice_data$state_cost_347464848 
state_cost_347474844  <- choice_data$state_cost_347474844 
state_cost_347484849  <- choice_data$state_cost_347484849 
state_cost_347864825  <- choice_data$state_cost_347864825 
state_cost_348174871  <- choice_data$state_cost_348174871 
state_cost_348204895  <- choice_data$state_cost_348204895 
state_cost_348214898  <- choice_data$state_cost_348214898 
state_cost_348234877  <- choice_data$state_cost_348234877 
state_cost_348564927  <- choice_data$state_cost_348564927 
state_cost_348714847  <- choice_data$state_cost_348714847 
state_cost_348814855  <- choice_data$state_cost_348814855 
state_cost_349195043  <- choice_data$state_cost_349195043 
state_cost_349355042  <- choice_data$state_cost_349355042 
state_cost_349725007  <- choice_data$state_cost_349725007 
state_cost_349955017  <- choice_data$state_cost_349955017 
state_cost_352134741  <- choice_data$state_cost_352134741 
state_cost_356264111  <- choice_data$state_cost_356264111 
state_cost_356284112  <- choice_data$state_cost_356284112 
state_cost_356993944  <- choice_data$state_cost_356993944 
state_cost_357804051  <- choice_data$state_cost_357804051 
state_cost_359132078  <- choice_data$state_cost_359132078 
state_cost_359142077  <- choice_data$state_cost_359142077 
state_cost_359162076  <- choice_data$state_cost_359162076 
state_cost_359172075  <- choice_data$state_cost_359172075 
state_cost_359182079  <- choice_data$state_cost_359182079 
state_cost_359262073  <- choice_data$state_cost_359262073 
state_cost_359442095  <- choice_data$state_cost_359442095 
state_cost_359462094  <- choice_data$state_cost_359462094 
state_cost_359472093  <- choice_data$state_cost_359472093 
state_cost_359482096  <- choice_data$state_cost_359482096 
state_cost_359645019  <- choice_data$state_cost_359645019 
state_cost_361862324  <- choice_data$state_cost_361862324 
state_cost_361882325  <- choice_data$state_cost_361882325 
state_cost_363012478  <- choice_data$state_cost_363012478 
state_cost_363042475  <- choice_data$state_cost_363042475 
state_cost_363052474  <- choice_data$state_cost_363052474 
state_cost_363072473  <- choice_data$state_cost_363072473 
state_cost_363082477  <- choice_data$state_cost_363082477 
state_cost_364472674  <- choice_data$state_cost_364472674 
state_cost_366673145  <- choice_data$state_cost_366673145 
state_cost_366683146  <- choice_data$state_cost_366683146 
state_cost_368711323  <- choice_data$state_cost_368711323 
state_cost_368863465  <- choice_data$state_cost_368863465 
state_cost_368873467  <- choice_data$state_cost_368873467 
state_cost_369013468  <- choice_data$state_cost_369013468 
state_cost_369033466  <- choice_data$state_cost_369033466 
state_cost_369043469  <- choice_data$state_cost_369043469 
state_cost_369073471  <- choice_data$state_cost_369073471 
state_cost_369083470  <- choice_data$state_cost_369083470 
state_cost_369683635  <- choice_data$state_cost_369683635 
state_cost_369693633  <- choice_data$state_cost_369693633 
state_cost_369713637  <- choice_data$state_cost_369713637 
state_cost_369743645  <- choice_data$state_cost_369743645 
state_cost_369763644  <- choice_data$state_cost_369763644 
state_cost_369783647  <- choice_data$state_cost_369783647 
state_cost_371463754  <- choice_data$state_cost_371463754 
state_cost_372173850  <- choice_data$state_cost_372173850 
state_cost_373365237  <- choice_data$state_cost_373365237 
state_cost_373385241  <- choice_data$state_cost_373385241 
state_cost_373395239  <- choice_data$state_cost_373395239 
state_cost_373464016  <- choice_data$state_cost_373464016 
state_cost_373474014  <- choice_data$state_cost_373474014 
state_cost_373484017  <- choice_data$state_cost_373484017 
state_cost_373564018  <- choice_data$state_cost_373564018 
state_cost_374134136  <- choice_data$state_cost_374134136 
state_cost_374144135  <- choice_data$state_cost_374144135 
state_cost_374164134  <- choice_data$state_cost_374164134 
state_cost_374174133  <- choice_data$state_cost_374174133 
state_cost_374184137  <- choice_data$state_cost_374184137 
state_cost_374264138  <- choice_data$state_cost_374264138 
state_cost_376404291  <- choice_data$state_cost_376404291 
state_cost_376424292  <- choice_data$state_cost_376424292 
state_cost_378864512  <- choice_data$state_cost_378864512 
state_cost_378884513  <- choice_data$state_cost_378884513 
state_cost_379344565  <- choice_data$state_cost_379344565 
state_cost_379354567  <- choice_data$state_cost_379354567 
state_cost_379364564  <- choice_data$state_cost_379364564 
state_cost_379374563  <- choice_data$state_cost_379374563 
state_cost_379384566  <- choice_data$state_cost_379384566 
state_cost_379864600  <- choice_data$state_cost_379864600 
state_cost_379874598  <- choice_data$state_cost_379874598 
state_cost_379884601  <- choice_data$state_cost_379884601 
state_cost_379914710  <- choice_data$state_cost_379914710 
state_cost_379934708  <- choice_data$state_cost_379934708 
state_cost_379944707  <- choice_data$state_cost_379944707 
state_cost_379964706  <- choice_data$state_cost_379964706 
state_cost_379974705  <- choice_data$state_cost_379974705 
state_cost_379984709  <- choice_data$state_cost_379984709 
state_cost_380064712  <- choice_data$state_cost_380064712 
state_cost_380084711  <- choice_data$state_cost_380084711 
state_cost_380884122  <- choice_data$state_cost_380884122 
state_cost_381765067  <- choice_data$state_cost_381765067 
state_cost_381775064  <- choice_data$state_cost_381775064 
state_cost_381785066  <- choice_data$state_cost_381785066 
state_cost_398664701  <- choice_data$state_cost_398664701 
state_cost_401181854  <- choice_data$state_cost_401181854 
state_cost_401933622  <- choice_data$state_cost_401933622 
state_cost_402973593  <- choice_data$state_cost_402973593 
state_cost_403133619  <- choice_data$state_cost_403133619 
state_cost_403945197  <- choice_data$state_cost_403945197 
state_cost_404115227  <- choice_data$state_cost_404115227 
state_cost_405923616  <- choice_data$state_cost_405923616 
state_cost_405933617  <- choice_data$state_cost_405933617 
state_cost_405943615  <- choice_data$state_cost_405943615 
state_cost_405983628  <- choice_data$state_cost_405983628 
state_cost_405993626  <- choice_data$state_cost_405993626 
state_cost_406044636  <- choice_data$state_cost_406044636 
state_cost_406144352  <- choice_data$state_cost_406144352 
state_cost_406255200  <- choice_data$state_cost_406255200 
state_cost_406325242  <- choice_data$state_cost_406325242 
state_cost_409264670  <- choice_data$state_cost_409264670 
state_cost_409304631  <- choice_data$state_cost_409304631 
state_cost_409464717  <- choice_data$state_cost_409464717 
state_cost_409564691  <- choice_data$state_cost_409564691 
state_cost_409654718  <- choice_data$state_cost_409654718 
state_cost_409764615  <- choice_data$state_cost_409764615 
state_cost_410762864  <- choice_data$state_cost_410762864 
state_cost_410772862  <- choice_data$state_cost_410772862 
state_cost_410782865  <- choice_data$state_cost_410782865 
state_cost_412092062  <- choice_data$state_cost_412092062 
state_cost_413605215  <- choice_data$state_cost_413605215 
state_cost_415002465  <- choice_data$state_cost_415002465 
state_cost_415924639  <- choice_data$state_cost_415924639 
state_cost_416015183  <- choice_data$state_cost_416015183 
state_cost_416045206  <- choice_data$state_cost_416045206 
state_cost_416255207  <- choice_data$state_cost_416255207 
state_cost_416535202  <- choice_data$state_cost_416535202 
state_cost_416815221  <- choice_data$state_cost_416815221 
state_cost_416885191  <- choice_data$state_cost_416885191 
state_cost_416925185  <- choice_data$state_cost_416925185 
state_cost_416935184  <- choice_data$state_cost_416935184 
state_cost_416945182  <- choice_data$state_cost_416945182 
state_cost_416965209  <- choice_data$state_cost_416965209 
state_cost_416985210  <- choice_data$state_cost_416985210 
state_cost_417015211  <- choice_data$state_cost_417015211 
state_cost_417045220  <- choice_data$state_cost_417045220 
state_cost_417055219  <- choice_data$state_cost_417055219 
state_cost_417073998  <- choice_data$state_cost_417073998 
state_cost_417154626  <- choice_data$state_cost_417154626 
state_cost_417194633  <- choice_data$state_cost_417194633 
state_cost_417464623  <- choice_data$state_cost_417464623 
state_cost_417474622  <- choice_data$state_cost_417474622 
state_cost_417835190  <- choice_data$state_cost_417835190 
state_cost_417964664  <- choice_data$state_cost_417964664 
state_cost_417974663  <- choice_data$state_cost_417974663 
state_cost_418064700  <- choice_data$state_cost_418064700 
state_cost_418461979  <- choice_data$state_cost_418461979 
state_cost_419895193  <- choice_data$state_cost_419895193 
state_cost_420025194  <- choice_data$state_cost_420025194 
state_cost_420095196  <- choice_data$state_cost_420095196 
state_cost_420795195  <- choice_data$state_cost_420795195 
state_cost_421663588  <- choice_data$state_cost_421663588 
state_cost_421673587  <- choice_data$state_cost_421673587 
state_cost_423123372  <- choice_data$state_cost_423123372 
state_cost_424444531  <- choice_data$state_cost_424444531 
state_cost_426663583  <- choice_data$state_cost_426663583 
state_cost_426763366  <- choice_data$state_cost_426763366 
state_cost_426873343  <- choice_data$state_cost_426873343 
state_cost_426993362  <- choice_data$state_cost_426993362 
state_cost_427033371  <- choice_data$state_cost_427033371 
state_cost_427123353  <- choice_data$state_cost_427123353 
state_cost_427163350  <- choice_data$state_cost_427163350 
state_cost_427173349  <- choice_data$state_cost_427173349 
state_cost_427183352  <- choice_data$state_cost_427183352 
state_cost_429845112  <- choice_data$state_cost_429845112 
state_cost_432191119  <- choice_data$state_cost_432191119 
state_cost_432441094  <- choice_data$state_cost_432441094 
state_cost_432821121  <- choice_data$state_cost_432821121 
state_cost_432851114  <- choice_data$state_cost_432851114 
state_cost_432871118  <- choice_data$state_cost_432871118 
state_cost_432961130  <- choice_data$state_cost_432961130 
state_cost_433021096  <- choice_data$state_cost_433021096 
state_cost_433161127  <- choice_data$state_cost_433161127 
state_cost_433281141  <- choice_data$state_cost_433281141 
state_cost_433301142  <- choice_data$state_cost_433301142 
state_cost_433311136  <- choice_data$state_cost_433311136 
state_cost_433331137  <- choice_data$state_cost_433331137 
state_cost_433341134  <- choice_data$state_cost_433341134 
state_cost_433351138  <- choice_data$state_cost_433351138 
state_cost_433361110  <- choice_data$state_cost_433361110 
state_cost_433371133  <- choice_data$state_cost_433371133 
state_cost_433381135  <- choice_data$state_cost_433381135 
state_cost_433861169  <- choice_data$state_cost_433861169 
state_cost_433871168  <- choice_data$state_cost_433871168 
state_cost_433881170  <- choice_data$state_cost_433881170 
state_cost_434091115  <- choice_data$state_cost_434091115 
state_cost_434101116  <- choice_data$state_cost_434101116 
state_cost_434151107  <- choice_data$state_cost_434151107 
state_cost_434181139  <- choice_data$state_cost_434181139 
state_cost_434261790  <- choice_data$state_cost_434261790 
state_cost_438484334  <- choice_data$state_cost_438484334 
state_cost_443501003  <- choice_data$state_cost_443501003 
state_cost_444191536  <- choice_data$state_cost_444191536 
state_cost_444561561  <- choice_data$state_cost_444561561 
state_cost_444761542  <- choice_data$state_cost_444761542 
state_cost_444991554  <- choice_data$state_cost_444991554 
state_cost_445161553  <- choice_data$state_cost_445161553 
state_cost_445201565  <- choice_data$state_cost_445201565 
state_cost_446032066  <- choice_data$state_cost_446032066 
state_cost_446573204  <- choice_data$state_cost_446573204 
state_cost_446583206  <- choice_data$state_cost_446583206 
state_cost_452453828  <- choice_data$state_cost_452453828 
state_cost_452463826  <- choice_data$state_cost_452463826 
state_cost_452473825  <- choice_data$state_cost_452473825 
state_cost_452483827  <- choice_data$state_cost_452483827 
state_cost_452753858  <- choice_data$state_cost_452753858 
state_cost_452763855  <- choice_data$state_cost_452763855 
state_cost_452773854  <- choice_data$state_cost_452773854 
state_cost_452783857  <- choice_data$state_cost_452783857 
state_cost_454184020  <- choice_data$state_cost_454184020 
state_cost_454194019  <- choice_data$state_cost_454194019 
state_cost_458844377  <- choice_data$state_cost_458844377 
state_cost_458864376  <- choice_data$state_cost_458864376 
state_cost_458874375  <- choice_data$state_cost_458874375 
state_cost_458884378  <- choice_data$state_cost_458884378 
state_cost_463502472  <- choice_data$state_cost_463502472 
state_cost_463512471  <- choice_data$state_cost_463512471 
state_cost_465043156  <- choice_data$state_cost_465043156 
state_cost_465063157  <- choice_data$state_cost_465063157 
state_cost_477851437  <- choice_data$state_cost_477851437 
state_cost_477861438  <- choice_data$state_cost_477861438 
state_cost_480992500  <- choice_data$state_cost_480992500 
state_cost_481022508  <- choice_data$state_cost_481022508 
state_cost_481032505  <- choice_data$state_cost_481032505 
state_cost_481042504  <- choice_data$state_cost_481042504 
state_cost_481052499  <- choice_data$state_cost_481052499 
state_cost_481062503  <- choice_data$state_cost_481062503 
state_cost_490864235  <- choice_data$state_cost_490864235 
state_cost_491854246  <- choice_data$state_cost_491854246 
state_cost_491864248  <- choice_data$state_cost_491864248 
state_cost_491894249  <- choice_data$state_cost_491894249 
state_cost_506864806  <- choice_data$state_cost_506864806 
state_cost_516964816  <- choice_data$state_cost_516964816 
state_cost_521461289  <- choice_data$state_cost_521461289 
state_cost_521861292  <- choice_data$state_cost_521861292 
state_cost_523121283  <- choice_data$state_cost_523121283 
state_cost_523141284  <- choice_data$state_cost_523141284 
state_cost_523161282  <- choice_data$state_cost_523161282 
state_cost_523171287  <- choice_data$state_cost_523171287 
state_cost_523181285  <- choice_data$state_cost_523181285 
state_cost_523191286  <- choice_data$state_cost_523191286 
state_cost_525631877  <- choice_data$state_cost_525631877 
state_cost_525801874  <- choice_data$state_cost_525801874 
state_cost_525811875  <- choice_data$state_cost_525811875 
state_cost_525821873  <- choice_data$state_cost_525821873 
state_cost_525931865  <- choice_data$state_cost_525931865 
state_cost_525941866  <- choice_data$state_cost_525941866 
state_cost_525951870  <- choice_data$state_cost_525951870 
state_cost_525961862  <- choice_data$state_cost_525961862 
state_cost_525971869  <- choice_data$state_cost_525971869 
state_cost_525981863  <- choice_data$state_cost_525981863 
state_cost_525991868  <- choice_data$state_cost_525991868 
state_cost_528063140  <- choice_data$state_cost_528063140 
state_cost_528353142  <- choice_data$state_cost_528353142 
state_cost_532103914  <- choice_data$state_cost_532103914 
state_cost_532113924  <- choice_data$state_cost_532113924 
state_cost_532133912  <- choice_data$state_cost_532133912 
state_cost_532143913  <- choice_data$state_cost_532143913 
state_cost_532163911  <- choice_data$state_cost_532163911 
state_cost_532183915  <- choice_data$state_cost_532183915 
state_cost_544363564  <- choice_data$state_cost_544363564 
state_cost_544463799  <- choice_data$state_cost_544463799 
state_cost_544473798  <- choice_data$state_cost_544473798 
state_cost_544483801  <- choice_data$state_cost_544483801 
state_cost_547061635  <- choice_data$state_cost_547061635 
state_cost_550663565  <- choice_data$state_cost_550663565 
state_cost_550683566  <- choice_data$state_cost_550683566 
state_cost_550843804  <- choice_data$state_cost_550843804 
state_cost_550863803  <- choice_data$state_cost_550863803 
state_cost_550873802  <- choice_data$state_cost_550873802 
state_cost_550883805  <- choice_data$state_cost_550883805 
state_cost_551063991  <- choice_data$state_cost_551063991 
state_cost_555063810  <- choice_data$state_cost_555063810 
state_cost_561953918  <- choice_data$state_cost_561953918 
state_cost_561963916  <- choice_data$state_cost_561963916 
state_cost_562063832  <- choice_data$state_cost_562063832 
state_cost_571201242  <- choice_data$state_cost_571201242 
state_cost_571251241  <- choice_data$state_cost_571251241 
state_cost_571291244  <- choice_data$state_cost_571291244 
state_cost_571441252  <- choice_data$state_cost_571441252 
state_cost_571481243  <- choice_data$state_cost_571481243 
state_cost_571571246  <- choice_data$state_cost_571571246 
state_cost_571581251  <- choice_data$state_cost_571581251 
state_cost_571611240  <- choice_data$state_cost_571611240 
state_cost_571741250  <- choice_data$state_cost_571741250 
state_cost_588011108  <- choice_data$state_cost_588011108 
state_cost_588352963  <- choice_data$state_cost_588352963 
state_cost_588362961  <- choice_data$state_cost_588362961 
state_cost_588382962  <- choice_data$state_cost_588382962 
state_cost_588402957  <- choice_data$state_cost_588402957 
state_cost_588422975  <- choice_data$state_cost_588422975 
state_cost_588602998  <- choice_data$state_cost_588602998 
state_cost_588682969  <- choice_data$state_cost_588682969 
state_cost_588722960  <- choice_data$state_cost_588722960 
state_cost_588732971  <- choice_data$state_cost_588732971 
state_cost_588752994  <- choice_data$state_cost_588752994 
state_cost_588762993  <- choice_data$state_cost_588762993 
state_cost_588852997  <- choice_data$state_cost_588852997 
state_cost_588862996  <- choice_data$state_cost_588862996 
state_cost_590371737  <- choice_data$state_cost_590371737 
state_cost_591001960  <- choice_data$state_cost_591001960 
state_cost_592312659  <- choice_data$state_cost_592312659 
state_cost_620613048  <- choice_data$state_cost_620613048 
state_cost_620973829  <- choice_data$state_cost_620973829 
state_cost_624003402  <- choice_data$state_cost_624003402 
state_cost_624053409  <- choice_data$state_cost_624053409 
state_cost_624203414  <- choice_data$state_cost_624203414 
state_cost_633554549  <- choice_data$state_cost_633554549 
state_cost_633574553  <- choice_data$state_cost_633574553 
state_cost_633594561  <- choice_data$state_cost_633594561 
state_cost_635284382  <- choice_data$state_cost_635284382 
state_cost_635304384  <- choice_data$state_cost_635304384 
state_cost_637554961  <- choice_data$state_cost_637554961 
state_cost_639595188  <- choice_data$state_cost_639595188 
state_cost_639635214  <- choice_data$state_cost_639635214 
state_cost_641361756  <- choice_data$state_cost_641361756 
state_cost_645731333  <- choice_data$state_cost_645731333 
state_cost_646011145  <- choice_data$state_cost_646011145 
state_cost_646761226  <- choice_data$state_cost_646761226 
state_cost_647111326  <- choice_data$state_cost_647111326 
state_cost_647121331  <- choice_data$state_cost_647121331 
state_cost_647161327  <- choice_data$state_cost_647161327 
state_cost_647361348  <- choice_data$state_cost_647361348 
state_cost_647551338  <- choice_data$state_cost_647551338 
state_cost_647591339  <- choice_data$state_cost_647591339 
state_cost_647761385  <- choice_data$state_cost_647761385 
state_cost_648582044  <- choice_data$state_cost_648582044 
state_cost_648622040  <- choice_data$state_cost_648622040 
state_cost_648632042  <- choice_data$state_cost_648632042 
state_cost_648642046  <- choice_data$state_cost_648642046 
state_cost_648652045  <- choice_data$state_cost_648652045 
state_cost_648662041  <- choice_data$state_cost_648662041 
state_cost_648672039  <- choice_data$state_cost_648672039 
state_cost_648682043  <- choice_data$state_cost_648682043 
state_cost_648702047  <- choice_data$state_cost_648702047 
state_cost_648761840  <- choice_data$state_cost_648761840 
state_cost_649962135  <- choice_data$state_cost_649962135 
state_cost_650662318  <- choice_data$state_cost_650662318 
state_cost_651262343  <- choice_data$state_cost_651262343 
state_cost_651272341  <- choice_data$state_cost_651272341 
state_cost_651952633  <- choice_data$state_cost_651952633 
state_cost_651994947  <- choice_data$state_cost_651994947 
state_cost_652004948  <- choice_data$state_cost_652004948 
state_cost_652482816  <- choice_data$state_cost_652482816 
state_cost_652512814  <- choice_data$state_cost_652512814 
state_cost_652532812  <- choice_data$state_cost_652532812 
state_cost_652542810  <- choice_data$state_cost_652542810 
state_cost_652562809  <- choice_data$state_cost_652562809 
state_cost_652572808  <- choice_data$state_cost_652572808 
state_cost_652582813  <- choice_data$state_cost_652582813 
state_cost_666364310  <- choice_data$state_cost_666364310 
state_cost_668364802  <- choice_data$state_cost_668364802 
state_cost_669362356  <- choice_data$state_cost_669362356 
state_cost_670063521  <- choice_data$state_cost_670063521 
state_cost_672665510  <- choice_data$state_cost_672665510 
state_cost_672675509  <- choice_data$state_cost_672675509 
state_cost_675263042  <- choice_data$state_cost_675263042 
state_cost_675273041  <- choice_data$state_cost_675273041 
state_cost_675283044  <- choice_data$state_cost_675283044 
state_cost_675563070  <- choice_data$state_cost_675563070 
state_cost_675573069  <- choice_data$state_cost_675573069 
state_cost_675863071  <- choice_data$state_cost_675863071 
state_cost_681261155  <- choice_data$state_cost_681261155 
state_cost_681271154  <- choice_data$state_cost_681271154 
state_cost_683061942  <- choice_data$state_cost_683061942 
state_cost_686113450  <- choice_data$state_cost_686113450 
state_cost_688464799  <- choice_data$state_cost_688464799 
state_cost_696111809  <- choice_data$state_cost_696111809 
state_cost_696341816  <- choice_data$state_cost_696341816 
state_cost_696361815  <- choice_data$state_cost_696361815 
state_cost_696371814  <- choice_data$state_cost_696371814 
state_cost_696381817  <- choice_data$state_cost_696381817 
state_cost_696561822  <- choice_data$state_cost_696561822 
state_cost_696571821  <- choice_data$state_cost_696571821 
state_cost_696581823  <- choice_data$state_cost_696581823 
state_cost_696661833  <- choice_data$state_cost_696661833 
state_cost_696671832  <- choice_data$state_cost_696671832 
state_cost_699404353  <- choice_data$state_cost_699404353 
state_cost_699454355  <- choice_data$state_cost_699454355 
state_cost_699464356  <- choice_data$state_cost_699464356 
state_cost_699474354  <- choice_data$state_cost_699474354 
state_cost_727221293  <- choice_data$state_cost_727221293 
state_cost_730504347  <- choice_data$state_cost_730504347 
state_cost_730514350  <- choice_data$state_cost_730514350 
state_cost_730524343  <- choice_data$state_cost_730524343 
state_cost_730534345  <- choice_data$state_cost_730534345 
state_cost_730544346  <- choice_data$state_cost_730544346 
state_cost_730554344  <- choice_data$state_cost_730554344 
state_cost_731361254  <- choice_data$state_cost_731361254 
state_cost_734561684  <- choice_data$state_cost_734561684 
state_cost_735171645  <- choice_data$state_cost_735171645 
state_cost_735261686  <- choice_data$state_cost_735261686 
state_cost_737021965  <- choice_data$state_cost_737021965 
state_cost_737151958  <- choice_data$state_cost_737151958 
state_cost_739861750  <- choice_data$state_cost_739861750 
state_cost_740862313  <- choice_data$state_cost_740862313 
state_cost_747822800  <- choice_data$state_cost_747822800 
state_cost_750873008  <- choice_data$state_cost_750873008 
state_cost_752083115  <- choice_data$state_cost_752083115 
state_cost_752103114  <- choice_data$state_cost_752103114 
state_cost_752123110  <- choice_data$state_cost_752123110 
state_cost_752143108  <- choice_data$state_cost_752143108 
state_cost_752163111  <- choice_data$state_cost_752163111 
state_cost_760323516  <- choice_data$state_cost_760323516 
state_cost_760363507  <- choice_data$state_cost_760363507 
state_cost_760383514  <- choice_data$state_cost_760383514 
state_cost_760403520  <- choice_data$state_cost_760403520 
state_cost_760423511  <- choice_data$state_cost_760423511 
state_cost_760443513  <- choice_data$state_cost_760443513 
state_cost_762273543  <- choice_data$state_cost_762273543 
state_cost_764363723  <- choice_data$state_cost_764363723 
state_cost_764783788  <- choice_data$state_cost_764783788 
state_cost_764863795  <- choice_data$state_cost_764863795 
state_cost_764873794  <- choice_data$state_cost_764873794 
state_cost_764883796  <- choice_data$state_cost_764883796 
state_cost_765263831  <- choice_data$state_cost_765263831 
state_cost_767673999  <- choice_data$state_cost_767673999 
state_cost_773184326  <- choice_data$state_cost_773184326 
state_cost_773754673  <- choice_data$state_cost_773754673 
state_cost_773784677  <- choice_data$state_cost_773784677 
state_cost_773954559  <- choice_data$state_cost_773954559 
state_cost_774724901  <- choice_data$state_cost_774724901 
state_cost_774724901  <- choice_data$state_cost_774724901 
state_cost_774875094  <- choice_data$state_cost_774875094 
state_cost_777355268  <- choice_data$state_cost_777355268 
state_cost_777404660  <- choice_data$state_cost_777404660 
state_cost_777745327  <- choice_data$state_cost_777745327 
state_cost_777765329  <- choice_data$state_cost_777765329 
state_cost_777865135  <- choice_data$state_cost_777865135 
state_cost_778424689  <- choice_data$state_cost_778424689 
state_cost_778474697  <- choice_data$state_cost_778474697 
state_cost_778524716  <- choice_data$state_cost_778524716 
state_cost_784563813  <- choice_data$state_cost_784563813 
state_cost_788663814  <- choice_data$state_cost_788663814 
state_cost_793363815  <- choice_data$state_cost_793363815 
state_cost_798163816  <- choice_data$state_cost_798163816 
state_cost_800961711  <- choice_data$state_cost_800961711 
state_cost_801452153  <- choice_data$state_cost_801452153 
state_cost_802362680  <- choice_data$state_cost_802362680 
state_cost_804404276  <- choice_data$state_cost_804404276 
state_cost_804564370  <- choice_data$state_cost_804564370 
state_cost_804574369  <- choice_data$state_cost_804574369 
state_cost_804584371  <- choice_data$state_cost_804584371 
state_cost_806861638  <- choice_data$state_cost_806861638 
state_cost_806871637  <- choice_data$state_cost_806871637 
state_cost_808262658  <- choice_data$state_cost_808262658 
state_cost_808272657  <- choice_data$state_cost_808272657 
state_cost_811963839  <- choice_data$state_cost_811963839 
state_cost_812043837  <- choice_data$state_cost_812043837 
state_cost_812063836  <- choice_data$state_cost_812063836 
state_cost_812073835  <- choice_data$state_cost_812073835 
state_cost_812083838  <- choice_data$state_cost_812083838 
state_cost_814065047  <- choice_data$state_cost_814065047 
state_cost_826061716  <- choice_data$state_cost_826061716 
state_cost_826071715  <- choice_data$state_cost_826071715 
state_cost_826101669  <- choice_data$state_cost_826101669 
state_cost_826111670  <- choice_data$state_cost_826111670 
state_cost_826181673  <- choice_data$state_cost_826181673 
state_cost_826261648  <- choice_data$state_cost_826261648 
state_cost_826271647  <- choice_data$state_cost_826271647 
state_cost_826361667  <- choice_data$state_cost_826361667 
state_cost_826371666  <- choice_data$state_cost_826371666 
state_cost_827861643  <- choice_data$state_cost_827861643 
state_cost_827871639  <- choice_data$state_cost_827871639 
state_cost_828201723  <- choice_data$state_cost_828201723 
state_cost_828261702  <- choice_data$state_cost_828261702 
state_cost_828271708  <- choice_data$state_cost_828271708 
state_cost_828401700  <- choice_data$state_cost_828401700 
state_cost_828461681  <- choice_data$state_cost_828461681 
state_cost_828471697  <- choice_data$state_cost_828471697 
state_cost_828491699  <- choice_data$state_cost_828491699 
state_cost_828661729  <- choice_data$state_cost_828661729 
state_cost_828671728  <- choice_data$state_cost_828671728 
state_cost_828811672  <- choice_data$state_cost_828811672 
state_cost_829572064  <- choice_data$state_cost_829572064 
state_cost_833632660  <- choice_data$state_cost_833632660 
state_cost_833702664  <- choice_data$state_cost_833702664 
state_cost_839063438  <- choice_data$state_cost_839063438 
state_cost_839073437  <- choice_data$state_cost_839073437 
state_cost_844563834  <- choice_data$state_cost_844563834 
state_cost_844573833  <- choice_data$state_cost_844573833 
state_cost_844863807  <- choice_data$state_cost_844863807 
state_cost_844963844  <- choice_data$state_cost_844963844 
state_cost_846174003  <- choice_data$state_cost_846174003 
state_cost_855261710  <- choice_data$state_cost_855261710 
state_cost_856063806  <- choice_data$state_cost_856063806 
state_cost_861121724  <- choice_data$state_cost_861121724 
state_cost_862503016  <- choice_data$state_cost_862503016 
state_cost_862513015  <- choice_data$state_cost_862513015 
state_cost_863103198  <- choice_data$state_cost_863103198 
state_cost_863903545  <- choice_data$state_cost_863903545 
state_cost_865063847  <- choice_data$state_cost_865063847 
state_cost_865073846  <- choice_data$state_cost_865073846 
state_cost_866375097  <- choice_data$state_cost_866375097 
state_cost_866692777  <- choice_data$state_cost_866692777 
state_cost_866702776  <- choice_data$state_cost_866702776 
state_cost_866722775  <- choice_data$state_cost_866722775 
state_cost_866922771  <- choice_data$state_cost_866922771 
state_cost_866932770  <- choice_data$state_cost_866932770 
state_cost_867964779  <- choice_data$state_cost_867964779 
state_cost_868074765  <- choice_data$state_cost_868074765 
state_cost_868814774  <- choice_data$state_cost_868814774 
state_cost_868834760  <- choice_data$state_cost_868834760 
state_cost_868844758  <- choice_data$state_cost_868844758 
state_cost_868854775  <- choice_data$state_cost_868854775 
state_cost_868864757  <- choice_data$state_cost_868864757 
state_cost_868874756  <- choice_data$state_cost_868874756 
state_cost_868884776  <- choice_data$state_cost_868884776 
state_cost_869164762  <- choice_data$state_cost_869164762 
state_cost_874082985  <- choice_data$state_cost_874082985 
state_cost_874092984  <- choice_data$state_cost_874092984 
state_cost_874851780  <- choice_data$state_cost_874851780 
state_cost_875861912  <- choice_data$state_cost_875861912 
state_cost_875961916  <- choice_data$state_cost_875961916 
state_cost_876431990  <- choice_data$state_cost_876431990 
state_cost_879363013  <- choice_data$state_cost_879363013 
state_cost_879373012  <- choice_data$state_cost_879373012 
state_cost_879383014  <- choice_data$state_cost_879383014 
state_cost_879973197  <- choice_data$state_cost_879973197 
state_cost_880363412  <- choice_data$state_cost_880363412 
state_cost_881473546  <- choice_data$state_cost_881473546 
state_cost_882943897  <- choice_data$state_cost_882943897 
state_cost_882963898  <- choice_data$state_cost_882963898 
state_cost_885564398  <- choice_data$state_cost_885564398 
state_cost_887665099  <- choice_data$state_cost_887665099 
state_cost_887675098  <- choice_data$state_cost_887675098 
state_cost_887685100  <- choice_data$state_cost_887685100 
state_cost_891211179  <- choice_data$state_cost_891211179 
state_cost_891541783  <- choice_data$state_cost_891541783 
state_cost_891751778  <- choice_data$state_cost_891751778 
state_cost_891912983  <- choice_data$state_cost_891912983 
state_cost_891932981  <- choice_data$state_cost_891932981 
state_cost_891942977  <- choice_data$state_cost_891942977 
state_cost_891962980  <- choice_data$state_cost_891962980 
state_cost_891972979  <- choice_data$state_cost_891972979 
state_cost_891982982  <- choice_data$state_cost_891982982 
state_cost_891992978  <- choice_data$state_cost_891992978 
state_cost_892153005  <- choice_data$state_cost_892153005 
state_cost_892421420  <- choice_data$state_cost_892421420 
state_cost_892781907  <- choice_data$state_cost_892781907 
state_cost_893863010  <- choice_data$state_cost_893863010 
state_cost_893873009  <- choice_data$state_cost_893873009 
state_cost_893883011  <- choice_data$state_cost_893883011 
state_cost_894473195  <- choice_data$state_cost_894473195 
state_cost_894963405  <- choice_data$state_cost_894963405 
state_cost_895331989  <- choice_data$state_cost_895331989 
state_cost_895773544  <- choice_data$state_cost_895773544 
state_cost_896174404  <- choice_data$state_cost_896174404 
state_cost_897864410  <- choice_data$state_cost_897864410 
state_cost_897874409  <- choice_data$state_cost_897874409 
state_cost_897884411  <- choice_data$state_cost_897884411 
state_cost_898362619  <- choice_data$state_cost_898362619 
state_cost_898372618  <- choice_data$state_cost_898372618 
state_cost_898875089  <- choice_data$state_cost_898875089 
state_cost_899165095  <- choice_data$state_cost_899165095 
state_cost_902836770  <- choice_data$state_cost_902836770 
state_cost_9046891337 <- choice_data$state_cost_9046891337
state_cost_9368593476 <- choice_data$state_cost_9368593476
state_cost_9369673634 <- choice_data$state_cost_9369673634
state_cost_9465743475 <- choice_data$state_cost_9465743475
state_cost_9885364401 <- choice_data$state_cost_9885364401


chosen <- choice_data$reduced_item_code
chosen_weight <- choice_data$volume_liters

available_9999       <- 1
available_734190     <- 1*(state_cost_734190     > 0)
available_1573935    <- 1*(state_cost_1573935    > 0)
available_2870505    <- 1*(state_cost_2870505    > 0)
available_4356521    <- 1*(state_cost_4356521    > 0)
available_4626831    <- 1*(state_cost_4626831    > 0)
available_5915426    <- 1*(state_cost_5915426    > 0)
available_6144352    <- 1*(state_cost_6144352    > 0)
available_8417089    <- 1*(state_cost_8417089    > 0)
available_8751020    <- 1*(state_cost_8751020    > 0)
available_8920617    <- 1*(state_cost_8920617    > 0)
available_10548697   <- 1*(state_cost_10548697   > 0)
available_10549698   <- 1*(state_cost_10549698   > 0)
available_10550695   <- 1*(state_cost_10550695   > 0)
available_10553690   <- 1*(state_cost_10553690   > 0)
available_10555696   <- 1*(state_cost_10555696   > 0)
available_11586693   <- 1*(state_cost_11586693   > 0)
available_11588694   <- 1*(state_cost_11588694   > 0)
available_11771692   <- 1*(state_cost_11771692   > 0)
available_11773688   <- 1*(state_cost_11773688   > 0)
available_11774687   <- 1*(state_cost_11774687   > 0)
available_11776686   <- 1*(state_cost_11776686   > 0)
available_11777685   <- 1*(state_cost_11777685   > 0)
available_11786699   <- 1*(state_cost_11786699   > 0)
available_11788689   <- 1*(state_cost_11788689   > 0)
available_13774351   <- 1*(state_cost_13774351   > 0)
available_14001522   <- 1*(state_cost_14001522   > 0)
available_15776973   <- 1*(state_cost_15776973   > 0)
available_16516267   <- 1*(state_cost_16516267   > 0)
available_16517266   <- 1*(state_cost_16517266   > 0)
available_16518268   <- 1*(state_cost_16518268   > 0)
available_16676562   <- 1*(state_cost_16676562   > 0)
available_16987789   <- 1*(state_cost_16987789   > 0)
available_16988790   <- 1*(state_cost_16988790   > 0)
available_17086850   <- 1*(state_cost_17086850   > 0)
available_17127981   <- 1*(state_cost_17127981   > 0)
available_17991132   <- 1*(state_cost_17991132   > 0)
available_18006841   <- 1*(state_cost_18006841   > 0)
available_21480620   <- 1*(state_cost_21480620   > 0)
available_22291300   <- 1*(state_cost_22291300   > 0)
available_22783572   <- 1*(state_cost_22783572   > 0)
available_22784571   <- 1*(state_cost_22784571   > 0)
available_22786570   <- 1*(state_cost_22786570   > 0)
available_22787569   <- 1*(state_cost_22787569   > 0)
available_22788573   <- 1*(state_cost_22788573   > 0)
available_27025847   <- 1*(state_cost_27025847   > 0)
available_27474650   <- 1*(state_cost_27474650   > 0)
available_28086580   <- 1*(state_cost_28086580   > 0)
available_28087579   <- 1*(state_cost_28087579   > 0)
available_28088582   <- 1*(state_cost_28088582   > 0)
available_28206761   <- 1*(state_cost_28206761   > 0)
available_28233766   <- 1*(state_cost_28233766   > 0)
available_28236769   <- 1*(state_cost_28236769   > 0)
available_29287548   <- 1*(state_cost_29287548   > 0)
available_29288550   <- 1*(state_cost_29288550   > 0)
available_29566892   <- 1*(state_cost_29566892   > 0)
available_29568893   <- 1*(state_cost_29568893   > 0)
available_31352818   <- 1*(state_cost_31352818   > 0)
available_33102819   <- 1*(state_cost_33102819   > 0)
available_34001179   <- 1*(state_cost_34001179   > 0)
available_34004177   <- 1*(state_cost_34004177   > 0)
available_34006175   <- 1*(state_cost_34006175   > 0)
available_34007174   <- 1*(state_cost_34007174   > 0)
available_34008178   <- 1*(state_cost_34008178   > 0)
available_34014137   <- 1*(state_cost_34014137   > 0)
available_34021113   <- 1*(state_cost_34021113   > 0)
available_34026169   <- 1*(state_cost_34026169   > 0)
available_34029146   <- 1*(state_cost_34029146   > 0)
available_34030147   <- 1*(state_cost_34030147   > 0)
available_34032148   <- 1*(state_cost_34032148   > 0)
available_34036168   <- 1*(state_cost_34036168   > 0)
available_34052171   <- 1*(state_cost_34052171   > 0)
available_34061172   <- 1*(state_cost_34061172   > 0)
available_34076184   <- 1*(state_cost_34076184   > 0)
available_34116159   <- 1*(state_cost_34116159   > 0)
available_34117158   <- 1*(state_cost_34117158   > 0)
available_34155616   <- 1*(state_cost_34155616   > 0)
available_34162614   <- 1*(state_cost_34162614   > 0)
available_35315561   <- 1*(state_cost_35315561   > 0)
available_35316556   <- 1*(state_cost_35316556   > 0)
available_35317555   <- 1*(state_cost_35317555   > 0)
available_35318559   <- 1*(state_cost_35318559   > 0)
available_35354162   <- 1*(state_cost_35354162   > 0)
available_35416952   <- 1*(state_cost_35416952   > 0)
available_35418953   <- 1*(state_cost_35418953   > 0)
available_36572532   <- 1*(state_cost_36572532   > 0)
available_40482820   <- 1*(state_cost_40482820   > 0)
available_41271111   <- 1*(state_cost_41271111   > 0)
available_41279896   <- 1*(state_cost_41279896   > 0)
available_41284608   <- 1*(state_cost_41284608   > 0)
available_41299880   <- 1*(state_cost_41299880   > 0)
available_41316966   <- 1*(state_cost_41316966   > 0)
available_41320957   <- 1*(state_cost_41320957   > 0)
available_41413922   <- 1*(state_cost_41413922   > 0)
available_41840902   <- 1*(state_cost_41840902   > 0)
available_41927891   <- 1*(state_cost_41927891   > 0)
available_42006282   <- 1*(state_cost_42006282   > 0)
available_43022218   <- 1*(state_cost_43022218   > 0)
available_43024215   <- 1*(state_cost_43024215   > 0)
available_43025219   <- 1*(state_cost_43025219   > 0)
available_43026214   <- 1*(state_cost_43026214   > 0)
available_43027213   <- 1*(state_cost_43027213   > 0)
available_43028216   <- 1*(state_cost_43028216   > 0)
available_43031437   <- 1*(state_cost_43031437   > 0)
available_43035438   <- 1*(state_cost_43035438   > 0)
available_43036432   <- 1*(state_cost_43036432   > 0)
available_43037431   <- 1*(state_cost_43037431   > 0)
available_43038436   <- 1*(state_cost_43038436   > 0)
available_43046475   <- 1*(state_cost_43046475   > 0)
available_43048476   <- 1*(state_cost_43048476   > 0)
available_43050426   <- 1*(state_cost_43050426   > 0)
available_43051427   <- 1*(state_cost_43051427   > 0)
available_43066467   <- 1*(state_cost_43066467   > 0)
available_43076208   <- 1*(state_cost_43076208   > 0)
available_43077207   <- 1*(state_cost_43077207   > 0)
available_43116455   <- 1*(state_cost_43116455   > 0)
available_43117454   <- 1*(state_cost_43117454   > 0)
available_43120477   <- 1*(state_cost_43120477   > 0)
available_43121483   <- 1*(state_cost_43121483   > 0)
available_43123481   <- 1*(state_cost_43123481   > 0)
available_43124480   <- 1*(state_cost_43124480   > 0)
available_43125484   <- 1*(state_cost_43125484   > 0)
available_43126479   <- 1*(state_cost_43126479   > 0)
available_43127478   <- 1*(state_cost_43127478   > 0)
available_43128482   <- 1*(state_cost_43128482   > 0)
available_43136446   <- 1*(state_cost_43136446   > 0)
available_43137443   <- 1*(state_cost_43137443   > 0)
available_43138448   <- 1*(state_cost_43138448   > 0)
available_43145469   <- 1*(state_cost_43145469   > 0)
available_43156402   <- 1*(state_cost_43156402   > 0)
available_43197488   <- 1*(state_cost_43197488   > 0)
available_43205458   <- 1*(state_cost_43205458   > 0)
available_43209415   <- 1*(state_cost_43209415   > 0)
available_43984204   <- 1*(state_cost_43984204   > 0)
available_43986205   <- 1*(state_cost_43986205   > 0)
available_44217554   <- 1*(state_cost_44217554   > 0)
available_44258701   <- 1*(state_cost_44258701   > 0)
available_47161262   <- 1*(state_cost_47161262   > 0)
available_47171261   <- 1*(state_cost_47171261   > 0)
available_48661746   <- 1*(state_cost_48661746   > 0)
available_48671745   <- 1*(state_cost_48671745   > 0)
available_48681749   <- 1*(state_cost_48681749   > 0)
available_49361993   <- 1*(state_cost_49361993   > 0)
available_50062247   <- 1*(state_cost_50062247   > 0)
available_50362273   <- 1*(state_cost_50362273   > 0)
available_50372272   <- 1*(state_cost_50372272   > 0)
available_52882750   <- 1*(state_cost_52882750   > 0)
available_52892748   <- 1*(state_cost_52892748   > 0)
available_52902749   <- 1*(state_cost_52902749   > 0)
available_53262930   <- 1*(state_cost_53262930   > 0)
available_53272929   <- 1*(state_cost_53272929   > 0)
available_53462949   <- 1*(state_cost_53462949   > 0)
available_53472952   <- 1*(state_cost_53472952   > 0)
available_53492951   <- 1*(state_cost_53492951   > 0)
available_54056332   <- 1*(state_cost_54056332   > 0)
available_54057331   <- 1*(state_cost_54057331   > 0)
available_54646335   <- 1*(state_cost_54646335   > 0)
available_54647334   <- 1*(state_cost_54647334   > 0)
available_54863315   <- 1*(state_cost_54863315   > 0)
available_55246362   <- 1*(state_cost_55246362   > 0)
available_55963447   <- 1*(state_cost_55963447   > 0)
available_56063448   <- 1*(state_cost_56063448   > 0)
available_56828417   <- 1*(state_cost_56828417   > 0)
available_56840464   <- 1*(state_cost_56840464   > 0)
available_56843466   <- 1*(state_cost_56843466   > 0)
available_56846465   <- 1*(state_cost_56846465   > 0)
available_56850463   <- 1*(state_cost_56850463   > 0)
available_56957542   <- 1*(state_cost_56957542   > 0)
available_56958543   <- 1*(state_cost_56958543   > 0)
available_57051553   <- 1*(state_cost_57051553   > 0)
available_57092420   <- 1*(state_cost_57092420   > 0)
available_64336401   <- 1*(state_cost_64336401   > 0)
available_68022508   <- 1*(state_cost_68022508   > 0)
available_68036500   <- 1*(state_cost_68036500   > 0)
available_68037498   <- 1*(state_cost_68037498   > 0)
available_68038502   <- 1*(state_cost_68038502   > 0)
available_68039501   <- 1*(state_cost_68039501   > 0)
available_68049509   <- 1*(state_cost_68049509   > 0)
available_69971349   <- 1*(state_cost_69971349   > 0)
available_69981351   <- 1*(state_cost_69981351   > 0)
available_71886256   <- 1*(state_cost_71886256   > 0)
available_72816991   <- 1*(state_cost_72816991   > 0)
available_79026343   <- 1*(state_cost_79026343   > 0)
available_80571355   <- 1*(state_cost_80571355   > 0)
available_80574352   <- 1*(state_cost_80574352   > 0)
available_80576351   <- 1*(state_cost_80576351   > 0)
available_80577350   <- 1*(state_cost_80577350   > 0)
available_80578354   <- 1*(state_cost_80578354   > 0)
available_81966346   <- 1*(state_cost_81966346   > 0)
available_82082628   <- 1*(state_cost_82082628   > 0)
available_82126349   <- 1*(state_cost_82126349   > 0)
available_82127348   <- 1*(state_cost_82127348   > 0)
available_82147356   <- 1*(state_cost_82147356   > 0)
available_82187359   <- 1*(state_cost_82187359   > 0)
available_88253245   <- 1*(state_cost_88253245   > 0)
available_88263244   <- 1*(state_cost_88263244   > 0)
available_88273241   <- 1*(state_cost_88273241   > 0)
available_88283243   <- 1*(state_cost_88283243   > 0)
available_89139984   <- 1*(state_cost_89139984   > 0)
available_92783722   <- 1*(state_cost_92783722   > 0)
available_94263877   <- 1*(state_cost_94263877   > 0)
available_94283878   <- 1*(state_cost_94283878   > 0)
available_100064432  <- 1*(state_cost_100064432  > 0)
available_100084433  <- 1*(state_cost_100084433  > 0)
available_100094431  <- 1*(state_cost_100094431  > 0)
available_102785180  <- 1*(state_cost_102785180  > 0)
available_106251054  <- 1*(state_cost_106251054  > 0)
available_106261050  <- 1*(state_cost_106261050  > 0)
available_106271048  <- 1*(state_cost_106271048  > 0)
available_106281053  <- 1*(state_cost_106281053  > 0)
available_108051512  <- 1*(state_cost_108051512  > 0)
available_108071511  <- 1*(state_cost_108071511  > 0)
available_108081510  <- 1*(state_cost_108081510  > 0)
available_108181507  <- 1*(state_cost_108181507  > 0)
available_108361481  <- 1*(state_cost_108361481  > 0)
available_108902109  <- 1*(state_cost_108902109  > 0)
available_112901489  <- 1*(state_cost_112901489  > 0)
available_112931487  <- 1*(state_cost_112931487  > 0)
available_112941486  <- 1*(state_cost_112941486  > 0)
available_112961476  <- 1*(state_cost_112961476  > 0)
available_112971485  <- 1*(state_cost_112971485  > 0)
available_112981488  <- 1*(state_cost_112981488  > 0)
available_113464508  <- 1*(state_cost_113464508  > 0)
available_113474507  <- 1*(state_cost_113474507  > 0)
available_113484509  <- 1*(state_cost_113484509  > 0)
available_113661518  <- 1*(state_cost_113661518  > 0)
available_119361068  <- 1*(state_cost_119361068  > 0)
available_124041066  <- 1*(state_cost_124041066  > 0)
available_124061065  <- 1*(state_cost_124061065  > 0)
available_124071064  <- 1*(state_cost_124071064  > 0)
available_124081067  <- 1*(state_cost_124081067  > 0)
available_124641076  <- 1*(state_cost_124641076  > 0)
available_124661075  <- 1*(state_cost_124661075  > 0)
available_124671074  <- 1*(state_cost_124671074  > 0)
available_124761073  <- 1*(state_cost_124761073  > 0)
available_124781072  <- 1*(state_cost_124781072  > 0)
available_124791071  <- 1*(state_cost_124791071  > 0)
available_126681083  <- 1*(state_cost_126681083  > 0)
available_128564285  <- 1*(state_cost_128564285  > 0)
available_128884281  <- 1*(state_cost_128884281  > 0)
available_130361080  <- 1*(state_cost_130361080  > 0)
available_130371079  <- 1*(state_cost_130371079  > 0)
available_130381081  <- 1*(state_cost_130381081  > 0)
available_133883651  <- 1*(state_cost_133883651  > 0)
available_136383284  <- 1*(state_cost_136383284  > 0)
available_141923976  <- 1*(state_cost_141923976  > 0)
available_141993977  <- 1*(state_cost_141993977  > 0)
available_152465358  <- 1*(state_cost_152465358  > 0)
available_152475357  <- 1*(state_cost_152475357  > 0)
available_152485359  <- 1*(state_cost_152485359  > 0)
available_152965362  <- 1*(state_cost_152965362  > 0)
available_156262825  <- 1*(state_cost_156262825  > 0)
available_156272824  <- 1*(state_cost_156272824  > 0)
available_156282827  <- 1*(state_cost_156282827  > 0)
available_156442826  <- 1*(state_cost_156442826  > 0)
available_156774143  <- 1*(state_cost_156774143  > 0)
available_159405139  <- 1*(state_cost_159405139  > 0)
available_172061186  <- 1*(state_cost_172061186  > 0)
available_178261887  <- 1*(state_cost_178261887  > 0)
available_178301888  <- 1*(state_cost_178301888  > 0)
available_179161934  <- 1*(state_cost_179161934  > 0)
available_179561971  <- 1*(state_cost_179561971  > 0)
available_179581973  <- 1*(state_cost_179581973  > 0)
available_181961984  <- 1*(state_cost_181961984  > 0)
available_181981986  <- 1*(state_cost_181981986  > 0)
available_184072158  <- 1*(state_cost_184072158  > 0)
available_184082159  <- 1*(state_cost_184082159  > 0)
available_190262884  <- 1*(state_cost_190262884  > 0)
available_190612905  <- 1*(state_cost_190612905  > 0)
available_190632876  <- 1*(state_cost_190632876  > 0)
available_190642874  <- 1*(state_cost_190642874  > 0)
available_190662873  <- 1*(state_cost_190662873  > 0)
available_190672872  <- 1*(state_cost_190672872  > 0)
available_190682877  <- 1*(state_cost_190682877  > 0)
available_190822906  <- 1*(state_cost_190822906  > 0)
available_190962911  <- 1*(state_cost_190962911  > 0)
available_191122890  <- 1*(state_cost_191122890  > 0)
available_192263131  <- 1*(state_cost_192263131  > 0)
available_194763335  <- 1*(state_cost_194763335  > 0)
available_194773333  <- 1*(state_cost_194773333  > 0)
available_194783336  <- 1*(state_cost_194783336  > 0)
available_194863332  <- 1*(state_cost_194863332  > 0)
available_202463689  <- 1*(state_cost_202463689  > 0)
available_202473688  <- 1*(state_cost_202473688  > 0)
available_202483690  <- 1*(state_cost_202483690  > 0)
available_202863693  <- 1*(state_cost_202863693  > 0)
available_215954944  <- 1*(state_cost_215954944  > 0)
available_215964941  <- 1*(state_cost_215964941  > 0)
available_215974940  <- 1*(state_cost_215974940  > 0)
available_215984943  <- 1*(state_cost_215984943  > 0)
available_221215325  <- 1*(state_cost_221215325  > 0)
available_221555322  <- 1*(state_cost_221555322  > 0)
available_221565319  <- 1*(state_cost_221565319  > 0)
available_221575318  <- 1*(state_cost_221575318  > 0)
available_222135384  <- 1*(state_cost_222135384  > 0)
available_222165385  <- 1*(state_cost_222165385  > 0)
available_232771028  <- 1*(state_cost_232771028  > 0)
available_232781030  <- 1*(state_cost_232781030  > 0)
available_236262086  <- 1*(state_cost_236262086  > 0)
available_237082092  <- 1*(state_cost_237082092  > 0)
available_238232083  <- 1*(state_cost_238232083  > 0)
available_238242082  <- 1*(state_cost_238242082  > 0)
available_238262081  <- 1*(state_cost_238262081  > 0)
available_238272080  <- 1*(state_cost_238272080  > 0)
available_238282084  <- 1*(state_cost_238282084  > 0)
available_241562463  <- 1*(state_cost_241562463  > 0)
available_241572462  <- 1*(state_cost_241572462  > 0)
available_241582464  <- 1*(state_cost_241582464  > 0)
available_244543085  <- 1*(state_cost_244543085  > 0)
available_244563084  <- 1*(state_cost_244563084  > 0)
available_244573083  <- 1*(state_cost_244573083  > 0)
available_244583087  <- 1*(state_cost_244583087  > 0)
available_244663089  <- 1*(state_cost_244663089  > 0)
available_247063452  <- 1*(state_cost_247063452  > 0)
available_247283453  <- 1*(state_cost_247283453  > 0)
available_256034475  <- 1*(state_cost_256034475  > 0)
available_256044474  <- 1*(state_cost_256044474  > 0)
available_256064473  <- 1*(state_cost_256064473  > 0)
available_256074472  <- 1*(state_cost_256074472  > 0)
available_256084476  <- 1*(state_cost_256084476  > 0)
available_256164478  <- 1*(state_cost_256164478  > 0)
available_258764867  <- 1*(state_cost_258764867  > 0)
available_258774866  <- 1*(state_cost_258774866  > 0)
available_258784869  <- 1*(state_cost_258784869  > 0)
available_265862175  <- 1*(state_cost_265862175  > 0)
available_266562181  <- 1*(state_cost_266562181  > 0)
available_268202791  <- 1*(state_cost_268202791  > 0)
available_268212792  <- 1*(state_cost_268212792  > 0)
available_268222784  <- 1*(state_cost_268222784  > 0)
available_268232789  <- 1*(state_cost_268232789  > 0)
available_268242793  <- 1*(state_cost_268242793  > 0)
available_268262788  <- 1*(state_cost_268262788  > 0)
available_268272787  <- 1*(state_cost_268272787  > 0)
available_268282790  <- 1*(state_cost_268282790  > 0)
available_269062795  <- 1*(state_cost_269062795  > 0)
available_270271374  <- 1*(state_cost_270271374  > 0)
available_270562907  <- 1*(state_cost_270562907  > 0)
available_271024938  <- 1*(state_cost_271024938  > 0)
available_273922903  <- 1*(state_cost_273922903  > 0)
available_274102893  <- 1*(state_cost_274102893  > 0)
available_274541302  <- 1*(state_cost_274541302  > 0)
available_274791372  <- 1*(state_cost_274791372  > 0)
available_275444212  <- 1*(state_cost_275444212  > 0)
available_275544501  <- 1*(state_cost_275544501  > 0)
available_275624505  <- 1*(state_cost_275624505  > 0)
available_275852626  <- 1*(state_cost_275852626  > 0)
available_286252492  <- 1*(state_cost_286252492  > 0)
available_287184070  <- 1*(state_cost_287184070  > 0)
available_288664915  <- 1*(state_cost_288664915  > 0)
available_288674914  <- 1*(state_cost_288674914  > 0)
available_288684918  <- 1*(state_cost_288684918  > 0)
available_288904925  <- 1*(state_cost_288904925  > 0)
available_297281031  <- 1*(state_cost_297281031  > 0)
available_299932071  <- 1*(state_cost_299932071  > 0)
available_299942070  <- 1*(state_cost_299942070  > 0)
available_299962069  <- 1*(state_cost_299962069  > 0)
available_299972068  <- 1*(state_cost_299972068  > 0)
available_299982072  <- 1*(state_cost_299982072  > 0)
available_300362067  <- 1*(state_cost_300362067  > 0)
available_300562090  <- 1*(state_cost_300562090  > 0)
available_300572089  <- 1*(state_cost_300572089  > 0)
available_300582091  <- 1*(state_cost_300582091  > 0)
available_302362196  <- 1*(state_cost_302362196  > 0)
available_302382197  <- 1*(state_cost_302382197  > 0)
available_303162321  <- 1*(state_cost_303162321  > 0)
available_303172320  <- 1*(state_cost_303172320  > 0)
available_303182322  <- 1*(state_cost_303182322  > 0)
available_305262468  <- 1*(state_cost_305262468  > 0)
available_305272467  <- 1*(state_cost_305272467  > 0)
available_305282469  <- 1*(state_cost_305282469  > 0)
available_311673458  <- 1*(state_cost_311673458  > 0)
available_312083459  <- 1*(state_cost_312083459  > 0)
available_314723602  <- 1*(state_cost_314723602  > 0)
available_314733605  <- 1*(state_cost_314733605  > 0)
available_314743603  <- 1*(state_cost_314743603  > 0)
available_314753601  <- 1*(state_cost_314753601  > 0)
available_316543821  <- 1*(state_cost_316543821  > 0)
available_316563820  <- 1*(state_cost_316563820  > 0)
available_316573819  <- 1*(state_cost_316573819  > 0)
available_316583822  <- 1*(state_cost_316583822  > 0)
available_316663823  <- 1*(state_cost_316663823  > 0)
available_317183995  <- 1*(state_cost_317183995  > 0)
available_317193994  <- 1*(state_cost_317193994  > 0)
available_322314488  <- 1*(state_cost_322314488  > 0)
available_322324485  <- 1*(state_cost_322324485  > 0)
available_322334486  <- 1*(state_cost_322334486  > 0)
available_322344484  <- 1*(state_cost_322344484  > 0)
available_322354489  <- 1*(state_cost_322354489  > 0)
available_322364483  <- 1*(state_cost_322364483  > 0)
available_322374482  <- 1*(state_cost_322374482  > 0)
available_322384487  <- 1*(state_cost_322384487  > 0)
available_332564494  <- 1*(state_cost_332564494  > 0)
available_336634891  <- 1*(state_cost_336634891  > 0)
available_337163842  <- 1*(state_cost_337163842  > 0)
available_337173841  <- 1*(state_cost_337173841  > 0)
available_341641324  <- 1*(state_cost_341641324  > 0)
available_341971322  <- 1*(state_cost_341971322  > 0)
available_341981344  <- 1*(state_cost_341981344  > 0)
available_343592414  <- 1*(state_cost_343592414  > 0)
available_343662146  <- 1*(state_cost_343662146  > 0)
available_343682147  <- 1*(state_cost_343682147  > 0)
available_344222417  <- 1*(state_cost_344222417  > 0)
available_344232415  <- 1*(state_cost_344232415  > 0)
available_344252418  <- 1*(state_cost_344252418  > 0)
available_344332416  <- 1*(state_cost_344332416  > 0)
available_344493093  <- 1*(state_cost_344493093  > 0)
available_344563096  <- 1*(state_cost_344563096  > 0)
available_344573094  <- 1*(state_cost_344573094  > 0)
available_344583097  <- 1*(state_cost_344583097  > 0)
available_345463959  <- 1*(state_cost_345463959  > 0)
available_345663961  <- 1*(state_cost_345663961  > 0)
available_345784107  <- 1*(state_cost_345784107  > 0)
available_345794105  <- 1*(state_cost_345794105  > 0)
available_346904740  <- 1*(state_cost_346904740  > 0)
available_347464848  <- 1*(state_cost_347464848  > 0)
available_347474844  <- 1*(state_cost_347474844  > 0)
available_347484849  <- 1*(state_cost_347484849  > 0)
available_347864825  <- 1*(state_cost_347864825  > 0)
available_348174871  <- 1*(state_cost_348174871  > 0)
available_348204895  <- 1*(state_cost_348204895  > 0)
available_348214898  <- 1*(state_cost_348214898  > 0)
available_348234877  <- 1*(state_cost_348234877  > 0)
available_348564927  <- 1*(state_cost_348564927  > 0)
available_348714847  <- 1*(state_cost_348714847  > 0)
available_348814855  <- 1*(state_cost_348814855  > 0)
available_349195043  <- 1*(state_cost_349195043  > 0)
available_349355042  <- 1*(state_cost_349355042  > 0)
available_349725007  <- 1*(state_cost_349725007  > 0)
available_349955017  <- 1*(state_cost_349955017  > 0)
available_352134741  <- 1*(state_cost_352134741  > 0)
available_356264111  <- 1*(state_cost_356264111  > 0)
available_356284112  <- 1*(state_cost_356284112  > 0)
available_356993944  <- 1*(state_cost_356993944  > 0)
available_357804051  <- 1*(state_cost_357804051  > 0)
available_359132078  <- 1*(state_cost_359132078  > 0)
available_359142077  <- 1*(state_cost_359142077  > 0)
available_359162076  <- 1*(state_cost_359162076  > 0)
available_359172075  <- 1*(state_cost_359172075  > 0)
available_359182079  <- 1*(state_cost_359182079  > 0)
available_359262073  <- 1*(state_cost_359262073  > 0)
available_359442095  <- 1*(state_cost_359442095  > 0)
available_359462094  <- 1*(state_cost_359462094  > 0)
available_359472093  <- 1*(state_cost_359472093  > 0)
available_359482096  <- 1*(state_cost_359482096  > 0)
available_359645019  <- 1*(state_cost_359645019  > 0)
available_361862324  <- 1*(state_cost_361862324  > 0)
available_361882325  <- 1*(state_cost_361882325  > 0)
available_363012478  <- 1*(state_cost_363012478  > 0)
available_363042475  <- 1*(state_cost_363042475  > 0)
available_363052474  <- 1*(state_cost_363052474  > 0)
available_363072473  <- 1*(state_cost_363072473  > 0)
available_363082477  <- 1*(state_cost_363082477  > 0)
available_364472674  <- 1*(state_cost_364472674  > 0)
available_366673145  <- 1*(state_cost_366673145  > 0)
available_366683146  <- 1*(state_cost_366683146  > 0)
available_368711323  <- 1*(state_cost_368711323  > 0)
available_368863465  <- 1*(state_cost_368863465  > 0)
available_368873467  <- 1*(state_cost_368873467  > 0)
available_369013468  <- 1*(state_cost_369013468  > 0)
available_369033466  <- 1*(state_cost_369033466  > 0)
available_369043469  <- 1*(state_cost_369043469  > 0)
available_369073471  <- 1*(state_cost_369073471  > 0)
available_369083470  <- 1*(state_cost_369083470  > 0)
available_369683635  <- 1*(state_cost_369683635  > 0)
available_369693633  <- 1*(state_cost_369693633  > 0)
available_369713637  <- 1*(state_cost_369713637  > 0)
available_369743645  <- 1*(state_cost_369743645  > 0)
available_369763644  <- 1*(state_cost_369763644  > 0)
available_369783647  <- 1*(state_cost_369783647  > 0)
available_371463754  <- 1*(state_cost_371463754  > 0)
available_372173850  <- 1*(state_cost_372173850  > 0)
available_373365237  <- 1*(state_cost_373365237  > 0)
available_373385241  <- 1*(state_cost_373385241  > 0)
available_373395239  <- 1*(state_cost_373395239  > 0)
available_373464016  <- 1*(state_cost_373464016  > 0)
available_373474014  <- 1*(state_cost_373474014  > 0)
available_373484017  <- 1*(state_cost_373484017  > 0)
available_373564018  <- 1*(state_cost_373564018  > 0)
available_374134136  <- 1*(state_cost_374134136  > 0)
available_374144135  <- 1*(state_cost_374144135  > 0)
available_374164134  <- 1*(state_cost_374164134  > 0)
available_374174133  <- 1*(state_cost_374174133  > 0)
available_374184137  <- 1*(state_cost_374184137  > 0)
available_374264138  <- 1*(state_cost_374264138  > 0)
available_376404291  <- 1*(state_cost_376404291  > 0)
available_376424292  <- 1*(state_cost_376424292  > 0)
available_378864512  <- 1*(state_cost_378864512  > 0)
available_378884513  <- 1*(state_cost_378884513  > 0)
available_379344565  <- 1*(state_cost_379344565  > 0)
available_379354567  <- 1*(state_cost_379354567  > 0)
available_379364564  <- 1*(state_cost_379364564  > 0)
available_379374563  <- 1*(state_cost_379374563  > 0)
available_379384566  <- 1*(state_cost_379384566  > 0)
available_379864600  <- 1*(state_cost_379864600  > 0)
available_379874598  <- 1*(state_cost_379874598  > 0)
available_379884601  <- 1*(state_cost_379884601  > 0)
available_379914710  <- 1*(state_cost_379914710  > 0)
available_379934708  <- 1*(state_cost_379934708  > 0)
available_379944707  <- 1*(state_cost_379944707  > 0)
available_379964706  <- 1*(state_cost_379964706  > 0)
available_379974705  <- 1*(state_cost_379974705  > 0)
available_379984709  <- 1*(state_cost_379984709  > 0)
available_380064712  <- 1*(state_cost_380064712  > 0)
available_380084711  <- 1*(state_cost_380084711  > 0)
available_380884122  <- 1*(state_cost_380884122  > 0)
available_381765067  <- 1*(state_cost_381765067  > 0)
available_381775064  <- 1*(state_cost_381775064  > 0)
available_381785066  <- 1*(state_cost_381785066  > 0)
available_398664701  <- 1*(state_cost_398664701  > 0)
available_401181854  <- 1*(state_cost_401181854  > 0)
available_401933622  <- 1*(state_cost_401933622  > 0)
available_402973593  <- 1*(state_cost_402973593  > 0)
available_403133619  <- 1*(state_cost_403133619  > 0)
available_403945197  <- 1*(state_cost_403945197  > 0)
available_404115227  <- 1*(state_cost_404115227  > 0)
available_405923616  <- 1*(state_cost_405923616  > 0)
available_405933617  <- 1*(state_cost_405933617  > 0)
available_405943615  <- 1*(state_cost_405943615  > 0)
available_405983628  <- 1*(state_cost_405983628  > 0)
available_405993626  <- 1*(state_cost_405993626  > 0)
available_406044636  <- 1*(state_cost_406044636  > 0)
available_406144352  <- 1*(state_cost_406144352  > 0)
available_406255200  <- 1*(state_cost_406255200  > 0)
available_406325242  <- 1*(state_cost_406325242  > 0)
available_409264670  <- 1*(state_cost_409264670  > 0)
available_409304631  <- 1*(state_cost_409304631  > 0)
available_409464717  <- 1*(state_cost_409464717  > 0)
available_409564691  <- 1*(state_cost_409564691  > 0)
available_409654718  <- 1*(state_cost_409654718  > 0)
available_409764615  <- 1*(state_cost_409764615  > 0)
available_410762864  <- 1*(state_cost_410762864  > 0)
available_410772862  <- 1*(state_cost_410772862  > 0)
available_410782865  <- 1*(state_cost_410782865  > 0)
available_412092062  <- 1*(state_cost_412092062  > 0)
available_413605215  <- 1*(state_cost_413605215  > 0)
available_415002465  <- 1*(state_cost_415002465  > 0)
available_415924639  <- 1*(state_cost_415924639  > 0)
available_416015183  <- 1*(state_cost_416015183  > 0)
available_416045206  <- 1*(state_cost_416045206  > 0)
available_416255207  <- 1*(state_cost_416255207  > 0)
available_416535202  <- 1*(state_cost_416535202  > 0)
available_416815221  <- 1*(state_cost_416815221  > 0)
available_416885191  <- 1*(state_cost_416885191  > 0)
available_416925185  <- 1*(state_cost_416925185  > 0)
available_416935184  <- 1*(state_cost_416935184  > 0)
available_416945182  <- 1*(state_cost_416945182  > 0)
available_416965209  <- 1*(state_cost_416965209  > 0)
available_416985210  <- 1*(state_cost_416985210  > 0)
available_417015211  <- 1*(state_cost_417015211  > 0)
available_417045220  <- 1*(state_cost_417045220  > 0)
available_417055219  <- 1*(state_cost_417055219  > 0)
available_417073998  <- 1*(state_cost_417073998  > 0)
available_417154626  <- 1*(state_cost_417154626  > 0)
available_417194633  <- 1*(state_cost_417194633  > 0)
available_417464623  <- 1*(state_cost_417464623  > 0)
available_417474622  <- 1*(state_cost_417474622  > 0)
available_417835190  <- 1*(state_cost_417835190  > 0)
available_417964664  <- 1*(state_cost_417964664  > 0)
available_417974663  <- 1*(state_cost_417974663  > 0)
available_418064700  <- 1*(state_cost_418064700  > 0)
available_418461979  <- 1*(state_cost_418461979  > 0)
available_419895193  <- 1*(state_cost_419895193  > 0)
available_420025194  <- 1*(state_cost_420025194  > 0)
available_420095196  <- 1*(state_cost_420095196  > 0)
available_420795195  <- 1*(state_cost_420795195  > 0)
available_421663588  <- 1*(state_cost_421663588  > 0)
available_421673587  <- 1*(state_cost_421673587  > 0)
available_423123372  <- 1*(state_cost_423123372  > 0)
available_424444531  <- 1*(state_cost_424444531  > 0)
available_426663583  <- 1*(state_cost_426663583  > 0)
available_426763366  <- 1*(state_cost_426763366  > 0)
available_426873343  <- 1*(state_cost_426873343  > 0)
available_426993362  <- 1*(state_cost_426993362  > 0)
available_427033371  <- 1*(state_cost_427033371  > 0)
available_427123353  <- 1*(state_cost_427123353  > 0)
available_427163350  <- 1*(state_cost_427163350  > 0)
available_427173349  <- 1*(state_cost_427173349  > 0)
available_427183352  <- 1*(state_cost_427183352  > 0)
available_429845112  <- 1*(state_cost_429845112  > 0)
available_432191119  <- 1*(state_cost_432191119  > 0)
available_432441094  <- 1*(state_cost_432441094  > 0)
available_432821121  <- 1*(state_cost_432821121  > 0)
available_432851114  <- 1*(state_cost_432851114  > 0)
available_432871118  <- 1*(state_cost_432871118  > 0)
available_432961130  <- 1*(state_cost_432961130  > 0)
available_433021096  <- 1*(state_cost_433021096  > 0)
available_433161127  <- 1*(state_cost_433161127  > 0)
available_433281141  <- 1*(state_cost_433281141  > 0)
available_433301142  <- 1*(state_cost_433301142  > 0)
available_433311136  <- 1*(state_cost_433311136  > 0)
available_433331137  <- 1*(state_cost_433331137  > 0)
available_433341134  <- 1*(state_cost_433341134  > 0)
available_433351138  <- 1*(state_cost_433351138  > 0)
available_433361110  <- 1*(state_cost_433361110  > 0)
available_433371133  <- 1*(state_cost_433371133  > 0)
available_433381135  <- 1*(state_cost_433381135  > 0)
available_433861169  <- 1*(state_cost_433861169  > 0)
available_433871168  <- 1*(state_cost_433871168  > 0)
available_433881170  <- 1*(state_cost_433881170  > 0)
available_434091115  <- 1*(state_cost_434091115  > 0)
available_434101116  <- 1*(state_cost_434101116  > 0)
available_434151107  <- 1*(state_cost_434151107  > 0)
available_434181139  <- 1*(state_cost_434181139  > 0)
available_434261790  <- 1*(state_cost_434261790  > 0)
available_438484334  <- 1*(state_cost_438484334  > 0)
available_443501003  <- 1*(state_cost_443501003  > 0)
available_444191536  <- 1*(state_cost_444191536  > 0)
available_444561561  <- 1*(state_cost_444561561  > 0)
available_444761542  <- 1*(state_cost_444761542  > 0)
available_444991554  <- 1*(state_cost_444991554  > 0)
available_445161553  <- 1*(state_cost_445161553  > 0)
available_445201565  <- 1*(state_cost_445201565  > 0)
available_446032066  <- 1*(state_cost_446032066  > 0)
available_446573204  <- 1*(state_cost_446573204  > 0)
available_446583206  <- 1*(state_cost_446583206  > 0)
available_452453828  <- 1*(state_cost_452453828  > 0)
available_452463826  <- 1*(state_cost_452463826  > 0)
available_452473825  <- 1*(state_cost_452473825  > 0)
available_452483827  <- 1*(state_cost_452483827  > 0)
available_452753858  <- 1*(state_cost_452753858  > 0)
available_452763855  <- 1*(state_cost_452763855  > 0)
available_452773854  <- 1*(state_cost_452773854  > 0)
available_452783857  <- 1*(state_cost_452783857  > 0)
available_454184020  <- 1*(state_cost_454184020  > 0)
available_454194019  <- 1*(state_cost_454194019  > 0)
available_458844377  <- 1*(state_cost_458844377  > 0)
available_458864376  <- 1*(state_cost_458864376  > 0)
available_458874375  <- 1*(state_cost_458874375  > 0)
available_458884378  <- 1*(state_cost_458884378  > 0)
available_463502472  <- 1*(state_cost_463502472  > 0)
available_463512471  <- 1*(state_cost_463512471  > 0)
available_465043156  <- 1*(state_cost_465043156  > 0)
available_465063157  <- 1*(state_cost_465063157  > 0)
available_477851437  <- 1*(state_cost_477851437  > 0)
available_477861438  <- 1*(state_cost_477861438  > 0)
available_480992500  <- 1*(state_cost_480992500  > 0)
available_481022508  <- 1*(state_cost_481022508  > 0)
available_481032505  <- 1*(state_cost_481032505  > 0)
available_481042504  <- 1*(state_cost_481042504  > 0)
available_481052499  <- 1*(state_cost_481052499  > 0)
available_481062503  <- 1*(state_cost_481062503  > 0)
available_490864235  <- 1*(state_cost_490864235  > 0)
available_491854246  <- 1*(state_cost_491854246  > 0)
available_491864248  <- 1*(state_cost_491864248  > 0)
available_491894249  <- 1*(state_cost_491894249  > 0)
available_506864806  <- 1*(state_cost_506864806  > 0)
available_516964816  <- 1*(state_cost_516964816  > 0)
available_521461289  <- 1*(state_cost_521461289  > 0)
available_521861292  <- 1*(state_cost_521861292  > 0)
available_523121283  <- 1*(state_cost_523121283  > 0)
available_523141284  <- 1*(state_cost_523141284  > 0)
available_523161282  <- 1*(state_cost_523161282  > 0)
available_523171287  <- 1*(state_cost_523171287  > 0)
available_523181285  <- 1*(state_cost_523181285  > 0)
available_523191286  <- 1*(state_cost_523191286  > 0)
available_525631877  <- 1*(state_cost_525631877  > 0)
available_525801874  <- 1*(state_cost_525801874  > 0)
available_525811875  <- 1*(state_cost_525811875  > 0)
available_525821873  <- 1*(state_cost_525821873  > 0)
available_525931865  <- 1*(state_cost_525931865  > 0)
available_525941866  <- 1*(state_cost_525941866  > 0)
available_525951870  <- 1*(state_cost_525951870  > 0)
available_525961862  <- 1*(state_cost_525961862  > 0)
available_525971869  <- 1*(state_cost_525971869  > 0)
available_525981863  <- 1*(state_cost_525981863  > 0)
available_525991868  <- 1*(state_cost_525991868  > 0)
available_528063140  <- 1*(state_cost_528063140  > 0)
available_528353142  <- 1*(state_cost_528353142  > 0)
available_532103914  <- 1*(state_cost_532103914  > 0)
available_532113924  <- 1*(state_cost_532113924  > 0)
available_532133912  <- 1*(state_cost_532133912  > 0)
available_532143913  <- 1*(state_cost_532143913  > 0)
available_532163911  <- 1*(state_cost_532163911  > 0)
available_532183915  <- 1*(state_cost_532183915  > 0)
available_544363564  <- 1*(state_cost_544363564  > 0)
available_544463799  <- 1*(state_cost_544463799  > 0)
available_544473798  <- 1*(state_cost_544473798  > 0)
available_544483801  <- 1*(state_cost_544483801  > 0)
available_547061635  <- 1*(state_cost_547061635  > 0)
available_550663565  <- 1*(state_cost_550663565  > 0)
available_550683566  <- 1*(state_cost_550683566  > 0)
available_550843804  <- 1*(state_cost_550843804  > 0)
available_550863803  <- 1*(state_cost_550863803  > 0)
available_550873802  <- 1*(state_cost_550873802  > 0)
available_550883805  <- 1*(state_cost_550883805  > 0)
available_551063991  <- 1*(state_cost_551063991  > 0)
available_555063810  <- 1*(state_cost_555063810  > 0)
available_561953918  <- 1*(state_cost_561953918  > 0)
available_561963916  <- 1*(state_cost_561963916  > 0)
available_562063832  <- 1*(state_cost_562063832  > 0)
available_571201242  <- 1*(state_cost_571201242  > 0)
available_571251241  <- 1*(state_cost_571251241  > 0)
available_571291244  <- 1*(state_cost_571291244  > 0)
available_571441252  <- 1*(state_cost_571441252  > 0)
available_571481243  <- 1*(state_cost_571481243  > 0)
available_571571246  <- 1*(state_cost_571571246  > 0)
available_571581251  <- 1*(state_cost_571581251  > 0)
available_571611240  <- 1*(state_cost_571611240  > 0)
available_571741250  <- 1*(state_cost_571741250  > 0)
available_588011108  <- 1*(state_cost_588011108  > 0)
available_588352963  <- 1*(state_cost_588352963  > 0)
available_588362961  <- 1*(state_cost_588362961  > 0)
available_588382962  <- 1*(state_cost_588382962  > 0)
available_588402957  <- 1*(state_cost_588402957  > 0)
available_588422975  <- 1*(state_cost_588422975  > 0)
available_588602998  <- 1*(state_cost_588602998  > 0)
available_588682969  <- 1*(state_cost_588682969  > 0)
available_588722960  <- 1*(state_cost_588722960  > 0)
available_588732971  <- 1*(state_cost_588732971  > 0)
available_588752994  <- 1*(state_cost_588752994  > 0)
available_588762993  <- 1*(state_cost_588762993  > 0)
available_588852997  <- 1*(state_cost_588852997  > 0)
available_588862996  <- 1*(state_cost_588862996  > 0)
available_590371737  <- 1*(state_cost_590371737  > 0)
available_591001960  <- 1*(state_cost_591001960  > 0)
available_592312659  <- 1*(state_cost_592312659  > 0)
available_620613048  <- 1*(state_cost_620613048  > 0)
available_620973829  <- 1*(state_cost_620973829  > 0)
available_624003402  <- 1*(state_cost_624003402  > 0)
available_624053409  <- 1*(state_cost_624053409  > 0)
available_624203414  <- 1*(state_cost_624203414  > 0)
available_633554549  <- 1*(state_cost_633554549  > 0)
available_633574553  <- 1*(state_cost_633574553  > 0)
available_633594561  <- 1*(state_cost_633594561  > 0)
available_635284382  <- 1*(state_cost_635284382  > 0)
available_635304384  <- 1*(state_cost_635304384  > 0)
available_637554961  <- 1*(state_cost_637554961  > 0)
available_639595188  <- 1*(state_cost_639595188  > 0)
available_639635214  <- 1*(state_cost_639635214  > 0)
available_641361756  <- 1*(state_cost_641361756  > 0)
available_645731333  <- 1*(state_cost_645731333  > 0)
available_646011145  <- 1*(state_cost_646011145  > 0)
available_646761226  <- 1*(state_cost_646761226  > 0)
available_647111326  <- 1*(state_cost_647111326  > 0)
available_647121331  <- 1*(state_cost_647121331  > 0)
available_647161327  <- 1*(state_cost_647161327  > 0)
available_647361348  <- 1*(state_cost_647361348  > 0)
available_647551338  <- 1*(state_cost_647551338  > 0)
available_647591339  <- 1*(state_cost_647591339  > 0)
available_647761385  <- 1*(state_cost_647761385  > 0)
available_648582044  <- 1*(state_cost_648582044  > 0)
available_648622040  <- 1*(state_cost_648622040  > 0)
available_648632042  <- 1*(state_cost_648632042  > 0)
available_648642046  <- 1*(state_cost_648642046  > 0)
available_648652045  <- 1*(state_cost_648652045  > 0)
available_648662041  <- 1*(state_cost_648662041  > 0)
available_648672039  <- 1*(state_cost_648672039  > 0)
available_648682043  <- 1*(state_cost_648682043  > 0)
available_648702047  <- 1*(state_cost_648702047  > 0)
available_648761840  <- 1*(state_cost_648761840  > 0)
available_649962135  <- 1*(state_cost_649962135  > 0)
available_650662318  <- 1*(state_cost_650662318  > 0)
available_651262343  <- 1*(state_cost_651262343  > 0)
available_651272341  <- 1*(state_cost_651272341  > 0)
available_651952633  <- 1*(state_cost_651952633  > 0)
available_651994947  <- 1*(state_cost_651994947  > 0)
available_652004948  <- 1*(state_cost_652004948  > 0)
available_652482816  <- 1*(state_cost_652482816  > 0)
available_652512814  <- 1*(state_cost_652512814  > 0)
available_652532812  <- 1*(state_cost_652532812  > 0)
available_652542810  <- 1*(state_cost_652542810  > 0)
available_652562809  <- 1*(state_cost_652562809  > 0)
available_652572808  <- 1*(state_cost_652572808  > 0)
available_652582813  <- 1*(state_cost_652582813  > 0)
available_666364310  <- 1*(state_cost_666364310  > 0)
available_668364802  <- 1*(state_cost_668364802  > 0)
available_669362356  <- 1*(state_cost_669362356  > 0)
available_670063521  <- 1*(state_cost_670063521  > 0)
available_672665510  <- 1*(state_cost_672665510  > 0)
available_672675509  <- 1*(state_cost_672675509  > 0)
available_675263042  <- 1*(state_cost_675263042  > 0)
available_675273041  <- 1*(state_cost_675273041  > 0)
available_675283044  <- 1*(state_cost_675283044  > 0)
available_675563070  <- 1*(state_cost_675563070  > 0)
available_675573069  <- 1*(state_cost_675573069  > 0)
available_675863071  <- 1*(state_cost_675863071  > 0)
available_681261155  <- 1*(state_cost_681261155  > 0)
available_681271154  <- 1*(state_cost_681271154  > 0)
available_683061942  <- 1*(state_cost_683061942  > 0)
available_686113450  <- 1*(state_cost_686113450  > 0)
available_688464799  <- 1*(state_cost_688464799  > 0)
available_696111809  <- 1*(state_cost_696111809  > 0)
available_696341816  <- 1*(state_cost_696341816  > 0)
available_696361815  <- 1*(state_cost_696361815  > 0)
available_696371814  <- 1*(state_cost_696371814  > 0)
available_696381817  <- 1*(state_cost_696381817  > 0)
available_696561822  <- 1*(state_cost_696561822  > 0)
available_696571821  <- 1*(state_cost_696571821  > 0)
available_696581823  <- 1*(state_cost_696581823  > 0)
available_696661833  <- 1*(state_cost_696661833  > 0)
available_696671832  <- 1*(state_cost_696671832  > 0)
available_699404353  <- 1*(state_cost_699404353  > 0)
available_699454355  <- 1*(state_cost_699454355  > 0)
available_699464356  <- 1*(state_cost_699464356  > 0)
available_699474354  <- 1*(state_cost_699474354  > 0)
available_727221293  <- 1*(state_cost_727221293  > 0)
available_730504347  <- 1*(state_cost_730504347  > 0)
available_730514350  <- 1*(state_cost_730514350  > 0)
available_730524343  <- 1*(state_cost_730524343  > 0)
available_730534345  <- 1*(state_cost_730534345  > 0)
available_730544346  <- 1*(state_cost_730544346  > 0)
available_730554344  <- 1*(state_cost_730554344  > 0)
available_731361254  <- 1*(state_cost_731361254  > 0)
available_734561684  <- 1*(state_cost_734561684  > 0)
available_735171645  <- 1*(state_cost_735171645  > 0)
available_735261686  <- 1*(state_cost_735261686  > 0)
available_737021965  <- 1*(state_cost_737021965  > 0)
available_737151958  <- 1*(state_cost_737151958  > 0)
available_739861750  <- 1*(state_cost_739861750  > 0)
available_740862313  <- 1*(state_cost_740862313  > 0)
available_747822800  <- 1*(state_cost_747822800  > 0)
available_750873008  <- 1*(state_cost_750873008  > 0)
available_752083115  <- 1*(state_cost_752083115  > 0)
available_752103114  <- 1*(state_cost_752103114  > 0)
available_752123110  <- 1*(state_cost_752123110  > 0)
available_752143108  <- 1*(state_cost_752143108  > 0)
available_752163111  <- 1*(state_cost_752163111  > 0)
available_760323516  <- 1*(state_cost_760323516  > 0)
available_760363507  <- 1*(state_cost_760363507  > 0)
available_760383514  <- 1*(state_cost_760383514  > 0)
available_760403520  <- 1*(state_cost_760403520  > 0)
available_760423511  <- 1*(state_cost_760423511  > 0)
available_760443513  <- 1*(state_cost_760443513  > 0)
available_762273543  <- 1*(state_cost_762273543  > 0)
available_764363723  <- 1*(state_cost_764363723  > 0)
available_764783788  <- 1*(state_cost_764783788  > 0)
available_764863795  <- 1*(state_cost_764863795  > 0)
available_764873794  <- 1*(state_cost_764873794  > 0)
available_764883796  <- 1*(state_cost_764883796  > 0)
available_765263831  <- 1*(state_cost_765263831  > 0)
available_767673999  <- 1*(state_cost_767673999  > 0)
available_773184326  <- 1*(state_cost_773184326  > 0)
available_773754673  <- 1*(state_cost_773754673  > 0)
available_773784677  <- 1*(state_cost_773784677  > 0)
available_773954559  <- 1*(state_cost_773954559  > 0)
available_774724901  <- 1*(state_cost_774724901  > 0)
available_774724901  <- 1*(state_cost_774724901  > 0)
available_774875094  <- 1*(state_cost_774875094  > 0)
available_777355268  <- 1*(state_cost_777355268  > 0)
available_777404660  <- 1*(state_cost_777404660  > 0)
available_777745327  <- 1*(state_cost_777745327  > 0)
available_777765329  <- 1*(state_cost_777765329  > 0)
available_777865135  <- 1*(state_cost_777865135  > 0)
available_778424689  <- 1*(state_cost_778424689  > 0)
available_778474697  <- 1*(state_cost_778474697  > 0)
available_778524716  <- 1*(state_cost_778524716  > 0)
available_784563813  <- 1*(state_cost_784563813  > 0)
available_788663814  <- 1*(state_cost_788663814  > 0)
available_793363815  <- 1*(state_cost_793363815  > 0)
available_798163816  <- 1*(state_cost_798163816  > 0)
available_800961711  <- 1*(state_cost_800961711  > 0)
available_801452153  <- 1*(state_cost_801452153  > 0)
available_802362680  <- 1*(state_cost_802362680  > 0)
available_804404276  <- 1*(state_cost_804404276  > 0)
available_804564370  <- 1*(state_cost_804564370  > 0)
available_804574369  <- 1*(state_cost_804574369  > 0)
available_804584371  <- 1*(state_cost_804584371  > 0)
available_806861638  <- 1*(state_cost_806861638  > 0)
available_806871637  <- 1*(state_cost_806871637  > 0)
available_808262658  <- 1*(state_cost_808262658  > 0)
available_808272657  <- 1*(state_cost_808272657  > 0)
available_811963839  <- 1*(state_cost_811963839  > 0)
available_812043837  <- 1*(state_cost_812043837  > 0)
available_812063836  <- 1*(state_cost_812063836  > 0)
available_812073835  <- 1*(state_cost_812073835  > 0)
available_812083838  <- 1*(state_cost_812083838  > 0)
available_814065047  <- 1*(state_cost_814065047  > 0)
available_826061716  <- 1*(state_cost_826061716  > 0)
available_826071715  <- 1*(state_cost_826071715  > 0)
available_826101669  <- 1*(state_cost_826101669  > 0)
available_826111670  <- 1*(state_cost_826111670  > 0)
available_826181673  <- 1*(state_cost_826181673  > 0)
available_826261648  <- 1*(state_cost_826261648  > 0)
available_826271647  <- 1*(state_cost_826271647  > 0)
available_826361667  <- 1*(state_cost_826361667  > 0)
available_826371666  <- 1*(state_cost_826371666  > 0)
available_827861643  <- 1*(state_cost_827861643  > 0)
available_827871639  <- 1*(state_cost_827871639  > 0)
available_828201723  <- 1*(state_cost_828201723  > 0)
available_828261702  <- 1*(state_cost_828261702  > 0)
available_828271708  <- 1*(state_cost_828271708  > 0)
available_828401700  <- 1*(state_cost_828401700  > 0)
available_828461681  <- 1*(state_cost_828461681  > 0)
available_828471697  <- 1*(state_cost_828471697  > 0)
available_828491699  <- 1*(state_cost_828491699  > 0)
available_828661729  <- 1*(state_cost_828661729  > 0)
available_828671728  <- 1*(state_cost_828671728  > 0)
available_828811672  <- 1*(state_cost_828811672  > 0)
available_829572064  <- 1*(state_cost_829572064  > 0)
available_833632660  <- 1*(state_cost_833632660  > 0)
available_833702664  <- 1*(state_cost_833702664  > 0)
available_839063438  <- 1*(state_cost_839063438  > 0)
available_839073437  <- 1*(state_cost_839073437  > 0)
available_844563834  <- 1*(state_cost_844563834  > 0)
available_844573833  <- 1*(state_cost_844573833  > 0)
available_844863807  <- 1*(state_cost_844863807  > 0)
available_844963844  <- 1*(state_cost_844963844  > 0)
available_846174003  <- 1*(state_cost_846174003  > 0)
available_855261710  <- 1*(state_cost_855261710  > 0)
available_856063806  <- 1*(state_cost_856063806  > 0)
available_861121724  <- 1*(state_cost_861121724  > 0)
available_862503016  <- 1*(state_cost_862503016  > 0)
available_862513015  <- 1*(state_cost_862513015  > 0)
available_863103198  <- 1*(state_cost_863103198  > 0)
available_863903545  <- 1*(state_cost_863903545  > 0)
available_865063847  <- 1*(state_cost_865063847  > 0)
available_865073846  <- 1*(state_cost_865073846  > 0)
available_866375097  <- 1*(state_cost_866375097  > 0)
available_866692777  <- 1*(state_cost_866692777  > 0)
available_866702776  <- 1*(state_cost_866702776  > 0)
available_866722775  <- 1*(state_cost_866722775  > 0)
available_866922771  <- 1*(state_cost_866922771  > 0)
available_866932770  <- 1*(state_cost_866932770  > 0)
available_867964779  <- 1*(state_cost_867964779  > 0)
available_868074765  <- 1*(state_cost_868074765  > 0)
available_868814774  <- 1*(state_cost_868814774  > 0)
available_868834760  <- 1*(state_cost_868834760  > 0)
available_868844758  <- 1*(state_cost_868844758  > 0)
available_868854775  <- 1*(state_cost_868854775  > 0)
available_868864757  <- 1*(state_cost_868864757  > 0)
available_868874756  <- 1*(state_cost_868874756  > 0)
available_868884776  <- 1*(state_cost_868884776  > 0)
available_869164762  <- 1*(state_cost_869164762  > 0)
available_874082985  <- 1*(state_cost_874082985  > 0)
available_874092984  <- 1*(state_cost_874092984  > 0)
available_874851780  <- 1*(state_cost_874851780  > 0)
available_875861912  <- 1*(state_cost_875861912  > 0)
available_875961916  <- 1*(state_cost_875961916  > 0)
available_876431990  <- 1*(state_cost_876431990  > 0)
available_879363013  <- 1*(state_cost_879363013  > 0)
available_879373012  <- 1*(state_cost_879373012  > 0)
available_879383014  <- 1*(state_cost_879383014  > 0)
available_879973197  <- 1*(state_cost_879973197  > 0)
available_880363412  <- 1*(state_cost_880363412  > 0)
available_881473546  <- 1*(state_cost_881473546  > 0)
available_882943897  <- 1*(state_cost_882943897  > 0)
available_882963898  <- 1*(state_cost_882963898  > 0)
available_885564398  <- 1*(state_cost_885564398  > 0)
available_887665099  <- 1*(state_cost_887665099  > 0)
available_887675098  <- 1*(state_cost_887675098  > 0)
available_887685100  <- 1*(state_cost_887685100  > 0)
available_891211179  <- 1*(state_cost_891211179  > 0)
available_891541783  <- 1*(state_cost_891541783  > 0)
available_891751778  <- 1*(state_cost_891751778  > 0)
available_891912983  <- 1*(state_cost_891912983  > 0)
available_891932981  <- 1*(state_cost_891932981  > 0)
available_891942977  <- 1*(state_cost_891942977  > 0)
available_891962980  <- 1*(state_cost_891962980  > 0)
available_891972979  <- 1*(state_cost_891972979  > 0)
available_891982982  <- 1*(state_cost_891982982  > 0)
available_891992978  <- 1*(state_cost_891992978  > 0)
available_892153005  <- 1*(state_cost_892153005  > 0)
available_892421420  <- 1*(state_cost_892421420  > 0)
available_892781907  <- 1*(state_cost_892781907  > 0)
available_893863010  <- 1*(state_cost_893863010  > 0)
available_893873009  <- 1*(state_cost_893873009  > 0)
available_893883011  <- 1*(state_cost_893883011  > 0)
available_894473195  <- 1*(state_cost_894473195  > 0)
available_894963405  <- 1*(state_cost_894963405  > 0)
available_895331989  <- 1*(state_cost_895331989  > 0)
available_895773544  <- 1*(state_cost_895773544  > 0)
available_896174404  <- 1*(state_cost_896174404  > 0)
available_897864410  <- 1*(state_cost_897864410  > 0)
available_897874409  <- 1*(state_cost_897874409  > 0)
available_897884411  <- 1*(state_cost_897884411  > 0)
available_898362619  <- 1*(state_cost_898362619  > 0)
available_898372618  <- 1*(state_cost_898372618  > 0)
available_898875089  <- 1*(state_cost_898875089  > 0)
available_899165095  <- 1*(state_cost_899165095  > 0)
available_902836770  <- 1*(state_cost_902836770  > 0)
available_9046891337 <- 1*(state_cost_9046891337 > 0)
available_9368593476 <- 1*(state_cost_9368593476 > 0)
available_9369673634 <- 1*(state_cost_9369673634 > 0)
available_9465743475 <- 1*(state_cost_9465743475 > 0)
available_9885364401 <- 1*(state_cost_9885364401 > 0)

# The likelihood function
likelihood <- function(fc, b) {
  # Assign fixed parameters to named variables for convenience
  cc <- 1
  PRICE1 <- b[, cc]; cc <- cc + 1
  ASC9999       <- b[, cc]; cc <- cc + 1    
  ASC734190     <- b[, cc]; cc <- cc + 1
  ASC1573935    <- b[, cc]; cc <- cc + 1
  ASC2870505    <- b[, cc]; cc <- cc + 1
  ASC4356521    <- b[, cc]; cc <- cc + 1
  ASC4626831    <- b[, cc]; cc <- cc + 1
  ASC5915426    <- b[, cc]; cc <- cc + 1
  ASC6144352    <- b[, cc]; cc <- cc + 1
  ASC8417089    <- b[, cc]; cc <- cc + 1
  ASC8751020    <- b[, cc]; cc <- cc + 1
  ASC8920617    <- b[, cc]; cc <- cc + 1
  ASC10548697   <- b[, cc]; cc <- cc + 1
  ASC10549698   <- b[, cc]; cc <- cc + 1
  ASC10550695   <- b[, cc]; cc <- cc + 1
  ASC10553690   <- b[, cc]; cc <- cc + 1
  ASC10555696   <- b[, cc]; cc <- cc + 1
  ASC11586693   <- b[, cc]; cc <- cc + 1
  ASC11588694   <- b[, cc]; cc <- cc + 1
  ASC11771692   <- b[, cc]; cc <- cc + 1
  ASC11773688   <- b[, cc]; cc <- cc + 1
  ASC11774687   <- b[, cc]; cc <- cc + 1
  ASC11776686   <- b[, cc]; cc <- cc + 1
  ASC11777685   <- b[, cc]; cc <- cc + 1
  ASC11786699   <- b[, cc]; cc <- cc + 1
  ASC11788689   <- b[, cc]; cc <- cc + 1
  ASC13774351   <- b[, cc]; cc <- cc + 1
  ASC14001522   <- b[, cc]; cc <- cc + 1
  ASC15776973   <- b[, cc]; cc <- cc + 1
  ASC16516267   <- b[, cc]; cc <- cc + 1
  ASC16517266   <- b[, cc]; cc <- cc + 1
  ASC16518268   <- b[, cc]; cc <- cc + 1
  ASC16676562   <- b[, cc]; cc <- cc + 1
  ASC16987789   <- b[, cc]; cc <- cc + 1
  ASC16988790   <- b[, cc]; cc <- cc + 1
  ASC17086850   <- b[, cc]; cc <- cc + 1
  ASC17127981   <- b[, cc]; cc <- cc + 1
  ASC17991132   <- b[, cc]; cc <- cc + 1
  ASC18006841   <- b[, cc]; cc <- cc + 1
  ASC21480620   <- b[, cc]; cc <- cc + 1
  ASC22291300   <- b[, cc]; cc <- cc + 1
  ASC22783572   <- b[, cc]; cc <- cc + 1
  ASC22784571   <- b[, cc]; cc <- cc + 1
  ASC22786570   <- b[, cc]; cc <- cc + 1
  ASC22787569   <- b[, cc]; cc <- cc + 1
  ASC22788573   <- b[, cc]; cc <- cc + 1
  ASC27025847   <- b[, cc]; cc <- cc + 1
  ASC27474650   <- b[, cc]; cc <- cc + 1
  ASC28086580   <- b[, cc]; cc <- cc + 1
  ASC28087579   <- b[, cc]; cc <- cc + 1
  ASC28088582   <- b[, cc]; cc <- cc + 1
  ASC28206761   <- b[, cc]; cc <- cc + 1
  ASC28233766   <- b[, cc]; cc <- cc + 1
  ASC28236769   <- b[, cc]; cc <- cc + 1
  ASC29287548   <- b[, cc]; cc <- cc + 1
  ASC29288550   <- b[, cc]; cc <- cc + 1
  ASC29566892   <- b[, cc]; cc <- cc + 1
  ASC29568893   <- b[, cc]; cc <- cc + 1
  ASC31352818   <- b[, cc]; cc <- cc + 1
  ASC33102819   <- b[, cc]; cc <- cc + 1
  ASC34001179   <- b[, cc]; cc <- cc + 1
  ASC34004177   <- b[, cc]; cc <- cc + 1
  ASC34006175   <- b[, cc]; cc <- cc + 1
  ASC34007174   <- b[, cc]; cc <- cc + 1
  ASC34008178   <- b[, cc]; cc <- cc + 1
  ASC34014137   <- b[, cc]; cc <- cc + 1
  ASC34021113   <- b[, cc]; cc <- cc + 1
  ASC34026169   <- b[, cc]; cc <- cc + 1
  ASC34029146   <- b[, cc]; cc <- cc + 1
  ASC34030147   <- b[, cc]; cc <- cc + 1
  ASC34032148   <- b[, cc]; cc <- cc + 1    
  ASC34036168   <- b[, cc]; cc <- cc + 1
  ASC34052171   <- b[, cc]; cc <- cc + 1
  ASC34061172   <- b[, cc]; cc <- cc + 1
  ASC34076184   <- b[, cc]; cc <- cc + 1
  ASC34116159   <- b[, cc]; cc <- cc + 1
  ASC34117158   <- b[, cc]; cc <- cc + 1
  ASC34155616   <- b[, cc]; cc <- cc + 1
  ASC34162614   <- b[, cc]; cc <- cc + 1
  ASC35315561   <- b[, cc]; cc <- cc + 1
  ASC35316556   <- b[, cc]; cc <- cc + 1
  ASC35317555   <- b[, cc]; cc <- cc + 1
  ASC35318559   <- b[, cc]; cc <- cc + 1
  ASC35354162   <- b[, cc]; cc <- cc + 1
  ASC35416952   <- b[, cc]; cc <- cc + 1
  ASC35418953   <- b[, cc]; cc <- cc + 1
  ASC36572532   <- b[, cc]; cc <- cc + 1
  ASC40482820   <- b[, cc]; cc <- cc + 1
  ASC41271111   <- b[, cc]; cc <- cc + 1
  ASC41279896   <- b[, cc]; cc <- cc + 1
  ASC41284608   <- b[, cc]; cc <- cc + 1
  ASC41299880   <- b[, cc]; cc <- cc + 1
  ASC41316966   <- b[, cc]; cc <- cc + 1
  ASC41320957   <- b[, cc]; cc <- cc + 1
  ASC41413922   <- b[, cc]; cc <- cc + 1
  ASC41840902   <- b[, cc]; cc <- cc + 1
  ASC41927891   <- b[, cc]; cc <- cc + 1
  ASC42006282   <- b[, cc]; cc <- cc + 1
  ASC43022218   <- b[, cc]; cc <- cc + 1
  ASC43024215   <- b[, cc]; cc <- cc + 1
  ASC43025219   <- b[, cc]; cc <- cc + 1
  ASC43026214   <- b[, cc]; cc <- cc + 1
  ASC43027213   <- b[, cc]; cc <- cc + 1
  ASC43028216   <- b[, cc]; cc <- cc + 1
  ASC43031437   <- b[, cc]; cc <- cc + 1
  ASC43035438   <- b[, cc]; cc <- cc + 1
  ASC43036432   <- b[, cc]; cc <- cc + 1
  ASC43037431   <- b[, cc]; cc <- cc + 1
  ASC43038436   <- b[, cc]; cc <- cc + 1
  ASC43046475   <- b[, cc]; cc <- cc + 1
  ASC43048476   <- b[, cc]; cc <- cc + 1
  ASC43050426   <- b[, cc]; cc <- cc + 1
  ASC43051427   <- b[, cc]; cc <- cc + 1
  ASC43066467   <- b[, cc]; cc <- cc + 1
  ASC43076208   <- b[, cc]; cc <- cc + 1
  ASC43077207   <- b[, cc]; cc <- cc + 1
  ASC43116455   <- b[, cc]; cc <- cc + 1
  ASC43117454   <- b[, cc]; cc <- cc + 1
  ASC43120477   <- b[, cc]; cc <- cc + 1
  ASC43121483   <- b[, cc]; cc <- cc + 1
  ASC43123481   <- b[, cc]; cc <- cc + 1
  ASC43124480   <- b[, cc]; cc <- cc + 1
  ASC43125484   <- b[, cc]; cc <- cc + 1
  ASC43126479   <- b[, cc]; cc <- cc + 1
  ASC43127478   <- b[, cc]; cc <- cc + 1
  ASC43128482   <- b[, cc]; cc <- cc + 1
  ASC43136446   <- b[, cc]; cc <- cc + 1
  ASC43137443   <- b[, cc]; cc <- cc + 1
  ASC43138448   <- b[, cc]; cc <- cc + 1
  ASC43145469   <- b[, cc]; cc <- cc + 1
  ASC43156402   <- b[, cc]; cc <- cc + 1
  ASC43197488   <- b[, cc]; cc <- cc + 1
  ASC43205458   <- b[, cc]; cc <- cc + 1
  ASC43209415   <- b[, cc]; cc <- cc + 1
  ASC43984204   <- b[, cc]; cc <- cc + 1
  ASC43986205   <- b[, cc]; cc <- cc + 1
  ASC44217554   <- b[, cc]; cc <- cc + 1
  ASC44258701   <- b[, cc]; cc <- cc + 1
  ASC47161262   <- b[, cc]; cc <- cc + 1
  ASC47171261   <- b[, cc]; cc <- cc + 1    
  ASC48661746   <- b[, cc]; cc <- cc + 1
  ASC48671745   <- b[, cc]; cc <- cc + 1
  ASC48681749   <- b[, cc]; cc <- cc + 1
  ASC49361993   <- b[, cc]; cc <- cc + 1
  ASC50062247   <- b[, cc]; cc <- cc + 1
  ASC50362273   <- b[, cc]; cc <- cc + 1
  ASC50372272   <- b[, cc]; cc <- cc + 1
  ASC52882750   <- b[, cc]; cc <- cc + 1
  ASC52892748   <- b[, cc]; cc <- cc + 1
  ASC52902749   <- b[, cc]; cc <- cc + 1
  ASC53262930   <- b[, cc]; cc <- cc + 1
  ASC53272929   <- b[, cc]; cc <- cc + 1
  ASC53462949   <- b[, cc]; cc <- cc + 1
  ASC53472952   <- b[, cc]; cc <- cc + 1
  ASC53492951   <- b[, cc]; cc <- cc + 1
  ASC54056332   <- b[, cc]; cc <- cc + 1
  ASC54057331   <- b[, cc]; cc <- cc + 1
  ASC54646335   <- b[, cc]; cc <- cc + 1
  ASC54647334   <- b[, cc]; cc <- cc + 1
  ASC54863315   <- b[, cc]; cc <- cc + 1
  ASC55246362   <- b[, cc]; cc <- cc + 1
  ASC55963447   <- b[, cc]; cc <- cc + 1
  ASC56063448   <- b[, cc]; cc <- cc + 1
  ASC56828417   <- b[, cc]; cc <- cc + 1
  ASC56840464   <- b[, cc]; cc <- cc + 1
  ASC56843466   <- b[, cc]; cc <- cc + 1
  ASC56846465   <- b[, cc]; cc <- cc + 1
  ASC56850463   <- b[, cc]; cc <- cc + 1
  ASC56957542   <- b[, cc]; cc <- cc + 1
  ASC56958543   <- b[, cc]; cc <- cc + 1
  ASC57051553   <- b[, cc]; cc <- cc + 1
  ASC57092420   <- b[, cc]; cc <- cc + 1
  ASC64336401   <- b[, cc]; cc <- cc + 1
  ASC68022508   <- b[, cc]; cc <- cc + 1
  ASC68036500   <- b[, cc]; cc <- cc + 1
  ASC68037498   <- b[, cc]; cc <- cc + 1
  ASC68038502   <- b[, cc]; cc <- cc + 1
  ASC68039501   <- b[, cc]; cc <- cc + 1
  ASC68049509   <- b[, cc]; cc <- cc + 1
  ASC69971349   <- b[, cc]; cc <- cc + 1
  ASC69981351   <- b[, cc]; cc <- cc + 1
  ASC71886256   <- b[, cc]; cc <- cc + 1
  ASC72816991   <- b[, cc]; cc <- cc + 1
  ASC79026343   <- b[, cc]; cc <- cc + 1
  ASC80571355   <- b[, cc]; cc <- cc + 1
  ASC80574352   <- b[, cc]; cc <- cc + 1
  ASC80576351   <- b[, cc]; cc <- cc + 1
  ASC80577350   <- b[, cc]; cc <- cc + 1
  ASC80578354   <- b[, cc]; cc <- cc + 1
  ASC81966346   <- b[, cc]; cc <- cc + 1
  ASC82082628   <- b[, cc]; cc <- cc + 1
  ASC82126349   <- b[, cc]; cc <- cc + 1
  ASC82127348   <- b[, cc]; cc <- cc + 1
  ASC82147356   <- b[, cc]; cc <- cc + 1
  ASC82187359   <- b[, cc]; cc <- cc + 1
  ASC88253245   <- b[, cc]; cc <- cc + 1
  ASC88263244   <- b[, cc]; cc <- cc + 1
  ASC88273241   <- b[, cc]; cc <- cc + 1
  ASC88283243   <- b[, cc]; cc <- cc + 1
  ASC89139984   <- b[, cc]; cc <- cc + 1
  ASC92783722   <- b[, cc]; cc <- cc + 1
  ASC94263877   <- b[, cc]; cc <- cc + 1
  ASC94283878   <- b[, cc]; cc <- cc + 1
  ASC100064432  <- b[, cc]; cc <- cc + 1
  ASC100084433  <- b[, cc]; cc <- cc + 1
  ASC100094431  <- b[, cc]; cc <- cc + 1
  ASC102785180  <- b[, cc]; cc <- cc + 1
  ASC106251054  <- b[, cc]; cc <- cc + 1
  ASC106261050  <- b[, cc]; cc <- cc + 1    
  ASC106271048  <- b[, cc]; cc <- cc + 1
  ASC106281053  <- b[, cc]; cc <- cc + 1
  ASC108051512  <- b[, cc]; cc <- cc + 1
  ASC108071511  <- b[, cc]; cc <- cc + 1
  ASC108081510  <- b[, cc]; cc <- cc + 1
  ASC108181507  <- b[, cc]; cc <- cc + 1
  ASC108361481  <- b[, cc]; cc <- cc + 1
  ASC108902109  <- b[, cc]; cc <- cc + 1
  ASC112901489  <- b[, cc]; cc <- cc + 1
  ASC112931487  <- b[, cc]; cc <- cc + 1
  ASC112941486  <- b[, cc]; cc <- cc + 1
  ASC112961476  <- b[, cc]; cc <- cc + 1
  ASC112971485  <- b[, cc]; cc <- cc + 1
  ASC112981488  <- b[, cc]; cc <- cc + 1
  ASC113464508  <- b[, cc]; cc <- cc + 1
  ASC113474507  <- b[, cc]; cc <- cc + 1
  ASC113484509  <- b[, cc]; cc <- cc + 1
  ASC113661518  <- b[, cc]; cc <- cc + 1
  ASC119361068  <- b[, cc]; cc <- cc + 1
  ASC124041066  <- b[, cc]; cc <- cc + 1
  ASC124061065  <- b[, cc]; cc <- cc + 1
  ASC124071064  <- b[, cc]; cc <- cc + 1
  ASC124081067  <- b[, cc]; cc <- cc + 1
  ASC124641076  <- b[, cc]; cc <- cc + 1
  ASC124661075  <- b[, cc]; cc <- cc + 1
  ASC124671074  <- b[, cc]; cc <- cc + 1
  ASC124761073  <- b[, cc]; cc <- cc + 1
  ASC124781072  <- b[, cc]; cc <- cc + 1
  ASC124791071  <- b[, cc]; cc <- cc + 1
  ASC126681083  <- b[, cc]; cc <- cc + 1
  ASC128564285  <- b[, cc]; cc <- cc + 1
  ASC128884281  <- b[, cc]; cc <- cc + 1
  ASC130361080  <- b[, cc]; cc <- cc + 1
  ASC130371079  <- b[, cc]; cc <- cc + 1
  ASC130381081  <- b[, cc]; cc <- cc + 1
  ASC133883651  <- b[, cc]; cc <- cc + 1
  ASC136383284  <- b[, cc]; cc <- cc + 1
  ASC141923976  <- b[, cc]; cc <- cc + 1
  ASC141993977  <- b[, cc]; cc <- cc + 1
  ASC152465358  <- b[, cc]; cc <- cc + 1
  ASC152475357  <- b[, cc]; cc <- cc + 1
  ASC152485359  <- b[, cc]; cc <- cc + 1
  ASC152965362  <- b[, cc]; cc <- cc + 1
  ASC156262825  <- b[, cc]; cc <- cc + 1
  ASC156272824  <- b[, cc]; cc <- cc + 1
  ASC156282827  <- b[, cc]; cc <- cc + 1
  ASC156442826  <- b[, cc]; cc <- cc + 1
  ASC156774143  <- b[, cc]; cc <- cc + 1
  ASC159405139  <- b[, cc]; cc <- cc + 1
  ASC172061186  <- b[, cc]; cc <- cc + 1
  ASC178261887  <- b[, cc]; cc <- cc + 1
  ASC178301888  <- b[, cc]; cc <- cc + 1
  ASC179161934  <- b[, cc]; cc <- cc + 1
  ASC179561971  <- b[, cc]; cc <- cc + 1
  ASC179581973  <- b[, cc]; cc <- cc + 1
  ASC181961984  <- b[, cc]; cc <- cc + 1
  ASC181981986  <- b[, cc]; cc <- cc + 1
  ASC184072158  <- b[, cc]; cc <- cc + 1
  ASC184082159  <- b[, cc]; cc <- cc + 1
  ASC190262884  <- b[, cc]; cc <- cc + 1
  ASC190612905  <- b[, cc]; cc <- cc + 1
  ASC190632876  <- b[, cc]; cc <- cc + 1
  ASC190642874  <- b[, cc]; cc <- cc + 1
  ASC190662873  <- b[, cc]; cc <- cc + 1
  ASC190672872  <- b[, cc]; cc <- cc + 1
  ASC190682877  <- b[, cc]; cc <- cc + 1
  ASC190822906  <- b[, cc]; cc <- cc + 1
  ASC190962911  <- b[, cc]; cc <- cc + 1
  ASC191122890  <- b[, cc]; cc <- cc + 1    
  ASC192263131  <- b[, cc]; cc <- cc + 1
  ASC194763335  <- b[, cc]; cc <- cc + 1
  ASC194773333  <- b[, cc]; cc <- cc + 1
  ASC194783336  <- b[, cc]; cc <- cc + 1
  ASC194863332  <- b[, cc]; cc <- cc + 1
  ASC202463689  <- b[, cc]; cc <- cc + 1
  ASC202473688  <- b[, cc]; cc <- cc + 1
  ASC202483690  <- b[, cc]; cc <- cc + 1
  ASC202863693  <- b[, cc]; cc <- cc + 1
  ASC215954944  <- b[, cc]; cc <- cc + 1
  ASC215964941  <- b[, cc]; cc <- cc + 1
  ASC215974940  <- b[, cc]; cc <- cc + 1
  ASC215984943  <- b[, cc]; cc <- cc + 1
  ASC221215325  <- b[, cc]; cc <- cc + 1
  ASC221555322  <- b[, cc]; cc <- cc + 1
  ASC221565319  <- b[, cc]; cc <- cc + 1
  ASC221575318  <- b[, cc]; cc <- cc + 1
  ASC222135384  <- b[, cc]; cc <- cc + 1
  ASC222165385  <- b[, cc]; cc <- cc + 1
  ASC232771028  <- b[, cc]; cc <- cc + 1
  ASC232781030  <- b[, cc]; cc <- cc + 1
  ASC236262086  <- b[, cc]; cc <- cc + 1
  ASC237082092  <- b[, cc]; cc <- cc + 1
  ASC238232083  <- b[, cc]; cc <- cc + 1
  ASC238242082  <- b[, cc]; cc <- cc + 1
  ASC238262081  <- b[, cc]; cc <- cc + 1
  ASC238272080  <- b[, cc]; cc <- cc + 1
  ASC238282084  <- b[, cc]; cc <- cc + 1
  ASC241562463  <- b[, cc]; cc <- cc + 1
  ASC241572462  <- b[, cc]; cc <- cc + 1
  ASC241582464  <- b[, cc]; cc <- cc + 1
  ASC244543085  <- b[, cc]; cc <- cc + 1
  ASC244563084  <- b[, cc]; cc <- cc + 1
  ASC244573083  <- b[, cc]; cc <- cc + 1
  ASC244583087  <- b[, cc]; cc <- cc + 1
  ASC244663089  <- b[, cc]; cc <- cc + 1
  ASC247063452  <- b[, cc]; cc <- cc + 1
  ASC247283453  <- b[, cc]; cc <- cc + 1
  ASC256034475  <- b[, cc]; cc <- cc + 1
  ASC256044474  <- b[, cc]; cc <- cc + 1
  ASC256064473  <- b[, cc]; cc <- cc + 1
  ASC256074472  <- b[, cc]; cc <- cc + 1
  ASC256084476  <- b[, cc]; cc <- cc + 1
  ASC256164478  <- b[, cc]; cc <- cc + 1
  ASC258764867  <- b[, cc]; cc <- cc + 1
  ASC258774866  <- b[, cc]; cc <- cc + 1
  ASC258784869  <- b[, cc]; cc <- cc + 1
  ASC265862175  <- b[, cc]; cc <- cc + 1
  ASC266562181  <- b[, cc]; cc <- cc + 1
  ASC268202791  <- b[, cc]; cc <- cc + 1
  ASC268212792  <- b[, cc]; cc <- cc + 1
  ASC268222784  <- b[, cc]; cc <- cc + 1
  ASC268232789  <- b[, cc]; cc <- cc + 1
  ASC268242793  <- b[, cc]; cc <- cc + 1
  ASC268262788  <- b[, cc]; cc <- cc + 1
  ASC268272787  <- b[, cc]; cc <- cc + 1
  ASC268282790  <- b[, cc]; cc <- cc + 1
  ASC269062795  <- b[, cc]; cc <- cc + 1
  ASC270271374  <- b[, cc]; cc <- cc + 1
  ASC270562907  <- b[, cc]; cc <- cc + 1
  ASC271024938  <- b[, cc]; cc <- cc + 1
  ASC273922903  <- b[, cc]; cc <- cc + 1
  ASC274102893  <- b[, cc]; cc <- cc + 1
  ASC274541302  <- b[, cc]; cc <- cc + 1
  ASC274791372  <- b[, cc]; cc <- cc + 1
  ASC275444212  <- b[, cc]; cc <- cc + 1
  ASC275544501  <- b[, cc]; cc <- cc + 1
  ASC275624505  <- b[, cc]; cc <- cc + 1
  ASC275852626  <- b[, cc]; cc <- cc + 1    
  ASC286252492  <- b[, cc]; cc <- cc + 1
  ASC287184070  <- b[, cc]; cc <- cc + 1
  ASC288664915  <- b[, cc]; cc <- cc + 1
  ASC288674914  <- b[, cc]; cc <- cc + 1
  ASC288684918  <- b[, cc]; cc <- cc + 1
  ASC288904925  <- b[, cc]; cc <- cc + 1
  ASC297281031  <- b[, cc]; cc <- cc + 1
  ASC299932071  <- b[, cc]; cc <- cc + 1
  ASC299942070  <- b[, cc]; cc <- cc + 1
  ASC299962069  <- b[, cc]; cc <- cc + 1
  ASC299972068  <- b[, cc]; cc <- cc + 1
  ASC299982072  <- b[, cc]; cc <- cc + 1
  ASC300362067  <- b[, cc]; cc <- cc + 1
  ASC300562090  <- b[, cc]; cc <- cc + 1
  ASC300572089  <- b[, cc]; cc <- cc + 1
  ASC300582091  <- b[, cc]; cc <- cc + 1
  ASC302362196  <- b[, cc]; cc <- cc + 1
  ASC302382197  <- b[, cc]; cc <- cc + 1
  ASC303162321  <- b[, cc]; cc <- cc + 1
  ASC303172320  <- b[, cc]; cc <- cc + 1
  ASC303182322  <- b[, cc]; cc <- cc + 1
  ASC305262468  <- b[, cc]; cc <- cc + 1
  ASC305272467  <- b[, cc]; cc <- cc + 1
  ASC305282469  <- b[, cc]; cc <- cc + 1
  ASC311673458  <- b[, cc]; cc <- cc + 1
  ASC312083459  <- b[, cc]; cc <- cc + 1
  ASC314723602  <- b[, cc]; cc <- cc + 1
  ASC314733605  <- b[, cc]; cc <- cc + 1
  ASC314743603  <- b[, cc]; cc <- cc + 1
  ASC314753601  <- b[, cc]; cc <- cc + 1
  ASC316543821  <- b[, cc]; cc <- cc + 1
  ASC316563820  <- b[, cc]; cc <- cc + 1
  ASC316573819  <- b[, cc]; cc <- cc + 1
  ASC316583822  <- b[, cc]; cc <- cc + 1
  ASC316663823  <- b[, cc]; cc <- cc + 1
  ASC317183995  <- b[, cc]; cc <- cc + 1
  ASC317193994  <- b[, cc]; cc <- cc + 1
  ASC322314488  <- b[, cc]; cc <- cc + 1
  ASC322324485  <- b[, cc]; cc <- cc + 1
  ASC322334486  <- b[, cc]; cc <- cc + 1
  ASC322344484  <- b[, cc]; cc <- cc + 1
  ASC322354489  <- b[, cc]; cc <- cc + 1
  ASC322364483  <- b[, cc]; cc <- cc + 1
  ASC322374482  <- b[, cc]; cc <- cc + 1
  ASC322384487  <- b[, cc]; cc <- cc + 1
  ASC332564494  <- b[, cc]; cc <- cc + 1
  ASC336634891  <- b[, cc]; cc <- cc + 1
  ASC337163842  <- b[, cc]; cc <- cc + 1
  ASC337173841  <- b[, cc]; cc <- cc + 1
  ASC341641324  <- b[, cc]; cc <- cc + 1
  ASC341971322  <- b[, cc]; cc <- cc + 1
  ASC341981344  <- b[, cc]; cc <- cc + 1
  ASC343592414  <- b[, cc]; cc <- cc + 1
  ASC343662146  <- b[, cc]; cc <- cc + 1
  ASC343682147  <- b[, cc]; cc <- cc + 1
  ASC344222417  <- b[, cc]; cc <- cc + 1
  ASC344232415  <- b[, cc]; cc <- cc + 1
  ASC344252418  <- b[, cc]; cc <- cc + 1
  ASC344332416  <- b[, cc]; cc <- cc + 1
  ASC344493093  <- b[, cc]; cc <- cc + 1
  ASC344563096  <- b[, cc]; cc <- cc + 1
  ASC344573094  <- b[, cc]; cc <- cc + 1
  ASC344583097  <- b[, cc]; cc <- cc + 1
  ASC345463959  <- b[, cc]; cc <- cc + 1
  ASC345663961  <- b[, cc]; cc <- cc + 1
  ASC345784107  <- b[, cc]; cc <- cc + 1
  ASC345794105  <- b[, cc]; cc <- cc + 1
  ASC346904740  <- b[, cc]; cc <- cc + 1
  ASC347464848  <- b[, cc]; cc <- cc + 1    
  ASC347474844  <- b[, cc]; cc <- cc + 1
  ASC347484849  <- b[, cc]; cc <- cc + 1
  ASC347864825  <- b[, cc]; cc <- cc + 1
  ASC348174871  <- b[, cc]; cc <- cc + 1
  ASC348204895  <- b[, cc]; cc <- cc + 1
  ASC348214898  <- b[, cc]; cc <- cc + 1
  ASC348234877  <- b[, cc]; cc <- cc + 1
  ASC348564927  <- b[, cc]; cc <- cc + 1
  ASC348714847  <- b[, cc]; cc <- cc + 1
  ASC348814855  <- b[, cc]; cc <- cc + 1
  ASC349195043  <- b[, cc]; cc <- cc + 1
  ASC349355042  <- b[, cc]; cc <- cc + 1
  ASC349725007  <- b[, cc]; cc <- cc + 1
  ASC349955017  <- b[, cc]; cc <- cc + 1
  ASC352134741  <- b[, cc]; cc <- cc + 1
  ASC356264111  <- b[, cc]; cc <- cc + 1
  ASC356284112  <- b[, cc]; cc <- cc + 1
  ASC356993944  <- b[, cc]; cc <- cc + 1
  ASC357804051  <- b[, cc]; cc <- cc + 1
  ASC359132078  <- b[, cc]; cc <- cc + 1
  ASC359142077  <- b[, cc]; cc <- cc + 1
  ASC359162076  <- b[, cc]; cc <- cc + 1
  ASC359172075  <- b[, cc]; cc <- cc + 1
  ASC359182079  <- b[, cc]; cc <- cc + 1
  ASC359262073  <- b[, cc]; cc <- cc + 1
  ASC359442095  <- b[, cc]; cc <- cc + 1
  ASC359462094  <- b[, cc]; cc <- cc + 1
  ASC359472093  <- b[, cc]; cc <- cc + 1
  ASC359482096  <- b[, cc]; cc <- cc + 1
  ASC359645019  <- b[, cc]; cc <- cc + 1
  ASC361862324  <- b[, cc]; cc <- cc + 1
  ASC361882325  <- b[, cc]; cc <- cc + 1
  ASC363012478  <- b[, cc]; cc <- cc + 1
  ASC363042475  <- b[, cc]; cc <- cc + 1
  ASC363052474  <- b[, cc]; cc <- cc + 1
  ASC363072473  <- b[, cc]; cc <- cc + 1
  ASC363082477  <- b[, cc]; cc <- cc + 1
  ASC364472674  <- b[, cc]; cc <- cc + 1
  ASC366673145  <- b[, cc]; cc <- cc + 1
  ASC366683146  <- b[, cc]; cc <- cc + 1
  ASC368711323  <- b[, cc]; cc <- cc + 1
  ASC368863465  <- b[, cc]; cc <- cc + 1
  ASC368873467  <- b[, cc]; cc <- cc + 1
  ASC369013468  <- b[, cc]; cc <- cc + 1
  ASC369033466  <- b[, cc]; cc <- cc + 1
  ASC369043469  <- b[, cc]; cc <- cc + 1
  ASC369073471  <- b[, cc]; cc <- cc + 1
  ASC369083470  <- b[, cc]; cc <- cc + 1
  ASC369683635  <- b[, cc]; cc <- cc + 1
  ASC369693633  <- b[, cc]; cc <- cc + 1
  ASC369713637  <- b[, cc]; cc <- cc + 1
  ASC369743645  <- b[, cc]; cc <- cc + 1
  ASC369763644  <- b[, cc]; cc <- cc + 1
  ASC369783647  <- b[, cc]; cc <- cc + 1
  ASC371463754  <- b[, cc]; cc <- cc + 1
  ASC372173850  <- b[, cc]; cc <- cc + 1
  ASC373365237  <- b[, cc]; cc <- cc + 1
  ASC373385241  <- b[, cc]; cc <- cc + 1
  ASC373395239  <- b[, cc]; cc <- cc + 1
  ASC373464016  <- b[, cc]; cc <- cc + 1
  ASC373474014  <- b[, cc]; cc <- cc + 1
  ASC373484017  <- b[, cc]; cc <- cc + 1
  ASC373564018  <- b[, cc]; cc <- cc + 1
  ASC374134136  <- b[, cc]; cc <- cc + 1
  ASC374144135  <- b[, cc]; cc <- cc + 1
  ASC374164134  <- b[, cc]; cc <- cc + 1
  ASC374174133  <- b[, cc]; cc <- cc + 1
  ASC374184137  <- b[, cc]; cc <- cc + 1
  ASC374264138  <- b[, cc]; cc <- cc + 1    
  ASC376404291  <- b[, cc]; cc <- cc + 1
  ASC376424292  <- b[, cc]; cc <- cc + 1
  ASC378864512  <- b[, cc]; cc <- cc + 1
  ASC378884513  <- b[, cc]; cc <- cc + 1
  ASC379344565  <- b[, cc]; cc <- cc + 1
  ASC379354567  <- b[, cc]; cc <- cc + 1
  ASC379364564  <- b[, cc]; cc <- cc + 1
  ASC379374563  <- b[, cc]; cc <- cc + 1
  ASC379384566  <- b[, cc]; cc <- cc + 1
  ASC379864600  <- b[, cc]; cc <- cc + 1
  ASC379874598  <- b[, cc]; cc <- cc + 1
  ASC379884601  <- b[, cc]; cc <- cc + 1
  ASC379914710  <- b[, cc]; cc <- cc + 1
  ASC379934708  <- b[, cc]; cc <- cc + 1
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
  ASC398664701  <- b[, cc]; cc <- cc + 1
  ASC401181854  <- b[, cc]; cc <- cc + 1
  ASC401933622  <- b[, cc]; cc <- cc + 1
  ASC402973593  <- b[, cc]; cc <- cc + 1
  ASC403133619  <- b[, cc]; cc <- cc + 1
  ASC403945197  <- b[, cc]; cc <- cc + 1
  ASC404115227  <- b[, cc]; cc <- cc + 1
  ASC405923616  <- b[, cc]; cc <- cc + 1
  ASC405933617  <- b[, cc]; cc <- cc + 1
  ASC405943615  <- b[, cc]; cc <- cc + 1
  ASC405983628  <- b[, cc]; cc <- cc + 1
  ASC405993626  <- b[, cc]; cc <- cc + 1
  ASC406044636  <- b[, cc]; cc <- cc + 1
  ASC406144352  <- b[, cc]; cc <- cc + 1
  ASC406255200  <- b[, cc]; cc <- cc + 1
  ASC406325242  <- b[, cc]; cc <- cc + 1
  ASC409264670  <- b[, cc]; cc <- cc + 1
  ASC409304631  <- b[, cc]; cc <- cc + 1
  ASC409464717  <- b[, cc]; cc <- cc + 1
  ASC409564691  <- b[, cc]; cc <- cc + 1
  ASC409654718  <- b[, cc]; cc <- cc + 1
  ASC409764615  <- b[, cc]; cc <- cc + 1
  ASC410762864  <- b[, cc]; cc <- cc + 1
  ASC410772862  <- b[, cc]; cc <- cc + 1
  ASC410782865  <- b[, cc]; cc <- cc + 1
  ASC412092062  <- b[, cc]; cc <- cc + 1
  ASC413605215  <- b[, cc]; cc <- cc + 1
  ASC415002465  <- b[, cc]; cc <- cc + 1
  ASC415924639  <- b[, cc]; cc <- cc + 1
  ASC416015183  <- b[, cc]; cc <- cc + 1
  ASC416045206  <- b[, cc]; cc <- cc + 1
  ASC416255207  <- b[, cc]; cc <- cc + 1
  ASC416535202  <- b[, cc]; cc <- cc + 1
  ASC416815221  <- b[, cc]; cc <- cc + 1
  ASC416885191  <- b[, cc]; cc <- cc + 1
  ASC416925185  <- b[, cc]; cc <- cc + 1
  ASC416935184  <- b[, cc]; cc <- cc + 1
  ASC416945182  <- b[, cc]; cc <- cc + 1
  ASC416965209  <- b[, cc]; cc <- cc + 1
  ASC416985210  <- b[, cc]; cc <- cc + 1
  ASC417015211  <- b[, cc]; cc <- cc + 1
  ASC417045220  <- b[, cc]; cc <- cc + 1
  ASC417055219  <- b[, cc]; cc <- cc + 1
  ASC417073998  <- b[, cc]; cc <- cc + 1
  ASC417154626  <- b[, cc]; cc <- cc + 1    
  ASC417194633  <- b[, cc]; cc <- cc + 1
  ASC417464623  <- b[, cc]; cc <- cc + 1
  ASC417474622  <- b[, cc]; cc <- cc + 1
  ASC417835190  <- b[, cc]; cc <- cc + 1
  ASC417964664  <- b[, cc]; cc <- cc + 1
  ASC417974663  <- b[, cc]; cc <- cc + 1
  ASC418064700  <- b[, cc]; cc <- cc + 1
  ASC418461979  <- b[, cc]; cc <- cc + 1
  ASC419895193  <- b[, cc]; cc <- cc + 1
  ASC420025194  <- b[, cc]; cc <- cc + 1
  ASC420095196  <- b[, cc]; cc <- cc + 1
  ASC420795195  <- b[, cc]; cc <- cc + 1
  ASC421663588  <- b[, cc]; cc <- cc + 1
  ASC421673587  <- b[, cc]; cc <- cc + 1
  ASC423123372  <- b[, cc]; cc <- cc + 1
  ASC424444531  <- b[, cc]; cc <- cc + 1
  ASC426663583  <- b[, cc]; cc <- cc + 1
  ASC426763366  <- b[, cc]; cc <- cc + 1
  ASC426873343  <- b[, cc]; cc <- cc + 1
  ASC426993362  <- b[, cc]; cc <- cc + 1
  ASC427033371  <- b[, cc]; cc <- cc + 1
  ASC427123353  <- b[, cc]; cc <- cc + 1
  ASC427163350  <- b[, cc]; cc <- cc + 1
  ASC427173349  <- b[, cc]; cc <- cc + 1
  ASC427183352  <- b[, cc]; cc <- cc + 1
  ASC429845112  <- b[, cc]; cc <- cc + 1
  ASC432191119  <- b[, cc]; cc <- cc + 1
  ASC432441094  <- b[, cc]; cc <- cc + 1
  ASC432821121  <- b[, cc]; cc <- cc + 1
  ASC432851114  <- b[, cc]; cc <- cc + 1
  ASC432871118  <- b[, cc]; cc <- cc + 1
  ASC432961130  <- b[, cc]; cc <- cc + 1
  ASC433021096  <- b[, cc]; cc <- cc + 1
  ASC433161127  <- b[, cc]; cc <- cc + 1
  ASC433281141  <- b[, cc]; cc <- cc + 1
  ASC433301142  <- b[, cc]; cc <- cc + 1
  ASC433311136  <- b[, cc]; cc <- cc + 1
  ASC433331137  <- b[, cc]; cc <- cc + 1
  ASC433341134  <- b[, cc]; cc <- cc + 1
  ASC433351138  <- b[, cc]; cc <- cc + 1
  ASC433361110  <- b[, cc]; cc <- cc + 1
  ASC433371133  <- b[, cc]; cc <- cc + 1
  ASC433381135  <- b[, cc]; cc <- cc + 1
  ASC433861169  <- b[, cc]; cc <- cc + 1
  ASC433871168  <- b[, cc]; cc <- cc + 1
  ASC433881170  <- b[, cc]; cc <- cc + 1
  ASC434091115  <- b[, cc]; cc <- cc + 1
  ASC434101116  <- b[, cc]; cc <- cc + 1
  ASC434151107  <- b[, cc]; cc <- cc + 1
  ASC434181139  <- b[, cc]; cc <- cc + 1
  ASC434261790  <- b[, cc]; cc <- cc + 1
  ASC438484334  <- b[, cc]; cc <- cc + 1
  ASC443501003  <- b[, cc]; cc <- cc + 1
  ASC444191536  <- b[, cc]; cc <- cc + 1
  ASC444561561  <- b[, cc]; cc <- cc + 1
  ASC444761542  <- b[, cc]; cc <- cc + 1
  ASC444991554  <- b[, cc]; cc <- cc + 1
  ASC445161553  <- b[, cc]; cc <- cc + 1
  ASC445201565  <- b[, cc]; cc <- cc + 1
  ASC446032066  <- b[, cc]; cc <- cc + 1
  ASC446573204  <- b[, cc]; cc <- cc + 1
  ASC446583206  <- b[, cc]; cc <- cc + 1
  ASC452453828  <- b[, cc]; cc <- cc + 1
  ASC452463826  <- b[, cc]; cc <- cc + 1
  ASC452473825  <- b[, cc]; cc <- cc + 1
  ASC452483827  <- b[, cc]; cc <- cc + 1
  ASC452753858  <- b[, cc]; cc <- cc + 1
  ASC452763855  <- b[, cc]; cc <- cc + 1
  ASC452773854  <- b[, cc]; cc <- cc + 1    
  ASC452783857  <- b[, cc]; cc <- cc + 1
  ASC454184020  <- b[, cc]; cc <- cc + 1
  ASC454194019  <- b[, cc]; cc <- cc + 1
  ASC458844377  <- b[, cc]; cc <- cc + 1
  ASC458864376  <- b[, cc]; cc <- cc + 1
  ASC458874375  <- b[, cc]; cc <- cc + 1
  ASC458884378  <- b[, cc]; cc <- cc + 1
  ASC463502472  <- b[, cc]; cc <- cc + 1
  ASC463512471  <- b[, cc]; cc <- cc + 1
  ASC465043156  <- b[, cc]; cc <- cc + 1
  ASC465063157  <- b[, cc]; cc <- cc + 1
  ASC477851437  <- b[, cc]; cc <- cc + 1
  ASC477861438  <- b[, cc]; cc <- cc + 1
  ASC480992500  <- b[, cc]; cc <- cc + 1
  ASC481022508  <- b[, cc]; cc <- cc + 1
  ASC481032505  <- b[, cc]; cc <- cc + 1
  ASC481042504  <- b[, cc]; cc <- cc + 1
  ASC481052499  <- b[, cc]; cc <- cc + 1
  ASC481062503  <- b[, cc]; cc <- cc + 1
  ASC490864235  <- b[, cc]; cc <- cc + 1
  ASC491854246  <- b[, cc]; cc <- cc + 1
  ASC491864248  <- b[, cc]; cc <- cc + 1
  ASC491894249  <- b[, cc]; cc <- cc + 1
  ASC506864806  <- b[, cc]; cc <- cc + 1
  ASC516964816  <- b[, cc]; cc <- cc + 1
  ASC521461289  <- b[, cc]; cc <- cc + 1
  ASC521861292  <- b[, cc]; cc <- cc + 1
  ASC523121283  <- b[, cc]; cc <- cc + 1
  ASC523141284  <- b[, cc]; cc <- cc + 1
  ASC523161282  <- b[, cc]; cc <- cc + 1
  ASC523171287  <- b[, cc]; cc <- cc + 1
  ASC523181285  <- b[, cc]; cc <- cc + 1
  ASC523191286  <- b[, cc]; cc <- cc + 1
  ASC525631877  <- b[, cc]; cc <- cc + 1
  ASC525801874  <- b[, cc]; cc <- cc + 1
  ASC525811875  <- b[, cc]; cc <- cc + 1
  ASC525821873  <- b[, cc]; cc <- cc + 1
  ASC525931865  <- b[, cc]; cc <- cc + 1
  ASC525941866  <- b[, cc]; cc <- cc + 1
  ASC525951870  <- b[, cc]; cc <- cc + 1
  ASC525961862  <- b[, cc]; cc <- cc + 1
  ASC525971869  <- b[, cc]; cc <- cc + 1
  ASC525981863  <- b[, cc]; cc <- cc + 1
  ASC525991868  <- b[, cc]; cc <- cc + 1
  ASC528063140  <- b[, cc]; cc <- cc + 1
  ASC528353142  <- b[, cc]; cc <- cc + 1
  ASC532103914  <- b[, cc]; cc <- cc + 1
  ASC532113924  <- b[, cc]; cc <- cc + 1
  ASC532133912  <- b[, cc]; cc <- cc + 1
  ASC532143913  <- b[, cc]; cc <- cc + 1
  ASC532163911  <- b[, cc]; cc <- cc + 1
  ASC532183915  <- b[, cc]; cc <- cc + 1
  ASC544363564  <- b[, cc]; cc <- cc + 1
  ASC544463799  <- b[, cc]; cc <- cc + 1
  ASC544473798  <- b[, cc]; cc <- cc + 1
  ASC544483801  <- b[, cc]; cc <- cc + 1
  ASC547061635  <- b[, cc]; cc <- cc + 1
  ASC550663565  <- b[, cc]; cc <- cc + 1
  ASC550683566  <- b[, cc]; cc <- cc + 1
  ASC550843804  <- b[, cc]; cc <- cc + 1
  ASC550863803  <- b[, cc]; cc <- cc + 1
  ASC550873802  <- b[, cc]; cc <- cc + 1
  ASC550883805  <- b[, cc]; cc <- cc + 1
  ASC551063991  <- b[, cc]; cc <- cc + 1
  ASC555063810  <- b[, cc]; cc <- cc + 1
  ASC561953918  <- b[, cc]; cc <- cc + 1
  ASC561963916  <- b[, cc]; cc <- cc + 1
  ASC562063832  <- b[, cc]; cc <- cc + 1
  ASC571201242  <- b[, cc]; cc <- cc + 1    
  ASC571251241  <- b[, cc]; cc <- cc + 1
  ASC571291244  <- b[, cc]; cc <- cc + 1
  ASC571441252  <- b[, cc]; cc <- cc + 1
  ASC571481243  <- b[, cc]; cc <- cc + 1
  ASC571571246  <- b[, cc]; cc <- cc + 1
  ASC571581251  <- b[, cc]; cc <- cc + 1
  ASC571611240  <- b[, cc]; cc <- cc + 1
  ASC571741250  <- b[, cc]; cc <- cc + 1
  ASC588011108  <- b[, cc]; cc <- cc + 1
  ASC588352963  <- b[, cc]; cc <- cc + 1
  ASC588362961  <- b[, cc]; cc <- cc + 1
  ASC588382962  <- b[, cc]; cc <- cc + 1
  ASC588402957  <- b[, cc]; cc <- cc + 1
  ASC588422975  <- b[, cc]; cc <- cc + 1
  ASC588602998  <- b[, cc]; cc <- cc + 1
  ASC588682969  <- b[, cc]; cc <- cc + 1
  ASC588722960  <- b[, cc]; cc <- cc + 1
  ASC588732971  <- b[, cc]; cc <- cc + 1
  ASC588752994  <- b[, cc]; cc <- cc + 1
  ASC588762993  <- b[, cc]; cc <- cc + 1
  ASC588852997  <- b[, cc]; cc <- cc + 1
  ASC588862996  <- b[, cc]; cc <- cc + 1
  ASC590371737  <- b[, cc]; cc <- cc + 1
  ASC591001960  <- b[, cc]; cc <- cc + 1
  ASC592312659  <- b[, cc]; cc <- cc + 1
  ASC620613048  <- b[, cc]; cc <- cc + 1
  ASC620973829  <- b[, cc]; cc <- cc + 1
  ASC624003402  <- b[, cc]; cc <- cc + 1
  ASC624053409  <- b[, cc]; cc <- cc + 1
  ASC624203414  <- b[, cc]; cc <- cc + 1
  ASC633554549  <- b[, cc]; cc <- cc + 1
  ASC633574553  <- b[, cc]; cc <- cc + 1
  ASC633594561  <- b[, cc]; cc <- cc + 1
  ASC635284382  <- b[, cc]; cc <- cc + 1
  ASC635304384  <- b[, cc]; cc <- cc + 1
  ASC637554961  <- b[, cc]; cc <- cc + 1
  ASC639595188  <- b[, cc]; cc <- cc + 1
  ASC639635214  <- b[, cc]; cc <- cc + 1
  ASC641361756  <- b[, cc]; cc <- cc + 1
  ASC645731333  <- b[, cc]; cc <- cc + 1
  ASC646011145  <- b[, cc]; cc <- cc + 1
  ASC646761226  <- b[, cc]; cc <- cc + 1
  ASC647111326  <- b[, cc]; cc <- cc + 1
  ASC647121331  <- b[, cc]; cc <- cc + 1
  ASC647161327  <- b[, cc]; cc <- cc + 1
  ASC647361348  <- b[, cc]; cc <- cc + 1
  ASC647551338  <- b[, cc]; cc <- cc + 1
  ASC647591339  <- b[, cc]; cc <- cc + 1
  ASC647761385  <- b[, cc]; cc <- cc + 1
  ASC648582044  <- b[, cc]; cc <- cc + 1
  ASC648622040  <- b[, cc]; cc <- cc + 1
  ASC648632042  <- b[, cc]; cc <- cc + 1
  ASC648642046  <- b[, cc]; cc <- cc + 1
  ASC648652045  <- b[, cc]; cc <- cc + 1
  ASC648662041  <- b[, cc]; cc <- cc + 1
  ASC648672039  <- b[, cc]; cc <- cc + 1
  ASC648682043  <- b[, cc]; cc <- cc + 1
  ASC648702047  <- b[, cc]; cc <- cc + 1
  ASC648761840  <- b[, cc]; cc <- cc + 1
  ASC649962135  <- b[, cc]; cc <- cc + 1
  ASC650662318  <- b[, cc]; cc <- cc + 1
  ASC651262343  <- b[, cc]; cc <- cc + 1
  ASC651272341  <- b[, cc]; cc <- cc + 1
  ASC651952633  <- b[, cc]; cc <- cc + 1
  ASC651994947  <- b[, cc]; cc <- cc + 1
  ASC652004948  <- b[, cc]; cc <- cc + 1
  ASC652482816  <- b[, cc]; cc <- cc + 1
  ASC652512814  <- b[, cc]; cc <- cc + 1
  ASC652532812  <- b[, cc]; cc <- cc + 1    
  ASC652542810  <- b[, cc]; cc <- cc + 1
  ASC652562809  <- b[, cc]; cc <- cc + 1
  ASC652572808  <- b[, cc]; cc <- cc + 1
  ASC652582813  <- b[, cc]; cc <- cc + 1
  ASC666364310  <- b[, cc]; cc <- cc + 1
  ASC668364802  <- b[, cc]; cc <- cc + 1
  ASC669362356  <- b[, cc]; cc <- cc + 1
  ASC670063521  <- b[, cc]; cc <- cc + 1
  ASC672665510  <- b[, cc]; cc <- cc + 1
  ASC672675509  <- b[, cc]; cc <- cc + 1
  ASC675263042  <- b[, cc]; cc <- cc + 1
  ASC675273041  <- b[, cc]; cc <- cc + 1
  ASC675283044  <- b[, cc]; cc <- cc + 1
  ASC675563070  <- b[, cc]; cc <- cc + 1
  ASC675573069  <- b[, cc]; cc <- cc + 1
  ASC675863071  <- b[, cc]; cc <- cc + 1
  ASC681261155  <- b[, cc]; cc <- cc + 1
  ASC681271154  <- b[, cc]; cc <- cc + 1
  ASC683061942  <- b[, cc]; cc <- cc + 1
  ASC686113450  <- b[, cc]; cc <- cc + 1
  ASC688464799  <- b[, cc]; cc <- cc + 1
  ASC696111809  <- b[, cc]; cc <- cc + 1
  ASC696341816  <- b[, cc]; cc <- cc + 1
  ASC696361815  <- b[, cc]; cc <- cc + 1
  ASC696371814  <- b[, cc]; cc <- cc + 1
  ASC696381817  <- b[, cc]; cc <- cc + 1
  ASC696561822  <- b[, cc]; cc <- cc + 1
  ASC696571821  <- b[, cc]; cc <- cc + 1
  ASC696581823  <- b[, cc]; cc <- cc + 1
  ASC696661833  <- b[, cc]; cc <- cc + 1
  ASC696671832  <- b[, cc]; cc <- cc + 1
  ASC699404353  <- b[, cc]; cc <- cc + 1
  ASC699454355  <- b[, cc]; cc <- cc + 1
  ASC699464356  <- b[, cc]; cc <- cc + 1
  ASC699474354  <- b[, cc]; cc <- cc + 1
  ASC727221293  <- b[, cc]; cc <- cc + 1
  ASC730504347  <- b[, cc]; cc <- cc + 1
  ASC730514350  <- b[, cc]; cc <- cc + 1
  ASC730524343  <- b[, cc]; cc <- cc + 1
  ASC730534345  <- b[, cc]; cc <- cc + 1
  ASC730544346  <- b[, cc]; cc <- cc + 1
  ASC730554344  <- b[, cc]; cc <- cc + 1
  ASC731361254  <- b[, cc]; cc <- cc + 1
  ASC734561684  <- b[, cc]; cc <- cc + 1
  ASC735171645  <- b[, cc]; cc <- cc + 1
  ASC735261686  <- b[, cc]; cc <- cc + 1
  ASC737021965  <- b[, cc]; cc <- cc + 1
  ASC737151958  <- b[, cc]; cc <- cc + 1
  ASC739861750  <- b[, cc]; cc <- cc + 1
  ASC740862313  <- b[, cc]; cc <- cc + 1
  ASC747822800  <- b[, cc]; cc <- cc + 1
  ASC750873008  <- b[, cc]; cc <- cc + 1
  ASC752083115  <- b[, cc]; cc <- cc + 1
  ASC752103114  <- b[, cc]; cc <- cc + 1
  ASC752123110  <- b[, cc]; cc <- cc + 1
  ASC752143108  <- b[, cc]; cc <- cc + 1
  ASC752163111  <- b[, cc]; cc <- cc + 1
  ASC760323516  <- b[, cc]; cc <- cc + 1
  ASC760363507  <- b[, cc]; cc <- cc + 1
  ASC760383514  <- b[, cc]; cc <- cc + 1
  ASC760403520  <- b[, cc]; cc <- cc + 1
  ASC760423511  <- b[, cc]; cc <- cc + 1
  ASC760443513  <- b[, cc]; cc <- cc + 1
  ASC762273543  <- b[, cc]; cc <- cc + 1
  ASC764363723  <- b[, cc]; cc <- cc + 1
  ASC764783788  <- b[, cc]; cc <- cc + 1
  ASC764863795  <- b[, cc]; cc <- cc + 1
  ASC764873794  <- b[, cc]; cc <- cc + 1
  ASC764883796  <- b[, cc]; cc <- cc + 1    
  ASC765263831  <- b[, cc]; cc <- cc + 1
  ASC767673999  <- b[, cc]; cc <- cc + 1
  ASC773184326  <- b[, cc]; cc <- cc + 1
  ASC773754673  <- b[, cc]; cc <- cc + 1
  ASC773784677  <- b[, cc]; cc <- cc + 1
  ASC773954559  <- b[, cc]; cc <- cc + 1
  ASC774724901  <- b[, cc]; cc <- cc + 1
  ASC774724901  <- b[, cc]; cc <- cc + 1
  ASC774875094  <- b[, cc]; cc <- cc + 1
  ASC777355268  <- b[, cc]; cc <- cc + 1
  ASC777404660  <- b[, cc]; cc <- cc + 1
  ASC777745327  <- b[, cc]; cc <- cc + 1
  ASC777765329  <- b[, cc]; cc <- cc + 1
  ASC777865135  <- b[, cc]; cc <- cc + 1
  ASC778424689  <- b[, cc]; cc <- cc + 1
  ASC778474697  <- b[, cc]; cc <- cc + 1
  ASC778524716  <- b[, cc]; cc <- cc + 1
  ASC784563813  <- b[, cc]; cc <- cc + 1
  ASC788663814  <- b[, cc]; cc <- cc + 1
  ASC793363815  <- b[, cc]; cc <- cc + 1
  ASC798163816  <- b[, cc]; cc <- cc + 1
  ASC800961711  <- b[, cc]; cc <- cc + 1
  ASC801452153  <- b[, cc]; cc <- cc + 1
  ASC802362680  <- b[, cc]; cc <- cc + 1
  ASC804404276  <- b[, cc]; cc <- cc + 1
  ASC804564370  <- b[, cc]; cc <- cc + 1
  ASC804574369  <- b[, cc]; cc <- cc + 1
  ASC804584371  <- b[, cc]; cc <- cc + 1
  ASC806861638  <- b[, cc]; cc <- cc + 1
  ASC806871637  <- b[, cc]; cc <- cc + 1
  ASC808262658  <- b[, cc]; cc <- cc + 1
  ASC808272657  <- b[, cc]; cc <- cc + 1
  ASC811963839  <- b[, cc]; cc <- cc + 1
  ASC812043837  <- b[, cc]; cc <- cc + 1
  ASC812063836  <- b[, cc]; cc <- cc + 1
  ASC812073835  <- b[, cc]; cc <- cc + 1
  ASC812083838  <- b[, cc]; cc <- cc + 1
  ASC814065047  <- b[, cc]; cc <- cc + 1
  ASC826061716  <- b[, cc]; cc <- cc + 1
  ASC826071715  <- b[, cc]; cc <- cc + 1
  ASC826101669  <- b[, cc]; cc <- cc + 1
  ASC826111670  <- b[, cc]; cc <- cc + 1
  ASC826181673  <- b[, cc]; cc <- cc + 1
  ASC826261648  <- b[, cc]; cc <- cc + 1
  ASC826271647  <- b[, cc]; cc <- cc + 1
  ASC826361667  <- b[, cc]; cc <- cc + 1
  ASC826371666  <- b[, cc]; cc <- cc + 1
  ASC827861643  <- b[, cc]; cc <- cc + 1
  ASC827871639  <- b[, cc]; cc <- cc + 1
  ASC828201723  <- b[, cc]; cc <- cc + 1
  ASC828261702  <- b[, cc]; cc <- cc + 1
  ASC828271708  <- b[, cc]; cc <- cc + 1
  ASC828401700  <- b[, cc]; cc <- cc + 1
  ASC828461681  <- b[, cc]; cc <- cc + 1
  ASC828471697  <- b[, cc]; cc <- cc + 1
  ASC828491699  <- b[, cc]; cc <- cc + 1
  ASC828661729  <- b[, cc]; cc <- cc + 1
  ASC828671728  <- b[, cc]; cc <- cc + 1
  ASC828811672  <- b[, cc]; cc <- cc + 1
  ASC829572064  <- b[, cc]; cc <- cc + 1
  ASC833632660  <- b[, cc]; cc <- cc + 1
  ASC833702664  <- b[, cc]; cc <- cc + 1
  ASC839063438  <- b[, cc]; cc <- cc + 1
  ASC839073437  <- b[, cc]; cc <- cc + 1
  ASC844563834  <- b[, cc]; cc <- cc + 1
  ASC844573833  <- b[, cc]; cc <- cc + 1
  ASC844863807  <- b[, cc]; cc <- cc + 1
  ASC844963844  <- b[, cc]; cc <- cc + 1
  ASC846174003  <- b[, cc]; cc <- cc + 1    
  ASC855261710  <- b[, cc]; cc <- cc + 1
  ASC856063806  <- b[, cc]; cc <- cc + 1
  ASC861121724  <- b[, cc]; cc <- cc + 1
  ASC862503016  <- b[, cc]; cc <- cc + 1
  ASC862513015  <- b[, cc]; cc <- cc + 1
  ASC863103198  <- b[, cc]; cc <- cc + 1
  ASC863903545  <- b[, cc]; cc <- cc + 1
  ASC865063847  <- b[, cc]; cc <- cc + 1
  ASC865073846  <- b[, cc]; cc <- cc + 1
  ASC866375097  <- b[, cc]; cc <- cc + 1
  ASC866692777  <- b[, cc]; cc <- cc + 1
  ASC866702776  <- b[, cc]; cc <- cc + 1
  ASC866722775  <- b[, cc]; cc <- cc + 1
  ASC866922771  <- b[, cc]; cc <- cc + 1
  ASC866932770  <- b[, cc]; cc <- cc + 1
  ASC867964779  <- b[, cc]; cc <- cc + 1
  ASC868074765  <- b[, cc]; cc <- cc + 1
  ASC868814774  <- b[, cc]; cc <- cc + 1
  ASC868834760  <- b[, cc]; cc <- cc + 1
  ASC868844758  <- b[, cc]; cc <- cc + 1
  ASC868854775  <- b[, cc]; cc <- cc + 1
  ASC868864757  <- b[, cc]; cc <- cc + 1
  ASC868874756  <- b[, cc]; cc <- cc + 1
  ASC868884776  <- b[, cc]; cc <- cc + 1
  ASC869164762  <- b[, cc]; cc <- cc + 1
  ASC874082985  <- b[, cc]; cc <- cc + 1
  ASC874092984  <- b[, cc]; cc <- cc + 1
  ASC874851780  <- b[, cc]; cc <- cc + 1
  ASC875861912  <- b[, cc]; cc <- cc + 1
  ASC875961916  <- b[, cc]; cc <- cc + 1
  ASC876431990  <- b[, cc]; cc <- cc + 1
  ASC879363013  <- b[, cc]; cc <- cc + 1
  ASC879373012  <- b[, cc]; cc <- cc + 1
  ASC879383014  <- b[, cc]; cc <- cc + 1
  ASC879973197  <- b[, cc]; cc <- cc + 1
  ASC880363412  <- b[, cc]; cc <- cc + 1
  ASC881473546  <- b[, cc]; cc <- cc + 1
  ASC882943897  <- b[, cc]; cc <- cc + 1
  ASC882963898  <- b[, cc]; cc <- cc + 1
  ASC885564398  <- b[, cc]; cc <- cc + 1
  ASC887665099  <- b[, cc]; cc <- cc + 1
  ASC887675098  <- b[, cc]; cc <- cc + 1
  ASC887685100  <- b[, cc]; cc <- cc + 1
  ASC891211179  <- b[, cc]; cc <- cc + 1
  ASC891541783  <- b[, cc]; cc <- cc + 1
  ASC891751778  <- b[, cc]; cc <- cc + 1
  ASC891912983  <- b[, cc]; cc <- cc + 1
  ASC891932981  <- b[, cc]; cc <- cc + 1
  ASC891942977  <- b[, cc]; cc <- cc + 1
  ASC891962980  <- b[, cc]; cc <- cc + 1
  ASC891972979  <- b[, cc]; cc <- cc + 1
  ASC891982982  <- b[, cc]; cc <- cc + 1
  ASC891992978  <- b[, cc]; cc <- cc + 1
  ASC892153005  <- b[, cc]; cc <- cc + 1
  ASC892421420  <- b[, cc]; cc <- cc + 1
  ASC892781907  <- b[, cc]; cc <- cc + 1
  ASC893863010  <- b[, cc]; cc <- cc + 1
  ASC893873009  <- b[, cc]; cc <- cc + 1
  ASC893883011  <- b[, cc]; cc <- cc + 1
  ASC894473195  <- b[, cc]; cc <- cc + 1
  ASC894963405  <- b[, cc]; cc <- cc + 1
  ASC895331989  <- b[, cc]; cc <- cc + 1
  ASC895773544  <- b[, cc]; cc <- cc + 1
  ASC896174404  <- b[, cc]; cc <- cc + 1
  ASC897864410  <- b[, cc]; cc <- cc + 1
  ASC897874409  <- b[, cc]; cc <- cc + 1
  ASC897884411  <- b[, cc]; cc <- cc + 1
  ASC898362619  <- b[, cc]; cc <- cc + 1
  ASC898372618  <- b[, cc]; cc <- cc + 1    
  ASC898875089  <- b[, cc]; cc <- cc + 1
  ASC899165095  <- b[, cc]; cc <- cc + 1
  ASC902836770  <- b[, cc]; cc <- cc + 1
  ASC9046891337 <- b[, cc]; cc <- cc + 1
  ASC9368593476 <- b[, cc]; cc <- cc + 1
  ASC9369673634 <- b[, cc]; cc <- cc + 1
  ASC9465743475 <- b[, cc]; cc <- cc + 1
  ASC9885364401 <- b[, cc];
  
  v9999       <- 0     
  v734190     <- ASC734190     + PRICE1 * state_cost_734190    
  v1573935    <- ASC1573935    + PRICE1 * state_cost_1573935   
  v2870505    <- ASC2870505    + PRICE1 * state_cost_2870505   
  v4356521    <- ASC4356521    + PRICE1 * state_cost_4356521   
  v4626831    <- ASC4626831    + PRICE1 * state_cost_4626831   
  v5915426    <- ASC5915426    + PRICE1 * state_cost_5915426   
  v6144352    <- ASC6144352    + PRICE1 * state_cost_6144352   
  v8417089    <- ASC8417089    + PRICE1 * state_cost_8417089   
  v8751020    <- ASC8751020    + PRICE1 * state_cost_8751020   
  v8920617    <- ASC8920617    + PRICE1 * state_cost_8920617   
  v10548697   <- ASC10548697   + PRICE1 * state_cost_10548697  
  v10549698   <- ASC10549698   + PRICE1 * state_cost_10549698  
  v10550695   <- ASC10550695   + PRICE1 * state_cost_10550695  
  v10553690   <- ASC10553690   + PRICE1 * state_cost_10553690  
  v10555696   <- ASC10555696   + PRICE1 * state_cost_10555696  
  v11586693   <- ASC11586693   + PRICE1 * state_cost_11586693  
  v11588694   <- ASC11588694   + PRICE1 * state_cost_11588694  
  v11771692   <- ASC11771692   + PRICE1 * state_cost_11771692  
  v11773688   <- ASC11773688   + PRICE1 * state_cost_11773688  
  v11774687   <- ASC11774687   + PRICE1 * state_cost_11774687  
  v11776686   <- ASC11776686   + PRICE1 * state_cost_11776686  
  v11777685   <- ASC11777685   + PRICE1 * state_cost_11777685  
  v11786699   <- ASC11786699   + PRICE1 * state_cost_11786699  
  v11788689   <- ASC11788689   + PRICE1 * state_cost_11788689  
  v13774351   <- ASC13774351   + PRICE1 * state_cost_13774351  
  v14001522   <- ASC14001522   + PRICE1 * state_cost_14001522  
  v15776973   <- ASC15776973   + PRICE1 * state_cost_15776973  
  v16516267   <- ASC16516267   + PRICE1 * state_cost_16516267  
  v16517266   <- ASC16517266   + PRICE1 * state_cost_16517266  
  v16518268   <- ASC16518268   + PRICE1 * state_cost_16518268  
  v16676562   <- ASC16676562   + PRICE1 * state_cost_16676562  
  v16987789   <- ASC16987789   + PRICE1 * state_cost_16987789  
  v16988790   <- ASC16988790   + PRICE1 * state_cost_16988790  
  v17086850   <- ASC17086850   + PRICE1 * state_cost_17086850  
  v17127981   <- ASC17127981   + PRICE1 * state_cost_17127981  
  v17991132   <- ASC17991132   + PRICE1 * state_cost_17991132  
  v18006841   <- ASC18006841   + PRICE1 * state_cost_18006841  
  v21480620   <- ASC21480620   + PRICE1 * state_cost_21480620  
  v22291300   <- ASC22291300   + PRICE1 * state_cost_22291300  
  v22783572   <- ASC22783572   + PRICE1 * state_cost_22783572  
  v22784571   <- ASC22784571   + PRICE1 * state_cost_22784571  
  v22786570   <- ASC22786570   + PRICE1 * state_cost_22786570  
  v22787569   <- ASC22787569   + PRICE1 * state_cost_22787569  
  v22788573   <- ASC22788573   + PRICE1 * state_cost_22788573  
  v27025847   <- ASC27025847   + PRICE1 * state_cost_27025847  
  v27474650   <- ASC27474650   + PRICE1 * state_cost_27474650  
  v28086580   <- ASC28086580   + PRICE1 * state_cost_28086580  
  v28087579   <- ASC28087579   + PRICE1 * state_cost_28087579  
  v28088582   <- ASC28088582   + PRICE1 * state_cost_28088582  
  v28206761   <- ASC28206761   + PRICE1 * state_cost_28206761  
  v28233766   <- ASC28233766   + PRICE1 * state_cost_28233766  
  v28236769   <- ASC28236769   + PRICE1 * state_cost_28236769  
  v29287548   <- ASC29287548   + PRICE1 * state_cost_29287548  
  v29288550   <- ASC29288550   + PRICE1 * state_cost_29288550  
  v29566892   <- ASC29566892   + PRICE1 * state_cost_29566892  
  v29568893   <- ASC29568893   + PRICE1 * state_cost_29568893  
  v31352818   <- ASC31352818   + PRICE1 * state_cost_31352818  
  v33102819   <- ASC33102819   + PRICE1 * state_cost_33102819  
  v34001179   <- ASC34001179   + PRICE1 * state_cost_34001179  
  v34004177   <- ASC34004177   + PRICE1 * state_cost_34004177  
  v34006175   <- ASC34006175   + PRICE1 * state_cost_34006175  
  v34007174   <- ASC34007174   + PRICE1 * state_cost_34007174  
  v34008178   <- ASC34008178   + PRICE1 * state_cost_34008178  
  v34014137   <- ASC34014137   + PRICE1 * state_cost_34014137  
  v34021113   <- ASC34021113   + PRICE1 * state_cost_34021113  
  v34026169   <- ASC34026169   + PRICE1 * state_cost_34026169  
  v34029146   <- ASC34029146   + PRICE1 * state_cost_34029146  
  v34030147   <- ASC34030147   + PRICE1 * state_cost_34030147  
  v34032148   <- ASC34032148   + PRICE1 * state_cost_34032148   
  v34036168   <- ASC34036168   + PRICE1 * state_cost_34036168  
  v34052171   <- ASC34052171   + PRICE1 * state_cost_34052171  
  v34061172   <- ASC34061172   + PRICE1 * state_cost_34061172  
  v34076184   <- ASC34076184   + PRICE1 * state_cost_34076184  
  v34116159   <- ASC34116159   + PRICE1 * state_cost_34116159  
  v34117158   <- ASC34117158   + PRICE1 * state_cost_34117158  
  v34155616   <- ASC34155616   + PRICE1 * state_cost_34155616  
  v34162614   <- ASC34162614   + PRICE1 * state_cost_34162614  
  v35315561   <- ASC35315561   + PRICE1 * state_cost_35315561  
  v35316556   <- ASC35316556   + PRICE1 * state_cost_35316556  
  v35317555   <- ASC35317555   + PRICE1 * state_cost_35317555  
  v35318559   <- ASC35318559   + PRICE1 * state_cost_35318559  
  v35354162   <- ASC35354162   + PRICE1 * state_cost_35354162  
  v35416952   <- ASC35416952   + PRICE1 * state_cost_35416952  
  v35418953   <- ASC35418953   + PRICE1 * state_cost_35418953  
  v36572532   <- ASC36572532   + PRICE1 * state_cost_36572532  
  v40482820   <- ASC40482820   + PRICE1 * state_cost_40482820  
  v41271111   <- ASC41271111   + PRICE1 * state_cost_41271111  
  v41279896   <- ASC41279896   + PRICE1 * state_cost_41279896  
  v41284608   <- ASC41284608   + PRICE1 * state_cost_41284608  
  v41299880   <- ASC41299880   + PRICE1 * state_cost_41299880  
  v41316966   <- ASC41316966   + PRICE1 * state_cost_41316966  
  v41320957   <- ASC41320957   + PRICE1 * state_cost_41320957  
  v41413922   <- ASC41413922   + PRICE1 * state_cost_41413922  
  v41840902   <- ASC41840902   + PRICE1 * state_cost_41840902  
  v41927891   <- ASC41927891   + PRICE1 * state_cost_41927891  
  v42006282   <- ASC42006282   + PRICE1 * state_cost_42006282  
  v43022218   <- ASC43022218   + PRICE1 * state_cost_43022218  
  v43024215   <- ASC43024215   + PRICE1 * state_cost_43024215  
  v43025219   <- ASC43025219   + PRICE1 * state_cost_43025219  
  v43026214   <- ASC43026214   + PRICE1 * state_cost_43026214  
  v43027213   <- ASC43027213   + PRICE1 * state_cost_43027213  
  v43028216   <- ASC43028216   + PRICE1 * state_cost_43028216  
  v43031437   <- ASC43031437   + PRICE1 * state_cost_43031437  
  v43035438   <- ASC43035438   + PRICE1 * state_cost_43035438  
  v43036432   <- ASC43036432   + PRICE1 * state_cost_43036432  
  v43037431   <- ASC43037431   + PRICE1 * state_cost_43037431  
  v43038436   <- ASC43038436   + PRICE1 * state_cost_43038436  
  v43046475   <- ASC43046475   + PRICE1 * state_cost_43046475  
  v43048476   <- ASC43048476   + PRICE1 * state_cost_43048476  
  v43050426   <- ASC43050426   + PRICE1 * state_cost_43050426  
  v43051427   <- ASC43051427   + PRICE1 * state_cost_43051427  
  v43066467   <- ASC43066467   + PRICE1 * state_cost_43066467  
  v43076208   <- ASC43076208   + PRICE1 * state_cost_43076208  
  v43077207   <- ASC43077207   + PRICE1 * state_cost_43077207  
  v43116455   <- ASC43116455   + PRICE1 * state_cost_43116455  
  v43117454   <- ASC43117454   + PRICE1 * state_cost_43117454  
  v43120477   <- ASC43120477   + PRICE1 * state_cost_43120477  
  v43121483   <- ASC43121483   + PRICE1 * state_cost_43121483  
  v43123481   <- ASC43123481   + PRICE1 * state_cost_43123481  
  v43124480   <- ASC43124480   + PRICE1 * state_cost_43124480  
  v43125484   <- ASC43125484   + PRICE1 * state_cost_43125484  
  v43126479   <- ASC43126479   + PRICE1 * state_cost_43126479  
  v43127478   <- ASC43127478   + PRICE1 * state_cost_43127478  
  v43128482   <- ASC43128482   + PRICE1 * state_cost_43128482  
  v43136446   <- ASC43136446   + PRICE1 * state_cost_43136446  
  v43137443   <- ASC43137443   + PRICE1 * state_cost_43137443  
  v43138448   <- ASC43138448   + PRICE1 * state_cost_43138448  
  v43145469   <- ASC43145469   + PRICE1 * state_cost_43145469  
  v43156402   <- ASC43156402   + PRICE1 * state_cost_43156402  
  v43197488   <- ASC43197488   + PRICE1 * state_cost_43197488  
  v43205458   <- ASC43205458   + PRICE1 * state_cost_43205458  
  v43209415   <- ASC43209415   + PRICE1 * state_cost_43209415  
  v43984204   <- ASC43984204   + PRICE1 * state_cost_43984204  
  v43986205   <- ASC43986205   + PRICE1 * state_cost_43986205  
  v44217554   <- ASC44217554   + PRICE1 * state_cost_44217554  
  v44258701   <- ASC44258701   + PRICE1 * state_cost_44258701  
  v47161262   <- ASC47161262   + PRICE1 * state_cost_47161262  
  v47171261   <- ASC47171261   + PRICE1 * state_cost_47171261   
  v48661746   <- ASC48661746   + PRICE1 * state_cost_48661746  
  v48671745   <- ASC48671745   + PRICE1 * state_cost_48671745  
  v48681749   <- ASC48681749   + PRICE1 * state_cost_48681749  
  v49361993   <- ASC49361993   + PRICE1 * state_cost_49361993  
  v50062247   <- ASC50062247   + PRICE1 * state_cost_50062247  
  v50362273   <- ASC50362273   + PRICE1 * state_cost_50362273  
  v50372272   <- ASC50372272   + PRICE1 * state_cost_50372272  
  v52882750   <- ASC52882750   + PRICE1 * state_cost_52882750  
  v52892748   <- ASC52892748   + PRICE1 * state_cost_52892748  
  v52902749   <- ASC52902749   + PRICE1 * state_cost_52902749  
  v53262930   <- ASC53262930   + PRICE1 * state_cost_53262930  
  v53272929   <- ASC53272929   + PRICE1 * state_cost_53272929  
  v53462949   <- ASC53462949   + PRICE1 * state_cost_53462949  
  v53472952   <- ASC53472952   + PRICE1 * state_cost_53472952  
  v53492951   <- ASC53492951   + PRICE1 * state_cost_53492951  
  v54056332   <- ASC54056332   + PRICE1 * state_cost_54056332  
  v54057331   <- ASC54057331   + PRICE1 * state_cost_54057331  
  v54646335   <- ASC54646335   + PRICE1 * state_cost_54646335  
  v54647334   <- ASC54647334   + PRICE1 * state_cost_54647334  
  v54863315   <- ASC54863315   + PRICE1 * state_cost_54863315  
  v55246362   <- ASC55246362   + PRICE1 * state_cost_55246362  
  v55963447   <- ASC55963447   + PRICE1 * state_cost_55963447  
  v56063448   <- ASC56063448   + PRICE1 * state_cost_56063448  
  v56828417   <- ASC56828417   + PRICE1 * state_cost_56828417  
  v56840464   <- ASC56840464   + PRICE1 * state_cost_56840464  
  v56843466   <- ASC56843466   + PRICE1 * state_cost_56843466  
  v56846465   <- ASC56846465   + PRICE1 * state_cost_56846465  
  v56850463   <- ASC56850463   + PRICE1 * state_cost_56850463  
  v56957542   <- ASC56957542   + PRICE1 * state_cost_56957542  
  v56958543   <- ASC56958543   + PRICE1 * state_cost_56958543  
  v57051553   <- ASC57051553   + PRICE1 * state_cost_57051553  
  v57092420   <- ASC57092420   + PRICE1 * state_cost_57092420  
  v64336401   <- ASC64336401   + PRICE1 * state_cost_64336401  
  v68022508   <- ASC68022508   + PRICE1 * state_cost_68022508  
  v68036500   <- ASC68036500   + PRICE1 * state_cost_68036500  
  v68037498   <- ASC68037498   + PRICE1 * state_cost_68037498  
  v68038502   <- ASC68038502   + PRICE1 * state_cost_68038502  
  v68039501   <- ASC68039501   + PRICE1 * state_cost_68039501  
  v68049509   <- ASC68049509   + PRICE1 * state_cost_68049509  
  v69971349   <- ASC69971349   + PRICE1 * state_cost_69971349  
  v69981351   <- ASC69981351   + PRICE1 * state_cost_69981351  
  v71886256   <- ASC71886256   + PRICE1 * state_cost_71886256  
  v72816991   <- ASC72816991   + PRICE1 * state_cost_72816991  
  v79026343   <- ASC79026343   + PRICE1 * state_cost_79026343  
  v80571355   <- ASC80571355   + PRICE1 * state_cost_80571355  
  v80574352   <- ASC80574352   + PRICE1 * state_cost_80574352  
  v80576351   <- ASC80576351   + PRICE1 * state_cost_80576351  
  v80577350   <- ASC80577350   + PRICE1 * state_cost_80577350  
  v80578354   <- ASC80578354   + PRICE1 * state_cost_80578354  
  v81966346   <- ASC81966346   + PRICE1 * state_cost_81966346  
  v82082628   <- ASC82082628   + PRICE1 * state_cost_82082628  
  v82126349   <- ASC82126349   + PRICE1 * state_cost_82126349  
  v82127348   <- ASC82127348   + PRICE1 * state_cost_82127348  
  v82147356   <- ASC82147356   + PRICE1 * state_cost_82147356  
  v82187359   <- ASC82187359   + PRICE1 * state_cost_82187359  
  v88253245   <- ASC88253245   + PRICE1 * state_cost_88253245  
  v88263244   <- ASC88263244   + PRICE1 * state_cost_88263244  
  v88273241   <- ASC88273241   + PRICE1 * state_cost_88273241  
  v88283243   <- ASC88283243   + PRICE1 * state_cost_88283243  
  v89139984   <- ASC89139984   + PRICE1 * state_cost_89139984  
  v92783722   <- ASC92783722   + PRICE1 * state_cost_92783722  
  v94263877   <- ASC94263877   + PRICE1 * state_cost_94263877  
  v94283878   <- ASC94283878   + PRICE1 * state_cost_94283878  
  v100064432  <- ASC100064432  + PRICE1 * state_cost_100064432 
  v100084433  <- ASC100084433  + PRICE1 * state_cost_100084433 
  v100094431  <- ASC100094431  + PRICE1 * state_cost_100094431 
  v102785180  <- ASC102785180  + PRICE1 * state_cost_102785180 
  v106251054  <- ASC106251054  + PRICE1 * state_cost_106251054 
  v106261050  <- ASC106261050  + PRICE1 * state_cost_106261050  
  v106271048  <- ASC106271048  + PRICE1 * state_cost_106271048 
  v106281053  <- ASC106281053  + PRICE1 * state_cost_106281053 
  v108051512  <- ASC108051512  + PRICE1 * state_cost_108051512 
  v108071511  <- ASC108071511  + PRICE1 * state_cost_108071511 
  v108081510  <- ASC108081510  + PRICE1 * state_cost_108081510 
  v108181507  <- ASC108181507  + PRICE1 * state_cost_108181507 
  v108361481  <- ASC108361481  + PRICE1 * state_cost_108361481 
  v108902109  <- ASC108902109  + PRICE1 * state_cost_108902109 
  v112901489  <- ASC112901489  + PRICE1 * state_cost_112901489 
  v112931487  <- ASC112931487  + PRICE1 * state_cost_112931487 
  v112941486  <- ASC112941486  + PRICE1 * state_cost_112941486 
  v112961476  <- ASC112961476  + PRICE1 * state_cost_112961476 
  v112971485  <- ASC112971485  + PRICE1 * state_cost_112971485 
  v112981488  <- ASC112981488  + PRICE1 * state_cost_112981488 
  v113464508  <- ASC113464508  + PRICE1 * state_cost_113464508 
  v113474507  <- ASC113474507  + PRICE1 * state_cost_113474507 
  v113484509  <- ASC113484509  + PRICE1 * state_cost_113484509 
  v113661518  <- ASC113661518  + PRICE1 * state_cost_113661518 
  v119361068  <- ASC119361068  + PRICE1 * state_cost_119361068 
  v124041066  <- ASC124041066  + PRICE1 * state_cost_124041066 
  v124061065  <- ASC124061065  + PRICE1 * state_cost_124061065 
  v124071064  <- ASC124071064  + PRICE1 * state_cost_124071064 
  v124081067  <- ASC124081067  + PRICE1 * state_cost_124081067 
  v124641076  <- ASC124641076  + PRICE1 * state_cost_124641076 
  v124661075  <- ASC124661075  + PRICE1 * state_cost_124661075 
  v124671074  <- ASC124671074  + PRICE1 * state_cost_124671074 
  v124761073  <- ASC124761073  + PRICE1 * state_cost_124761073 
  v124781072  <- ASC124781072  + PRICE1 * state_cost_124781072 
  v124791071  <- ASC124791071  + PRICE1 * state_cost_124791071 
  v126681083  <- ASC126681083  + PRICE1 * state_cost_126681083 
  v128564285  <- ASC128564285  + PRICE1 * state_cost_128564285 
  v128884281  <- ASC128884281  + PRICE1 * state_cost_128884281 
  v130361080  <- ASC130361080  + PRICE1 * state_cost_130361080 
  v130371079  <- ASC130371079  + PRICE1 * state_cost_130371079 
  v130381081  <- ASC130381081  + PRICE1 * state_cost_130381081 
  v133883651  <- ASC133883651  + PRICE1 * state_cost_133883651 
  v136383284  <- ASC136383284  + PRICE1 * state_cost_136383284 
  v141923976  <- ASC141923976  + PRICE1 * state_cost_141923976 
  v141993977  <- ASC141993977  + PRICE1 * state_cost_141993977 
  v152465358  <- ASC152465358  + PRICE1 * state_cost_152465358 
  v152475357  <- ASC152475357  + PRICE1 * state_cost_152475357 
  v152485359  <- ASC152485359  + PRICE1 * state_cost_152485359 
  v152965362  <- ASC152965362  + PRICE1 * state_cost_152965362 
  v156262825  <- ASC156262825  + PRICE1 * state_cost_156262825 
  v156272824  <- ASC156272824  + PRICE1 * state_cost_156272824 
  v156282827  <- ASC156282827  + PRICE1 * state_cost_156282827 
  v156442826  <- ASC156442826  + PRICE1 * state_cost_156442826 
  v156774143  <- ASC156774143  + PRICE1 * state_cost_156774143 
  v159405139  <- ASC159405139  + PRICE1 * state_cost_159405139 
  v172061186  <- ASC172061186  + PRICE1 * state_cost_172061186 
  v178261887  <- ASC178261887  + PRICE1 * state_cost_178261887 
  v178301888  <- ASC178301888  + PRICE1 * state_cost_178301888 
  v179161934  <- ASC179161934  + PRICE1 * state_cost_179161934 
  v179561971  <- ASC179561971  + PRICE1 * state_cost_179561971 
  v179581973  <- ASC179581973  + PRICE1 * state_cost_179581973 
  v181961984  <- ASC181961984  + PRICE1 * state_cost_181961984 
  v181981986  <- ASC181981986  + PRICE1 * state_cost_181981986 
  v184072158  <- ASC184072158  + PRICE1 * state_cost_184072158 
  v184082159  <- ASC184082159  + PRICE1 * state_cost_184082159 
  v190262884  <- ASC190262884  + PRICE1 * state_cost_190262884 
  v190612905  <- ASC190612905  + PRICE1 * state_cost_190612905 
  v190632876  <- ASC190632876  + PRICE1 * state_cost_190632876 
  v190642874  <- ASC190642874  + PRICE1 * state_cost_190642874 
  v190662873  <- ASC190662873  + PRICE1 * state_cost_190662873 
  v190672872  <- ASC190672872  + PRICE1 * state_cost_190672872 
  v190682877  <- ASC190682877  + PRICE1 * state_cost_190682877 
  v190822906  <- ASC190822906  + PRICE1 * state_cost_190822906 
  v190962911  <- ASC190962911  + PRICE1 * state_cost_190962911 
  v191122890  <- ASC191122890  + PRICE1 * state_cost_191122890  
  v192263131  <- ASC192263131  + PRICE1 * state_cost_192263131 
  v194763335  <- ASC194763335  + PRICE1 * state_cost_194763335 
  v194773333  <- ASC194773333  + PRICE1 * state_cost_194773333 
  v194783336  <- ASC194783336  + PRICE1 * state_cost_194783336 
  v194863332  <- ASC194863332  + PRICE1 * state_cost_194863332 
  v202463689  <- ASC202463689  + PRICE1 * state_cost_202463689 
  v202473688  <- ASC202473688  + PRICE1 * state_cost_202473688 
  v202483690  <- ASC202483690  + PRICE1 * state_cost_202483690 
  v202863693  <- ASC202863693  + PRICE1 * state_cost_202863693 
  v215954944  <- ASC215954944  + PRICE1 * state_cost_215954944 
  v215964941  <- ASC215964941  + PRICE1 * state_cost_215964941 
  v215974940  <- ASC215974940  + PRICE1 * state_cost_215974940 
  v215984943  <- ASC215984943  + PRICE1 * state_cost_215984943 
  v221215325  <- ASC221215325  + PRICE1 * state_cost_221215325 
  v221555322  <- ASC221555322  + PRICE1 * state_cost_221555322 
  v221565319  <- ASC221565319  + PRICE1 * state_cost_221565319 
  v221575318  <- ASC221575318  + PRICE1 * state_cost_221575318 
  v222135384  <- ASC222135384  + PRICE1 * state_cost_222135384 
  v222165385  <- ASC222165385  + PRICE1 * state_cost_222165385 
  v232771028  <- ASC232771028  + PRICE1 * state_cost_232771028 
  v232781030  <- ASC232781030  + PRICE1 * state_cost_232781030 
  v236262086  <- ASC236262086  + PRICE1 * state_cost_236262086 
  v237082092  <- ASC237082092  + PRICE1 * state_cost_237082092 
  v238232083  <- ASC238232083  + PRICE1 * state_cost_238232083 
  v238242082  <- ASC238242082  + PRICE1 * state_cost_238242082 
  v238262081  <- ASC238262081  + PRICE1 * state_cost_238262081 
  v238272080  <- ASC238272080  + PRICE1 * state_cost_238272080 
  v238282084  <- ASC238282084  + PRICE1 * state_cost_238282084 
  v241562463  <- ASC241562463  + PRICE1 * state_cost_241562463 
  v241572462  <- ASC241572462  + PRICE1 * state_cost_241572462 
  v241582464  <- ASC241582464  + PRICE1 * state_cost_241582464 
  v244543085  <- ASC244543085  + PRICE1 * state_cost_244543085 
  v244563084  <- ASC244563084  + PRICE1 * state_cost_244563084 
  v244573083  <- ASC244573083  + PRICE1 * state_cost_244573083 
  v244583087  <- ASC244583087  + PRICE1 * state_cost_244583087 
  v244663089  <- ASC244663089  + PRICE1 * state_cost_244663089 
  v247063452  <- ASC247063452  + PRICE1 * state_cost_247063452 
  v247283453  <- ASC247283453  + PRICE1 * state_cost_247283453 
  v256034475  <- ASC256034475  + PRICE1 * state_cost_256034475 
  v256044474  <- ASC256044474  + PRICE1 * state_cost_256044474 
  v256064473  <- ASC256064473  + PRICE1 * state_cost_256064473 
  v256074472  <- ASC256074472  + PRICE1 * state_cost_256074472 
  v256084476  <- ASC256084476  + PRICE1 * state_cost_256084476 
  v256164478  <- ASC256164478  + PRICE1 * state_cost_256164478 
  v258764867  <- ASC258764867  + PRICE1 * state_cost_258764867 
  v258774866  <- ASC258774866  + PRICE1 * state_cost_258774866 
  v258784869  <- ASC258784869  + PRICE1 * state_cost_258784869 
  v265862175  <- ASC265862175  + PRICE1 * state_cost_265862175 
  v266562181  <- ASC266562181  + PRICE1 * state_cost_266562181 
  v268202791  <- ASC268202791  + PRICE1 * state_cost_268202791 
  v268212792  <- ASC268212792  + PRICE1 * state_cost_268212792 
  v268222784  <- ASC268222784  + PRICE1 * state_cost_268222784 
  v268232789  <- ASC268232789  + PRICE1 * state_cost_268232789 
  v268242793  <- ASC268242793  + PRICE1 * state_cost_268242793 
  v268262788  <- ASC268262788  + PRICE1 * state_cost_268262788 
  v268272787  <- ASC268272787  + PRICE1 * state_cost_268272787 
  v268282790  <- ASC268282790  + PRICE1 * state_cost_268282790 
  v269062795  <- ASC269062795  + PRICE1 * state_cost_269062795 
  v270271374  <- ASC270271374  + PRICE1 * state_cost_270271374 
  v270562907  <- ASC270562907  + PRICE1 * state_cost_270562907 
  v271024938  <- ASC271024938  + PRICE1 * state_cost_271024938 
  v273922903  <- ASC273922903  + PRICE1 * state_cost_273922903 
  v274102893  <- ASC274102893  + PRICE1 * state_cost_274102893 
  v274541302  <- ASC274541302  + PRICE1 * state_cost_274541302 
  v274791372  <- ASC274791372  + PRICE1 * state_cost_274791372 
  v275444212  <- ASC275444212  + PRICE1 * state_cost_275444212 
  v275544501  <- ASC275544501  + PRICE1 * state_cost_275544501 
  v275624505  <- ASC275624505  + PRICE1 * state_cost_275624505 
  v275852626  <- ASC275852626  + PRICE1 * state_cost_275852626  
  v286252492  <- ASC286252492  + PRICE1 * state_cost_286252492 
  v287184070  <- ASC287184070  + PRICE1 * state_cost_287184070 
  v288664915  <- ASC288664915  + PRICE1 * state_cost_288664915 
  v288674914  <- ASC288674914  + PRICE1 * state_cost_288674914 
  v288684918  <- ASC288684918  + PRICE1 * state_cost_288684918 
  v288904925  <- ASC288904925  + PRICE1 * state_cost_288904925 
  v297281031  <- ASC297281031  + PRICE1 * state_cost_297281031 
  v299932071  <- ASC299932071  + PRICE1 * state_cost_299932071 
  v299942070  <- ASC299942070  + PRICE1 * state_cost_299942070 
  v299962069  <- ASC299962069  + PRICE1 * state_cost_299962069 
  v299972068  <- ASC299972068  + PRICE1 * state_cost_299972068 
  v299982072  <- ASC299982072  + PRICE1 * state_cost_299982072 
  v300362067  <- ASC300362067  + PRICE1 * state_cost_300362067 
  v300562090  <- ASC300562090  + PRICE1 * state_cost_300562090 
  v300572089  <- ASC300572089  + PRICE1 * state_cost_300572089 
  v300582091  <- ASC300582091  + PRICE1 * state_cost_300582091 
  v302362196  <- ASC302362196  + PRICE1 * state_cost_302362196 
  v302382197  <- ASC302382197  + PRICE1 * state_cost_302382197 
  v303162321  <- ASC303162321  + PRICE1 * state_cost_303162321 
  v303172320  <- ASC303172320  + PRICE1 * state_cost_303172320 
  v303182322  <- ASC303182322  + PRICE1 * state_cost_303182322 
  v305262468  <- ASC305262468  + PRICE1 * state_cost_305262468 
  v305272467  <- ASC305272467  + PRICE1 * state_cost_305272467 
  v305282469  <- ASC305282469  + PRICE1 * state_cost_305282469 
  v311673458  <- ASC311673458  + PRICE1 * state_cost_311673458 
  v312083459  <- ASC312083459  + PRICE1 * state_cost_312083459 
  v314723602  <- ASC314723602  + PRICE1 * state_cost_314723602 
  v314733605  <- ASC314733605  + PRICE1 * state_cost_314733605 
  v314743603  <- ASC314743603  + PRICE1 * state_cost_314743603 
  v314753601  <- ASC314753601  + PRICE1 * state_cost_314753601 
  v316543821  <- ASC316543821  + PRICE1 * state_cost_316543821 
  v316563820  <- ASC316563820  + PRICE1 * state_cost_316563820 
  v316573819  <- ASC316573819  + PRICE1 * state_cost_316573819 
  v316583822  <- ASC316583822  + PRICE1 * state_cost_316583822 
  v316663823  <- ASC316663823  + PRICE1 * state_cost_316663823 
  v317183995  <- ASC317183995  + PRICE1 * state_cost_317183995 
  v317193994  <- ASC317193994  + PRICE1 * state_cost_317193994 
  v322314488  <- ASC322314488  + PRICE1 * state_cost_322314488 
  v322324485  <- ASC322324485  + PRICE1 * state_cost_322324485 
  v322334486  <- ASC322334486  + PRICE1 * state_cost_322334486 
  v322344484  <- ASC322344484  + PRICE1 * state_cost_322344484 
  v322354489  <- ASC322354489  + PRICE1 * state_cost_322354489 
  v322364483  <- ASC322364483  + PRICE1 * state_cost_322364483 
  v322374482  <- ASC322374482  + PRICE1 * state_cost_322374482 
  v322384487  <- ASC322384487  + PRICE1 * state_cost_322384487 
  v332564494  <- ASC332564494  + PRICE1 * state_cost_332564494 
  v336634891  <- ASC336634891  + PRICE1 * state_cost_336634891 
  v337163842  <- ASC337163842  + PRICE1 * state_cost_337163842 
  v337173841  <- ASC337173841  + PRICE1 * state_cost_337173841 
  v341641324  <- ASC341641324  + PRICE1 * state_cost_341641324 
  v341971322  <- ASC341971322  + PRICE1 * state_cost_341971322 
  v341981344  <- ASC341981344  + PRICE1 * state_cost_341981344 
  v343592414  <- ASC343592414  + PRICE1 * state_cost_343592414 
  v343662146  <- ASC343662146  + PRICE1 * state_cost_343662146 
  v343682147  <- ASC343682147  + PRICE1 * state_cost_343682147 
  v344222417  <- ASC344222417  + PRICE1 * state_cost_344222417 
  v344232415  <- ASC344232415  + PRICE1 * state_cost_344232415 
  v344252418  <- ASC344252418  + PRICE1 * state_cost_344252418 
  v344332416  <- ASC344332416  + PRICE1 * state_cost_344332416 
  v344493093  <- ASC344493093  + PRICE1 * state_cost_344493093 
  v344563096  <- ASC344563096  + PRICE1 * state_cost_344563096 
  v344573094  <- ASC344573094  + PRICE1 * state_cost_344573094 
  v344583097  <- ASC344583097  + PRICE1 * state_cost_344583097 
  v345463959  <- ASC345463959  + PRICE1 * state_cost_345463959 
  v345663961  <- ASC345663961  + PRICE1 * state_cost_345663961 
  v345784107  <- ASC345784107  + PRICE1 * state_cost_345784107 
  v345794105  <- ASC345794105  + PRICE1 * state_cost_345794105 
  v346904740  <- ASC346904740  + PRICE1 * state_cost_346904740 
  v347464848  <- ASC347464848  + PRICE1 * state_cost_347464848  
  v347474844  <- ASC347474844  + PRICE1 * state_cost_347474844 
  v347484849  <- ASC347484849  + PRICE1 * state_cost_347484849 
  v347864825  <- ASC347864825  + PRICE1 * state_cost_347864825 
  v348174871  <- ASC348174871  + PRICE1 * state_cost_348174871 
  v348204895  <- ASC348204895  + PRICE1 * state_cost_348204895 
  v348214898  <- ASC348214898  + PRICE1 * state_cost_348214898 
  v348234877  <- ASC348234877  + PRICE1 * state_cost_348234877 
  v348564927  <- ASC348564927  + PRICE1 * state_cost_348564927 
  v348714847  <- ASC348714847  + PRICE1 * state_cost_348714847 
  v348814855  <- ASC348814855  + PRICE1 * state_cost_348814855 
  v349195043  <- ASC349195043  + PRICE1 * state_cost_349195043 
  v349355042  <- ASC349355042  + PRICE1 * state_cost_349355042 
  v349725007  <- ASC349725007  + PRICE1 * state_cost_349725007 
  v349955017  <- ASC349955017  + PRICE1 * state_cost_349955017 
  v352134741  <- ASC352134741  + PRICE1 * state_cost_352134741 
  v356264111  <- ASC356264111  + PRICE1 * state_cost_356264111 
  v356284112  <- ASC356284112  + PRICE1 * state_cost_356284112 
  v356993944  <- ASC356993944  + PRICE1 * state_cost_356993944 
  v357804051  <- ASC357804051  + PRICE1 * state_cost_357804051 
  v359132078  <- ASC359132078  + PRICE1 * state_cost_359132078 
  v359142077  <- ASC359142077  + PRICE1 * state_cost_359142077 
  v359162076  <- ASC359162076  + PRICE1 * state_cost_359162076 
  v359172075  <- ASC359172075  + PRICE1 * state_cost_359172075 
  v359182079  <- ASC359182079  + PRICE1 * state_cost_359182079 
  v359262073  <- ASC359262073  + PRICE1 * state_cost_359262073 
  v359442095  <- ASC359442095  + PRICE1 * state_cost_359442095 
  v359462094  <- ASC359462094  + PRICE1 * state_cost_359462094 
  v359472093  <- ASC359472093  + PRICE1 * state_cost_359472093 
  v359482096  <- ASC359482096  + PRICE1 * state_cost_359482096 
  v359645019  <- ASC359645019  + PRICE1 * state_cost_359645019 
  v361862324  <- ASC361862324  + PRICE1 * state_cost_361862324 
  v361882325  <- ASC361882325  + PRICE1 * state_cost_361882325 
  v363012478  <- ASC363012478  + PRICE1 * state_cost_363012478 
  v363042475  <- ASC363042475  + PRICE1 * state_cost_363042475 
  v363052474  <- ASC363052474  + PRICE1 * state_cost_363052474 
  v363072473  <- ASC363072473  + PRICE1 * state_cost_363072473 
  v363082477  <- ASC363082477  + PRICE1 * state_cost_363082477 
  v364472674  <- ASC364472674  + PRICE1 * state_cost_364472674 
  v366673145  <- ASC366673145  + PRICE1 * state_cost_366673145 
  v366683146  <- ASC366683146  + PRICE1 * state_cost_366683146 
  v368711323  <- ASC368711323  + PRICE1 * state_cost_368711323 
  v368863465  <- ASC368863465  + PRICE1 * state_cost_368863465 
  v368873467  <- ASC368873467  + PRICE1 * state_cost_368873467 
  v369013468  <- ASC369013468  + PRICE1 * state_cost_369013468 
  v369033466  <- ASC369033466  + PRICE1 * state_cost_369033466 
  v369043469  <- ASC369043469  + PRICE1 * state_cost_369043469 
  v369073471  <- ASC369073471  + PRICE1 * state_cost_369073471 
  v369083470  <- ASC369083470  + PRICE1 * state_cost_369083470 
  v369683635  <- ASC369683635  + PRICE1 * state_cost_369683635 
  v369693633  <- ASC369693633  + PRICE1 * state_cost_369693633 
  v369713637  <- ASC369713637  + PRICE1 * state_cost_369713637 
  v369743645  <- ASC369743645  + PRICE1 * state_cost_369743645 
  v369763644  <- ASC369763644  + PRICE1 * state_cost_369763644 
  v369783647  <- ASC369783647  + PRICE1 * state_cost_369783647 
  v371463754  <- ASC371463754  + PRICE1 * state_cost_371463754 
  v372173850  <- ASC372173850  + PRICE1 * state_cost_372173850 
  v373365237  <- ASC373365237  + PRICE1 * state_cost_373365237 
  v373385241  <- ASC373385241  + PRICE1 * state_cost_373385241 
  v373395239  <- ASC373395239  + PRICE1 * state_cost_373395239 
  v373464016  <- ASC373464016  + PRICE1 * state_cost_373464016 
  v373474014  <- ASC373474014  + PRICE1 * state_cost_373474014 
  v373484017  <- ASC373484017  + PRICE1 * state_cost_373484017 
  v373564018  <- ASC373564018  + PRICE1 * state_cost_373564018 
  v374134136  <- ASC374134136  + PRICE1 * state_cost_374134136 
  v374144135  <- ASC374144135  + PRICE1 * state_cost_374144135 
  v374164134  <- ASC374164134  + PRICE1 * state_cost_374164134 
  v374174133  <- ASC374174133  + PRICE1 * state_cost_374174133 
  v374184137  <- ASC374184137  + PRICE1 * state_cost_374184137 
  v374264138  <- ASC374264138  + PRICE1 * state_cost_374264138  
  v376404291  <- ASC376404291  + PRICE1 * state_cost_376404291 
  v376424292  <- ASC376424292  + PRICE1 * state_cost_376424292 
  v378864512  <- ASC378864512  + PRICE1 * state_cost_378864512 
  v378884513  <- ASC378884513  + PRICE1 * state_cost_378884513 
  v379344565  <- ASC379344565  + PRICE1 * state_cost_379344565 
  v379354567  <- ASC379354567  + PRICE1 * state_cost_379354567 
  v379364564  <- ASC379364564  + PRICE1 * state_cost_379364564 
  v379374563  <- ASC379374563  + PRICE1 * state_cost_379374563 
  v379384566  <- ASC379384566  + PRICE1 * state_cost_379384566 
  v379864600  <- ASC379864600  + PRICE1 * state_cost_379864600 
  v379874598  <- ASC379874598  + PRICE1 * state_cost_379874598 
  v379884601  <- ASC379884601  + PRICE1 * state_cost_379884601 
  v379914710  <- ASC379914710  + PRICE1 * state_cost_379914710 
  v379934708  <- ASC379934708  + PRICE1 * state_cost_379934708 
  v379944707  <- ASC379944707  + PRICE1 * state_cost_379944707 
  v379964706  <- ASC379964706  + PRICE1 * state_cost_379964706 
  v379974705  <- ASC379974705  + PRICE1 * state_cost_379974705 
  v379984709  <- ASC379984709  + PRICE1 * state_cost_379984709 
  v380064712  <- ASC380064712  + PRICE1 * state_cost_380064712 
  v380084711  <- ASC380084711  + PRICE1 * state_cost_380084711 
  v380884122  <- ASC380884122  + PRICE1 * state_cost_380884122 
  v381765067  <- ASC381765067  + PRICE1 * state_cost_381765067 
  v381775064  <- ASC381775064  + PRICE1 * state_cost_381775064 
  v381785066  <- ASC381785066  + PRICE1 * state_cost_381785066 
  v398664701  <- ASC398664701  + PRICE1 * state_cost_398664701 
  v401181854  <- ASC401181854  + PRICE1 * state_cost_401181854 
  v401933622  <- ASC401933622  + PRICE1 * state_cost_401933622 
  v402973593  <- ASC402973593  + PRICE1 * state_cost_402973593 
  v403133619  <- ASC403133619  + PRICE1 * state_cost_403133619 
  v403945197  <- ASC403945197  + PRICE1 * state_cost_403945197 
  v404115227  <- ASC404115227  + PRICE1 * state_cost_404115227 
  v405923616  <- ASC405923616  + PRICE1 * state_cost_405923616 
  v405933617  <- ASC405933617  + PRICE1 * state_cost_405933617 
  v405943615  <- ASC405943615  + PRICE1 * state_cost_405943615 
  v405983628  <- ASC405983628  + PRICE1 * state_cost_405983628 
  v405993626  <- ASC405993626  + PRICE1 * state_cost_405993626 
  v406044636  <- ASC406044636  + PRICE1 * state_cost_406044636 
  v406144352  <- ASC406144352  + PRICE1 * state_cost_406144352 
  v406255200  <- ASC406255200  + PRICE1 * state_cost_406255200 
  v406325242  <- ASC406325242  + PRICE1 * state_cost_406325242 
  v409264670  <- ASC409264670  + PRICE1 * state_cost_409264670 
  v409304631  <- ASC409304631  + PRICE1 * state_cost_409304631 
  v409464717  <- ASC409464717  + PRICE1 * state_cost_409464717 
  v409564691  <- ASC409564691  + PRICE1 * state_cost_409564691 
  v409654718  <- ASC409654718  + PRICE1 * state_cost_409654718 
  v409764615  <- ASC409764615  + PRICE1 * state_cost_409764615 
  v410762864  <- ASC410762864  + PRICE1 * state_cost_410762864 
  v410772862  <- ASC410772862  + PRICE1 * state_cost_410772862 
  v410782865  <- ASC410782865  + PRICE1 * state_cost_410782865 
  v412092062  <- ASC412092062  + PRICE1 * state_cost_412092062 
  v413605215  <- ASC413605215  + PRICE1 * state_cost_413605215 
  v415002465  <- ASC415002465  + PRICE1 * state_cost_415002465 
  v415924639  <- ASC415924639  + PRICE1 * state_cost_415924639 
  v416015183  <- ASC416015183  + PRICE1 * state_cost_416015183 
  v416045206  <- ASC416045206  + PRICE1 * state_cost_416045206 
  v416255207  <- ASC416255207  + PRICE1 * state_cost_416255207 
  v416535202  <- ASC416535202  + PRICE1 * state_cost_416535202 
  v416815221  <- ASC416815221  + PRICE1 * state_cost_416815221 
  v416885191  <- ASC416885191  + PRICE1 * state_cost_416885191 
  v416925185  <- ASC416925185  + PRICE1 * state_cost_416925185 
  v416935184  <- ASC416935184  + PRICE1 * state_cost_416935184 
  v416945182  <- ASC416945182  + PRICE1 * state_cost_416945182 
  v416965209  <- ASC416965209  + PRICE1 * state_cost_416965209 
  v416985210  <- ASC416985210  + PRICE1 * state_cost_416985210 
  v417015211  <- ASC417015211  + PRICE1 * state_cost_417015211 
  v417045220  <- ASC417045220  + PRICE1 * state_cost_417045220 
  v417055219  <- ASC417055219  + PRICE1 * state_cost_417055219 
  v417073998  <- ASC417073998  + PRICE1 * state_cost_417073998 
  v417154626  <- ASC417154626  + PRICE1 * state_cost_417154626  
  v417194633  <- ASC417194633  + PRICE1 * state_cost_417194633 
  v417464623  <- ASC417464623  + PRICE1 * state_cost_417464623 
  v417474622  <- ASC417474622  + PRICE1 * state_cost_417474622 
  v417835190  <- ASC417835190  + PRICE1 * state_cost_417835190 
  v417964664  <- ASC417964664  + PRICE1 * state_cost_417964664 
  v417974663  <- ASC417974663  + PRICE1 * state_cost_417974663 
  v418064700  <- ASC418064700  + PRICE1 * state_cost_418064700 
  v418461979  <- ASC418461979  + PRICE1 * state_cost_418461979 
  v419895193  <- ASC419895193  + PRICE1 * state_cost_419895193 
  v420025194  <- ASC420025194  + PRICE1 * state_cost_420025194 
  v420095196  <- ASC420095196  + PRICE1 * state_cost_420095196 
  v420795195  <- ASC420795195  + PRICE1 * state_cost_420795195 
  v421663588  <- ASC421663588  + PRICE1 * state_cost_421663588 
  v421673587  <- ASC421673587  + PRICE1 * state_cost_421673587 
  v423123372  <- ASC423123372  + PRICE1 * state_cost_423123372 
  v424444531  <- ASC424444531  + PRICE1 * state_cost_424444531 
  v426663583  <- ASC426663583  + PRICE1 * state_cost_426663583 
  v426763366  <- ASC426763366  + PRICE1 * state_cost_426763366 
  v426873343  <- ASC426873343  + PRICE1 * state_cost_426873343 
  v426993362  <- ASC426993362  + PRICE1 * state_cost_426993362 
  v427033371  <- ASC427033371  + PRICE1 * state_cost_427033371 
  v427123353  <- ASC427123353  + PRICE1 * state_cost_427123353 
  v427163350  <- ASC427163350  + PRICE1 * state_cost_427163350 
  v427173349  <- ASC427173349  + PRICE1 * state_cost_427173349 
  v427183352  <- ASC427183352  + PRICE1 * state_cost_427183352 
  v429845112  <- ASC429845112  + PRICE1 * state_cost_429845112 
  v432191119  <- ASC432191119  + PRICE1 * state_cost_432191119 
  v432441094  <- ASC432441094  + PRICE1 * state_cost_432441094 
  v432821121  <- ASC432821121  + PRICE1 * state_cost_432821121 
  v432851114  <- ASC432851114  + PRICE1 * state_cost_432851114 
  v432871118  <- ASC432871118  + PRICE1 * state_cost_432871118 
  v432961130  <- ASC432961130  + PRICE1 * state_cost_432961130 
  v433021096  <- ASC433021096  + PRICE1 * state_cost_433021096 
  v433161127  <- ASC433161127  + PRICE1 * state_cost_433161127 
  v433281141  <- ASC433281141  + PRICE1 * state_cost_433281141 
  v433301142  <- ASC433301142  + PRICE1 * state_cost_433301142 
  v433311136  <- ASC433311136  + PRICE1 * state_cost_433311136 
  v433331137  <- ASC433331137  + PRICE1 * state_cost_433331137 
  v433341134  <- ASC433341134  + PRICE1 * state_cost_433341134 
  v433351138  <- ASC433351138  + PRICE1 * state_cost_433351138 
  v433361110  <- ASC433361110  + PRICE1 * state_cost_433361110 
  v433371133  <- ASC433371133  + PRICE1 * state_cost_433371133 
  v433381135  <- ASC433381135  + PRICE1 * state_cost_433381135 
  v433861169  <- ASC433861169  + PRICE1 * state_cost_433861169 
  v433871168  <- ASC433871168  + PRICE1 * state_cost_433871168 
  v433881170  <- ASC433881170  + PRICE1 * state_cost_433881170 
  v434091115  <- ASC434091115  + PRICE1 * state_cost_434091115 
  v434101116  <- ASC434101116  + PRICE1 * state_cost_434101116 
  v434151107  <- ASC434151107  + PRICE1 * state_cost_434151107 
  v434181139  <- ASC434181139  + PRICE1 * state_cost_434181139 
  v434261790  <- ASC434261790  + PRICE1 * state_cost_434261790 
  v438484334  <- ASC438484334  + PRICE1 * state_cost_438484334 
  v443501003  <- ASC443501003  + PRICE1 * state_cost_443501003 
  v444191536  <- ASC444191536  + PRICE1 * state_cost_444191536 
  v444561561  <- ASC444561561  + PRICE1 * state_cost_444561561 
  v444761542  <- ASC444761542  + PRICE1 * state_cost_444761542 
  v444991554  <- ASC444991554  + PRICE1 * state_cost_444991554 
  v445161553  <- ASC445161553  + PRICE1 * state_cost_445161553 
  v445201565  <- ASC445201565  + PRICE1 * state_cost_445201565 
  v446032066  <- ASC446032066  + PRICE1 * state_cost_446032066 
  v446573204  <- ASC446573204  + PRICE1 * state_cost_446573204 
  v446583206  <- ASC446583206  + PRICE1 * state_cost_446583206 
  v452453828  <- ASC452453828  + PRICE1 * state_cost_452453828 
  v452463826  <- ASC452463826  + PRICE1 * state_cost_452463826 
  v452473825  <- ASC452473825  + PRICE1 * state_cost_452473825 
  v452483827  <- ASC452483827  + PRICE1 * state_cost_452483827 
  v452753858  <- ASC452753858  + PRICE1 * state_cost_452753858 
  v452763855  <- ASC452763855  + PRICE1 * state_cost_452763855 
  v452773854  <- ASC452773854  + PRICE1 * state_cost_452773854  
  v452783857  <- ASC452783857  + PRICE1 * state_cost_452783857 
  v454184020  <- ASC454184020  + PRICE1 * state_cost_454184020 
  v454194019  <- ASC454194019  + PRICE1 * state_cost_454194019 
  v458844377  <- ASC458844377  + PRICE1 * state_cost_458844377 
  v458864376  <- ASC458864376  + PRICE1 * state_cost_458864376 
  v458874375  <- ASC458874375  + PRICE1 * state_cost_458874375 
  v458884378  <- ASC458884378  + PRICE1 * state_cost_458884378 
  v463502472  <- ASC463502472  + PRICE1 * state_cost_463502472 
  v463512471  <- ASC463512471  + PRICE1 * state_cost_463512471 
  v465043156  <- ASC465043156  + PRICE1 * state_cost_465043156 
  v465063157  <- ASC465063157  + PRICE1 * state_cost_465063157 
  v477851437  <- ASC477851437  + PRICE1 * state_cost_477851437 
  v477861438  <- ASC477861438  + PRICE1 * state_cost_477861438 
  v480992500  <- ASC480992500  + PRICE1 * state_cost_480992500 
  v481022508  <- ASC481022508  + PRICE1 * state_cost_481022508 
  v481032505  <- ASC481032505  + PRICE1 * state_cost_481032505 
  v481042504  <- ASC481042504  + PRICE1 * state_cost_481042504 
  v481052499  <- ASC481052499  + PRICE1 * state_cost_481052499 
  v481062503  <- ASC481062503  + PRICE1 * state_cost_481062503 
  v490864235  <- ASC490864235  + PRICE1 * state_cost_490864235 
  v491854246  <- ASC491854246  + PRICE1 * state_cost_491854246 
  v491864248  <- ASC491864248  + PRICE1 * state_cost_491864248 
  v491894249  <- ASC491894249  + PRICE1 * state_cost_491894249 
  v506864806  <- ASC506864806  + PRICE1 * state_cost_506864806 
  v516964816  <- ASC516964816  + PRICE1 * state_cost_516964816 
  v521461289  <- ASC521461289  + PRICE1 * state_cost_521461289 
  v521861292  <- ASC521861292  + PRICE1 * state_cost_521861292 
  v523121283  <- ASC523121283  + PRICE1 * state_cost_523121283 
  v523141284  <- ASC523141284  + PRICE1 * state_cost_523141284 
  v523161282  <- ASC523161282  + PRICE1 * state_cost_523161282 
  v523171287  <- ASC523171287  + PRICE1 * state_cost_523171287 
  v523181285  <- ASC523181285  + PRICE1 * state_cost_523181285 
  v523191286  <- ASC523191286  + PRICE1 * state_cost_523191286 
  v525631877  <- ASC525631877  + PRICE1 * state_cost_525631877 
  v525801874  <- ASC525801874  + PRICE1 * state_cost_525801874 
  v525811875  <- ASC525811875  + PRICE1 * state_cost_525811875 
  v525821873  <- ASC525821873  + PRICE1 * state_cost_525821873 
  v525931865  <- ASC525931865  + PRICE1 * state_cost_525931865 
  v525941866  <- ASC525941866  + PRICE1 * state_cost_525941866 
  v525951870  <- ASC525951870  + PRICE1 * state_cost_525951870 
  v525961862  <- ASC525961862  + PRICE1 * state_cost_525961862 
  v525971869  <- ASC525971869  + PRICE1 * state_cost_525971869 
  v525981863  <- ASC525981863  + PRICE1 * state_cost_525981863 
  v525991868  <- ASC525991868  + PRICE1 * state_cost_525991868 
  v528063140  <- ASC528063140  + PRICE1 * state_cost_528063140 
  v528353142  <- ASC528353142  + PRICE1 * state_cost_528353142 
  v532103914  <- ASC532103914  + PRICE1 * state_cost_532103914 
  v532113924  <- ASC532113924  + PRICE1 * state_cost_532113924 
  v532133912  <- ASC532133912  + PRICE1 * state_cost_532133912 
  v532143913  <- ASC532143913  + PRICE1 * state_cost_532143913 
  v532163911  <- ASC532163911  + PRICE1 * state_cost_532163911 
  v532183915  <- ASC532183915  + PRICE1 * state_cost_532183915 
  v544363564  <- ASC544363564  + PRICE1 * state_cost_544363564 
  v544463799  <- ASC544463799  + PRICE1 * state_cost_544463799 
  v544473798  <- ASC544473798  + PRICE1 * state_cost_544473798 
  v544483801  <- ASC544483801  + PRICE1 * state_cost_544483801 
  v547061635  <- ASC547061635  + PRICE1 * state_cost_547061635 
  v550663565  <- ASC550663565  + PRICE1 * state_cost_550663565 
  v550683566  <- ASC550683566  + PRICE1 * state_cost_550683566 
  v550843804  <- ASC550843804  + PRICE1 * state_cost_550843804 
  v550863803  <- ASC550863803  + PRICE1 * state_cost_550863803 
  v550873802  <- ASC550873802  + PRICE1 * state_cost_550873802 
  v550883805  <- ASC550883805  + PRICE1 * state_cost_550883805 
  v551063991  <- ASC551063991  + PRICE1 * state_cost_551063991 
  v555063810  <- ASC555063810  + PRICE1 * state_cost_555063810 
  v561953918  <- ASC561953918  + PRICE1 * state_cost_561953918 
  v561963916  <- ASC561963916  + PRICE1 * state_cost_561963916 
  v562063832  <- ASC562063832  + PRICE1 * state_cost_562063832 
  v571201242  <- ASC571201242  + PRICE1 * state_cost_571201242  
  v571251241  <- ASC571251241  + PRICE1 * state_cost_571251241 
  v571291244  <- ASC571291244  + PRICE1 * state_cost_571291244 
  v571441252  <- ASC571441252  + PRICE1 * state_cost_571441252 
  v571481243  <- ASC571481243  + PRICE1 * state_cost_571481243 
  v571571246  <- ASC571571246  + PRICE1 * state_cost_571571246 
  v571581251  <- ASC571581251  + PRICE1 * state_cost_571581251 
  v571611240  <- ASC571611240  + PRICE1 * state_cost_571611240 
  v571741250  <- ASC571741250  + PRICE1 * state_cost_571741250 
  v588011108  <- ASC588011108  + PRICE1 * state_cost_588011108 
  v588352963  <- ASC588352963  + PRICE1 * state_cost_588352963 
  v588362961  <- ASC588362961  + PRICE1 * state_cost_588362961 
  v588382962  <- ASC588382962  + PRICE1 * state_cost_588382962 
  v588402957  <- ASC588402957  + PRICE1 * state_cost_588402957 
  v588422975  <- ASC588422975  + PRICE1 * state_cost_588422975 
  v588602998  <- ASC588602998  + PRICE1 * state_cost_588602998 
  v588682969  <- ASC588682969  + PRICE1 * state_cost_588682969 
  v588722960  <- ASC588722960  + PRICE1 * state_cost_588722960 
  v588732971  <- ASC588732971  + PRICE1 * state_cost_588732971 
  v588752994  <- ASC588752994  + PRICE1 * state_cost_588752994 
  v588762993  <- ASC588762993  + PRICE1 * state_cost_588762993 
  v588852997  <- ASC588852997  + PRICE1 * state_cost_588852997 
  v588862996  <- ASC588862996  + PRICE1 * state_cost_588862996 
  v590371737  <- ASC590371737  + PRICE1 * state_cost_590371737 
  v591001960  <- ASC591001960  + PRICE1 * state_cost_591001960 
  v592312659  <- ASC592312659  + PRICE1 * state_cost_592312659 
  v620613048  <- ASC620613048  + PRICE1 * state_cost_620613048 
  v620973829  <- ASC620973829  + PRICE1 * state_cost_620973829 
  v624003402  <- ASC624003402  + PRICE1 * state_cost_624003402 
  v624053409  <- ASC624053409  + PRICE1 * state_cost_624053409 
  v624203414  <- ASC624203414  + PRICE1 * state_cost_624203414 
  v633554549  <- ASC633554549  + PRICE1 * state_cost_633554549 
  v633574553  <- ASC633574553  + PRICE1 * state_cost_633574553 
  v633594561  <- ASC633594561  + PRICE1 * state_cost_633594561 
  v635284382  <- ASC635284382  + PRICE1 * state_cost_635284382 
  v635304384  <- ASC635304384  + PRICE1 * state_cost_635304384 
  v637554961  <- ASC637554961  + PRICE1 * state_cost_637554961 
  v639595188  <- ASC639595188  + PRICE1 * state_cost_639595188 
  v639635214  <- ASC639635214  + PRICE1 * state_cost_639635214 
  v641361756  <- ASC641361756  + PRICE1 * state_cost_641361756 
  v645731333  <- ASC645731333  + PRICE1 * state_cost_645731333 
  v646011145  <- ASC646011145  + PRICE1 * state_cost_646011145 
  v646761226  <- ASC646761226  + PRICE1 * state_cost_646761226 
  v647111326  <- ASC647111326  + PRICE1 * state_cost_647111326 
  v647121331  <- ASC647121331  + PRICE1 * state_cost_647121331 
  v647161327  <- ASC647161327  + PRICE1 * state_cost_647161327 
  v647361348  <- ASC647361348  + PRICE1 * state_cost_647361348 
  v647551338  <- ASC647551338  + PRICE1 * state_cost_647551338 
  v647591339  <- ASC647591339  + PRICE1 * state_cost_647591339 
  v647761385  <- ASC647761385  + PRICE1 * state_cost_647761385 
  v648582044  <- ASC648582044  + PRICE1 * state_cost_648582044 
  v648622040  <- ASC648622040  + PRICE1 * state_cost_648622040 
  v648632042  <- ASC648632042  + PRICE1 * state_cost_648632042 
  v648642046  <- ASC648642046  + PRICE1 * state_cost_648642046 
  v648652045  <- ASC648652045  + PRICE1 * state_cost_648652045 
  v648662041  <- ASC648662041  + PRICE1 * state_cost_648662041 
  v648672039  <- ASC648672039  + PRICE1 * state_cost_648672039 
  v648682043  <- ASC648682043  + PRICE1 * state_cost_648682043 
  v648702047  <- ASC648702047  + PRICE1 * state_cost_648702047 
  v648761840  <- ASC648761840  + PRICE1 * state_cost_648761840 
  v649962135  <- ASC649962135  + PRICE1 * state_cost_649962135 
  v650662318  <- ASC650662318  + PRICE1 * state_cost_650662318 
  v651262343  <- ASC651262343  + PRICE1 * state_cost_651262343 
  v651272341  <- ASC651272341  + PRICE1 * state_cost_651272341 
  v651952633  <- ASC651952633  + PRICE1 * state_cost_651952633 
  v651994947  <- ASC651994947  + PRICE1 * state_cost_651994947 
  v652004948  <- ASC652004948  + PRICE1 * state_cost_652004948 
  v652482816  <- ASC652482816  + PRICE1 * state_cost_652482816 
  v652512814  <- ASC652512814  + PRICE1 * state_cost_652512814 
  v652532812  <- ASC652532812  + PRICE1 * state_cost_652532812  
  v652542810  <- ASC652542810  + PRICE1 * state_cost_652542810 
  v652562809  <- ASC652562809  + PRICE1 * state_cost_652562809 
  v652572808  <- ASC652572808  + PRICE1 * state_cost_652572808 
  v652582813  <- ASC652582813  + PRICE1 * state_cost_652582813 
  v666364310  <- ASC666364310  + PRICE1 * state_cost_666364310 
  v668364802  <- ASC668364802  + PRICE1 * state_cost_668364802 
  v669362356  <- ASC669362356  + PRICE1 * state_cost_669362356 
  v670063521  <- ASC670063521  + PRICE1 * state_cost_670063521 
  v672665510  <- ASC672665510  + PRICE1 * state_cost_672665510 
  v672675509  <- ASC672675509  + PRICE1 * state_cost_672675509 
  v675263042  <- ASC675263042  + PRICE1 * state_cost_675263042 
  v675273041  <- ASC675273041  + PRICE1 * state_cost_675273041 
  v675283044  <- ASC675283044  + PRICE1 * state_cost_675283044 
  v675563070  <- ASC675563070  + PRICE1 * state_cost_675563070 
  v675573069  <- ASC675573069  + PRICE1 * state_cost_675573069 
  v675863071  <- ASC675863071  + PRICE1 * state_cost_675863071 
  v681261155  <- ASC681261155  + PRICE1 * state_cost_681261155 
  v681271154  <- ASC681271154  + PRICE1 * state_cost_681271154 
  v683061942  <- ASC683061942  + PRICE1 * state_cost_683061942 
  v686113450  <- ASC686113450  + PRICE1 * state_cost_686113450 
  v688464799  <- ASC688464799  + PRICE1 * state_cost_688464799 
  v696111809  <- ASC696111809  + PRICE1 * state_cost_696111809 
  v696341816  <- ASC696341816  + PRICE1 * state_cost_696341816 
  v696361815  <- ASC696361815  + PRICE1 * state_cost_696361815 
  v696371814  <- ASC696371814  + PRICE1 * state_cost_696371814 
  v696381817  <- ASC696381817  + PRICE1 * state_cost_696381817 
  v696561822  <- ASC696561822  + PRICE1 * state_cost_696561822 
  v696571821  <- ASC696571821  + PRICE1 * state_cost_696571821 
  v696581823  <- ASC696581823  + PRICE1 * state_cost_696581823 
  v696661833  <- ASC696661833  + PRICE1 * state_cost_696661833 
  v696671832  <- ASC696671832  + PRICE1 * state_cost_696671832 
  v699404353  <- ASC699404353  + PRICE1 * state_cost_699404353 
  v699454355  <- ASC699454355  + PRICE1 * state_cost_699454355 
  v699464356  <- ASC699464356  + PRICE1 * state_cost_699464356 
  v699474354  <- ASC699474354  + PRICE1 * state_cost_699474354 
  v727221293  <- ASC727221293  + PRICE1 * state_cost_727221293 
  v730504347  <- ASC730504347  + PRICE1 * state_cost_730504347 
  v730514350  <- ASC730514350  + PRICE1 * state_cost_730514350 
  v730524343  <- ASC730524343  + PRICE1 * state_cost_730524343 
  v730534345  <- ASC730534345  + PRICE1 * state_cost_730534345 
  v730544346  <- ASC730544346  + PRICE1 * state_cost_730544346 
  v730554344  <- ASC730554344  + PRICE1 * state_cost_730554344 
  v731361254  <- ASC731361254  + PRICE1 * state_cost_731361254 
  v734561684  <- ASC734561684  + PRICE1 * state_cost_734561684 
  v735171645  <- ASC735171645  + PRICE1 * state_cost_735171645 
  v735261686  <- ASC735261686  + PRICE1 * state_cost_735261686 
  v737021965  <- ASC737021965  + PRICE1 * state_cost_737021965 
  v737151958  <- ASC737151958  + PRICE1 * state_cost_737151958 
  v739861750  <- ASC739861750  + PRICE1 * state_cost_739861750 
  v740862313  <- ASC740862313  + PRICE1 * state_cost_740862313 
  v747822800  <- ASC747822800  + PRICE1 * state_cost_747822800 
  v750873008  <- ASC750873008  + PRICE1 * state_cost_750873008 
  v752083115  <- ASC752083115  + PRICE1 * state_cost_752083115 
  v752103114  <- ASC752103114  + PRICE1 * state_cost_752103114 
  v752123110  <- ASC752123110  + PRICE1 * state_cost_752123110 
  v752143108  <- ASC752143108  + PRICE1 * state_cost_752143108 
  v752163111  <- ASC752163111  + PRICE1 * state_cost_752163111 
  v760323516  <- ASC760323516  + PRICE1 * state_cost_760323516 
  v760363507  <- ASC760363507  + PRICE1 * state_cost_760363507 
  v760383514  <- ASC760383514  + PRICE1 * state_cost_760383514 
  v760403520  <- ASC760403520  + PRICE1 * state_cost_760403520 
  v760423511  <- ASC760423511  + PRICE1 * state_cost_760423511 
  v760443513  <- ASC760443513  + PRICE1 * state_cost_760443513 
  v762273543  <- ASC762273543  + PRICE1 * state_cost_762273543 
  v764363723  <- ASC764363723  + PRICE1 * state_cost_764363723 
  v764783788  <- ASC764783788  + PRICE1 * state_cost_764783788 
  v764863795  <- ASC764863795  + PRICE1 * state_cost_764863795 
  v764873794  <- ASC764873794  + PRICE1 * state_cost_764873794 
  v764883796  <- ASC764883796  + PRICE1 * state_cost_764883796  
  v765263831  <- ASC765263831  + PRICE1 * state_cost_765263831 
  v767673999  <- ASC767673999  + PRICE1 * state_cost_767673999 
  v773184326  <- ASC773184326  + PRICE1 * state_cost_773184326 
  v773754673  <- ASC773754673  + PRICE1 * state_cost_773754673 
  v773784677  <- ASC773784677  + PRICE1 * state_cost_773784677 
  v773954559  <- ASC773954559  + PRICE1 * state_cost_773954559 
  v774724901  <- ASC774724901  + PRICE1 * state_cost_774724901 
  v774724901  <- ASC774724901  + PRICE1 * state_cost_774724901 
  v774875094  <- ASC774875094  + PRICE1 * state_cost_774875094 
  v777355268  <- ASC777355268  + PRICE1 * state_cost_777355268 
  v777404660  <- ASC777404660  + PRICE1 * state_cost_777404660 
  v777745327  <- ASC777745327  + PRICE1 * state_cost_777745327 
  v777765329  <- ASC777765329  + PRICE1 * state_cost_777765329 
  v777865135  <- ASC777865135  + PRICE1 * state_cost_777865135 
  v778424689  <- ASC778424689  + PRICE1 * state_cost_778424689 
  v778474697  <- ASC778474697  + PRICE1 * state_cost_778474697 
  v778524716  <- ASC778524716  + PRICE1 * state_cost_778524716 
  v784563813  <- ASC784563813  + PRICE1 * state_cost_784563813 
  v788663814  <- ASC788663814  + PRICE1 * state_cost_788663814 
  v793363815  <- ASC793363815  + PRICE1 * state_cost_793363815 
  v798163816  <- ASC798163816  + PRICE1 * state_cost_798163816 
  v800961711  <- ASC800961711  + PRICE1 * state_cost_800961711 
  v801452153  <- ASC801452153  + PRICE1 * state_cost_801452153 
  v802362680  <- ASC802362680  + PRICE1 * state_cost_802362680 
  v804404276  <- ASC804404276  + PRICE1 * state_cost_804404276 
  v804564370  <- ASC804564370  + PRICE1 * state_cost_804564370 
  v804574369  <- ASC804574369  + PRICE1 * state_cost_804574369 
  v804584371  <- ASC804584371  + PRICE1 * state_cost_804584371 
  v806861638  <- ASC806861638  + PRICE1 * state_cost_806861638 
  v806871637  <- ASC806871637  + PRICE1 * state_cost_806871637 
  v808262658  <- ASC808262658  + PRICE1 * state_cost_808262658 
  v808272657  <- ASC808272657  + PRICE1 * state_cost_808272657 
  v811963839  <- ASC811963839  + PRICE1 * state_cost_811963839 
  v812043837  <- ASC812043837  + PRICE1 * state_cost_812043837 
  v812063836  <- ASC812063836  + PRICE1 * state_cost_812063836 
  v812073835  <- ASC812073835  + PRICE1 * state_cost_812073835 
  v812083838  <- ASC812083838  + PRICE1 * state_cost_812083838 
  v814065047  <- ASC814065047  + PRICE1 * state_cost_814065047 
  v826061716  <- ASC826061716  + PRICE1 * state_cost_826061716 
  v826071715  <- ASC826071715  + PRICE1 * state_cost_826071715 
  v826101669  <- ASC826101669  + PRICE1 * state_cost_826101669 
  v826111670  <- ASC826111670  + PRICE1 * state_cost_826111670 
  v826181673  <- ASC826181673  + PRICE1 * state_cost_826181673 
  v826261648  <- ASC826261648  + PRICE1 * state_cost_826261648 
  v826271647  <- ASC826271647  + PRICE1 * state_cost_826271647 
  v826361667  <- ASC826361667  + PRICE1 * state_cost_826361667 
  v826371666  <- ASC826371666  + PRICE1 * state_cost_826371666 
  v827861643  <- ASC827861643  + PRICE1 * state_cost_827861643 
  v827871639  <- ASC827871639  + PRICE1 * state_cost_827871639 
  v828201723  <- ASC828201723  + PRICE1 * state_cost_828201723 
  v828261702  <- ASC828261702  + PRICE1 * state_cost_828261702 
  v828271708  <- ASC828271708  + PRICE1 * state_cost_828271708 
  v828401700  <- ASC828401700  + PRICE1 * state_cost_828401700 
  v828461681  <- ASC828461681  + PRICE1 * state_cost_828461681 
  v828471697  <- ASC828471697  + PRICE1 * state_cost_828471697 
  v828491699  <- ASC828491699  + PRICE1 * state_cost_828491699 
  v828661729  <- ASC828661729  + PRICE1 * state_cost_828661729 
  v828671728  <- ASC828671728  + PRICE1 * state_cost_828671728 
  v828811672  <- ASC828811672  + PRICE1 * state_cost_828811672 
  v829572064  <- ASC829572064  + PRICE1 * state_cost_829572064 
  v833632660  <- ASC833632660  + PRICE1 * state_cost_833632660 
  v833702664  <- ASC833702664  + PRICE1 * state_cost_833702664 
  v839063438  <- ASC839063438  + PRICE1 * state_cost_839063438 
  v839073437  <- ASC839073437  + PRICE1 * state_cost_839073437 
  v844563834  <- ASC844563834  + PRICE1 * state_cost_844563834 
  v844573833  <- ASC844573833  + PRICE1 * state_cost_844573833 
  v844863807  <- ASC844863807  + PRICE1 * state_cost_844863807 
  v844963844  <- ASC844963844  + PRICE1 * state_cost_844963844 
  v846174003  <- ASC846174003  + PRICE1 * state_cost_846174003  
  v855261710  <- ASC855261710  + PRICE1 * state_cost_855261710 
  v856063806  <- ASC856063806  + PRICE1 * state_cost_856063806 
  v861121724  <- ASC861121724  + PRICE1 * state_cost_861121724 
  v862503016  <- ASC862503016  + PRICE1 * state_cost_862503016 
  v862513015  <- ASC862513015  + PRICE1 * state_cost_862513015 
  v863103198  <- ASC863103198  + PRICE1 * state_cost_863103198 
  v863903545  <- ASC863903545  + PRICE1 * state_cost_863903545 
  v865063847  <- ASC865063847  + PRICE1 * state_cost_865063847 
  v865073846  <- ASC865073846  + PRICE1 * state_cost_865073846 
  v866375097  <- ASC866375097  + PRICE1 * state_cost_866375097 
  v866692777  <- ASC866692777  + PRICE1 * state_cost_866692777 
  v866702776  <- ASC866702776  + PRICE1 * state_cost_866702776 
  v866722775  <- ASC866722775  + PRICE1 * state_cost_866722775 
  v866922771  <- ASC866922771  + PRICE1 * state_cost_866922771 
  v866932770  <- ASC866932770  + PRICE1 * state_cost_866932770 
  v867964779  <- ASC867964779  + PRICE1 * state_cost_867964779 
  v868074765  <- ASC868074765  + PRICE1 * state_cost_868074765 
  v868814774  <- ASC868814774  + PRICE1 * state_cost_868814774 
  v868834760  <- ASC868834760  + PRICE1 * state_cost_868834760 
  v868844758  <- ASC868844758  + PRICE1 * state_cost_868844758 
  v868854775  <- ASC868854775  + PRICE1 * state_cost_868854775 
  v868864757  <- ASC868864757  + PRICE1 * state_cost_868864757 
  v868874756  <- ASC868874756  + PRICE1 * state_cost_868874756 
  v868884776  <- ASC868884776  + PRICE1 * state_cost_868884776 
  v869164762  <- ASC869164762  + PRICE1 * state_cost_869164762 
  v874082985  <- ASC874082985  + PRICE1 * state_cost_874082985 
  v874092984  <- ASC874092984  + PRICE1 * state_cost_874092984 
  v874851780  <- ASC874851780  + PRICE1 * state_cost_874851780 
  v875861912  <- ASC875861912  + PRICE1 * state_cost_875861912 
  v875961916  <- ASC875961916  + PRICE1 * state_cost_875961916 
  v876431990  <- ASC876431990  + PRICE1 * state_cost_876431990 
  v879363013  <- ASC879363013  + PRICE1 * state_cost_879363013 
  v879373012  <- ASC879373012  + PRICE1 * state_cost_879373012 
  v879383014  <- ASC879383014  + PRICE1 * state_cost_879383014 
  v879973197  <- ASC879973197  + PRICE1 * state_cost_879973197 
  v880363412  <- ASC880363412  + PRICE1 * state_cost_880363412 
  v881473546  <- ASC881473546  + PRICE1 * state_cost_881473546 
  v882943897  <- ASC882943897  + PRICE1 * state_cost_882943897 
  v882963898  <- ASC882963898  + PRICE1 * state_cost_882963898 
  v885564398  <- ASC885564398  + PRICE1 * state_cost_885564398 
  v887665099  <- ASC887665099  + PRICE1 * state_cost_887665099 
  v887675098  <- ASC887675098  + PRICE1 * state_cost_887675098 
  v887685100  <- ASC887685100  + PRICE1 * state_cost_887685100 
  v891211179  <- ASC891211179  + PRICE1 * state_cost_891211179 
  v891541783  <- ASC891541783  + PRICE1 * state_cost_891541783 
  v891751778  <- ASC891751778  + PRICE1 * state_cost_891751778 
  v891912983  <- ASC891912983  + PRICE1 * state_cost_891912983 
  v891932981  <- ASC891932981  + PRICE1 * state_cost_891932981 
  v891942977  <- ASC891942977  + PRICE1 * state_cost_891942977 
  v891962980  <- ASC891962980  + PRICE1 * state_cost_891962980 
  v891972979  <- ASC891972979  + PRICE1 * state_cost_891972979 
  v891982982  <- ASC891982982  + PRICE1 * state_cost_891982982 
  v891992978  <- ASC891992978  + PRICE1 * state_cost_891992978 
  v892153005  <- ASC892153005  + PRICE1 * state_cost_892153005 
  v892421420  <- ASC892421420  + PRICE1 * state_cost_892421420 
  v892781907  <- ASC892781907  + PRICE1 * state_cost_892781907 
  v893863010  <- ASC893863010  + PRICE1 * state_cost_893863010 
  v893873009  <- ASC893873009  + PRICE1 * state_cost_893873009 
  v893883011  <- ASC893883011  + PRICE1 * state_cost_893883011 
  v894473195  <- ASC894473195  + PRICE1 * state_cost_894473195 
  v894963405  <- ASC894963405  + PRICE1 * state_cost_894963405 
  v895331989  <- ASC895331989  + PRICE1 * state_cost_895331989 
  v895773544  <- ASC895773544  + PRICE1 * state_cost_895773544 
  v896174404  <- ASC896174404  + PRICE1 * state_cost_896174404 
  v897864410  <- ASC897864410  + PRICE1 * state_cost_897864410 
  v897874409  <- ASC897874409  + PRICE1 * state_cost_897874409 
  v897884411  <- ASC897884411  + PRICE1 * state_cost_897884411 
  v898362619  <- ASC898362619  + PRICE1 * state_cost_898362619 
  v898372618  <- ASC898372618  + PRICE1 * state_cost_898372618  
  v898875089  <- ASC898875089  + PRICE1 * state_cost_898875089 
  v899165095  <- ASC899165095  + PRICE1 * state_cost_899165095 
  v902836770  <- ASC902836770  + PRICE1 * state_cost_902836770 
  v9046891337 <- ASC9046891337 + PRICE1 * state_cost_9046891337
  v9368593476 <- ASC9368593476 + PRICE1 * state_cost_9368593476
  v9369673634 <- ASC9369673634 + PRICE1 * state_cost_9369673634
  v9465743475 <- ASC9465743475 + PRICE1 * state_cost_9465743475
  v9885364401 <- ASC9885364401 + PRICE1 * state_cost_9885364401
  
  
  p <-(exp(v9999      ) * (chosen == 9999)      +
         exp(v734190    ) * (chosen == 734190    ) +
         exp(v1573935   ) * (chosen == 1573935   ) +
         exp(v2870505   ) * (chosen == 2870505   ) +
         exp(v4356521   ) * (chosen == 4356521   ) +
         exp(v4626831   ) * (chosen == 4626831   ) +
         exp(v5915426   ) * (chosen == 5915426   ) +
         exp(v6144352   ) * (chosen == 6144352   ) +
         exp(v8417089   ) * (chosen == 8417089   ) +
         exp(v8751020   ) * (chosen == 8751020   ) +
         exp(v8920617   ) * (chosen == 8920617   ) +
         exp(v10548697  ) * (chosen == 10548697  ) +
         exp(v10549698  ) * (chosen == 10549698  ) +
         exp(v10550695  ) * (chosen == 10550695  ) +
         exp(v10553690  ) * (chosen == 10553690  ) +
         exp(v10555696  ) * (chosen == 10555696  ) +
         exp(v11586693  ) * (chosen == 11586693  ) +
         exp(v11588694  ) * (chosen == 11588694  ) +
         exp(v11771692  ) * (chosen == 11771692  ) +
         exp(v11773688  ) * (chosen == 11773688  ) +
         exp(v11774687  ) * (chosen == 11774687  ) +
         exp(v11776686  ) * (chosen == 11776686  ) +
         exp(v11777685  ) * (chosen == 11777685  ) +
         exp(v11786699  ) * (chosen == 11786699  ) +
         exp(v11788689  ) * (chosen == 11788689  ) +
         exp(v13774351  ) * (chosen == 13774351  ) +
         exp(v14001522  ) * (chosen == 14001522  ) +
         exp(v15776973  ) * (chosen == 15776973  ) +
         exp(v16516267  ) * (chosen == 16516267  ) +
         exp(v16517266  ) * (chosen == 16517266  ) +
         exp(v16518268  ) * (chosen == 16518268  ) +
         exp(v16676562  ) * (chosen == 16676562  ) +
         exp(v16987789  ) * (chosen == 16987789  ) +
         exp(v16988790  ) * (chosen == 16988790  ) +
         exp(v17086850  ) * (chosen == 17086850  ) +
         exp(v17127981  ) * (chosen == 17127981  ) +
         exp(v17991132  ) * (chosen == 17991132  ) +
         exp(v18006841  ) * (chosen == 18006841  ) +
         exp(v21480620  ) * (chosen == 21480620  ) +
         exp(v22291300  ) * (chosen == 22291300  ) +
         exp(v22783572  ) * (chosen == 22783572  ) +
         exp(v22784571  ) * (chosen == 22784571  ) +
         exp(v22786570  ) * (chosen == 22786570  ) +
         exp(v22787569  ) * (chosen == 22787569  ) +
         exp(v22788573  ) * (chosen == 22788573  ) +
         exp(v27025847  ) * (chosen == 27025847  ) +
         exp(v27474650  ) * (chosen == 27474650  ) +
         exp(v28086580  ) * (chosen == 28086580  ) +
         exp(v28087579  ) * (chosen == 28087579  ) +
         exp(v28088582  ) * (chosen == 28088582  ) +
         exp(v28206761  ) * (chosen == 28206761  ) +
         exp(v28233766  ) * (chosen == 28233766  ) +
         exp(v28236769  ) * (chosen == 28236769  ) +
         exp(v29287548  ) * (chosen == 29287548  ) +
         exp(v29288550  ) * (chosen == 29288550  ) +
         exp(v29566892  ) * (chosen == 29566892  ) +
         exp(v29568893  ) * (chosen == 29568893  ) +
         exp(v31352818  ) * (chosen == 31352818  ) +
         exp(v33102819  ) * (chosen == 33102819  ) +
         exp(v34001179  ) * (chosen == 34001179  ) +
         exp(v34004177  ) * (chosen == 34004177  ) +
         exp(v34006175  ) * (chosen == 34006175  ) +
         exp(v34007174  ) * (chosen == 34007174  ) +
         exp(v34008178  ) * (chosen == 34008178  ) +
         exp(v34014137  ) * (chosen == 34014137  ) +
         exp(v34021113  ) * (chosen == 34021113  ) +
         exp(v34026169  ) * (chosen == 34026169  ) +
         exp(v34029146  ) * (chosen == 34029146  ) +
         exp(v34030147  ) * (chosen == 34030147  ) +
         exp(v34032148  ) * (chosen == 34032148  ) +
         exp(v34036168  ) * (chosen == 34036168  ) +
         exp(v34052171  ) * (chosen == 34052171  ) +
         exp(v34061172  ) * (chosen == 34061172  ) +
         exp(v34076184  ) * (chosen == 34076184  ) +
         exp(v34116159  ) * (chosen == 34116159  ) +
         exp(v34117158  ) * (chosen == 34117158  ) +
         exp(v34155616  ) * (chosen == 34155616  ) +
         exp(v34162614  ) * (chosen == 34162614  ) +
         exp(v35315561  ) * (chosen == 35315561  ) +
         exp(v35316556  ) * (chosen == 35316556  ) +
         exp(v35317555  ) * (chosen == 35317555  ) +
         exp(v35318559  ) * (chosen == 35318559  ) +
         exp(v35354162  ) * (chosen == 35354162  ) +
         exp(v35416952  ) * (chosen == 35416952  ) +
         exp(v35418953  ) * (chosen == 35418953  ) +
         exp(v36572532  ) * (chosen == 36572532  ) +
         exp(v40482820  ) * (chosen == 40482820  ) +
         exp(v41271111  ) * (chosen == 41271111  ) +
         exp(v41279896  ) * (chosen == 41279896  ) +
         exp(v41284608  ) * (chosen == 41284608  ) +
         exp(v41299880  ) * (chosen == 41299880  ) +
         exp(v41316966  ) * (chosen == 41316966  ) +
         exp(v41320957  ) * (chosen == 41320957  ) +
         exp(v41413922  ) * (chosen == 41413922  ) +
         exp(v41840902  ) * (chosen == 41840902  ) +
         exp(v41927891  ) * (chosen == 41927891  ) +
         exp(v42006282  ) * (chosen == 42006282  ) +
         exp(v43022218  ) * (chosen == 43022218  ) +
         exp(v43024215  ) * (chosen == 43024215  ) +
         exp(v43025219  ) * (chosen == 43025219  ) +
         exp(v43026214  ) * (chosen == 43026214  ) +
         exp(v43027213  ) * (chosen == 43027213  ) +
         exp(v43028216  ) * (chosen == 43028216  ) +
         exp(v43031437  ) * (chosen == 43031437  ) +
         exp(v43035438  ) * (chosen == 43035438  ) +
         exp(v43036432  ) * (chosen == 43036432  ) +
         exp(v43037431  ) * (chosen == 43037431  ) +
         exp(v43038436  ) * (chosen == 43038436  ) +
         exp(v43046475  ) * (chosen == 43046475  ) +
         exp(v43048476  ) * (chosen == 43048476  ) +
         exp(v43050426  ) * (chosen == 43050426  ) +
         exp(v43051427  ) * (chosen == 43051427  ) +
         exp(v43066467  ) * (chosen == 43066467  ) +
         exp(v43076208  ) * (chosen == 43076208  ) +
         exp(v43077207  ) * (chosen == 43077207  ) +
         exp(v43116455  ) * (chosen == 43116455  ) +
         exp(v43117454  ) * (chosen == 43117454  ) +
         exp(v43120477  ) * (chosen == 43120477  ) +
         exp(v43121483  ) * (chosen == 43121483  ) +
         exp(v43123481  ) * (chosen == 43123481  ) +
         exp(v43124480  ) * (chosen == 43124480  ) +
         exp(v43125484  ) * (chosen == 43125484  ) +
         exp(v43126479  ) * (chosen == 43126479  ) +
         exp(v43127478  ) * (chosen == 43127478  ) +
         exp(v43128482  ) * (chosen == 43128482  ) +
         exp(v43136446  ) * (chosen == 43136446  ) +
         exp(v43137443  ) * (chosen == 43137443  ) +
         exp(v43138448  ) * (chosen == 43138448  ) +
         exp(v43145469  ) * (chosen == 43145469  ) +
         exp(v43156402  ) * (chosen == 43156402  ) +
         exp(v43197488  ) * (chosen == 43197488  ) +
         exp(v43205458  ) * (chosen == 43205458  ) +
         exp(v43209415  ) * (chosen == 43209415  ) +
         exp(v43984204  ) * (chosen == 43984204  ) +
         exp(v43986205  ) * (chosen == 43986205  ) +
         exp(v44217554  ) * (chosen == 44217554  ) +
         exp(v44258701  ) * (chosen == 44258701  ) +
         exp(v47161262  ) * (chosen == 47161262  ) +
         exp(v47171261  ) * (chosen == 47171261  ) +
         exp(v48661746  ) * (chosen == 48661746  ) +
         exp(v48671745  ) * (chosen == 48671745  ) +
         exp(v48681749  ) * (chosen == 48681749  ) +
         exp(v49361993  ) * (chosen == 49361993  ) +
         exp(v50062247  ) * (chosen == 50062247  ) +
         exp(v50362273  ) * (chosen == 50362273  ) +
         exp(v50372272  ) * (chosen == 50372272  ) +
         exp(v52882750  ) * (chosen == 52882750  ) +
         exp(v52892748  ) * (chosen == 52892748  ) +
         exp(v52902749  ) * (chosen == 52902749  ) +
         exp(v53262930  ) * (chosen == 53262930  ) +
         exp(v53272929  ) * (chosen == 53272929  ) +
         exp(v53462949  ) * (chosen == 53462949  ) +
         exp(v53472952  ) * (chosen == 53472952  ) +
         exp(v53492951  ) * (chosen == 53492951  ) +
         exp(v54056332  ) * (chosen == 54056332  ) +
         exp(v54057331  ) * (chosen == 54057331  ) +
         exp(v54646335  ) * (chosen == 54646335  ) +
         exp(v54647334  ) * (chosen == 54647334  ) +
         exp(v54863315  ) * (chosen == 54863315  ) +
         exp(v55246362  ) * (chosen == 55246362  ) +
         exp(v55963447  ) * (chosen == 55963447  ) +
         exp(v56063448  ) * (chosen == 56063448  ) +
         exp(v56828417  ) * (chosen == 56828417  ) +
         exp(v56840464  ) * (chosen == 56840464  ) +
         exp(v56843466  ) * (chosen == 56843466  ) +
         exp(v56846465  ) * (chosen == 56846465  ) +
         exp(v56850463  ) * (chosen == 56850463  ) +
         exp(v56957542  ) * (chosen == 56957542  ) +
         exp(v56958543  ) * (chosen == 56958543  ) +
         exp(v57051553  ) * (chosen == 57051553  ) +
         exp(v57092420  ) * (chosen == 57092420  ) +
         exp(v64336401  ) * (chosen == 64336401  ) +
         exp(v68022508  ) * (chosen == 68022508  ) +
         exp(v68036500  ) * (chosen == 68036500  ) +
         exp(v68037498  ) * (chosen == 68037498  ) +
         exp(v68038502  ) * (chosen == 68038502  ) +
         exp(v68039501  ) * (chosen == 68039501  ) +
         exp(v68049509  ) * (chosen == 68049509  ) +
         exp(v69971349  ) * (chosen == 69971349  ) +
         exp(v69981351  ) * (chosen == 69981351  ) +
         exp(v71886256  ) * (chosen == 71886256  ) +
         exp(v72816991  ) * (chosen == 72816991  ) +
         exp(v79026343  ) * (chosen == 79026343  ) +
         exp(v80571355  ) * (chosen == 80571355  ) +
         exp(v80574352  ) * (chosen == 80574352  ) +
         exp(v80576351  ) * (chosen == 80576351  ) +
         exp(v80577350  ) * (chosen == 80577350  ) +
         exp(v80578354  ) * (chosen == 80578354  ) +
         exp(v81966346  ) * (chosen == 81966346  ) +
         exp(v82082628  ) * (chosen == 82082628  ) +
         exp(v82126349  ) * (chosen == 82126349  ) +
         exp(v82127348  ) * (chosen == 82127348  ) +
         exp(v82147356  ) * (chosen == 82147356  ) +
         exp(v82187359  ) * (chosen == 82187359  ) +
         exp(v88253245  ) * (chosen == 88253245  ) +
         exp(v88263244  ) * (chosen == 88263244  ) +
         exp(v88273241  ) * (chosen == 88273241  ) +
         exp(v88283243  ) * (chosen == 88283243  ) +
         exp(v89139984  ) * (chosen == 89139984  ) +
         exp(v92783722  ) * (chosen == 92783722  ) +
         exp(v94263877  ) * (chosen == 94263877  ) +
         exp(v94283878  ) * (chosen == 94283878  ) +
         exp(v100064432 ) * (chosen == 100064432 ) +
         exp(v100084433 ) * (chosen == 100084433 ) +
         exp(v100094431 ) * (chosen == 100094431 ) +
         exp(v102785180 ) * (chosen == 102785180 ) +
         exp(v106251054 ) * (chosen == 106251054 ) +
         exp(v106261050 ) * (chosen == 106261050 ) +
         exp(v106271048 ) * (chosen == 106271048 ) +
         exp(v106281053 ) * (chosen == 106281053 ) +
         exp(v108051512 ) * (chosen == 108051512 ) +
         exp(v108071511 ) * (chosen == 108071511 ) +
         exp(v108081510 ) * (chosen == 108081510 ) +
         exp(v108181507 ) * (chosen == 108181507 ) +
         exp(v108361481 ) * (chosen == 108361481 ) +
         exp(v108902109 ) * (chosen == 108902109 ) +
         exp(v112901489 ) * (chosen == 112901489 ) +
         exp(v112931487 ) * (chosen == 112931487 ) +
         exp(v112941486 ) * (chosen == 112941486 ) +
         exp(v112961476 ) * (chosen == 112961476 ) +
         exp(v112971485 ) * (chosen == 112971485 ) +
         exp(v112981488 ) * (chosen == 112981488 ) +
         exp(v113464508 ) * (chosen == 113464508 ) +
         exp(v113474507 ) * (chosen == 113474507 ) +
         exp(v113484509 ) * (chosen == 113484509 ) +
         exp(v113661518 ) * (chosen == 113661518 ) +
         exp(v119361068 ) * (chosen == 119361068 ) +
         exp(v124041066 ) * (chosen == 124041066 ) +
         exp(v124061065 ) * (chosen == 124061065 ) +
         exp(v124071064 ) * (chosen == 124071064 ) +
         exp(v124081067 ) * (chosen == 124081067 ) +
         exp(v124641076 ) * (chosen == 124641076 ) +
         exp(v124661075 ) * (chosen == 124661075 ) +
         exp(v124671074 ) * (chosen == 124671074 ) +
         exp(v124761073 ) * (chosen == 124761073 ) +
         exp(v124781072 ) * (chosen == 124781072 ) +
         exp(v124791071 ) * (chosen == 124791071 ) +
         exp(v126681083 ) * (chosen == 126681083 ) +
         exp(v128564285 ) * (chosen == 128564285 ) +
         exp(v128884281 ) * (chosen == 128884281 ) +
         exp(v130361080 ) * (chosen == 130361080 ) +
         exp(v130371079 ) * (chosen == 130371079 ) +
         exp(v130381081 ) * (chosen == 130381081 ) +
         exp(v133883651 ) * (chosen == 133883651 ) +
         exp(v136383284 ) * (chosen == 136383284 ) +
         exp(v141923976 ) * (chosen == 141923976 ) +
         exp(v141993977 ) * (chosen == 141993977 ) +
         exp(v152465358 ) * (chosen == 152465358 ) +
         exp(v152475357 ) * (chosen == 152475357 ) +
         exp(v152485359 ) * (chosen == 152485359 ) +
         exp(v152965362 ) * (chosen == 152965362 ) +
         exp(v156262825 ) * (chosen == 156262825 ) +
         exp(v156272824 ) * (chosen == 156272824 ) +
         exp(v156282827 ) * (chosen == 156282827 ) +
         exp(v156442826 ) * (chosen == 156442826 ) +
         exp(v156774143 ) * (chosen == 156774143 ) +
         exp(v159405139 ) * (chosen == 159405139 ) +
         exp(v172061186 ) * (chosen == 172061186 ) +
         exp(v178261887 ) * (chosen == 178261887 ) +
         exp(v178301888 ) * (chosen == 178301888 ) +
         exp(v179161934 ) * (chosen == 179161934 ) +
         exp(v179561971 ) * (chosen == 179561971 ) +
         exp(v179581973 ) * (chosen == 179581973 ) +
         exp(v181961984 ) * (chosen == 181961984 ) +
         exp(v181981986 ) * (chosen == 181981986 ) +
         exp(v184072158 ) * (chosen == 184072158 ) +
         exp(v184082159 ) * (chosen == 184082159 ) +
         exp(v190262884 ) * (chosen == 190262884 ) +
         exp(v190612905 ) * (chosen == 190612905 ) +
         exp(v190632876 ) * (chosen == 190632876 ) +
         exp(v190642874 ) * (chosen == 190642874 ) +
         exp(v190662873 ) * (chosen == 190662873 ) +
         exp(v190672872 ) * (chosen == 190672872 ) +
         exp(v190682877 ) * (chosen == 190682877 ) +
         exp(v190822906 ) * (chosen == 190822906 ) +
         exp(v190962911 ) * (chosen == 190962911 ) +
         exp(v191122890 ) * (chosen == 191122890 ) +
         exp(v192263131 ) * (chosen == 192263131 ) +
         exp(v194763335 ) * (chosen == 194763335 ) +
         exp(v194773333 ) * (chosen == 194773333 ) +
         exp(v194783336 ) * (chosen == 194783336 ) +
         exp(v194863332 ) * (chosen == 194863332 ) +
         exp(v202463689 ) * (chosen == 202463689 ) +
         exp(v202473688 ) * (chosen == 202473688 ) +
         exp(v202483690 ) * (chosen == 202483690 ) +
         exp(v202863693 ) * (chosen == 202863693 ) +
         exp(v215954944 ) * (chosen == 215954944 ) +
         exp(v215964941 ) * (chosen == 215964941 ) +
         exp(v215974940 ) * (chosen == 215974940 ) +
         exp(v215984943 ) * (chosen == 215984943 ) +
         exp(v221215325 ) * (chosen == 221215325 ) +
         exp(v221555322 ) * (chosen == 221555322 ) +
         exp(v221565319 ) * (chosen == 221565319 ) +
         exp(v221575318 ) * (chosen == 221575318 ) +
         exp(v222135384 ) * (chosen == 222135384 ) +
         exp(v222165385 ) * (chosen == 222165385 ) +
         exp(v232771028 ) * (chosen == 232771028 ) +
         exp(v232781030 ) * (chosen == 232781030 ) +
         exp(v236262086 ) * (chosen == 236262086 ) +
         exp(v237082092 ) * (chosen == 237082092 ) +
         exp(v238232083 ) * (chosen == 238232083 ) +
         exp(v238242082 ) * (chosen == 238242082 ) +
         exp(v238262081 ) * (chosen == 238262081 ) +
         exp(v238272080 ) * (chosen == 238272080 ) +
         exp(v238282084 ) * (chosen == 238282084 ) +
         exp(v241562463 ) * (chosen == 241562463 ) +
         exp(v241572462 ) * (chosen == 241572462 ) +
         exp(v241582464 ) * (chosen == 241582464 ) +
         exp(v244543085 ) * (chosen == 244543085 ) +
         exp(v244563084 ) * (chosen == 244563084 ) +
         exp(v244573083 ) * (chosen == 244573083 ) +
         exp(v244583087 ) * (chosen == 244583087 ) +
         exp(v244663089 ) * (chosen == 244663089 ) +
         exp(v247063452 ) * (chosen == 247063452 ) +
         exp(v247283453 ) * (chosen == 247283453 ) +
         exp(v256034475 ) * (chosen == 256034475 ) +
         exp(v256044474 ) * (chosen == 256044474 ) +
         exp(v256064473 ) * (chosen == 256064473 ) +
         exp(v256074472 ) * (chosen == 256074472 ) +
         exp(v256084476 ) * (chosen == 256084476 ) +
         exp(v256164478 ) * (chosen == 256164478 ) +
         exp(v258764867 ) * (chosen == 258764867 ) +
         exp(v258774866 ) * (chosen == 258774866 ) +
         exp(v258784869 ) * (chosen == 258784869 ) +
         exp(v265862175 ) * (chosen == 265862175 ) +
         exp(v266562181 ) * (chosen == 266562181 ) +
         exp(v268202791 ) * (chosen == 268202791 ) +
         exp(v268212792 ) * (chosen == 268212792 ) +
         exp(v268222784 ) * (chosen == 268222784 ) +
         exp(v268232789 ) * (chosen == 268232789 ) +
         exp(v268242793 ) * (chosen == 268242793 ) +
         exp(v268262788 ) * (chosen == 268262788 ) +
         exp(v268272787 ) * (chosen == 268272787 ) +
         exp(v268282790 ) * (chosen == 268282790 ) +
         exp(v269062795 ) * (chosen == 269062795 ) +
         exp(v270271374 ) * (chosen == 270271374 ) +
         exp(v270562907 ) * (chosen == 270562907 ) +
         exp(v271024938 ) * (chosen == 271024938 ) +
         exp(v273922903 ) * (chosen == 273922903 ) +
         exp(v274102893 ) * (chosen == 274102893 ) +
         exp(v274541302 ) * (chosen == 274541302 ) +
         exp(v274791372 ) * (chosen == 274791372 ) +
         exp(v275444212 ) * (chosen == 275444212 ) +
         exp(v275544501 ) * (chosen == 275544501 ) +
         exp(v275624505 ) * (chosen == 275624505 ) +
         exp(v275852626 ) * (chosen == 275852626 ) +
         exp(v286252492 ) * (chosen == 286252492 ) +
         exp(v287184070 ) * (chosen == 287184070 ) +
         exp(v288664915 ) * (chosen == 288664915 ) +
         exp(v288674914 ) * (chosen == 288674914 ) +
         exp(v288684918 ) * (chosen == 288684918 ) +
         exp(v288904925 ) * (chosen == 288904925 ) +
         exp(v297281031 ) * (chosen == 297281031 ) +
         exp(v299932071 ) * (chosen == 299932071 ) +
         exp(v299942070 ) * (chosen == 299942070 ) +
         exp(v299962069 ) * (chosen == 299962069 ) +
         exp(v299972068 ) * (chosen == 299972068 ) +
         exp(v299982072 ) * (chosen == 299982072 ) +
         exp(v300362067 ) * (chosen == 300362067 ) +
         exp(v300562090 ) * (chosen == 300562090 ) +
         exp(v300572089 ) * (chosen == 300572089 ) +
         exp(v300582091 ) * (chosen == 300582091 ) +
         exp(v302362196 ) * (chosen == 302362196 ) +
         exp(v302382197 ) * (chosen == 302382197 ) +
         exp(v303162321 ) * (chosen == 303162321 ) +
         exp(v303172320 ) * (chosen == 303172320 ) +
         exp(v303182322 ) * (chosen == 303182322 ) +
         exp(v305262468 ) * (chosen == 305262468 ) +
         exp(v305272467 ) * (chosen == 305272467 ) +
         exp(v305282469 ) * (chosen == 305282469 ) +
         exp(v311673458 ) * (chosen == 311673458 ) +
         exp(v312083459 ) * (chosen == 312083459 ) +
         exp(v314723602 ) * (chosen == 314723602 ) +
         exp(v314733605 ) * (chosen == 314733605 ) +
         exp(v314743603 ) * (chosen == 314743603 ) +
         exp(v314753601 ) * (chosen == 314753601 ) +
         exp(v316543821 ) * (chosen == 316543821 ) +
         exp(v316563820 ) * (chosen == 316563820 ) +
         exp(v316573819 ) * (chosen == 316573819 ) +
         exp(v316583822 ) * (chosen == 316583822 ) +
         exp(v316663823 ) * (chosen == 316663823 ) +
         exp(v317183995 ) * (chosen == 317183995 ) +
         exp(v317193994 ) * (chosen == 317193994 ) +
         exp(v322314488 ) * (chosen == 322314488 ) +
         exp(v322324485 ) * (chosen == 322324485 ) +
         exp(v322334486 ) * (chosen == 322334486 ) +
         exp(v322344484 ) * (chosen == 322344484 ) +
         exp(v322354489 ) * (chosen == 322354489 ) +
         exp(v322364483 ) * (chosen == 322364483 ) +
         exp(v322374482 ) * (chosen == 322374482 ) +
         exp(v322384487 ) * (chosen == 322384487 ) +
         exp(v332564494 ) * (chosen == 332564494 ) +
         exp(v336634891 ) * (chosen == 336634891 ) +
         exp(v337163842 ) * (chosen == 337163842 ) +
         exp(v337173841 ) * (chosen == 337173841 ) +
         exp(v341641324 ) * (chosen == 341641324 ) +
         exp(v341971322 ) * (chosen == 341971322 ) +
         exp(v341981344 ) * (chosen == 341981344 ) +
         exp(v343592414 ) * (chosen == 343592414 ) +
         exp(v343662146 ) * (chosen == 343662146 ) +
         exp(v343682147 ) * (chosen == 343682147 ) +
         exp(v344222417 ) * (chosen == 344222417 ) +
         exp(v344232415 ) * (chosen == 344232415 ) +
         exp(v344252418 ) * (chosen == 344252418 ) +
         exp(v344332416 ) * (chosen == 344332416 ) +
         exp(v344493093 ) * (chosen == 344493093 ) +
         exp(v344563096 ) * (chosen == 344563096 ) +
         exp(v344573094 ) * (chosen == 344573094 ) +
         exp(v344583097 ) * (chosen == 344583097 ) +
         exp(v345463959 ) * (chosen == 345463959 ) +
         exp(v345663961 ) * (chosen == 345663961 ) +
         exp(v345784107 ) * (chosen == 345784107 ) +
         exp(v345794105 ) * (chosen == 345794105 ) +
         exp(v346904740 ) * (chosen == 346904740 ) +
         exp(v347464848 ) * (chosen == 347464848 ) +
         exp(v347474844 ) * (chosen == 347474844 ) +
         exp(v347484849 ) * (chosen == 347484849 ) +
         exp(v347864825 ) * (chosen == 347864825 ) +
         exp(v348174871 ) * (chosen == 348174871 ) +
         exp(v348204895 ) * (chosen == 348204895 ) +
         exp(v348214898 ) * (chosen == 348214898 ) +
         exp(v348234877 ) * (chosen == 348234877 ) +
         exp(v348564927 ) * (chosen == 348564927 ) +
         exp(v348714847 ) * (chosen == 348714847 ) +
         exp(v348814855 ) * (chosen == 348814855 ) +
         exp(v349195043 ) * (chosen == 349195043 ) +
         exp(v349355042 ) * (chosen == 349355042 ) +
         exp(v349725007 ) * (chosen == 349725007 ) +
         exp(v349955017 ) * (chosen == 349955017 ) +
         exp(v352134741 ) * (chosen == 352134741 ) +
         exp(v356264111 ) * (chosen == 356264111 ) +
         exp(v356284112 ) * (chosen == 356284112 ) +
         exp(v356993944 ) * (chosen == 356993944 ) +
         exp(v357804051 ) * (chosen == 357804051 ) +
         exp(v359132078 ) * (chosen == 359132078 ) +
         exp(v359142077 ) * (chosen == 359142077 ) +
         exp(v359162076 ) * (chosen == 359162076 ) +
         exp(v359172075 ) * (chosen == 359172075 ) +
         exp(v359182079 ) * (chosen == 359182079 ) +
         exp(v359262073 ) * (chosen == 359262073 ) +
         exp(v359442095 ) * (chosen == 359442095 ) +
         exp(v359462094 ) * (chosen == 359462094 ) +
         exp(v359472093 ) * (chosen == 359472093 ) +
         exp(v359482096 ) * (chosen == 359482096 ) +
         exp(v359645019 ) * (chosen == 359645019 ) +
         exp(v361862324 ) * (chosen == 361862324 ) +
         exp(v361882325 ) * (chosen == 361882325 ) +
         exp(v363012478 ) * (chosen == 363012478 ) +
         exp(v363042475 ) * (chosen == 363042475 ) +
         exp(v363052474 ) * (chosen == 363052474 ) +
         exp(v363072473 ) * (chosen == 363072473 ) +
         exp(v363082477 ) * (chosen == 363082477 ) +
         exp(v364472674 ) * (chosen == 364472674 ) +
         exp(v366673145 ) * (chosen == 366673145 ) +
         exp(v366683146 ) * (chosen == 366683146 ) +
         exp(v368711323 ) * (chosen == 368711323 ) +
         exp(v368863465 ) * (chosen == 368863465 ) +
         exp(v368873467 ) * (chosen == 368873467 ) +
         exp(v369013468 ) * (chosen == 369013468 ) +
         exp(v369033466 ) * (chosen == 369033466 ) +
         exp(v369043469 ) * (chosen == 369043469 ) +
         exp(v369073471 ) * (chosen == 369073471 ) +
         exp(v369083470 ) * (chosen == 369083470 ) +
         exp(v369683635 ) * (chosen == 369683635 ) +
         exp(v369693633 ) * (chosen == 369693633 ) +
         exp(v369713637 ) * (chosen == 369713637 ) +
         exp(v369743645 ) * (chosen == 369743645 ) +
         exp(v369763644 ) * (chosen == 369763644 ) +
         exp(v369783647 ) * (chosen == 369783647 ) +
         exp(v371463754 ) * (chosen == 371463754 ) +
         exp(v372173850 ) * (chosen == 372173850 ) +
         exp(v373365237 ) * (chosen == 373365237 ) +
         exp(v373385241 ) * (chosen == 373385241 ) +
         exp(v373395239 ) * (chosen == 373395239 ) +
         exp(v373464016 ) * (chosen == 373464016 ) +
         exp(v373474014 ) * (chosen == 373474014 ) +
         exp(v373484017 ) * (chosen == 373484017 ) +
         exp(v373564018 ) * (chosen == 373564018 ) +
         exp(v374134136 ) * (chosen == 374134136 ) +
         exp(v374144135 ) * (chosen == 374144135 ) +
         exp(v374164134 ) * (chosen == 374164134 ) +
         exp(v374174133 ) * (chosen == 374174133 ) +
         exp(v374184137 ) * (chosen == 374184137 ) +
         exp(v374264138 ) * (chosen == 374264138 ) +
         exp(v376404291 ) * (chosen == 376404291 ) +
         exp(v376424292 ) * (chosen == 376424292 ) +
         exp(v378864512 ) * (chosen == 378864512 ) +
         exp(v378884513 ) * (chosen == 378884513 ) +
         exp(v379344565 ) * (chosen == 379344565 ) +
         exp(v379354567 ) * (chosen == 379354567 ) +
         exp(v379364564 ) * (chosen == 379364564 ) +
         exp(v379374563 ) * (chosen == 379374563 ) +
         exp(v379384566 ) * (chosen == 379384566 ) +
         exp(v379864600 ) * (chosen == 379864600 ) +
         exp(v379874598 ) * (chosen == 379874598 ) +
         exp(v379884601 ) * (chosen == 379884601 ) +
         exp(v379914710 ) * (chosen == 379914710 ) +
         exp(v379934708 ) * (chosen == 379934708 ) +
         exp(v379944707 ) * (chosen == 379944707 ) +
         exp(v379964706 ) * (chosen == 379964706 ) +
         exp(v379974705 ) * (chosen == 379974705 ) +
         exp(v379984709 ) * (chosen == 379984709 ) +
         exp(v380064712 ) * (chosen == 380064712 ) +
         exp(v380084711 ) * (chosen == 380084711 ) +
         exp(v380884122 ) * (chosen == 380884122 ) +
         exp(v381765067 ) * (chosen == 381765067 ) +
         exp(v381775064 ) * (chosen == 381775064 ) +
         exp(v381785066 ) * (chosen == 381785066 ) +
         exp(v398664701 ) * (chosen == 398664701 ) +
         exp(v401181854 ) * (chosen == 401181854 ) +
         exp(v401933622 ) * (chosen == 401933622 ) +
         exp(v402973593 ) * (chosen == 402973593 ) +
         exp(v403133619 ) * (chosen == 403133619 ) +
         exp(v403945197 ) * (chosen == 403945197 ) +
         exp(v404115227 ) * (chosen == 404115227 ) +
         exp(v405923616 ) * (chosen == 405923616 ) +
         exp(v405933617 ) * (chosen == 405933617 ) +
         exp(v405943615 ) * (chosen == 405943615 ) +
         exp(v405983628 ) * (chosen == 405983628 ) +
         exp(v405993626 ) * (chosen == 405993626 ) +
         exp(v406044636 ) * (chosen == 406044636 ) +
         exp(v406144352 ) * (chosen == 406144352 ) +
         exp(v406255200 ) * (chosen == 406255200 ) +
         exp(v406325242 ) * (chosen == 406325242 ) +
         exp(v409264670 ) * (chosen == 409264670 ) +
         exp(v409304631 ) * (chosen == 409304631 ) +
         exp(v409464717 ) * (chosen == 409464717 ) +
         exp(v409564691 ) * (chosen == 409564691 ) +
         exp(v409654718 ) * (chosen == 409654718 ) +
         exp(v409764615 ) * (chosen == 409764615 ) +
         exp(v410762864 ) * (chosen == 410762864 ) +
         exp(v410772862 ) * (chosen == 410772862 ) +
         exp(v410782865 ) * (chosen == 410782865 ) +
         exp(v412092062 ) * (chosen == 412092062 ) +
         exp(v413605215 ) * (chosen == 413605215 ) +
         exp(v415002465 ) * (chosen == 415002465 ) +
         exp(v415924639 ) * (chosen == 415924639 ) +
         exp(v416015183 ) * (chosen == 416015183 ) +
         exp(v416045206 ) * (chosen == 416045206 ) +
         exp(v416255207 ) * (chosen == 416255207 ) +
         exp(v416535202 ) * (chosen == 416535202 ) +
         exp(v416815221 ) * (chosen == 416815221 ) +
         exp(v416885191 ) * (chosen == 416885191 ) +
         exp(v416925185 ) * (chosen == 416925185 ) +
         exp(v416935184 ) * (chosen == 416935184 ) +
         exp(v416945182 ) * (chosen == 416945182 ) +
         exp(v416965209 ) * (chosen == 416965209 ) +
         exp(v416985210 ) * (chosen == 416985210 ) +
         exp(v417015211 ) * (chosen == 417015211 ) +
         exp(v417045220 ) * (chosen == 417045220 ) +
         exp(v417055219 ) * (chosen == 417055219 ) +
         exp(v417073998 ) * (chosen == 417073998 ) +
         exp(v417154626 ) * (chosen == 417154626 ) +
         exp(v417194633 ) * (chosen == 417194633 ) +
         exp(v417464623 ) * (chosen == 417464623 ) +
         exp(v417474622 ) * (chosen == 417474622 ) +
         exp(v417835190 ) * (chosen == 417835190 ) +
         exp(v417964664 ) * (chosen == 417964664 ) +
         exp(v417974663 ) * (chosen == 417974663 ) +
         exp(v418064700 ) * (chosen == 418064700 ) +
         exp(v418461979 ) * (chosen == 418461979 ) +
         exp(v419895193 ) * (chosen == 419895193 ) +
         exp(v420025194 ) * (chosen == 420025194 ) +
         exp(v420095196 ) * (chosen == 420095196 ) +
         exp(v420795195 ) * (chosen == 420795195 ) +
         exp(v421663588 ) * (chosen == 421663588 ) +
         exp(v421673587 ) * (chosen == 421673587 ) +
         exp(v423123372 ) * (chosen == 423123372 ) +
         exp(v424444531 ) * (chosen == 424444531 ) +
         exp(v426663583 ) * (chosen == 426663583 ) +
         exp(v426763366 ) * (chosen == 426763366 ) +
         exp(v426873343 ) * (chosen == 426873343 ) +
         exp(v426993362 ) * (chosen == 426993362 ) +
         exp(v427033371 ) * (chosen == 427033371 ) +
         exp(v427123353 ) * (chosen == 427123353 ) +
         exp(v427163350 ) * (chosen == 427163350 ) +
         exp(v427173349 ) * (chosen == 427173349 ) +
         exp(v427183352 ) * (chosen == 427183352 ) +
         exp(v429845112 ) * (chosen == 429845112 ) +
         exp(v432191119 ) * (chosen == 432191119 ) +
         exp(v432441094 ) * (chosen == 432441094 ) +
         exp(v432821121 ) * (chosen == 432821121 ) +
         exp(v432851114 ) * (chosen == 432851114 ) +
         exp(v432871118 ) * (chosen == 432871118 ) +
         exp(v432961130 ) * (chosen == 432961130 ) +
         exp(v433021096 ) * (chosen == 433021096 ) +
         exp(v433161127 ) * (chosen == 433161127 ) +
         exp(v433281141 ) * (chosen == 433281141 ) +
         exp(v433301142 ) * (chosen == 433301142 ) +
         exp(v433311136 ) * (chosen == 433311136 ) +
         exp(v433331137 ) * (chosen == 433331137 ) +
         exp(v433341134 ) * (chosen == 433341134 ) +
         exp(v433351138 ) * (chosen == 433351138 ) +
         exp(v433361110 ) * (chosen == 433361110 ) +
         exp(v433371133 ) * (chosen == 433371133 ) +
         exp(v433381135 ) * (chosen == 433381135 ) +
         exp(v433861169 ) * (chosen == 433861169 ) +
         exp(v433871168 ) * (chosen == 433871168 ) +
         exp(v433881170 ) * (chosen == 433881170 ) +
         exp(v434091115 ) * (chosen == 434091115 ) +
         exp(v434101116 ) * (chosen == 434101116 ) +
         exp(v434151107 ) * (chosen == 434151107 ) +
         exp(v434181139 ) * (chosen == 434181139 ) +
         exp(v434261790 ) * (chosen == 434261790 ) +
         exp(v438484334 ) * (chosen == 438484334 ) +
         exp(v443501003 ) * (chosen == 443501003 ) +
         exp(v444191536 ) * (chosen == 444191536 ) +
         exp(v444561561 ) * (chosen == 444561561 ) +
         exp(v444761542 ) * (chosen == 444761542 ) +
         exp(v444991554 ) * (chosen == 444991554 ) +
         exp(v445161553 ) * (chosen == 445161553 ) +
         exp(v445201565 ) * (chosen == 445201565 ) +
         exp(v446032066 ) * (chosen == 446032066 ) +
         exp(v446573204 ) * (chosen == 446573204 ) +
         exp(v446583206 ) * (chosen == 446583206 ) +
         exp(v452453828 ) * (chosen == 452453828 ) +
         exp(v452463826 ) * (chosen == 452463826 ) +
         exp(v452473825 ) * (chosen == 452473825 ) +
         exp(v452483827 ) * (chosen == 452483827 ) +
         exp(v452753858 ) * (chosen == 452753858 ) +
         exp(v452763855 ) * (chosen == 452763855 ) +
         exp(v452773854 ) * (chosen == 452773854 ) +
         exp(v452783857 ) * (chosen == 452783857 ) +
         exp(v454184020 ) * (chosen == 454184020 ) +
         exp(v454194019 ) * (chosen == 454194019 ) +
         exp(v458844377 ) * (chosen == 458844377 ) +
         exp(v458864376 ) * (chosen == 458864376 ) +
         exp(v458874375 ) * (chosen == 458874375 ) +
         exp(v458884378 ) * (chosen == 458884378 ) +
         exp(v463502472 ) * (chosen == 463502472 ) +
         exp(v463512471 ) * (chosen == 463512471 ) +
         exp(v465043156 ) * (chosen == 465043156 ) +
         exp(v465063157 ) * (chosen == 465063157 ) +
         exp(v477851437 ) * (chosen == 477851437 ) +
         exp(v477861438 ) * (chosen == 477861438 ) +
         exp(v480992500 ) * (chosen == 480992500 ) +
         exp(v481022508 ) * (chosen == 481022508 ) +
         exp(v481032505 ) * (chosen == 481032505 ) +
         exp(v481042504 ) * (chosen == 481042504 ) +
         exp(v481052499 ) * (chosen == 481052499 ) +
         exp(v481062503 ) * (chosen == 481062503 ) +
         exp(v490864235 ) * (chosen == 490864235 ) +
         exp(v491854246 ) * (chosen == 491854246 ) +
         exp(v491864248 ) * (chosen == 491864248 ) +
         exp(v491894249 ) * (chosen == 491894249 ) +
         exp(v506864806 ) * (chosen == 506864806 ) +
         exp(v516964816 ) * (chosen == 516964816 ) +
         exp(v521461289 ) * (chosen == 521461289 ) +
         exp(v521861292 ) * (chosen == 521861292 ) +
         exp(v523121283 ) * (chosen == 523121283 ) +
         exp(v523141284 ) * (chosen == 523141284 ) +
         exp(v523161282 ) * (chosen == 523161282 ) +
         exp(v523171287 ) * (chosen == 523171287 ) +
         exp(v523181285 ) * (chosen == 523181285 ) +
         exp(v523191286 ) * (chosen == 523191286 ) +
         exp(v525631877 ) * (chosen == 525631877 ) +
         exp(v525801874 ) * (chosen == 525801874 ) +
         exp(v525811875 ) * (chosen == 525811875 ) +
         exp(v525821873 ) * (chosen == 525821873 ) +
         exp(v525931865 ) * (chosen == 525931865 ) +
         exp(v525941866 ) * (chosen == 525941866 ) +
         exp(v525951870 ) * (chosen == 525951870 ) +
         exp(v525961862 ) * (chosen == 525961862 ) +
         exp(v525971869 ) * (chosen == 525971869 ) +
         exp(v525981863 ) * (chosen == 525981863 ) +
         exp(v525991868 ) * (chosen == 525991868 ) +
         exp(v528063140 ) * (chosen == 528063140 ) +
         exp(v528353142 ) * (chosen == 528353142 ) +
         exp(v532103914 ) * (chosen == 532103914 ) +
         exp(v532113924 ) * (chosen == 532113924 ) +
         exp(v532133912 ) * (chosen == 532133912 ) +
         exp(v532143913 ) * (chosen == 532143913 ) +
         exp(v532163911 ) * (chosen == 532163911 ) +
         exp(v532183915 ) * (chosen == 532183915 ) +
         exp(v544363564 ) * (chosen == 544363564 ) +
         exp(v544463799 ) * (chosen == 544463799 ) +
         exp(v544473798 ) * (chosen == 544473798 ) +
         exp(v544483801 ) * (chosen == 544483801 ) +
         exp(v547061635 ) * (chosen == 547061635 ) +
         exp(v550663565 ) * (chosen == 550663565 ) +
         exp(v550683566 ) * (chosen == 550683566 ) +
         exp(v550843804 ) * (chosen == 550843804 ) +
         exp(v550863803 ) * (chosen == 550863803 ) +
         exp(v550873802 ) * (chosen == 550873802 ) +
         exp(v550883805 ) * (chosen == 550883805 ) +
         exp(v551063991 ) * (chosen == 551063991 ) +
         exp(v555063810 ) * (chosen == 555063810 ) +
         exp(v561953918 ) * (chosen == 561953918 ) +
         exp(v561963916 ) * (chosen == 561963916 ) +
         exp(v562063832 ) * (chosen == 562063832 ) +
         exp(v571201242 ) * (chosen == 571201242 ) +
         exp(v571251241 ) * (chosen == 571251241 ) +
         exp(v571291244 ) * (chosen == 571291244 ) +
         exp(v571441252 ) * (chosen == 571441252 ) +
         exp(v571481243 ) * (chosen == 571481243 ) +
         exp(v571571246 ) * (chosen == 571571246 ) +
         exp(v571581251 ) * (chosen == 571581251 ) +
         exp(v571611240 ) * (chosen == 571611240 ) +
         exp(v571741250 ) * (chosen == 571741250 ) +
         exp(v588011108 ) * (chosen == 588011108 ) +
         exp(v588352963 ) * (chosen == 588352963 ) +
         exp(v588362961 ) * (chosen == 588362961 ) +
         exp(v588382962 ) * (chosen == 588382962 ) +
         exp(v588402957 ) * (chosen == 588402957 ) +
         exp(v588422975 ) * (chosen == 588422975 ) +
         exp(v588602998 ) * (chosen == 588602998 ) +
         exp(v588682969 ) * (chosen == 588682969 ) +
         exp(v588722960 ) * (chosen == 588722960 ) +
         exp(v588732971 ) * (chosen == 588732971 ) +
         exp(v588752994 ) * (chosen == 588752994 ) +
         exp(v588762993 ) * (chosen == 588762993 ) +
         exp(v588852997 ) * (chosen == 588852997 ) +
         exp(v588862996 ) * (chosen == 588862996 ) +
         exp(v590371737 ) * (chosen == 590371737 ) +
         exp(v591001960 ) * (chosen == 591001960 ) +
         exp(v592312659 ) * (chosen == 592312659 ) +
         exp(v620613048 ) * (chosen == 620613048 ) +
         exp(v620973829 ) * (chosen == 620973829 ) +
         exp(v624003402 ) * (chosen == 624003402 ) +
         exp(v624053409 ) * (chosen == 624053409 ) +
         exp(v624203414 ) * (chosen == 624203414 ) +
         exp(v633554549 ) * (chosen == 633554549 ) +
         exp(v633574553 ) * (chosen == 633574553 ) +
         exp(v633594561 ) * (chosen == 633594561 ) +
         exp(v635284382 ) * (chosen == 635284382 ) +
         exp(v635304384 ) * (chosen == 635304384 ) +
         exp(v637554961 ) * (chosen == 637554961 ) +
         exp(v639595188 ) * (chosen == 639595188 ) +
         exp(v639635214 ) * (chosen == 639635214 ) +
         exp(v641361756 ) * (chosen == 641361756 ) +
         exp(v645731333 ) * (chosen == 645731333 ) +
         exp(v646011145 ) * (chosen == 646011145 ) +
         exp(v646761226 ) * (chosen == 646761226 ) +
         exp(v647111326 ) * (chosen == 647111326 ) +
         exp(v647121331 ) * (chosen == 647121331 ) +
         exp(v647161327 ) * (chosen == 647161327 ) +
         exp(v647361348 ) * (chosen == 647361348 ) +
         exp(v647551338 ) * (chosen == 647551338 ) +
         exp(v647591339 ) * (chosen == 647591339 ) +
         exp(v647761385 ) * (chosen == 647761385 ) +
         exp(v648582044 ) * (chosen == 648582044 ) +
         exp(v648622040 ) * (chosen == 648622040 ) +
         exp(v648632042 ) * (chosen == 648632042 ) +
         exp(v648642046 ) * (chosen == 648642046 ) +
         exp(v648652045 ) * (chosen == 648652045 ) +
         exp(v648662041 ) * (chosen == 648662041 ) +
         exp(v648672039 ) * (chosen == 648672039 ) +
         exp(v648682043 ) * (chosen == 648682043 ) +
         exp(v648702047 ) * (chosen == 648702047 ) +
         exp(v648761840 ) * (chosen == 648761840 ) +
         exp(v649962135 ) * (chosen == 649962135 ) +
         exp(v650662318 ) * (chosen == 650662318 ) +
         exp(v651262343 ) * (chosen == 651262343 ) +
         exp(v651272341 ) * (chosen == 651272341 ) +
         exp(v651952633 ) * (chosen == 651952633 ) +
         exp(v651994947 ) * (chosen == 651994947 ) +
         exp(v652004948 ) * (chosen == 652004948 ) +
         exp(v652482816 ) * (chosen == 652482816 ) +
         exp(v652512814 ) * (chosen == 652512814 ) +
         exp(v652532812 ) * (chosen == 652532812 ) +
         exp(v652542810 ) * (chosen == 652542810 ) +
         exp(v652562809 ) * (chosen == 652562809 ) +
         exp(v652572808 ) * (chosen == 652572808 ) +
         exp(v652582813 ) * (chosen == 652582813 ) +
         exp(v666364310 ) * (chosen == 666364310 ) +
         exp(v668364802 ) * (chosen == 668364802 ) +
         exp(v669362356 ) * (chosen == 669362356 ) +
         exp(v670063521 ) * (chosen == 670063521 ) +
         exp(v672665510 ) * (chosen == 672665510 ) +
         exp(v672675509 ) * (chosen == 672675509 ) +
         exp(v675263042 ) * (chosen == 675263042 ) +
         exp(v675273041 ) * (chosen == 675273041 ) +
         exp(v675283044 ) * (chosen == 675283044 ) +
         exp(v675563070 ) * (chosen == 675563070 ) +
         exp(v675573069 ) * (chosen == 675573069 ) +
         exp(v675863071 ) * (chosen == 675863071 ) +
         exp(v681261155 ) * (chosen == 681261155 ) +
         exp(v681271154 ) * (chosen == 681271154 ) +
         exp(v683061942 ) * (chosen == 683061942 ) +
         exp(v686113450 ) * (chosen == 686113450 ) +
         exp(v688464799 ) * (chosen == 688464799 ) +
         exp(v696111809 ) * (chosen == 696111809 ) +
         exp(v696341816 ) * (chosen == 696341816 ) +
         exp(v696361815 ) * (chosen == 696361815 ) +
         exp(v696371814 ) * (chosen == 696371814 ) +
         exp(v696381817 ) * (chosen == 696381817 ) +
         exp(v696561822 ) * (chosen == 696561822 ) +
         exp(v696571821 ) * (chosen == 696571821 ) +
         exp(v696581823 ) * (chosen == 696581823 ) +
         exp(v696661833 ) * (chosen == 696661833 ) +
         exp(v696671832 ) * (chosen == 696671832 ) +
         exp(v699404353 ) * (chosen == 699404353 ) +
         exp(v699454355 ) * (chosen == 699454355 ) +
         exp(v699464356 ) * (chosen == 699464356 ) +
         exp(v699474354 ) * (chosen == 699474354 ) +
         exp(v727221293 ) * (chosen == 727221293 ) +
         exp(v730504347 ) * (chosen == 730504347 ) +
         exp(v730514350 ) * (chosen == 730514350 ) +
         exp(v730524343 ) * (chosen == 730524343 ) +
         exp(v730534345 ) * (chosen == 730534345 ) +
         exp(v730544346 ) * (chosen == 730544346 ) +
         exp(v730554344 ) * (chosen == 730554344 ) +
         exp(v731361254 ) * (chosen == 731361254 ) +
         exp(v734561684 ) * (chosen == 734561684 ) +
         exp(v735171645 ) * (chosen == 735171645 ) +
         exp(v735261686 ) * (chosen == 735261686 ) +
         exp(v737021965 ) * (chosen == 737021965 ) +
         exp(v737151958 ) * (chosen == 737151958 ) +
         exp(v739861750 ) * (chosen == 739861750 ) +
         exp(v740862313 ) * (chosen == 740862313 ) +
         exp(v747822800 ) * (chosen == 747822800 ) +
         exp(v750873008 ) * (chosen == 750873008 ) +
         exp(v752083115 ) * (chosen == 752083115 ) +
         exp(v752103114 ) * (chosen == 752103114 ) +
         exp(v752123110 ) * (chosen == 752123110 ) +
         exp(v752143108 ) * (chosen == 752143108 ) +
         exp(v752163111 ) * (chosen == 752163111 ) +
         exp(v760323516 ) * (chosen == 760323516 ) +
         exp(v760363507 ) * (chosen == 760363507 ) +
         exp(v760383514 ) * (chosen == 760383514 ) +
         exp(v760403520 ) * (chosen == 760403520 ) +
         exp(v760423511 ) * (chosen == 760423511 ) +
         exp(v760443513 ) * (chosen == 760443513 ) +
         exp(v762273543 ) * (chosen == 762273543 ) +
         exp(v764363723 ) * (chosen == 764363723 ) +
         exp(v764783788 ) * (chosen == 764783788 ) +
         exp(v764863795 ) * (chosen == 764863795 ) +
         exp(v764873794 ) * (chosen == 764873794 ) +
         exp(v764883796 ) * (chosen == 764883796 ) +
         exp(v765263831 ) * (chosen == 765263831 ) +
         exp(v767673999 ) * (chosen == 767673999 ) +
         exp(v773184326 ) * (chosen == 773184326 ) +
         exp(v773754673 ) * (chosen == 773754673 ) +
         exp(v773784677 ) * (chosen == 773784677 ) +
         exp(v773954559 ) * (chosen == 773954559 ) +
         exp(v774724901 ) * (chosen == 774724901 ) +
         exp(v774724901 ) * (chosen == 774724901 ) +
         exp(v774875094 ) * (chosen == 774875094 ) +
         exp(v777355268 ) * (chosen == 777355268 ) +
         exp(v777404660 ) * (chosen == 777404660 ) +
         exp(v777745327 ) * (chosen == 777745327 ) +
         exp(v777765329 ) * (chosen == 777765329 ) +
         exp(v777865135 ) * (chosen == 777865135 ) +
         exp(v778424689 ) * (chosen == 778424689 ) +
         exp(v778474697 ) * (chosen == 778474697 ) +
         exp(v778524716 ) * (chosen == 778524716 ) +
         exp(v784563813 ) * (chosen == 784563813 ) +
         exp(v788663814 ) * (chosen == 788663814 ) +
         exp(v793363815 ) * (chosen == 793363815 ) +
         exp(v798163816 ) * (chosen == 798163816 ) +
         exp(v800961711 ) * (chosen == 800961711 ) +
         exp(v801452153 ) * (chosen == 801452153 ) +
         exp(v802362680 ) * (chosen == 802362680 ) +
         exp(v804404276 ) * (chosen == 804404276 ) +
         exp(v804564370 ) * (chosen == 804564370 ) +
         exp(v804574369 ) * (chosen == 804574369 ) +
         exp(v804584371 ) * (chosen == 804584371 ) +
         exp(v806861638 ) * (chosen == 806861638 ) +
         exp(v806871637 ) * (chosen == 806871637 ) +
         exp(v808262658 ) * (chosen == 808262658 ) +
         exp(v808272657 ) * (chosen == 808272657 ) +
         exp(v811963839 ) * (chosen == 811963839 ) +
         exp(v812043837 ) * (chosen == 812043837 ) +
         exp(v812063836 ) * (chosen == 812063836 ) +
         exp(v812073835 ) * (chosen == 812073835 ) +
         exp(v812083838 ) * (chosen == 812083838 ) +
         exp(v814065047 ) * (chosen == 814065047 ) +
         exp(v826061716 ) * (chosen == 826061716 ) +
         exp(v826071715 ) * (chosen == 826071715 ) +
         exp(v826101669 ) * (chosen == 826101669 ) +
         exp(v826111670 ) * (chosen == 826111670 ) +
         exp(v826181673 ) * (chosen == 826181673 ) +
         exp(v826261648 ) * (chosen == 826261648 ) +
         exp(v826271647 ) * (chosen == 826271647 ) +
         exp(v826361667 ) * (chosen == 826361667 ) +
         exp(v826371666 ) * (chosen == 826371666 ) +
         exp(v827861643 ) * (chosen == 827861643 ) +
         exp(v827871639 ) * (chosen == 827871639 ) +
         exp(v828201723 ) * (chosen == 828201723 ) +
         exp(v828261702 ) * (chosen == 828261702 ) +
         exp(v828271708 ) * (chosen == 828271708 ) +
         exp(v828401700 ) * (chosen == 828401700 ) +
         exp(v828461681 ) * (chosen == 828461681 ) +
         exp(v828471697 ) * (chosen == 828471697 ) +
         exp(v828491699 ) * (chosen == 828491699 ) +
         exp(v828661729 ) * (chosen == 828661729 ) +
         exp(v828671728 ) * (chosen == 828671728 ) +
         exp(v828811672 ) * (chosen == 828811672 ) +
         exp(v829572064 ) * (chosen == 829572064 ) +
         exp(v833632660 ) * (chosen == 833632660 ) +
         exp(v833702664 ) * (chosen == 833702664 ) +
         exp(v839063438 ) * (chosen == 839063438 ) +
         exp(v839073437 ) * (chosen == 839073437 ) +
         exp(v844563834 ) * (chosen == 844563834 ) +
         exp(v844573833 ) * (chosen == 844573833 ) +
         exp(v844863807 ) * (chosen == 844863807 ) +
         exp(v844963844 ) * (chosen == 844963844 ) +
         exp(v846174003 ) * (chosen == 846174003 ) +
         exp(v855261710 ) * (chosen == 855261710 ) +
         exp(v856063806 ) * (chosen == 856063806 ) +
         exp(v861121724 ) * (chosen == 861121724 ) +
         exp(v862503016 ) * (chosen == 862503016 ) +
         exp(v862513015 ) * (chosen == 862513015 ) +
         exp(v863103198 ) * (chosen == 863103198 ) +
         exp(v863903545 ) * (chosen == 863903545 ) +
         exp(v865063847 ) * (chosen == 865063847 ) +
         exp(v865073846 ) * (chosen == 865073846 ) +
         exp(v866375097 ) * (chosen == 866375097 ) +
         exp(v866692777 ) * (chosen == 866692777 ) +
         exp(v866702776 ) * (chosen == 866702776 ) +
         exp(v866722775 ) * (chosen == 866722775 ) +
         exp(v866922771 ) * (chosen == 866922771 ) +
         exp(v866932770 ) * (chosen == 866932770 ) +
         exp(v867964779 ) * (chosen == 867964779 ) +
         exp(v868074765 ) * (chosen == 868074765 ) +
         exp(v868814774 ) * (chosen == 868814774 ) +
         exp(v868834760 ) * (chosen == 868834760 ) +
         exp(v868844758 ) * (chosen == 868844758 ) +
         exp(v868854775 ) * (chosen == 868854775 ) +
         exp(v868864757 ) * (chosen == 868864757 ) +
         exp(v868874756 ) * (chosen == 868874756 ) +
         exp(v868884776 ) * (chosen == 868884776 ) +
         exp(v869164762 ) * (chosen == 869164762 ) +
         exp(v874082985 ) * (chosen == 874082985 ) +
         exp(v874092984 ) * (chosen == 874092984 ) +
         exp(v874851780 ) * (chosen == 874851780 ) +
         exp(v875861912 ) * (chosen == 875861912 ) +
         exp(v875961916 ) * (chosen == 875961916 ) +
         exp(v876431990 ) * (chosen == 876431990 ) +
         exp(v879363013 ) * (chosen == 879363013 ) +
         exp(v879373012 ) * (chosen == 879373012 ) +
         exp(v879383014 ) * (chosen == 879383014 ) +
         exp(v879973197 ) * (chosen == 879973197 ) +
         exp(v880363412 ) * (chosen == 880363412 ) +
         exp(v881473546 ) * (chosen == 881473546 ) +
         exp(v882943897 ) * (chosen == 882943897 ) +
         exp(v882963898 ) * (chosen == 882963898 ) +
         exp(v885564398 ) * (chosen == 885564398 ) +
         exp(v887665099 ) * (chosen == 887665099 ) +
         exp(v887675098 ) * (chosen == 887675098 ) +
         exp(v887685100 ) * (chosen == 887685100 ) +
         exp(v891211179 ) * (chosen == 891211179 ) +
         exp(v891541783 ) * (chosen == 891541783 ) +
         exp(v891751778 ) * (chosen == 891751778 ) +
         exp(v891912983 ) * (chosen == 891912983 ) +
         exp(v891932981 ) * (chosen == 891932981 ) +
         exp(v891942977 ) * (chosen == 891942977 ) +
         exp(v891962980 ) * (chosen == 891962980 ) +
         exp(v891972979 ) * (chosen == 891972979 ) +
         exp(v891982982 ) * (chosen == 891982982 ) +
         exp(v891992978 ) * (chosen == 891992978 ) +
         exp(v892153005 ) * (chosen == 892153005 ) +
         exp(v892421420 ) * (chosen == 892421420 ) +
         exp(v892781907 ) * (chosen == 892781907 ) +
         exp(v893863010 ) * (chosen == 893863010 ) +
         exp(v893873009 ) * (chosen == 893873009 ) +
         exp(v893883011 ) * (chosen == 893883011 ) +
         exp(v894473195 ) * (chosen == 894473195 ) +
         exp(v894963405 ) * (chosen == 894963405 ) +
         exp(v895331989 ) * (chosen == 895331989 ) +
         exp(v895773544 ) * (chosen == 895773544 ) +
         exp(v896174404 ) * (chosen == 896174404 ) +
         exp(v897864410 ) * (chosen == 897864410 ) +
         exp(v897874409 ) * (chosen == 897874409 ) +
         exp(v897884411 ) * (chosen == 897884411 ) +
         exp(v898362619 ) * (chosen == 898362619 ) +
         exp(v898372618 ) * (chosen == 898372618 ) +
         exp(v898875089 ) * (chosen == 898875089 ) +
         exp(v899165095 ) * (chosen == 899165095 ) +
         exp(v902836770 ) * (chosen == 902836770 ) +
         exp(v9046891337) * (chosen == 9046891337) +
         exp(v9368593476) * (chosen == 9368593476) +
         exp(v9369673634) * (chosen == 9369673634) +
         exp(v9465743475) * (chosen == 9465743475) +
         exp(v9885364401) * (chosen == 9885364401))/
    ((exp(v9999      ) +
        available_734190     * exp(v734190    ) +
        available_1573935    * exp(v1573935   ) +
        available_2870505    * exp(v2870505   ) +
        available_4356521    * exp(v4356521   ) +
        available_4626831    * exp(v4626831   ) +
        available_5915426    * exp(v5915426   ) +
        available_6144352    * exp(v6144352   ) +
        available_8417089    * exp(v8417089   ) +
        available_8751020    * exp(v8751020   ) +
        available_8920617    * exp(v8920617   ) +
        available_10548697   * exp(v10548697  ) +
        available_10549698   * exp(v10549698  ) +
        available_10550695   * exp(v10550695  ) +
        available_10553690   * exp(v10553690  ) +
        available_10555696   * exp(v10555696  ) +
        available_11586693   * exp(v11586693  ) +
        available_11588694   * exp(v11588694  ) +
        available_11771692   * exp(v11771692  ) +
        available_11773688   * exp(v11773688  ) +
        available_11774687   * exp(v11774687  ) +
        available_11776686   * exp(v11776686  ) +
        available_11777685   * exp(v11777685  ) +
        available_11786699   * exp(v11786699  ) +
        available_11788689   * exp(v11788689  ) +
        available_13774351   * exp(v13774351  ) +
        available_14001522   * exp(v14001522  ) +
        available_15776973   * exp(v15776973  ) +
        available_16516267   * exp(v16516267  ) +
        available_16517266   * exp(v16517266  ) +
        available_16518268   * exp(v16518268  ) +
        available_16676562   * exp(v16676562  ) +
        available_16987789   * exp(v16987789  ) +
        available_16988790   * exp(v16988790  ) +
        available_17086850   * exp(v17086850  ) +
        available_17127981   * exp(v17127981  ) +
        available_17991132   * exp(v17991132  ) +
        available_18006841   * exp(v18006841  ) +
        available_21480620   * exp(v21480620  ) +
        available_22291300   * exp(v22291300  ) +
        available_22783572   * exp(v22783572  ) +
        available_22784571   * exp(v22784571  ) +
        available_22786570   * exp(v22786570  ) +
        available_22787569   * exp(v22787569  ) +
        available_22788573   * exp(v22788573  ) +
        available_27025847   * exp(v27025847  ) +
        available_27474650   * exp(v27474650  ) +
        available_28086580   * exp(v28086580  ) +
        available_28087579   * exp(v28087579  ) +
        available_28088582   * exp(v28088582  ) +
        available_28206761   * exp(v28206761  ) +
        available_28233766   * exp(v28233766  ) +
        available_28236769   * exp(v28236769  ) +
        available_29287548   * exp(v29287548  ) +
        available_29288550   * exp(v29288550  ) +
        available_29566892   * exp(v29566892  ) +
        available_29568893   * exp(v29568893  ) +
        available_31352818   * exp(v31352818  ) +
        available_33102819   * exp(v33102819  ) +
        available_34001179   * exp(v34001179  ) +
        available_34004177   * exp(v34004177  ) +
        available_34006175   * exp(v34006175  ) +
        available_34007174   * exp(v34007174  ) +
        available_34008178   * exp(v34008178  ) +
        available_34014137   * exp(v34014137  ) +
        available_34021113   * exp(v34021113  ) +
        available_34026169   * exp(v34026169  ) +
        available_34029146   * exp(v34029146  ) +
        available_34030147   * exp(v34030147  ) +
        available_34032148   * exp(v34032148  ) +
        available_34036168   * exp(v34036168  ) +
        available_34052171   * exp(v34052171  ) +
        available_34061172   * exp(v34061172  ) +
        available_34076184   * exp(v34076184  ) +
        available_34116159   * exp(v34116159  ) +
        available_34117158   * exp(v34117158  ) +
        available_34155616   * exp(v34155616  ) +
        available_34162614   * exp(v34162614  ) +
        available_35315561   * exp(v35315561  ) +
        available_35316556   * exp(v35316556  ) +
        available_35317555   * exp(v35317555  ) +
        available_35318559   * exp(v35318559  ) +
        available_35354162   * exp(v35354162  ) +
        available_35416952   * exp(v35416952  ) +
        available_35418953   * exp(v35418953  ) +
        available_36572532   * exp(v36572532  ) +
        available_40482820   * exp(v40482820  ) +
        available_41271111   * exp(v41271111  ) +
        available_41279896   * exp(v41279896  ) +
        available_41284608   * exp(v41284608  ) +
        available_41299880   * exp(v41299880  ) +
        available_41316966   * exp(v41316966  ) +
        available_41320957   * exp(v41320957  ) +
        available_41413922   * exp(v41413922  ) +
        available_41840902   * exp(v41840902  ) +
        available_41927891   * exp(v41927891  ) +
        available_42006282   * exp(v42006282  ) +
        available_43022218   * exp(v43022218  ) +
        available_43024215   * exp(v43024215  ) +
        available_43025219   * exp(v43025219  ) +
        available_43026214   * exp(v43026214  ) +
        available_43027213   * exp(v43027213  ) +
        available_43028216   * exp(v43028216  ) +
        available_43031437   * exp(v43031437  ) +
        available_43035438   * exp(v43035438  ) +
        available_43036432   * exp(v43036432  ) +
        available_43037431   * exp(v43037431  ) +
        available_43038436   * exp(v43038436  ) +
        available_43046475   * exp(v43046475  ) +
        available_43048476   * exp(v43048476  ) +
        available_43050426   * exp(v43050426  ) +
        available_43051427   * exp(v43051427  ) +
        available_43066467   * exp(v43066467  ) +
        available_43076208   * exp(v43076208  ) +
        available_43077207   * exp(v43077207  ) +
        available_43116455   * exp(v43116455  ) +
        available_43117454   * exp(v43117454  ) +
        available_43120477   * exp(v43120477  ) +
        available_43121483   * exp(v43121483  ) +
        available_43123481   * exp(v43123481  ) +
        available_43124480   * exp(v43124480  ) +
        available_43125484   * exp(v43125484  ) +
        available_43126479   * exp(v43126479  ) +
        available_43127478   * exp(v43127478  ) +
        available_43128482   * exp(v43128482  ) +
        available_43136446   * exp(v43136446  ) +
        available_43137443   * exp(v43137443  ) +
        available_43138448   * exp(v43138448  ) +
        available_43145469   * exp(v43145469  ) +
        available_43156402   * exp(v43156402  ) +
        available_43197488   * exp(v43197488  ) +
        available_43205458   * exp(v43205458  ) +
        available_43209415   * exp(v43209415  ) +
        available_43984204   * exp(v43984204  ) +
        available_43986205   * exp(v43986205  ) +
        available_44217554   * exp(v44217554  ) +
        available_44258701   * exp(v44258701  ) +
        available_47161262   * exp(v47161262  ) +
        available_47171261   * exp(v47171261  ) +
        available_48661746   * exp(v48661746  ) +
        available_48671745   * exp(v48671745  ) +
        available_48681749   * exp(v48681749  ) +
        available_49361993   * exp(v49361993  ) +
        available_50062247   * exp(v50062247  ) +
        available_50362273   * exp(v50362273  ) +
        available_50372272   * exp(v50372272  ) +
        available_52882750   * exp(v52882750  ) +
        available_52892748   * exp(v52892748  ) +
        available_52902749   * exp(v52902749  ) +
        available_53262930   * exp(v53262930  ) +
        available_53272929   * exp(v53272929  ) +
        available_53462949   * exp(v53462949  ) +
        available_53472952   * exp(v53472952  ) +
        available_53492951   * exp(v53492951  ) +
        available_54056332   * exp(v54056332  ) +
        available_54057331   * exp(v54057331  ) +
        available_54646335   * exp(v54646335  ) +
        available_54647334   * exp(v54647334  ) +
        available_54863315   * exp(v54863315  ) +
        available_55246362   * exp(v55246362  ) +
        available_55963447   * exp(v55963447  ) +
        available_56063448   * exp(v56063448  ) +
        available_56828417   * exp(v56828417  ) +
        available_56840464   * exp(v56840464  ) +
        available_56843466   * exp(v56843466  ) +
        available_56846465   * exp(v56846465  ) +
        available_56850463   * exp(v56850463  ) +
        available_56957542   * exp(v56957542  ) +
        available_56958543   * exp(v56958543  ) +
        available_57051553   * exp(v57051553  ) +
        available_57092420   * exp(v57092420  ) +
        available_64336401   * exp(v64336401  ) +
        available_68022508   * exp(v68022508  ) +
        available_68036500   * exp(v68036500  ) +
        available_68037498   * exp(v68037498  ) +
        available_68038502   * exp(v68038502  ) +
        available_68039501   * exp(v68039501  ) +
        available_68049509   * exp(v68049509  ) +
        available_69971349   * exp(v69971349  ) +
        available_69981351   * exp(v69981351  ) +
        available_71886256   * exp(v71886256  ) +
        available_72816991   * exp(v72816991  ) +
        available_79026343   * exp(v79026343  ) +
        available_80571355   * exp(v80571355  ) +
        available_80574352   * exp(v80574352  ) +
        available_80576351   * exp(v80576351  ) +
        available_80577350   * exp(v80577350  ) +
        available_80578354   * exp(v80578354  ) +
        available_81966346   * exp(v81966346  ) +
        available_82082628   * exp(v82082628  ) +
        available_82126349   * exp(v82126349  ) +
        available_82127348   * exp(v82127348  ) +
        available_82147356   * exp(v82147356  ) +
        available_82187359   * exp(v82187359  ) +
        available_88253245   * exp(v88253245  ) +
        available_88263244   * exp(v88263244  ) +
        available_88273241   * exp(v88273241  ) +
        available_88283243   * exp(v88283243  ) +
        available_89139984   * exp(v89139984  ) +
        available_92783722   * exp(v92783722  ) +
        available_94263877   * exp(v94263877  ) +
        available_94283878   * exp(v94283878  ) +
        available_100064432  * exp(v100064432 ) +
        available_100084433  * exp(v100084433 ) +
        available_100094431  * exp(v100094431 ) +
        available_102785180  * exp(v102785180 ) +
        available_106251054  * exp(v106251054 ) +
        available_106261050  * exp(v106261050 ) +
        available_106271048  * exp(v106271048 ) +
        available_106281053  * exp(v106281053 ) +
        available_108051512  * exp(v108051512 ) +
        available_108071511  * exp(v108071511 ) +
        available_108081510  * exp(v108081510 ) +
        available_108181507  * exp(v108181507 ) +
        available_108361481  * exp(v108361481 ) +
        available_108902109  * exp(v108902109 ) +
        available_112901489  * exp(v112901489 ) +
        available_112931487  * exp(v112931487 ) +
        available_112941486  * exp(v112941486 ) +
        available_112961476  * exp(v112961476 ) +
        available_112971485  * exp(v112971485 ) +
        available_112981488  * exp(v112981488 ) +
        available_113464508  * exp(v113464508 ) +
        available_113474507  * exp(v113474507 ) +
        available_113484509  * exp(v113484509 ) +
        available_113661518  * exp(v113661518 ) +
        available_119361068  * exp(v119361068 ) +
        available_124041066  * exp(v124041066 ) +
        available_124061065  * exp(v124061065 ) +
        available_124071064  * exp(v124071064 ) +
        available_124081067  * exp(v124081067 ) +
        available_124641076  * exp(v124641076 ) +
        available_124661075  * exp(v124661075 ) +
        available_124671074  * exp(v124671074 ) +
        available_124761073  * exp(v124761073 ) +
        available_124781072  * exp(v124781072 ) +
        available_124791071  * exp(v124791071 ) +
        available_126681083  * exp(v126681083 ) +
        available_128564285  * exp(v128564285 ) +
        available_128884281  * exp(v128884281 ) +
        available_130361080  * exp(v130361080 ) +
        available_130371079  * exp(v130371079 ) +
        available_130381081  * exp(v130381081 ) +
        available_133883651  * exp(v133883651 ) +
        available_136383284  * exp(v136383284 ) +
        available_141923976  * exp(v141923976 ) +
        available_141993977  * exp(v141993977 ) +
        available_152465358  * exp(v152465358 ) +
        available_152475357  * exp(v152475357 ) +
        available_152485359  * exp(v152485359 ) +
        available_152965362  * exp(v152965362 ) +
        available_156262825  * exp(v156262825 ) +
        available_156272824  * exp(v156272824 ) +
        available_156282827  * exp(v156282827 ) +
        available_156442826  * exp(v156442826 ) +
        available_156774143  * exp(v156774143 ) +
        available_159405139  * exp(v159405139 ) +
        available_172061186  * exp(v172061186 ) +
        available_178261887  * exp(v178261887 ) +
        available_178301888  * exp(v178301888 ) +
        available_179161934  * exp(v179161934 ) +
        available_179561971  * exp(v179561971 ) +
        available_179581973  * exp(v179581973 ) +
        available_181961984  * exp(v181961984 ) +
        available_181981986  * exp(v181981986 ) +
        available_184072158  * exp(v184072158 ) +
        available_184082159  * exp(v184082159 ) +
        available_190262884  * exp(v190262884 ) +
        available_190612905  * exp(v190612905 ) +
        available_190632876  * exp(v190632876 ) +
        available_190642874  * exp(v190642874 ) +
        available_190662873  * exp(v190662873 ) +
        available_190672872  * exp(v190672872 ) +
        available_190682877  * exp(v190682877 ) +
        available_190822906  * exp(v190822906 ) +
        available_190962911  * exp(v190962911 ) +
        available_191122890  * exp(v191122890 ) +
        available_192263131  * exp(v192263131 ) +
        available_194763335  * exp(v194763335 ) +
        available_194773333  * exp(v194773333 ) +
        available_194783336  * exp(v194783336 ) +
        available_194863332  * exp(v194863332 ) +
        available_202463689  * exp(v202463689 ) +
        available_202473688  * exp(v202473688 ) +
        available_202483690  * exp(v202483690 ) +
        available_202863693  * exp(v202863693 ) +
        available_215954944  * exp(v215954944 ) +
        available_215964941  * exp(v215964941 ) +
        available_215974940  * exp(v215974940 ) +
        available_215984943  * exp(v215984943 ) +
        available_221215325  * exp(v221215325 ) +
        available_221555322  * exp(v221555322 ) +
        available_221565319  * exp(v221565319 ) +
        available_221575318  * exp(v221575318 ) +
        available_222135384  * exp(v222135384 ) +
        available_222165385  * exp(v222165385 ) +
        available_232771028  * exp(v232771028 ) +
        available_232781030  * exp(v232781030 ) +
        available_236262086  * exp(v236262086 ) +
        available_237082092  * exp(v237082092 ) +
        available_238232083  * exp(v238232083 ) +
        available_238242082  * exp(v238242082 ) +
        available_238262081  * exp(v238262081 ) +
        available_238272080  * exp(v238272080 ) +
        available_238282084  * exp(v238282084 ) +
        available_241562463  * exp(v241562463 ) +
        available_241572462  * exp(v241572462 ) +
        available_241582464  * exp(v241582464 ) +
        available_244543085  * exp(v244543085 ) +
        available_244563084  * exp(v244563084 ) +
        available_244573083  * exp(v244573083 ) +
        available_244583087  * exp(v244583087 ) +
        available_244663089  * exp(v244663089 ) +
        available_247063452  * exp(v247063452 ) +
        available_247283453  * exp(v247283453 ) +
        available_256034475  * exp(v256034475 ) +
        available_256044474  * exp(v256044474 ) +
        available_256064473  * exp(v256064473 ) +
        available_256074472  * exp(v256074472 ) +
        available_256084476  * exp(v256084476 ) +
        available_256164478  * exp(v256164478 ) +
        available_258764867  * exp(v258764867 ) +
        available_258774866  * exp(v258774866 ) +
        available_258784869  * exp(v258784869 ) +
        available_265862175  * exp(v265862175 ) +
        available_266562181  * exp(v266562181 ) +
        available_268202791  * exp(v268202791 ) +
        available_268212792  * exp(v268212792 ) +
        available_268222784  * exp(v268222784 ) +
        available_268232789  * exp(v268232789 ) +
        available_268242793  * exp(v268242793 ) +
        available_268262788  * exp(v268262788 ) +
        available_268272787  * exp(v268272787 ) +
        available_268282790  * exp(v268282790 ) +
        available_269062795  * exp(v269062795 ) +
        available_270271374  * exp(v270271374 ) +
        available_270562907  * exp(v270562907 ) +
        available_271024938  * exp(v271024938 ) +
        available_273922903  * exp(v273922903 ) +
        available_274102893  * exp(v274102893 ) +
        available_274541302  * exp(v274541302 ) +
        available_274791372  * exp(v274791372 ) +
        available_275444212  * exp(v275444212 ) +
        available_275544501  * exp(v275544501 ) +
        available_275624505  * exp(v275624505 ) +
        available_275852626  * exp(v275852626 ) +
        available_286252492  * exp(v286252492 ) +
        available_287184070  * exp(v287184070 ) +
        available_288664915  * exp(v288664915 ) +
        available_288674914  * exp(v288674914 ) +
        available_288684918  * exp(v288684918 ) +
        available_288904925  * exp(v288904925 ) +
        available_297281031  * exp(v297281031 ) +
        available_299932071  * exp(v299932071 ) +
        available_299942070  * exp(v299942070 ) +
        available_299962069  * exp(v299962069 ) +
        available_299972068  * exp(v299972068 ) +
        available_299982072  * exp(v299982072 ) +
        available_300362067  * exp(v300362067 ) +
        available_300562090  * exp(v300562090 ) +
        available_300572089  * exp(v300572089 ) +
        available_300582091  * exp(v300582091 ) +
        available_302362196  * exp(v302362196 ) +
        available_302382197  * exp(v302382197 ) +
        available_303162321  * exp(v303162321 ) +
        available_303172320  * exp(v303172320 ) +
        available_303182322  * exp(v303182322 ) +
        available_305262468  * exp(v305262468 ) +
        available_305272467  * exp(v305272467 ) +
        available_305282469  * exp(v305282469 ) +
        available_311673458  * exp(v311673458 ) +
        available_312083459  * exp(v312083459 ) +
        available_314723602  * exp(v314723602 ) +
        available_314733605  * exp(v314733605 ) +
        available_314743603  * exp(v314743603 ) +
        available_314753601  * exp(v314753601 ) +
        available_316543821  * exp(v316543821 ) +
        available_316563820  * exp(v316563820 ) +
        available_316573819  * exp(v316573819 ) +
        available_316583822  * exp(v316583822 ) +
        available_316663823  * exp(v316663823 ) +
        available_317183995  * exp(v317183995 ) +
        available_317193994  * exp(v317193994 ) +
        available_322314488  * exp(v322314488 ) +
        available_322324485  * exp(v322324485 ) +
        available_322334486  * exp(v322334486 ) +
        available_322344484  * exp(v322344484 ) +
        available_322354489  * exp(v322354489 ) +
        available_322364483  * exp(v322364483 ) +
        available_322374482  * exp(v322374482 ) +
        available_322384487  * exp(v322384487 ) +
        available_332564494  * exp(v332564494 ) +
        available_336634891  * exp(v336634891 ) +
        available_337163842  * exp(v337163842 ) +
        available_337173841  * exp(v337173841 ) +
        available_341641324  * exp(v341641324 ) +
        available_341971322  * exp(v341971322 ) +
        available_341981344  * exp(v341981344 ) +
        available_343592414  * exp(v343592414 ) +
        available_343662146  * exp(v343662146 ) +
        available_343682147  * exp(v343682147 ) +
        available_344222417  * exp(v344222417 ) +
        available_344232415  * exp(v344232415 ) +
        available_344252418  * exp(v344252418 ) +
        available_344332416  * exp(v344332416 ) +
        available_344493093  * exp(v344493093 ) +
        available_344563096  * exp(v344563096 ) +
        available_344573094  * exp(v344573094 ) +
        available_344583097  * exp(v344583097 ) +
        available_345463959  * exp(v345463959 ) +
        available_345663961  * exp(v345663961 ) +
        available_345784107  * exp(v345784107 ) +
        available_345794105  * exp(v345794105 ) +
        available_346904740  * exp(v346904740 ) +
        available_347464848  * exp(v347464848 ) +
        available_347474844  * exp(v347474844 ) +
        available_347484849  * exp(v347484849 ) +
        available_347864825  * exp(v347864825 ) +
        available_348174871  * exp(v348174871 ) +
        available_348204895  * exp(v348204895 ) +
        available_348214898  * exp(v348214898 ) +
        available_348234877  * exp(v348234877 ) +
        available_348564927  * exp(v348564927 ) +
        available_348714847  * exp(v348714847 ) +
        available_348814855  * exp(v348814855 ) +
        available_349195043  * exp(v349195043 ) +
        available_349355042  * exp(v349355042 ) +
        available_349725007  * exp(v349725007 ) +
        available_349955017  * exp(v349955017 ) +
        available_352134741  * exp(v352134741 ) +
        available_356264111  * exp(v356264111 ) +
        available_356284112  * exp(v356284112 ) +
        available_356993944  * exp(v356993944 ) +
        available_357804051  * exp(v357804051 ) +
        available_359132078  * exp(v359132078 ) +
        available_359142077  * exp(v359142077 ) +
        available_359162076  * exp(v359162076 ) +
        available_359172075  * exp(v359172075 ) +
        available_359182079  * exp(v359182079 ) +
        available_359262073  * exp(v359262073 ) +
        available_359442095  * exp(v359442095 ) +
        available_359462094  * exp(v359462094 ) +
        available_359472093  * exp(v359472093 ) +
        available_359482096  * exp(v359482096 ) +
        available_359645019  * exp(v359645019 ) +
        available_361862324  * exp(v361862324 ) +
        available_361882325  * exp(v361882325 ) +
        available_363012478  * exp(v363012478 ) +
        available_363042475  * exp(v363042475 ) +
        available_363052474  * exp(v363052474 ) +
        available_363072473  * exp(v363072473 ) +
        available_363082477  * exp(v363082477 ) +
        available_364472674  * exp(v364472674 ) +
        available_366673145  * exp(v366673145 ) +
        available_366683146  * exp(v366683146 ) +
        available_368711323  * exp(v368711323 ) +
        available_368863465  * exp(v368863465 ) +
        available_368873467  * exp(v368873467 ) +
        available_369013468  * exp(v369013468 ) +
        available_369033466  * exp(v369033466 ) +
        available_369043469  * exp(v369043469 ) +
        available_369073471  * exp(v369073471 ) +
        available_369083470  * exp(v369083470 ) +
        available_369683635  * exp(v369683635 ) +
        available_369693633  * exp(v369693633 ) +
        available_369713637  * exp(v369713637 ) +
        available_369743645  * exp(v369743645 ) +
        available_369763644  * exp(v369763644 ) +
        available_369783647  * exp(v369783647 ) +
        available_371463754  * exp(v371463754 ) +
        available_372173850  * exp(v372173850 ) +
        available_373365237  * exp(v373365237 ) +
        available_373385241  * exp(v373385241 ) +
        available_373395239  * exp(v373395239 ) +
        available_373464016  * exp(v373464016 ) +
        available_373474014  * exp(v373474014 ) +
        available_373484017  * exp(v373484017 ) +
        available_373564018  * exp(v373564018 ) +
        available_374134136  * exp(v374134136 ) +
        available_374144135  * exp(v374144135 ) +
        available_374164134  * exp(v374164134 ) +
        available_374174133  * exp(v374174133 ) +
        available_374184137  * exp(v374184137 ) +
        available_374264138  * exp(v374264138 ) +
        available_376404291  * exp(v376404291 ) +
        available_376424292  * exp(v376424292 ) +
        available_378864512  * exp(v378864512 ) +
        available_378884513  * exp(v378884513 ) +
        available_379344565  * exp(v379344565 ) +
        available_379354567  * exp(v379354567 ) +
        available_379364564  * exp(v379364564 ) +
        available_379374563  * exp(v379374563 ) +
        available_379384566  * exp(v379384566 ) +
        available_379864600  * exp(v379864600 ) +
        available_379874598  * exp(v379874598 ) +
        available_379884601  * exp(v379884601 ) +
        available_379914710  * exp(v379914710 ) +
        available_379934708  * exp(v379934708 ) +
        available_379944707  * exp(v379944707 ) +
        available_379964706  * exp(v379964706 ) +
        available_379974705  * exp(v379974705 ) +
        available_379984709  * exp(v379984709 ) +
        available_380064712  * exp(v380064712 ) +
        available_380084711  * exp(v380084711 ) +
        available_380884122  * exp(v380884122 ) +
        available_381765067  * exp(v381765067 ) +
        available_381775064  * exp(v381775064 ) +
        available_381785066  * exp(v381785066 ) +
        available_398664701  * exp(v398664701 ) +
        available_401181854  * exp(v401181854 ) +
        available_401933622  * exp(v401933622 ) +
        available_402973593  * exp(v402973593 ) +
        available_403133619  * exp(v403133619 ) +
        available_403945197  * exp(v403945197 ) +
        available_404115227  * exp(v404115227 ) +
        available_405923616  * exp(v405923616 ) +
        available_405933617  * exp(v405933617 ) +
        available_405943615  * exp(v405943615 ) +
        available_405983628  * exp(v405983628 ) +
        available_405993626  * exp(v405993626 ) +
        available_406044636  * exp(v406044636 ) +
        available_406144352  * exp(v406144352 ) +
        available_406255200  * exp(v406255200 ) +
        available_406325242  * exp(v406325242 ) +
        available_409264670  * exp(v409264670 ) +
        available_409304631  * exp(v409304631 ) +
        available_409464717  * exp(v409464717 ) +
        available_409564691  * exp(v409564691 ) +
        available_409654718  * exp(v409654718 ) +
        available_409764615  * exp(v409764615 ) +
        available_410762864  * exp(v410762864 ) +
        available_410772862  * exp(v410772862 ) +
        available_410782865  * exp(v410782865 ) +
        available_412092062  * exp(v412092062 ) +
        available_413605215  * exp(v413605215 ) +
        available_415002465  * exp(v415002465 ) +
        available_415924639  * exp(v415924639 ) +
        available_416015183  * exp(v416015183 ) +
        available_416045206  * exp(v416045206 ) +
        available_416255207  * exp(v416255207 ) +
        available_416535202  * exp(v416535202 ) +
        available_416815221  * exp(v416815221 ) +
        available_416885191  * exp(v416885191 ) +
        available_416925185  * exp(v416925185 ) +
        available_416935184  * exp(v416935184 ) +
        available_416945182  * exp(v416945182 ) +
        available_416965209  * exp(v416965209 ) +
        available_416985210  * exp(v416985210 ) +
        available_417015211  * exp(v417015211 ) +
        available_417045220  * exp(v417045220 ) +
        available_417055219  * exp(v417055219 ) +
        available_417073998  * exp(v417073998 ) +
        available_417154626  * exp(v417154626 ) +
        available_417194633  * exp(v417194633 ) +
        available_417464623  * exp(v417464623 ) +
        available_417474622  * exp(v417474622 ) +
        available_417835190  * exp(v417835190 ) +
        available_417964664  * exp(v417964664 ) +
        available_417974663  * exp(v417974663 ) +
        available_418064700  * exp(v418064700 ) +
        available_418461979  * exp(v418461979 ) +
        available_419895193  * exp(v419895193 ) +
        available_420025194  * exp(v420025194 ) +
        available_420095196  * exp(v420095196 ) +
        available_420795195  * exp(v420795195 ) +
        available_421663588  * exp(v421663588 ) +
        available_421673587  * exp(v421673587 ) +
        available_423123372  * exp(v423123372 ) +
        available_424444531  * exp(v424444531 ) +
        available_426663583  * exp(v426663583 ) +
        available_426763366  * exp(v426763366 ) +
        available_426873343  * exp(v426873343 ) +
        available_426993362  * exp(v426993362 ) +
        available_427033371  * exp(v427033371 ) +
        available_427123353  * exp(v427123353 ) +
        available_427163350  * exp(v427163350 ) +
        available_427173349  * exp(v427173349 ) +
        available_427183352  * exp(v427183352 ) +
        available_429845112  * exp(v429845112 ) +
        available_432191119  * exp(v432191119 ) +
        available_432441094  * exp(v432441094 ) +
        available_432821121  * exp(v432821121 ) +
        available_432851114  * exp(v432851114 ) +
        available_432871118  * exp(v432871118 ) +
        available_432961130  * exp(v432961130 ) +
        available_433021096  * exp(v433021096 ) +
        available_433161127  * exp(v433161127 ) +
        available_433281141  * exp(v433281141 ) +
        available_433301142  * exp(v433301142 ) +
        available_433311136  * exp(v433311136 ) +
        available_433331137  * exp(v433331137 ) +
        available_433341134  * exp(v433341134 ) +
        available_433351138  * exp(v433351138 ) +
        available_433361110  * exp(v433361110 ) +
        available_433371133  * exp(v433371133 ) +
        available_433381135  * exp(v433381135 ) +
        available_433861169  * exp(v433861169 ) +
        available_433871168  * exp(v433871168 ) +
        available_433881170  * exp(v433881170 ) +
        available_434091115  * exp(v434091115 ) +
        available_434101116  * exp(v434101116 ) +
        available_434151107  * exp(v434151107 ) +
        available_434181139  * exp(v434181139 ) +
        available_434261790  * exp(v434261790 ) +
        available_438484334  * exp(v438484334 ) +
        available_443501003  * exp(v443501003 ) +
        available_444191536  * exp(v444191536 ) +
        available_444561561  * exp(v444561561 ) +
        available_444761542  * exp(v444761542 ) +
        available_444991554  * exp(v444991554 ) +
        available_445161553  * exp(v445161553 ) +
        available_445201565  * exp(v445201565 ) +
        available_446032066  * exp(v446032066 ) +
        available_446573204  * exp(v446573204 ) +
        available_446583206  * exp(v446583206 ) +
        available_452453828  * exp(v452453828 ) +
        available_452463826  * exp(v452463826 ) +
        available_452473825  * exp(v452473825 ) +
        available_452483827  * exp(v452483827 ) +
        available_452753858  * exp(v452753858 ) +
        available_452763855  * exp(v452763855 ) +
        available_452773854  * exp(v452773854 ) +
        available_452783857  * exp(v452783857 ) +
        available_454184020  * exp(v454184020 ) +
        available_454194019  * exp(v454194019 ) +
        available_458844377  * exp(v458844377 ) +
        available_458864376  * exp(v458864376 ) +
        available_458874375  * exp(v458874375 ) +
        available_458884378  * exp(v458884378 ) +
        available_463502472  * exp(v463502472 ) +
        available_463512471  * exp(v463512471 ) +
        available_465043156  * exp(v465043156 ) +
        available_465063157  * exp(v465063157 ) +
        available_477851437  * exp(v477851437 ) +
        available_477861438  * exp(v477861438 ) +
        available_480992500  * exp(v480992500 ) +
        available_481022508  * exp(v481022508 ) +
        available_481032505  * exp(v481032505 ) +
        available_481042504  * exp(v481042504 ) +
        available_481052499  * exp(v481052499 ) +
        available_481062503  * exp(v481062503 ) +
        available_490864235  * exp(v490864235 ) +
        available_491854246  * exp(v491854246 ) +
        available_491864248  * exp(v491864248 ) +
        available_491894249  * exp(v491894249 ) +
        available_506864806  * exp(v506864806 ) +
        available_516964816  * exp(v516964816 ) +
        available_521461289  * exp(v521461289 ) +
        available_521861292  * exp(v521861292 ) +
        available_523121283  * exp(v523121283 ) +
        available_523141284  * exp(v523141284 ) +
        available_523161282  * exp(v523161282 ) +
        available_523171287  * exp(v523171287 ) +
        available_523181285  * exp(v523181285 ) +
        available_523191286  * exp(v523191286 ) +
        available_525631877  * exp(v525631877 ) +
        available_525801874  * exp(v525801874 ) +
        available_525811875  * exp(v525811875 ) +
        available_525821873  * exp(v525821873 ) +
        available_525931865  * exp(v525931865 ) +
        available_525941866  * exp(v525941866 ) +
        available_525951870  * exp(v525951870 ) +
        available_525961862  * exp(v525961862 ) +
        available_525971869  * exp(v525971869 ) +
        available_525981863  * exp(v525981863 ) +
        available_525991868  * exp(v525991868 ) +
        available_528063140  * exp(v528063140 ) +
        available_528353142  * exp(v528353142 ) +
        available_532103914  * exp(v532103914 ) +
        available_532113924  * exp(v532113924 ) +
        available_532133912  * exp(v532133912 ) +
        available_532143913  * exp(v532143913 ) +
        available_532163911  * exp(v532163911 ) +
        available_532183915  * exp(v532183915 ) +
        available_544363564  * exp(v544363564 ) +
        available_544463799  * exp(v544463799 ) +
        available_544473798  * exp(v544473798 ) +
        available_544483801  * exp(v544483801 ) +
        available_547061635  * exp(v547061635 ) +
        available_550663565  * exp(v550663565 ) +
        available_550683566  * exp(v550683566 ) +
        available_550843804  * exp(v550843804 ) +
        available_550863803  * exp(v550863803 ) +
        available_550873802  * exp(v550873802 ) +
        available_550883805  * exp(v550883805 ) +
        available_551063991  * exp(v551063991 ) +
        available_555063810  * exp(v555063810 ) +
        available_561953918  * exp(v561953918 ) +
        available_561963916  * exp(v561963916 ) +
        available_562063832  * exp(v562063832 ) +
        available_571201242  * exp(v571201242 ) +
        available_571251241  * exp(v571251241 ) +
        available_571291244  * exp(v571291244 ) +
        available_571441252  * exp(v571441252 ) +
        available_571481243  * exp(v571481243 ) +
        available_571571246  * exp(v571571246 ) +
        available_571581251  * exp(v571581251 ) +
        available_571611240  * exp(v571611240 ) +
        available_571741250  * exp(v571741250 ) +
        available_588011108  * exp(v588011108 ) +
        available_588352963  * exp(v588352963 ) +
        available_588362961  * exp(v588362961 ) +
        available_588382962  * exp(v588382962 ) +
        available_588402957  * exp(v588402957 ) +
        available_588422975  * exp(v588422975 ) +
        available_588602998  * exp(v588602998 ) +
        available_588682969  * exp(v588682969 ) +
        available_588722960  * exp(v588722960 ) +
        available_588732971  * exp(v588732971 ) +
        available_588752994  * exp(v588752994 ) +
        available_588762993  * exp(v588762993 ) +
        available_588852997  * exp(v588852997 ) +
        available_588862996  * exp(v588862996 ) +
        available_590371737  * exp(v590371737 ) +
        available_591001960  * exp(v591001960 ) +
        available_592312659  * exp(v592312659 ) +
        available_620613048  * exp(v620613048 ) +
        available_620973829  * exp(v620973829 ) +
        available_624003402  * exp(v624003402 ) +
        available_624053409  * exp(v624053409 ) +
        available_624203414  * exp(v624203414 ) +
        available_633554549  * exp(v633554549 ) +
        available_633574553  * exp(v633574553 ) +
        available_633594561  * exp(v633594561 ) +
        available_635284382  * exp(v635284382 ) +
        available_635304384  * exp(v635304384 ) +
        available_637554961  * exp(v637554961 ) +
        available_639595188  * exp(v639595188 ) +
        available_639635214  * exp(v639635214 ) +
        available_641361756  * exp(v641361756 ) +
        available_645731333  * exp(v645731333 ) +
        available_646011145  * exp(v646011145 ) +
        available_646761226  * exp(v646761226 ) +
        available_647111326  * exp(v647111326 ) +
        available_647121331  * exp(v647121331 ) +
        available_647161327  * exp(v647161327 ) +
        available_647361348  * exp(v647361348 ) +
        available_647551338  * exp(v647551338 ) +
        available_647591339  * exp(v647591339 ) +
        available_647761385  * exp(v647761385 ) +
        available_648582044  * exp(v648582044 ) +
        available_648622040  * exp(v648622040 ) +
        available_648632042  * exp(v648632042 ) +
        available_648642046  * exp(v648642046 ) +
        available_648652045  * exp(v648652045 ) +
        available_648662041  * exp(v648662041 ) +
        available_648672039  * exp(v648672039 ) +
        available_648682043  * exp(v648682043 ) +
        available_648702047  * exp(v648702047 ) +
        available_648761840  * exp(v648761840 ) +
        available_649962135  * exp(v649962135 ) +
        available_650662318  * exp(v650662318 ) +
        available_651262343  * exp(v651262343 ) +
        available_651272341  * exp(v651272341 ) +
        available_651952633  * exp(v651952633 ) +
        available_651994947  * exp(v651994947 ) +
        available_652004948  * exp(v652004948 ) +
        available_652482816  * exp(v652482816 ) +
        available_652512814  * exp(v652512814 ) +
        available_652532812  * exp(v652532812 ) +
        available_652542810  * exp(v652542810 ) +
        available_652562809  * exp(v652562809 ) +
        available_652572808  * exp(v652572808 ) +
        available_652582813  * exp(v652582813 ) +
        available_666364310  * exp(v666364310 ) +
        available_668364802  * exp(v668364802 ) +
        available_669362356  * exp(v669362356 ) +
        available_670063521  * exp(v670063521 ) +
        available_672665510  * exp(v672665510 ) +
        available_672675509  * exp(v672675509 ) +
        available_675263042  * exp(v675263042 ) +
        available_675273041  * exp(v675273041 ) +
        available_675283044  * exp(v675283044 ) +
        available_675563070  * exp(v675563070 ) +
        available_675573069  * exp(v675573069 ) +
        available_675863071  * exp(v675863071 ) +
        available_681261155  * exp(v681261155 ) +
        available_681271154  * exp(v681271154 ) +
        available_683061942  * exp(v683061942 ) +
        available_686113450  * exp(v686113450 ) +
        available_688464799  * exp(v688464799 ) +
        available_696111809  * exp(v696111809 ) +
        available_696341816  * exp(v696341816 ) +
        available_696361815  * exp(v696361815 ) +
        available_696371814  * exp(v696371814 ) +
        available_696381817  * exp(v696381817 ) +
        available_696561822  * exp(v696561822 ) +
        available_696571821  * exp(v696571821 ) +
        available_696581823  * exp(v696581823 ) +
        available_696661833  * exp(v696661833 ) +
        available_696671832  * exp(v696671832 ) +
        available_699404353  * exp(v699404353 ) +
        available_699454355  * exp(v699454355 ) +
        available_699464356  * exp(v699464356 ) +
        available_699474354  * exp(v699474354 ) +
        available_727221293  * exp(v727221293 ) +
        available_730504347  * exp(v730504347 ) +
        available_730514350  * exp(v730514350 ) +
        available_730524343  * exp(v730524343 ) +
        available_730534345  * exp(v730534345 ) +
        available_730544346  * exp(v730544346 ) +
        available_730554344  * exp(v730554344 ) +
        available_731361254  * exp(v731361254 ) +
        available_734561684  * exp(v734561684 ) +
        available_735171645  * exp(v735171645 ) +
        available_735261686  * exp(v735261686 ) +
        available_737021965  * exp(v737021965 ) +
        available_737151958  * exp(v737151958 ) +
        available_739861750  * exp(v739861750 ) +
        available_740862313  * exp(v740862313 ) +
        available_747822800  * exp(v747822800 ) +
        available_750873008  * exp(v750873008 ) +
        available_752083115  * exp(v752083115 ) +
        available_752103114  * exp(v752103114 ) +
        available_752123110  * exp(v752123110 ) +
        available_752143108  * exp(v752143108 ) +
        available_752163111  * exp(v752163111 ) +
        available_760323516  * exp(v760323516 ) +
        available_760363507  * exp(v760363507 ) +
        available_760383514  * exp(v760383514 ) +
        available_760403520  * exp(v760403520 ) +
        available_760423511  * exp(v760423511 ) +
        available_760443513  * exp(v760443513 ) +
        available_762273543  * exp(v762273543 ) +
        available_764363723  * exp(v764363723 ) +
        available_764783788  * exp(v764783788 ) +
        available_764863795  * exp(v764863795 ) +
        available_764873794  * exp(v764873794 ) +
        available_764883796  * exp(v764883796 ) +
        available_765263831  * exp(v765263831 ) +
        available_767673999  * exp(v767673999 ) +
        available_773184326  * exp(v773184326 ) +
        available_773754673  * exp(v773754673 ) +
        available_773784677  * exp(v773784677 ) +
        available_773954559  * exp(v773954559 ) +
        available_774724901  * exp(v774724901 ) +
        available_774724901  * exp(v774724901 ) +
        available_774875094  * exp(v774875094 ) +
        available_777355268  * exp(v777355268 ) +
        available_777404660  * exp(v777404660 ) +
        available_777745327  * exp(v777745327 ) +
        available_777765329  * exp(v777765329 ) +
        available_777865135  * exp(v777865135 ) +
        available_778424689  * exp(v778424689 ) +
        available_778474697  * exp(v778474697 ) +
        available_778524716  * exp(v778524716 ) +
        available_784563813  * exp(v784563813 ) +
        available_788663814  * exp(v788663814 ) +
        available_793363815  * exp(v793363815 ) +
        available_798163816  * exp(v798163816 ) +
        available_800961711  * exp(v800961711 ) +
        available_801452153  * exp(v801452153 ) +
        available_802362680  * exp(v802362680 ) +
        available_804404276  * exp(v804404276 ) +
        available_804564370  * exp(v804564370 ) +
        available_804574369  * exp(v804574369 ) +
        available_804584371  * exp(v804584371 ) +
        available_806861638  * exp(v806861638 ) +
        available_806871637  * exp(v806871637 ) +
        available_808262658  * exp(v808262658 ) +
        available_808272657  * exp(v808272657 ) +
        available_811963839  * exp(v811963839 ) +
        available_812043837  * exp(v812043837 ) +
        available_812063836  * exp(v812063836 ) +
        available_812073835  * exp(v812073835 ) +
        available_812083838  * exp(v812083838 ) +
        available_814065047  * exp(v814065047 ) +
        available_826061716  * exp(v826061716 ) +
        available_826071715  * exp(v826071715 ) +
        available_826101669  * exp(v826101669 ) +
        available_826111670  * exp(v826111670 ) +
        available_826181673  * exp(v826181673 ) +
        available_826261648  * exp(v826261648 ) +
        available_826271647  * exp(v826271647 ) +
        available_826361667  * exp(v826361667 ) +
        available_826371666  * exp(v826371666 ) +
        available_827861643  * exp(v827861643 ) +
        available_827871639  * exp(v827871639 ) +
        available_828201723  * exp(v828201723 ) +
        available_828261702  * exp(v828261702 ) +
        available_828271708  * exp(v828271708 ) +
        available_828401700  * exp(v828401700 ) +
        available_828461681  * exp(v828461681 ) +
        available_828471697  * exp(v828471697 ) +
        available_828491699  * exp(v828491699 ) +
        available_828661729  * exp(v828661729 ) +
        available_828671728  * exp(v828671728 ) +
        available_828811672  * exp(v828811672 ) +
        available_829572064  * exp(v829572064 ) +
        available_833632660  * exp(v833632660 ) +
        available_833702664  * exp(v833702664 ) +
        available_839063438  * exp(v839063438 ) +
        available_839073437  * exp(v839073437 ) +
        available_844563834  * exp(v844563834 ) +
        available_844573833  * exp(v844573833 ) +
        available_844863807  * exp(v844863807 ) +
        available_844963844  * exp(v844963844 ) +
        available_846174003  * exp(v846174003 ) +
        available_855261710  * exp(v855261710 ) +
        available_856063806  * exp(v856063806 ) +
        available_861121724  * exp(v861121724 ) +
        available_862503016  * exp(v862503016 ) +
        available_862513015  * exp(v862513015 ) +
        available_863103198  * exp(v863103198 ) +
        available_863903545  * exp(v863903545 ) +
        available_865063847  * exp(v865063847 ) +
        available_865073846  * exp(v865073846 ) +
        available_866375097  * exp(v866375097 ) +
        available_866692777  * exp(v866692777 ) +
        available_866702776  * exp(v866702776 ) +
        available_866722775  * exp(v866722775 ) +
        available_866922771  * exp(v866922771 ) +
        available_866932770  * exp(v866932770 ) +
        available_867964779  * exp(v867964779 ) +
        available_868074765  * exp(v868074765 ) +
        available_868814774  * exp(v868814774 ) +
        available_868834760  * exp(v868834760 ) +
        available_868844758  * exp(v868844758 ) +
        available_868854775  * exp(v868854775 ) +
        available_868864757  * exp(v868864757 ) +
        available_868874756  * exp(v868874756 ) +
        available_868884776  * exp(v868884776 ) +
        available_869164762  * exp(v869164762 ) +
        available_874082985  * exp(v874082985 ) +
        available_874092984  * exp(v874092984 ) +
        available_874851780  * exp(v874851780 ) +
        available_875861912  * exp(v875861912 ) +
        available_875961916  * exp(v875961916 ) +
        available_876431990  * exp(v876431990 ) +
        available_879363013  * exp(v879363013 ) +
        available_879373012  * exp(v879373012 ) +
        available_879383014  * exp(v879383014 ) +
        available_879973197  * exp(v879973197 ) +
        available_880363412  * exp(v880363412 ) +
        available_881473546  * exp(v881473546 ) +
        available_882943897  * exp(v882943897 ) +
        available_882963898  * exp(v882963898 ) +
        available_885564398  * exp(v885564398 ) +
        available_887665099  * exp(v887665099 ) +
        available_887675098  * exp(v887675098 ) +
        available_887685100  * exp(v887685100 ) +
        available_891211179  * exp(v891211179 ) +
        available_891541783  * exp(v891541783 ) +
        available_891751778  * exp(v891751778 ) +
        available_891912983  * exp(v891912983 ) +
        available_891932981  * exp(v891932981 ) +
        available_891942977  * exp(v891942977 ) +
        available_891962980  * exp(v891962980 ) +
        available_891972979  * exp(v891972979 ) +
        available_891982982  * exp(v891982982 ) +
        available_891992978  * exp(v891992978 ) +
        available_892153005  * exp(v892153005 ) +
        available_892421420  * exp(v892421420 ) +
        available_892781907  * exp(v892781907 ) +
        available_893863010  * exp(v893863010 ) +
        available_893873009  * exp(v893873009 ) +
        available_893883011  * exp(v893883011 ) +
        available_894473195  * exp(v894473195 ) +
        available_894963405  * exp(v894963405 ) +
        available_895331989  * exp(v895331989 ) +
        available_895773544  * exp(v895773544 ) +
        available_896174404  * exp(v896174404 ) +
        available_897864410  * exp(v897864410 ) +
        available_897874409  * exp(v897874409 ) +
        available_897884411  * exp(v897884411 ) +
        available_898362619  * exp(v898362619 ) +
        available_898372618  * exp(v898372618 ) +
        available_898875089  * exp(v898875089 ) +
        available_899165095  * exp(v899165095 ) +
        available_902836770  * exp(v902836770 ) +
        available_9046891337 * exp(v9046891337) +
        available_9368593476 * exp(v9368593476) +
        available_9369673634 * exp(v9369673634) +
        available_9465743475 * exp(v9465743475) +
        available_9885364401 * exp(v9885364401)))
  return(p)
}	


gVarNamesNormal <- c(
  "PRICE1"       ,
  "ASC9999"      , 
  "ASC734190"    , 
  "ASC1573935"   , 
  "ASC2870505"   , 
  "ASC4356521"   , 
  "ASC4626831"   , 
  "ASC5915426"   , 
  "ASC6144352"   , 
  "ASC8417089"   , 
  "ASC8751020"   , 
  "ASC8920617"   , 
  "ASC10548697"  , 
  "ASC10549698"  , 
  "ASC10550695"  , 
  "ASC10553690"  , 
  "ASC10555696"  , 
  "ASC11586693"  , 
  "ASC11588694"  , 
  "ASC11771692"  , 
  "ASC11773688"  , 
  "ASC11774687"  , 
  "ASC11776686"  , 
  "ASC11777685"  , 
  "ASC11786699"  , 
  "ASC11788689"  , 
  "ASC13774351"  , 
  "ASC14001522"  , 
  "ASC15776973"  , 
  "ASC16516267"  , 
  "ASC16517266"  , 
  "ASC16518268"  , 
  "ASC16676562"  , 
  "ASC16987789"  , 
  "ASC16988790"  , 
  "ASC17086850"  , 
  "ASC17127981"  , 
  "ASC17991132"  , 
  "ASC18006841"  , 
  "ASC21480620"  , 
  "ASC22291300"  , 
  "ASC22783572"  , 
  "ASC22784571"  , 
  "ASC22786570"  , 
  "ASC22787569"  , 
  "ASC22788573"  , 
  "ASC27025847"  , 
  "ASC27474650"  , 
  "ASC28086580"  , 
  "ASC28087579"  , 
  "ASC28088582"  , 
  "ASC28206761"  , 
  "ASC28233766"  , 
  "ASC28236769"  , 
  "ASC29287548"  , 
  "ASC29288550"  , 
  "ASC29566892"  , 
  "ASC29568893"  , 
  "ASC31352818"  , 
  "ASC33102819"  , 
  "ASC34001179"  , 
  "ASC34004177"  , 
  "ASC34006175"  , 
  "ASC34007174"  , 
  "ASC34008178"  , 
  "ASC34014137"  , 
  "ASC34021113"  , 
  "ASC34026169"  , 
  "ASC34029146"  , 
  "ASC34030147"  , 
  "ASC34032148"  , 
  "ASC34036168"  , 
  "ASC34052171"  , 
  "ASC34061172"  , 
  "ASC34076184"  , 
  "ASC34116159"  , 
  "ASC34117158"  , 
  "ASC34155616"  , 
  "ASC34162614"  , 
  "ASC35315561"  , 
  "ASC35316556"  , 
  "ASC35317555"  , 
  "ASC35318559"  , 
  "ASC35354162"  , 
  "ASC35416952"  , 
  "ASC35418953"  , 
  "ASC36572532"  , 
  "ASC40482820"  , 
  "ASC41271111"  , 
  "ASC41279896"  , 
  "ASC41284608"  , 
  "ASC41299880"  , 
  "ASC41316966"  , 
  "ASC41320957"  , 
  "ASC41413922"  , 
  "ASC41840902"  , 
  "ASC41927891"  , 
  "ASC42006282"  , 
  "ASC43022218"  , 
  "ASC43024215"  , 
  "ASC43025219"  , 
  "ASC43026214"  , 
  "ASC43027213"  , 
  "ASC43028216"  , 
  "ASC43031437"  , 
  "ASC43035438"  , 
  "ASC43036432"  , 
  "ASC43037431"  , 
  "ASC43038436"  , 
  "ASC43046475"  , 
  "ASC43048476"  , 
  "ASC43050426"  , 
  "ASC43051427"  , 
  "ASC43066467"  , 
  "ASC43076208"  , 
  "ASC43077207"  , 
  "ASC43116455"  , 
  "ASC43117454"  , 
  "ASC43120477"  , 
  "ASC43121483"  , 
  "ASC43123481"  , 
  "ASC43124480"  , 
  "ASC43125484"  , 
  "ASC43126479"  , 
  "ASC43127478"  , 
  "ASC43128482"  , 
  "ASC43136446"  , 
  "ASC43137443"  , 
  "ASC43138448"  , 
  "ASC43145469"  , 
  "ASC43156402"  , 
  "ASC43197488"  , 
  "ASC43205458"  , 
  "ASC43209415"  , 
  "ASC43984204"  , 
  "ASC43986205"  , 
  "ASC44217554"  , 
  "ASC44258701"  , 
  "ASC47161262"  , 
  "ASC47171261"  , 
  "ASC48661746"  , 
  "ASC48671745"  , 
  "ASC48681749"  , 
  "ASC49361993"  , 
  "ASC50062247"  , 
  "ASC50362273"  , 
  "ASC50372272"  , 
  "ASC52882750"  , 
  "ASC52892748"  , 
  "ASC52902749"  , 
  "ASC53262930"  , 
  "ASC53272929"  , 
  "ASC53462949"  , 
  "ASC53472952"  , 
  "ASC53492951"  , 
  "ASC54056332"  , 
  "ASC54057331"  , 
  "ASC54646335"  , 
  "ASC54647334"  , 
  "ASC54863315"  , 
  "ASC55246362"  , 
  "ASC55963447"  , 
  "ASC56063448"  , 
  "ASC56828417"  , 
  "ASC56840464"  , 
  "ASC56843466"  , 
  "ASC56846465"  , 
  "ASC56850463"  , 
  "ASC56957542"  , 
  "ASC56958543"  , 
  "ASC57051553"  , 
  "ASC57092420"  , 
  "ASC64336401"  , 
  "ASC68022508"  , 
  "ASC68036500"  , 
  "ASC68037498"  , 
  "ASC68038502"  , 
  "ASC68039501"  , 
  "ASC68049509"  , 
  "ASC69971349"  , 
  "ASC69981351"  , 
  "ASC71886256"  , 
  "ASC72816991"  , 
  "ASC79026343"  , 
  "ASC80571355"  , 
  "ASC80574352"  , 
  "ASC80576351"  , 
  "ASC80577350"  , 
  "ASC80578354"  , 
  "ASC81966346"  , 
  "ASC82082628"  , 
  "ASC82126349"  , 
  "ASC82127348"  , 
  "ASC82147356"  , 
  "ASC82187359"  , 
  "ASC88253245"  , 
  "ASC88263244"  , 
  "ASC88273241"  , 
  "ASC88283243"  , 
  "ASC89139984"  , 
  "ASC92783722"  , 
  "ASC94263877"  , 
  "ASC94283878"  , 
  "ASC100064432" , 
  "ASC100084433" , 
  "ASC100094431" , 
  "ASC102785180" , 
  "ASC106251054" , 
  "ASC106261050" , 
  "ASC106271048" , 
  "ASC106281053" , 
  "ASC108051512" , 
  "ASC108071511" , 
  "ASC108081510" , 
  "ASC108181507" , 
  "ASC108361481" , 
  "ASC108902109" , 
  "ASC112901489" , 
  "ASC112931487" , 
  "ASC112941486" , 
  "ASC112961476" , 
  "ASC112971485" , 
  "ASC112981488" , 
  "ASC113464508" , 
  "ASC113474507" , 
  "ASC113484509" , 
  "ASC113661518" , 
  "ASC119361068" , 
  "ASC124041066" , 
  "ASC124061065" , 
  "ASC124071064" , 
  "ASC124081067" , 
  "ASC124641076" , 
  "ASC124661075" , 
  "ASC124671074" , 
  "ASC124761073" , 
  "ASC124781072" , 
  "ASC124791071" , 
  "ASC126681083" , 
  "ASC128564285" , 
  "ASC128884281" , 
  "ASC130361080" , 
  "ASC130371079" , 
  "ASC130381081" , 
  "ASC133883651" , 
  "ASC136383284" , 
  "ASC141923976" , 
  "ASC141993977" , 
  "ASC152465358" , 
  "ASC152475357" , 
  "ASC152485359" , 
  "ASC152965362" , 
  "ASC156262825" , 
  "ASC156272824" , 
  "ASC156282827" , 
  "ASC156442826" , 
  "ASC156774143" , 
  "ASC159405139" , 
  "ASC172061186" , 
  "ASC178261887" , 
  "ASC178301888" , 
  "ASC179161934" , 
  "ASC179561971" , 
  "ASC179581973" , 
  "ASC181961984" , 
  "ASC181981986" , 
  "ASC184072158" , 
  "ASC184082159" , 
  "ASC190262884" , 
  "ASC190612905" , 
  "ASC190632876" , 
  "ASC190642874" , 
  "ASC190662873" , 
  "ASC190672872" , 
  "ASC190682877" , 
  "ASC190822906" , 
  "ASC190962911" , 
  "ASC191122890" , 
  "ASC192263131" , 
  "ASC194763335" , 
  "ASC194773333" , 
  "ASC194783336" , 
  "ASC194863332" , 
  "ASC202463689" , 
  "ASC202473688" , 
  "ASC202483690" , 
  "ASC202863693" , 
  "ASC215954944" , 
  "ASC215964941" , 
  "ASC215974940" , 
  "ASC215984943" , 
  "ASC221215325" , 
  "ASC221555322" , 
  "ASC221565319" , 
  "ASC221575318" , 
  "ASC222135384" , 
  "ASC222165385" , 
  "ASC232771028" , 
  "ASC232781030" , 
  "ASC236262086" , 
  "ASC237082092" , 
  "ASC238232083" , 
  "ASC238242082" , 
  "ASC238262081" , 
  "ASC238272080" , 
  "ASC238282084" , 
  "ASC241562463" , 
  "ASC241572462" , 
  "ASC241582464" , 
  "ASC244543085" , 
  "ASC244563084" , 
  "ASC244573083" , 
  "ASC244583087" , 
  "ASC244663089" , 
  "ASC247063452" , 
  "ASC247283453" , 
  "ASC256034475" , 
  "ASC256044474" , 
  "ASC256064473" , 
  "ASC256074472" , 
  "ASC256084476" , 
  "ASC256164478" , 
  "ASC258764867" , 
  "ASC258774866" , 
  "ASC258784869" , 
  "ASC265862175" , 
  "ASC266562181" , 
  "ASC268202791" , 
  "ASC268212792" , 
  "ASC268222784" , 
  "ASC268232789" , 
  "ASC268242793" , 
  "ASC268262788" , 
  "ASC268272787" , 
  "ASC268282790" , 
  "ASC269062795" , 
  "ASC270271374" , 
  "ASC270562907" , 
  "ASC271024938" , 
  "ASC273922903" , 
  "ASC274102893" , 
  "ASC274541302" , 
  "ASC274791372" , 
  "ASC275444212" , 
  "ASC275544501" , 
  "ASC275624505" , 
  "ASC275852626" , 
  "ASC286252492" , 
  "ASC287184070" , 
  "ASC288664915" , 
  "ASC288674914" , 
  "ASC288684918" , 
  "ASC288904925" , 
  "ASC297281031" , 
  "ASC299932071" , 
  "ASC299942070" , 
  "ASC299962069" , 
  "ASC299972068" , 
  "ASC299982072" , 
  "ASC300362067" , 
  "ASC300562090" , 
  "ASC300572089" , 
  "ASC300582091" , 
  "ASC302362196" , 
  "ASC302382197" , 
  "ASC303162321" , 
  "ASC303172320" , 
  "ASC303182322" , 
  "ASC305262468" , 
  "ASC305272467" , 
  "ASC305282469" , 
  "ASC311673458" , 
  "ASC312083459" , 
  "ASC314723602" , 
  "ASC314733605" , 
  "ASC314743603" , 
  "ASC314753601" , 
  "ASC316543821" , 
  "ASC316563820" , 
  "ASC316573819" , 
  "ASC316583822" , 
  "ASC316663823" , 
  "ASC317183995" , 
  "ASC317193994" , 
  "ASC322314488" , 
  "ASC322324485" , 
  "ASC322334486" , 
  "ASC322344484" , 
  "ASC322354489" , 
  "ASC322364483" , 
  "ASC322374482" , 
  "ASC322384487" , 
  "ASC332564494" , 
  "ASC336634891" , 
  "ASC337163842" , 
  "ASC337173841" , 
  "ASC341641324" , 
  "ASC341971322" , 
  "ASC341981344" , 
  "ASC343592414" , 
  "ASC343662146" , 
  "ASC343682147" , 
  "ASC344222417" , 
  "ASC344232415" , 
  "ASC344252418" , 
  "ASC344332416" , 
  "ASC344493093" , 
  "ASC344563096" , 
  "ASC344573094" , 
  "ASC344583097" , 
  "ASC345463959" , 
  "ASC345663961" , 
  "ASC345784107" , 
  "ASC345794105" , 
  "ASC346904740" , 
  "ASC347464848" , 
  "ASC347474844" , 
  "ASC347484849" , 
  "ASC347864825" , 
  "ASC348174871" , 
  "ASC348204895" , 
  "ASC348214898" , 
  "ASC348234877" , 
  "ASC348564927" , 
  "ASC348714847" , 
  "ASC348814855" , 
  "ASC349195043" , 
  "ASC349355042" , 
  "ASC349725007" , 
  "ASC349955017" , 
  "ASC352134741" , 
  "ASC356264111" , 
  "ASC356284112" , 
  "ASC356993944" , 
  "ASC357804051" , 
  "ASC359132078" , 
  "ASC359142077" , 
  "ASC359162076" , 
  "ASC359172075" , 
  "ASC359182079" , 
  "ASC359262073" , 
  "ASC359442095" , 
  "ASC359462094" , 
  "ASC359472093" , 
  "ASC359482096" , 
  "ASC359645019" , 
  "ASC361862324" , 
  "ASC361882325" , 
  "ASC363012478" , 
  "ASC363042475" , 
  "ASC363052474" , 
  "ASC363072473" , 
  "ASC363082477" , 
  "ASC364472674" , 
  "ASC366673145" , 
  "ASC366683146" , 
  "ASC368711323" , 
  "ASC368863465" , 
  "ASC368873467" , 
  "ASC369013468" , 
  "ASC369033466" , 
  "ASC369043469" , 
  "ASC369073471" , 
  "ASC369083470" , 
  "ASC369683635" , 
  "ASC369693633" , 
  "ASC369713637" , 
  "ASC369743645" , 
  "ASC369763644" , 
  "ASC369783647" , 
  "ASC371463754" , 
  "ASC372173850" , 
  "ASC373365237" , 
  "ASC373385241" , 
  "ASC373395239" , 
  "ASC373464016" , 
  "ASC373474014" , 
  "ASC373484017" , 
  "ASC373564018" , 
  "ASC374134136" , 
  "ASC374144135" , 
  "ASC374164134" , 
  "ASC374174133" , 
  "ASC374184137" , 
  "ASC374264138" , 
  "ASC376404291" , 
  "ASC376424292" , 
  "ASC378864512" , 
  "ASC378884513" , 
  "ASC379344565" , 
  "ASC379354567" , 
  "ASC379364564" , 
  "ASC379374563" , 
  "ASC379384566" , 
  "ASC379864600" , 
  "ASC379874598" , 
  "ASC379884601" , 
  "ASC379914710" , 
  "ASC379934708" , 
  "ASC379944707" , 
  "ASC379964706" , 
  "ASC379974705" , 
  "ASC379984709" , 
  "ASC380064712" , 
  "ASC380084711" , 
  "ASC380884122" , 
  "ASC381765067" , 
  "ASC381775064" , 
  "ASC381785066" , 
  "ASC398664701" , 
  "ASC401181854" , 
  "ASC401933622" , 
  "ASC402973593" , 
  "ASC403133619" , 
  "ASC403945197" , 
  "ASC404115227" , 
  "ASC405923616" , 
  "ASC405933617" , 
  "ASC405943615" , 
  "ASC405983628" , 
  "ASC405993626" , 
  "ASC406044636" , 
  "ASC406144352" , 
  "ASC406255200" , 
  "ASC406325242" , 
  "ASC409264670" , 
  "ASC409304631" , 
  "ASC409464717" , 
  "ASC409564691" , 
  "ASC409654718" , 
  "ASC409764615" , 
  "ASC410762864" , 
  "ASC410772862" , 
  "ASC410782865" , 
  "ASC412092062" , 
  "ASC413605215" , 
  "ASC415002465" , 
  "ASC415924639" , 
  "ASC416015183" , 
  "ASC416045206" , 
  "ASC416255207" , 
  "ASC416535202" , 
  "ASC416815221" , 
  "ASC416885191" , 
  "ASC416925185" , 
  "ASC416935184" , 
  "ASC416945182" , 
  "ASC416965209" , 
  "ASC416985210" , 
  "ASC417015211" , 
  "ASC417045220" , 
  "ASC417055219" , 
  "ASC417073998" , 
  "ASC417154626" , 
  "ASC417194633" , 
  "ASC417464623" , 
  "ASC417474622" , 
  "ASC417835190" , 
  "ASC417964664" , 
  "ASC417974663" , 
  "ASC418064700" , 
  "ASC418461979" , 
  "ASC419895193" , 
  "ASC420025194" , 
  "ASC420095196" , 
  "ASC420795195" , 
  "ASC421663588" , 
  "ASC421673587" , 
  "ASC423123372" , 
  "ASC424444531" , 
  "ASC426663583" , 
  "ASC426763366" , 
  "ASC426873343" , 
  "ASC426993362" , 
  "ASC427033371" , 
  "ASC427123353" , 
  "ASC427163350" , 
  "ASC427173349" , 
  "ASC427183352" , 
  "ASC429845112" , 
  "ASC432191119" , 
  "ASC432441094" , 
  "ASC432821121" , 
  "ASC432851114" , 
  "ASC432871118" , 
  "ASC432961130" , 
  "ASC433021096" , 
  "ASC433161127" , 
  "ASC433281141" , 
  "ASC433301142" , 
  "ASC433311136" , 
  "ASC433331137" , 
  "ASC433341134" , 
  "ASC433351138" , 
  "ASC433361110" , 
  "ASC433371133" , 
  "ASC433381135" , 
  "ASC433861169" , 
  "ASC433871168" , 
  "ASC433881170" , 
  "ASC434091115" , 
  "ASC434101116" , 
  "ASC434151107" , 
  "ASC434181139" , 
  "ASC434261790" , 
  "ASC438484334" , 
  "ASC443501003" , 
  "ASC444191536" , 
  "ASC444561561" , 
  "ASC444761542" , 
  "ASC444991554" , 
  "ASC445161553" , 
  "ASC445201565" , 
  "ASC446032066" , 
  "ASC446573204" , 
  "ASC446583206" , 
  "ASC452453828" , 
  "ASC452463826" , 
  "ASC452473825" , 
  "ASC452483827" , 
  "ASC452753858" , 
  "ASC452763855" , 
  "ASC452773854" , 
  "ASC452783857" , 
  "ASC454184020" , 
  "ASC454194019" , 
  "ASC458844377" , 
  "ASC458864376" , 
  "ASC458874375" , 
  "ASC458884378" , 
  "ASC463502472" , 
  "ASC463512471" , 
  "ASC465043156" , 
  "ASC465063157" , 
  "ASC477851437" , 
  "ASC477861438" , 
  "ASC480992500" , 
  "ASC481022508" , 
  "ASC481032505" , 
  "ASC481042504" , 
  "ASC481052499" , 
  "ASC481062503" , 
  "ASC490864235" , 
  "ASC491854246" , 
  "ASC491864248" , 
  "ASC491894249" , 
  "ASC506864806" , 
  "ASC516964816" , 
  "ASC521461289" , 
  "ASC521861292" , 
  "ASC523121283" , 
  "ASC523141284" , 
  "ASC523161282" , 
  "ASC523171287" , 
  "ASC523181285" , 
  "ASC523191286" , 
  "ASC525631877" , 
  "ASC525801874" , 
  "ASC525811875" , 
  "ASC525821873" , 
  "ASC525931865" , 
  "ASC525941866" , 
  "ASC525951870" , 
  "ASC525961862" , 
  "ASC525971869" , 
  "ASC525981863" , 
  "ASC525991868" , 
  "ASC528063140" , 
  "ASC528353142" , 
  "ASC532103914" , 
  "ASC532113924" , 
  "ASC532133912" , 
  "ASC532143913" , 
  "ASC532163911" , 
  "ASC532183915" , 
  "ASC544363564" , 
  "ASC544463799" , 
  "ASC544473798" , 
  "ASC544483801" , 
  "ASC547061635" , 
  "ASC550663565" , 
  "ASC550683566" , 
  "ASC550843804" , 
  "ASC550863803" , 
  "ASC550873802" , 
  "ASC550883805" , 
  "ASC551063991" , 
  "ASC555063810" , 
  "ASC561953918" , 
  "ASC561963916" , 
  "ASC562063832" , 
  "ASC571201242" , 
  "ASC571251241" , 
  "ASC571291244" , 
  "ASC571441252" , 
  "ASC571481243" , 
  "ASC571571246" , 
  "ASC571581251" , 
  "ASC571611240" , 
  "ASC571741250" , 
  "ASC588011108" , 
  "ASC588352963" , 
  "ASC588362961" , 
  "ASC588382962" , 
  "ASC588402957" , 
  "ASC588422975" , 
  "ASC588602998" , 
  "ASC588682969" , 
  "ASC588722960" , 
  "ASC588732971" , 
  "ASC588752994" , 
  "ASC588762993" , 
  "ASC588852997" , 
  "ASC588862996" , 
  "ASC590371737" , 
  "ASC591001960" , 
  "ASC592312659" , 
  "ASC620613048" , 
  "ASC620973829" , 
  "ASC624003402" , 
  "ASC624053409" , 
  "ASC624203414" , 
  "ASC633554549" , 
  "ASC633574553" , 
  "ASC633594561" , 
  "ASC635284382" , 
  "ASC635304384" , 
  "ASC637554961" , 
  "ASC639595188" , 
  "ASC639635214" , 
  "ASC641361756" , 
  "ASC645731333" , 
  "ASC646011145" , 
  "ASC646761226" , 
  "ASC647111326" , 
  "ASC647121331" , 
  "ASC647161327" , 
  "ASC647361348" , 
  "ASC647551338" , 
  "ASC647591339" , 
  "ASC647761385" , 
  "ASC648582044" , 
  "ASC648622040" , 
  "ASC648632042" , 
  "ASC648642046" , 
  "ASC648652045" , 
  "ASC648662041" , 
  "ASC648672039" , 
  "ASC648682043" , 
  "ASC648702047" , 
  "ASC648761840" , 
  "ASC649962135" , 
  "ASC650662318" , 
  "ASC651262343" , 
  "ASC651272341" , 
  "ASC651952633" , 
  "ASC651994947" , 
  "ASC652004948" , 
  "ASC652482816" , 
  "ASC652512814" , 
  "ASC652532812" , 
  "ASC652542810" , 
  "ASC652562809" , 
  "ASC652572808" , 
  "ASC652582813" , 
  "ASC666364310" , 
  "ASC668364802" , 
  "ASC669362356" , 
  "ASC670063521" , 
  "ASC672665510" , 
  "ASC672675509" , 
  "ASC675263042" , 
  "ASC675273041" , 
  "ASC675283044" , 
  "ASC675563070" , 
  "ASC675573069" , 
  "ASC675863071" , 
  "ASC681261155" , 
  "ASC681271154" , 
  "ASC683061942" , 
  "ASC686113450" , 
  "ASC688464799" , 
  "ASC696111809" , 
  "ASC696341816" , 
  "ASC696361815" , 
  "ASC696371814" , 
  "ASC696381817" , 
  "ASC696561822" , 
  "ASC696571821" , 
  "ASC696581823" , 
  "ASC696661833" , 
  "ASC696671832" , 
  "ASC699404353" , 
  "ASC699454355" , 
  "ASC699464356" , 
  "ASC699474354" , 
  "ASC727221293" , 
  "ASC730504347" , 
  "ASC730514350" , 
  "ASC730524343" , 
  "ASC730534345" , 
  "ASC730544346" , 
  "ASC730554344" , 
  "ASC731361254" , 
  "ASC734561684" , 
  "ASC735171645" , 
  "ASC735261686" , 
  "ASC737021965" , 
  "ASC737151958" , 
  "ASC739861750" , 
  "ASC740862313" , 
  "ASC747822800" , 
  "ASC750873008" , 
  "ASC752083115" , 
  "ASC752103114" , 
  "ASC752123110" , 
  "ASC752143108" , 
  "ASC752163111" , 
  "ASC760323516" , 
  "ASC760363507" , 
  "ASC760383514" , 
  "ASC760403520" , 
  "ASC760423511" , 
  "ASC760443513" , 
  "ASC762273543" , 
  "ASC764363723" , 
  "ASC764783788" , 
  "ASC764863795" , 
  "ASC764873794" , 
  "ASC764883796" , 
  "ASC765263831" , 
  "ASC767673999" , 
  "ASC773184326" , 
  "ASC773754673" , 
  "ASC773784677" , 
  "ASC773954559" , 
  "ASC774724901" , 
  "ASC774724901" , 
  "ASC774875094" , 
  "ASC777355268" , 
  "ASC777404660" , 
  "ASC777745327" , 
  "ASC777765329" , 
  "ASC777865135" , 
  "ASC778424689" , 
  "ASC778474697" , 
  "ASC778524716" , 
  "ASC784563813" , 
  "ASC788663814" , 
  "ASC793363815" , 
  "ASC798163816" , 
  "ASC800961711" , 
  "ASC801452153" , 
  "ASC802362680" , 
  "ASC804404276" , 
  "ASC804564370" , 
  "ASC804574369" , 
  "ASC804584371" , 
  "ASC806861638" , 
  "ASC806871637" , 
  "ASC808262658" , 
  "ASC808272657" , 
  "ASC811963839" , 
  "ASC812043837" , 
  "ASC812063836" , 
  "ASC812073835" , 
  "ASC812083838" , 
  "ASC814065047" , 
  "ASC826061716" , 
  "ASC826071715" , 
  "ASC826101669" , 
  "ASC826111670" , 
  "ASC826181673" , 
  "ASC826261648" , 
  "ASC826271647" , 
  "ASC826361667" , 
  "ASC826371666" , 
  "ASC827861643" , 
  "ASC827871639" , 
  "ASC828201723" , 
  "ASC828261702" , 
  "ASC828271708" , 
  "ASC828401700" , 
  "ASC828461681" , 
  "ASC828471697" , 
  "ASC828491699" , 
  "ASC828661729" , 
  "ASC828671728" , 
  "ASC828811672" , 
  "ASC829572064" , 
  "ASC833632660" , 
  "ASC833702664" , 
  "ASC839063438" , 
  "ASC839073437" , 
  "ASC844563834" , 
  "ASC844573833" , 
  "ASC844863807" , 
  "ASC844963844" , 
  "ASC846174003" , 
  "ASC855261710" , 
  "ASC856063806" , 
  "ASC861121724" , 
  "ASC862503016" , 
  "ASC862513015" , 
  "ASC863103198" , 
  "ASC863903545" , 
  "ASC865063847" , 
  "ASC865073846" , 
  "ASC866375097" , 
  "ASC866692777" , 
  "ASC866702776" , 
  "ASC866722775" , 
  "ASC866922771" , 
  "ASC866932770" , 
  "ASC867964779" , 
  "ASC868074765" , 
  "ASC868814774" , 
  "ASC868834760" , 
  "ASC868844758" , 
  "ASC868854775" , 
  "ASC868864757" , 
  "ASC868874756" , 
  "ASC868884776" , 
  "ASC869164762" , 
  "ASC874082985" , 
  "ASC874092984" , 
  "ASC874851780" , 
  "ASC875861912" , 
  "ASC875961916" , 
  "ASC876431990" , 
  "ASC879363013" , 
  "ASC879373012" , 
  "ASC879383014" , 
  "ASC879973197" , 
  "ASC880363412" , 
  "ASC881473546" , 
  "ASC882943897" , 
  "ASC882963898" , 
  "ASC885564398" , 
  "ASC887665099" , 
  "ASC887675098" , 
  "ASC887685100" , 
  "ASC891211179" , 
  "ASC891541783" , 
  "ASC891751778" , 
  "ASC891912983" , 
  "ASC891932981" , 
  "ASC891942977" , 
  "ASC891962980" , 
  "ASC891972979" , 
  "ASC891982982" , 
  "ASC891992978" , 
  "ASC892153005" , 
  "ASC892421420" , 
  "ASC892781907" , 
  "ASC893863010" , 
  "ASC893873009" , 
  "ASC893883011" , 
  "ASC894473195" , 
  "ASC894963405" , 
  "ASC895331989" , 
  "ASC895773544" , 
  "ASC896174404" , 
  "ASC897864410" , 
  "ASC897874409" , 
  "ASC897884411" , 
  "ASC898362619" , 
  "ASC898372618" , 
  "ASC898875089" , 
  "ASC899165095" , 
  "ASC902836770" , 
  "ASC9046891337", 
  "ASC9368593476", 
  "ASC9369673634", 
  "ASC9465743475", 
  "ASC9885364401")

# For each variable, specify the distribution for its coefficient
# The options are:
# 1. normal
# 2. log-nomal
# 3. negative log-normal
# 4. normal with all values below zero massed at zero
# 5. Johnson SB with a specified min and max
# gDIST must have an entry for each value in gVarNamesNormal

gDIST <- c(rep(1,976))

# STARTING VALUES
svN <- c(rep(0,976))
# ITERATION SETTINGS
gNCREP    <- 10     # Number of iterations to use prior to convergence
gNEREP    <- 10          # Number of iterations to keep for averaging after convergence has been reached
gNSKIP    <- 1			  # Number of iterations to do in between retaining draws for averaging
gINFOSKIP <- 1          # How frequently to print info about the iteration process
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
model <- doHB(likelihood, choice_data, control)








