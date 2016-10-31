setwd("C:/Users/JW/Documents/Iowa papers")
#This code outputs information for each trial for each person into a row (long) or column (wide) format.  
require(ggplot2)
require(stargazer)
require(MASS)
require(KernSmooth)
require(plyr)
require(stats)
require(reshape2)
require(rdd)
##store.info = read.table('C:/Users/JW/Documents/Iowa papers/ABD_Report-2.xls',header=T)
##data=read.csv('C:/Users/JW/Documents/Iowa papers/popsandsales.csv',header=T) #complete data set
data=read.csv('C:/Users/JW/Documents/Iowa papers/rd_data_3.csv',header=T) #complete data setd
data$City <- toupper(data$City)
data$des_moines = 0
des_moines_cities <- c("ADEL",
                       "BODURANT",
                       "CARLISLE",
                       "DALLAS CENTER",
                       "DE SOTO",
                       "EARLHAM",
                       "GRANGER",
                       "GRIMES",
                       "GUTHRIE CENTER",
                       "MICTCHELLVILLE",
                       "NORWALK",
                       "PANORA",
                       "PERRY",
                       "PLEASANT HILL",
                       "POLK CITY",
                       "SAYLORVILLE",
                       "STUART",
                       "VAN METER",
                       "WINDSOR HEIGHTS",
                       "WINTERSET",
                       "WOODWARD",
                       "ACKWORTH",
                       "ADAIR",
                       "ALLEMAN",
                       "BAGLEY",
                       "BAYARD",
                       "BEVINGTON",
                       "BOUTON",
                       "CASEY",
                       "CUMMING",
                       "DAWSON",
                       "DEXTER",
                       "EAST PERU",
                       "ELKHART",
                       "HARTFORD",
                       "JAMAICA",
                       "LACONA",
                       "LINDEN",
                       "MACKSBURG",
                       "MARTENSDALE",
                       "MENLO",
                       "MILO",
                       "MINBURN",
                       "NEW VIRGINIA",
                       "PATTERSON",
                       "REDFIELD",
                       "RUNNELLS",
                       "SANDYVILLE",
                       "SHELDAHL",
                       "SPRING HILL",
                       "ST CHARLES",
                       "ST MARYS",
                       "TRURO",
                       "UNION",
                       "YALE",
                       "BERWICK",
                       "BOONEVILLE")

data$des_moines <- ifelse(data$City %in% des_moines_cities, 1, 0)

##############################################
data2 = subset(data,(Population < 3000))
detach(data2)
attach(data2)
data2$Popthou = Population/1000
data2$logpop = log(Population)
data2$logstore = log(Store.Count)
data2$rd_ind = 1*(Population>=1500) * (Population<10000)
data2$Popthousq = Population/1000*Population/1000
data2$Popthoucu = Population/1000*Population/1000*Population/1000
data2$Popthouint = data2$rd_ind * Population/1000
data2$Popthousqint = data2$rd_ind * Population/1000*Population/1000
data2$Popthoucuint = data2$rd_ind * Population/1000*Population/1000*Population/1000
data2$logpopint = data2$rd_ind * log(Population)
data2$scalepop = (Population - mean(Population))/max(Population)
data2$Poptan = atan((Population - mean(Population))/max(Population))
data2$volume_intensity      = ifelse(Square.Footage > 0,Volume/Square.Footage,0)
data2$variety_intensity   = ifelse(Square.Footage > 0,Item.Variety/Square.Footage,0)
data2$order_intensity     = ifelse(Square.Footage > 0,Order.Count/Square.Footage,0)
data2$operation_intensity = ifelse(Square.Footage > 0,Operation.Time/Square.Footage,0)
data2$average_sqft        = ifelse(Square.Footage > 0,Square.Footage/Store.Count,0)
data2$volume_per_store    = ifelse(Square.Footage > 0,Volume/Store.Count.Address,0)
data2$operation_time_per_store = ifelse(Store.Count.Address > 0, Operation.Time/Store.Count.Address,0)

detach(data2)
attach(data2)

Store.Count <- Store.Count.Address

#####################################
## Initial Parametric Analysis
#####################################

##LM on Number of Stores
rd_stores1 = lm(Store.Count ~ Popthou + rd_ind)
rd_stores2 = lm(Store.Count ~ Popthou + Popthousq + rd_ind)
rd_stores3 = lm(Store.Count ~ Popthou + Popthousq + Popthoucu + rd_ind)
rd_stores4 = lm(Store.Count ~ Popthou + Popthouint + rd_ind)
rd_stores5 = lm(Store.Count ~ Popthou + Popthouint + Popthousq + Popthousqint + rd_ind)
rd_stores6 = lm(Store.Count ~ Popthou + Popthouint + Popthousq + Popthousqint + Popthoucu + Popthoucuint + rd_ind)
rd_stores7 = lm(Store.Count ~ logpop + rd_ind)
rd_stores8 = lm(Store.Count ~ logpop + logpopint + rd_ind)
summary(rd_stores1)
summary(rd_stores2)
summary(rd_stores3)
summary(rd_stores4)
summary(rd_stores5)
summary(rd_stores6)
summary(rd_stores7)
summary(rd_stores8)

##Poisson on Number of Stores
poi1 = glm(Store.Count ~ Popthou + rd_ind, family = "poisson",data = data2)
poi2 = glm(Store.Count ~ Popthou + Popthousq + rd_ind, family = "poisson",data = data2)
poi3 = glm(Store.Count ~ Popthou + Popthousq + Popthoucu + rd_ind, family = "poisson",data = data2)
poi4 = glm(Store.Count ~ Popthou + Popthouint + rd_ind, family = "poisson",data = data2)
poi5 = glm(Store.Count ~ Popthou + Popthouint + Popthousq + Popthousqint + rd_ind, family = "poisson",data = data2)
poi6 = glm(Store.Count ~ Popthou + Popthouint + Popthousq + Popthousqint + Popthoucu + Popthoucuint + rd_ind, family = "poisson",data = data2)
poi7 = glm(Store.Count ~ logpop + rd_ind, family = "poisson",data = data2)
poi8 = glm(Store.Count ~ logpop + logpopint + rd_ind, family = "poisson",data = data2)
summary(poi1)
summary(poi2)
summary(poi3)
summary(poi4)
summary(poi5)
summary(poi6)
summary(poi7)
summary(poi8)

##Negative Binomial on Number of Stores
nb1 = glm.nb(Store.Count ~ Popthou + rd_ind, data = data2)
nb2 = glm.nb(Store.Count ~ Popthou + Popthousq + rd_ind, data = data2)
nb3 = glm.nb(Store.Count ~ Popthou + Popthousq + Popthoucu + rd_ind, data = data2)
nb4 = glm.nb(Store.Count ~ Popthou + Popthouint + rd_ind, data = data2)
nb5 = glm.nb(Store.Count ~ Popthou + Popthouint + Popthousq + Popthousqint + rd_ind, data = data2)
nb6 = glm.nb(Store.Count ~ Popthou + Popthouint + Popthousq + Popthousqint + Popthoucu + Popthoucuint + rd_ind, data = data2)
nb7 = glm.nb(Store.Count ~ logpop + rd_ind, data = data2)
nb8 = glm.nb(Store.Count ~ logpop + logpopint + rd_ind, data = data2)
summary(nb1)
summary(nb2)
summary(nb3)
summary(nb4)
summary(nb5)
summary(nb6)
summary(nb7)
summary(nb8)


##Ordered Probit on Number of Stores
store_factor = factor(Store.Count.Address)
probit1 = polr(store_factor ~ Popthou + rd_ind)
probit2 = polr(store_factor ~ Popthou + Popthousq + rd_ind)
probit3 = polr(store_factor ~ Popthou + Popthousq + Popthoucu + rd_ind)
probit4 = polr(store_factor ~ Popthou + Popthouint + rd_ind)
probit5 = polr(store_factor ~ Popthou + Popthouint + Popthousq + Popthousqint + rd_ind)
probit6 = polr(store_factor ~ Popthou + Popthouint + Popthousq + Popthousqint + Popthoucu + Popthoucuint + rd_ind)
probit7 = polr(store_factor ~ logpop + rd_ind)
probit8 = polr(store_factor ~ logpop + logpopint + rd_ind)
probit9 = polr(store_factor ~ logpop + logpopint + rd_ind)

summary(probit1)
summary(probit2)
summary(probit3)
summary(probit4)
summary(probit5)
summary(probit6)
summary(probit7)
summary(probit8)

## Regressing on logged number of stores, remember to toss the zeroes
# rd_stores1a = lm(logstore ~ Popthou + rd_ind + rd_ind_2)
# rd_stores2a = lm(logstore ~ Popthou + Popthousq + rd_ind + rd_ind_2)
# rd_stores3a = lm(logstore ~ Popthou + Popthousq + Popthoucu + rd_ind + rd_ind_2)
# rd_stores4a = lm(logstore ~ Popthou + Popthouint + Popthouint2 + rd_ind + rd_ind_2)
# rd_stores5a = lm(logstore ~ Popthou + Popthouint + Popthouint2 + Popthousq + Popthousqint + Popthousqint2 + rd_ind + rd_ind_2)
# rd_stores6a = lm(logstore ~ Popthou + Popthouint + Popthouint2 + Popthousq + Popthousqint + Popthousqint2 + Popthoucu + Popthoucuint + Popthoucuint2 + rd_ind + rd_ind_2)
# rd_stores7a = lm(logstore ~ logpop + rd_ind + rd_ind_2)
# rd_stores8a = lm(logstore ~ logpop + logpopint + logpopint2 + rd_ind + rd_ind_2)
# summary(rd_stores1a)
# summary(rd_stores2a)
# summary(rd_stores3a)
# summary(rd_stores4a)
# summary(rd_stores5a)
# summary(rd_stores6a)
# summary(rd_stores7a)
# summary(rd_stores8a)


##Some Plots
Linear =                      predict(poi1, type = "response")
Quadratic =                   predict(poi2, type = "response")
Cubic =                       predict(poi3, type = "response")
Log =                         predict(poi7, type = "response")
Linear_With_Interaction =     predict(poi4, type = "response")
Quadratic_With_Interaction =  predict(poi5, type = "response")
Cubic_With_Interaction =      predict(poi6, type = "response")
Log_With_Interaction =        predict(poi8, type = "response")
graphdata <- data.frame(cbind(Population, Store.Count, 
                              Linear, 
                              Linear_With_Interaction, 
                              Quadratic, 
                              Quadratic_With_Interaction, 
                              Cubic, 
                              Cubic_With_Interaction,
                              Log,
                              Log_With_Interaction))
dd = melt(graphdata, id=c("Population","Store.Count"))
ggplot(dd) + geom_line(aes(x=Population, y=value, colour=variable)) +
  geom_point(aes(x=Population, y=Store.Count)) +
  ggtitle("OLS: Store Count as Dependent Variable") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  scale_color_manual(values = c("red", "red4", "blue", "blue4", "green", "green4", "magenta", "magenta4")) +
  labs(y = "Store Count")



##Some Plots
Linear =                      predict(rd_stores1, type = "response")
Quadratic =                   predict(rd_stores2, type = "response")
Cubic =                       predict(rd_stores3, type = "response")
Linear_With_Interaction =     predict(rd_stores4, type = "response")
Quadratic_With_Interaction =  predict(rd_stores5, type = "response")
Cubic_With_Interaction =      predict(rd_stores6, type = "response")
Log =                         predict(rd_stores7, type = "response")
Log_With_Interaction =        predict(rd_stores8, type = "response")
graphdata <- data.frame(cbind(Population, Store.Count, 
                              Linear, 
                              Linear_With_Interaction, 
                              Quadratic, 
                              Quadratic_With_Interaction, 
                              Cubic, 
                              Cubic_With_Interaction,
                              Log,
                              Log_With_Interaction))
dd = melt(graphdata, id=c("Population","Store.Count"))
ggplot(dd) + geom_line(aes(x=Population, y=value, colour=variable)) +
             geom_point(aes(x=Population, y=Store.Count)) +
             ggtitle("OLS: Store Count as Dependent Variable") +
             theme(plot.title = element_text(lineheight=.8, face="bold")) +
             scale_color_manual(values = c("red", "red4", "blue", "blue4", "green", "green4", "gold", "gold4")) +
             labs(y = "Store Count")


##LM on Volume
rd_volume1 = lm(Volume ~ Popthou + rd_ind)
rd_volume2 = lm(Volume ~ Popthou + Popthousq + rd_ind)
rd_volume3 = lm(Volume ~ Popthou + Popthousq + Popthoucu + rd_ind)
rd_volume4 = lm(Volume ~ Popthou + Popthouint + rd_ind)
rd_volume5 = lm(Volume ~ Popthou + Popthouint + Popthousq + Popthousqint + rd_ind)
rd_volume6 = lm(Volume ~ Popthou + Popthouint + Popthousq + Popthousqint + Popthoucu + Popthoucuint + rd_ind)
rd_volume7 = lm(Volume ~ logpop + rd_ind)
rd_volume8 = lm(Volume ~ logpop + logpopint + rd_ind)
summary(rd_volume1)
summary(rd_volume2)
summary(rd_volume3)
summary(rd_volume4)
summary(rd_volume5)
summary(rd_volume6)
summary(rd_volume7)
summary(rd_volume8)

##Some Plots
Linear =                      predict(rd_volume1, type = "response")
Quadratic =                   predict(rd_volume2, type = "response")
Cubic =                       predict(rd_volume3, type = "response")
Linear_With_Interaction =     predict(rd_volume4, type = "response")
Quadratic_With_Interaction =  predict(rd_volume5, type = "response")
Cubic_With_Interaction =      predict(rd_volume6, type = "response")
Log =                         predict(rd_volume7, type = "response")
Log_With_Interaction =        predict(rd_volume8, type = "response")
graphdata <- data.frame(cbind(Population, Volume, 
                              Linear, 
                              Linear_With_Interaction, 
                              Quadratic, 
                              Quadratic_With_Interaction, 
                              Cubic, 
                              Cubic_With_Interaction,
                              Log,
                              Log_With_Interaction))
dd = melt(graphdata, id=c("Population","Volume"))
ggplot(dd) + geom_line(aes(x=Population, y=value, colour=variable)) +
  geom_point(aes(x=Population, y=Volume)) +
  ggtitle("OLS: Volume as Dependent Variable") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  scale_color_manual(values = c("red", "red4", "blue", "blue4", "green", "green4", "gold", "gold4")) +
  labs(y = "Volume")

##LM on Order Count
rd_Order.Count1 = lm(Order.Count ~ Popthou + rd_ind)
rd_Order.Count2 = lm(Order.Count ~ Popthou + Popthousq + rd_ind)
rd_Order.Count3 = lm(Order.Count ~ Popthou + Popthousq + Popthoucu + rd_ind)
rd_Order.Count4 = lm(Order.Count ~ Popthou + Popthouint + rd_ind)
rd_Order.Count5 = lm(Order.Count ~ Popthou + Popthouint + Popthousq + Popthousqint + rd_ind)
rd_Order.Count6 = lm(Order.Count ~ Popthou + Popthouint + Popthousq + Popthousqint + Popthoucu + Popthoucuint + rd_ind)
rd_Order.Count7 = lm(Order.Count ~ logpop + rd_ind)
rd_Order.Count8 = lm(Order.Count ~ logpop + logpopint + rd_ind)
summary(rd_Order.Count1)
summary(rd_Order.Count2)
summary(rd_Order.Count3)
summary(rd_Order.Count4)
summary(rd_Order.Count5)
summary(rd_Order.Count6)
summary(rd_Order.Count7)
summary(rd_Order.Count8)

##Some Plots
Linear =                      predict(rd_Order.Count1, type = "response")
Quadratic =                   predict(rd_Order.Count2, type = "response")
Cubic =                       predict(rd_Order.Count3, type = "response")
Linear_With_Interaction =     predict(rd_Order.Count4, type = "response")
Quadratic_With_Interaction =  predict(rd_Order.Count5, type = "response")
Cubic_With_Interaction =      predict(rd_Order.Count6, type = "response")
Log =                         predict(rd_Order.Count7, type = "response")
Log_With_Interaction =        predict(rd_Order.Count8, type = "response")
graphdata <- data.frame(cbind(Population, Order.Count, 
                              Linear, 
                              Linear_With_Interaction, 
                              Quadratic, 
                              Quadratic_With_Interaction, 
                              Cubic, 
                              Cubic_With_Interaction,
                              Log,
                              Log_With_Interaction))
dd = melt(graphdata, id=c("Population","Order.Count"))
ggplot(dd) + geom_line(aes(x=Population, y=value, colour=variable)) +
  geom_point(aes(x=Population, y=Order.Count)) +
  ggtitle("OLS: Order Count as Dependent Variable") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  scale_color_manual(values = c("red", "red4", "blue", "blue4", "green", "green4", "gold", "gold4")) +
  labs(y = "Order Count")



##LM on Operation Time
rd_Operation.Time1 = lm(Operation.Time ~ Popthou + rd_ind)
rd_Operation.Time2 = lm(Operation.Time ~ Popthou + Popthousq + rd_ind)
rd_Operation.Time3 = lm(Operation.Time ~ Popthou + Popthousq + Popthoucu + rd_ind)
rd_Operation.Time4 = lm(Operation.Time ~ Popthou + Popthouint + rd_ind)
rd_Operation.Time5 = lm(Operation.Time ~ Popthou + Popthouint + Popthousq + Popthousqint + rd_ind)
rd_Operation.Time6 = lm(Operation.Time ~ Popthou + Popthouint + Popthousq + Popthousqint + Popthoucu + Popthoucuint + rd_ind)
rd_Operation.Time7 = lm(Operation.Time ~ logpop + rd_ind)
rd_Operation.Time8 = lm(Operation.Time ~ logpop + logpopint + rd_ind)
summary(rd_Operation.Time1)
summary(rd_Operation.Time2)
summary(rd_Operation.Time3)
summary(rd_Operation.Time4)
summary(rd_Operation.Time5)
summary(rd_Operation.Time6)
summary(rd_Operation.Time7)
summary(rd_Operation.Time8)


##Some Plots
Linear =                      predict(rd_Operation.Time1, type = "response")
Quadratic =                   predict(rd_Operation.Time2, type = "response")
Cubic =                       predict(rd_Operation.Time3, type = "response")
Linear_With_Interaction =     predict(rd_Operation.Time4, type = "response")
Quadratic_With_Interaction =  predict(rd_Operation.Time5, type = "response")
Cubic_With_Interaction =      predict(rd_Operation.Time6, type = "response")
Log =                         predict(rd_Operation.Time7, type = "response")
Log_With_Interaction =        predict(rd_Operation.Time8, type = "response")
graphdata <- data.frame(cbind(Population, Operation.Time, 
                              Linear, 
                              Linear_With_Interaction, 
                              Quadratic, 
                              Quadratic_With_Interaction, 
                              Cubic, 
                              Cubic_With_Interaction,
                              Log,
                              Log_With_Interaction))
dd = melt(graphdata, id=c("Population","Operation.Time"))
ggplot(dd) + geom_line(aes(x=Population, y=value, colour=variable)) +
  geom_point(aes(x=Population, y=Operation.Time)) +
  ggtitle("OLS: Operation Time as Dependent Variable") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  scale_color_manual(values = c("red", "red4", "blue", "blue4", "green", "green4", "gold", "gold4")) +
  labs(y = "Operation Time")


##LM on Item Variety
rd_Item.Variety1 = lm(Item.Variety ~ Popthou + rd_ind)
rd_Item.Variety2 = lm(Item.Variety ~ Popthou + Popthousq + rd_ind)
rd_Item.Variety3 = lm(Item.Variety ~ Popthou + Popthousq + Popthoucu + rd_ind)
rd_Item.Variety4 = lm(Item.Variety ~ Popthou + Popthouint + rd_ind)
rd_Item.Variety5 = lm(Item.Variety ~ Popthou + Popthouint + Popthousq + Popthousqint + rd_ind)
rd_Item.Variety6 = lm(Item.Variety ~ Popthou + Popthouint + Popthousq + Popthousqint + Popthoucu + Popthoucuint + rd_ind)
rd_Item.Variety7 = lm(Item.Variety ~ logpop + rd_ind)
rd_Item.Variety8 = lm(Item.Variety ~ logpop + logpopint + rd_ind)
summary(rd_Item.Variety1)
summary(rd_Item.Variety2)
summary(rd_Item.Variety3)
summary(rd_Item.Variety4)
summary(rd_Item.Variety5)
summary(rd_Item.Variety6)
summary(rd_Item.Variety7)
summary(rd_Item.Variety8)

##Some Plots
Linear =                      predict(rd_Item.Variety1, type = "response")
Quadratic =                   predict(rd_Item.Variety2, type = "response")
Cubic =                       predict(rd_Item.Variety3, type = "response")
Linear_With_Interaction =     predict(rd_Item.Variety4, type = "response")
Quadratic_With_Interaction =  predict(rd_Item.Variety5, type = "response")
Cubic_With_Interaction =      predict(rd_Item.Variety6, type = "response")
Log =                         predict(rd_Item.Variety7, type = "response")
Log_With_Interaction =        predict(rd_Item.Variety8, type = "response")
graphdata <- data.frame(cbind(Population, Item.Variety, 
                              Linear, 
                              Linear_With_Interaction, 
                              Quadratic, 
                              Quadratic_With_Interaction, 
                              Cubic, 
                              Cubic_With_Interaction,
                              Log,
                              Log_With_Interaction))
dd = melt(graphdata, id=c("Population","Item.Variety"))
ggplot(dd) + geom_line(aes(x=Population, y=value, colour=variable)) +
  geom_point(aes(x=Population, y=Item.Variety)) +
  ggtitle("OLS: Item Variety as Dependent Variable") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  scale_color_manual(values = c("red", "red4", "blue", "blue4", "green", "green4", "gold", "gold4")) +
  labs(y = "Item Variety")


##LM on Average Cost
rd_Average.Cost1 = lm(Average.Cost ~ Popthou + rd_ind, data = subset(data2,Average.Cost > 0))
rd_Average.Cost2 = lm(Average.Cost ~ Popthou + Popthousq + rd_ind, data = subset(data2,Average.Cost > 0))
rd_Average.Cost3 = lm(Average.Cost ~ Popthou + Popthousq + Popthoucu + rd_ind, data = subset(data2,Average.Cost > 0))
rd_Average.Cost4 = lm(Average.Cost ~ Popthou + Popthouint + rd_ind, data = subset(data2,Average.Cost > 0))
rd_Average.Cost5 = lm(Average.Cost ~ Popthou + Popthouint + Popthousq + Popthousqint + rd_ind, data = subset(data2,Average.Cost > 0))
rd_Average.Cost6 = lm(Average.Cost ~ Popthou + Popthouint + Popthousq + Popthousqint + Popthoucu + Popthoucuint + rd_ind, data = subset(data2,Average.Cost > 0))
rd_Average.Cost7 = lm(Average.Cost ~ logpop + rd_ind, data = subset(data2,Average.Cost > 0))
rd_Average.Cost8 = lm(Average.Cost ~ logpop + logpopint + rd_ind, data = subset(data2,Average.Cost > 0))
summary(rd_Average.Cost1)
summary(rd_Average.Cost2)
summary(rd_Average.Cost3)
summary(rd_Average.Cost4)
summary(rd_Average.Cost5)
summary(rd_Average.Cost6)
summary(rd_Average.Cost7)
summary(rd_Average.Cost8)

##Some Plots
Linear =                      predict(rd_Average.Cost1, type = "response")
Quadratic =                   predict(rd_Average.Cost2, type = "response")
Cubic =                       predict(rd_Average.Cost3, type = "response")
Linear_With_Interaction =     predict(rd_Average.Cost4, type = "response")
Quadratic_With_Interaction =  predict(rd_Average.Cost5, type = "response")
Cubic_With_Interaction =      predict(rd_Average.Cost6, type = "response")
Log =                         predict(rd_Average.Cost7, type = "response")
Log_With_Interaction =        predict(rd_Average.Cost8, type = "response")
graphdata <- data.frame(cbind(Population, Average.Cost, 
                              Linear, 
                              Linear_With_Interaction, 
                              Quadratic, 
                              Quadratic_With_Interaction, 
                              Cubic, 
                              Cubic_With_Interaction,
                              Log,
                              Log_With_Interaction))
graphdata <- subset(graphdata, Average.Cost > 0)
dd = melt(graphdata, id=c("Population","Average.Cost"))
ggplot(dd) + geom_line(aes(x=Population, y=value, colour=variable)) +
  geom_point(aes(x=Population, y=Average.Cost)) +
  ggtitle("OLS: Average Cost as Dependent Variable") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  scale_color_manual(values = c("red", "red4", "blue", "blue4", "green", "green4", "gold", "gold4")) +
  labs(y = "Average Cost")


##LM on Square Footage
rd_Square.Footage1 = lm(Square.Footage ~ Popthou + rd_ind)
rd_Square.Footage2 = lm(Square.Footage ~ Popthou + Popthousq + rd_ind)
rd_Square.Footage3 = lm(Square.Footage ~ Popthou + Popthousq + Popthoucu + rd_ind)
rd_Square.Footage4 = lm(Square.Footage ~ Popthou + Popthouint + rd_ind)
rd_Square.Footage5 = lm(Square.Footage ~ Popthou + Popthouint + Popthousq + Popthousqint + rd_ind)
rd_Square.Footage6 = lm(Square.Footage ~ Popthou + Popthouint + Popthousq + Popthousqint + Popthoucu + Popthoucuint + rd_ind)
rd_Square.Footage7 = lm(Square.Footage ~ logpop + rd_ind)
rd_Square.Footage8 = lm(Square.Footage ~ logpop + logpopint + rd_ind)
summary(rd_Square.Footage1)
summary(rd_Square.Footage2)
summary(rd_Square.Footage3)
summary(rd_Square.Footage4)
summary(rd_Square.Footage5)
summary(rd_Square.Footage6)
summary(rd_Square.Footage7)
summary(rd_Square.Footage8)

##Some Plots
Linear =                      predict(rd_Square.Footage1, type = "response")
Quadratic =                   predict(rd_Square.Footage2, type = "response")
Cubic =                       predict(rd_Square.Footage3, type = "response")
Linear_With_Interaction =     predict(rd_Square.Footage4, type = "response")
Quadratic_With_Interaction =  predict(rd_Square.Footage5, type = "response")
Cubic_With_Interaction =      predict(rd_Square.Footage6, type = "response")
Log =                         predict(rd_Square.Footage7, type = "response")
Log_With_Interaction =        predict(rd_Square.Footage8, type = "response")
graphdata <- data.frame(cbind(Population, Square.Footage, 
                              Linear, 
                              Linear_With_Interaction, 
                              Quadratic, 
                              Quadratic_With_Interaction, 
                              Cubic, 
                              Cubic_With_Interaction,
                              Log,
                              Log_With_Interaction))
dd = melt(graphdata, id=c("Population","Square.Footage"))
ggplot(dd) + geom_line(aes(x=Population, y=value, colour=variable)) +
  geom_point(aes(x=Population, y=Square.Footage)) +
  ggtitle("OLS: Square Footage as Dependent Variable") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  scale_color_manual(values = c("red", "red4", "blue", "blue4", "green", "green4", "gold", "gold4")) +
  labs(y = "Square Footage")


##Checks on smooth non-change through threshold
summary(lm(Income ~ Popthou + Popthousq + Popthoucu + rd_ind))


#####################################
## Check to see if varying
## the cutoff matters
#####################################
store_factor = factor(Store.Count.Address)
rd_ind_check1 = 1*(Population>=1400)
rd_ind_check2 = 1*(Population>=1300)
rd_ind_check3 = 1*(Population>=1200)
rd_ind_check4 = 1*(Population>=1100)
rd_ind_check5 = 1*(Population>=1600)
rd_ind_check6 = 1*(Population>=1700)
rd_ind_check7 = 1*(Population>=1800)
rd_ind_check8 = 1*(Population>=1900)

check1int1 = logpop * rd_ind_check1
check2int1 = logpop * rd_ind_check2
check3int1 = logpop * rd_ind_check3
check4int1 = logpop * rd_ind_check4
check5int1 = logpop * rd_ind_check5
check6int1 = logpop * rd_ind_check6
check7int1 = logpop * rd_ind_check7
check8int1 = logpop * rd_ind_check8

store_count_check_1 = polr(store_factor ~ logpop + check1int1 + rd_ind_check1)
store_count_check_2 = polr(store_factor ~ logpop + check2int1 + rd_ind_check2)
store_count_check_3 = polr(store_factor ~ logpop + check3int1 + rd_ind_check3)
store_count_check_4 = polr(store_factor ~ logpop + check4int1 + rd_ind_check4)
store_count_check_5 = polr(store_factor ~ logpop + check5int1 + rd_ind_check5)
store_count_check_6 = polr(store_factor ~ logpop + check6int1 + rd_ind_check6)
store_count_check_7 = polr(store_factor ~ logpop + check7int1 + rd_ind_check7)
store_count_check_8 = polr(store_factor ~ logpop + check8int1 + rd_ind_check8)

volume_check_1 = lm(Volume ~ logpop + check1int1 + rd_ind_check1)
volume_check_2 = lm(Volume ~ logpop + check2int1 + rd_ind_check2)
volume_check_3 = lm(Volume ~ logpop + check3int1 + rd_ind_check3)
volume_check_4 = lm(Volume ~ logpop + check4int1 + rd_ind_check4)
volume_check_5 = lm(Volume ~ logpop + check5int1 + rd_ind_check5)
volume_check_6 = lm(Volume ~ logpop + check6int1 + rd_ind_check6)
volume_check_7 = lm(Volume ~ logpop + check7int1 + rd_ind_check7)
volume_check_8 = lm(Volume ~ logpop + check8int1 + rd_ind_check8)

square_footage_check_1 = lm(Square.Footage ~ logpop + check1int1 + rd_ind_check1)
square_footage_check_2 = lm(Square.Footage ~ logpop + check2int1 + rd_ind_check2)
square_footage_check_3 = lm(Square.Footage ~ logpop + check3int1 + rd_ind_check3)
square_footage_check_4 = lm(Square.Footage ~ logpop + check4int1 + rd_ind_check4)
square_footage_check_5 = lm(Square.Footage ~ logpop + check5int1 + rd_ind_check5)
square_footage_check_6 = lm(Square.Footage ~ logpop + check6int1 + rd_ind_check6)
square_footage_check_7 = lm(Square.Footage ~ logpop + check7int1 + rd_ind_check7)
square_footage_check_8 = lm(Square.Footage ~ logpop + check8int1 + rd_ind_check8)

average_cost_check_1 = lm(Average.Cost ~ logpop + check1int1 + rd_ind_check1)
average_cost_check_2 = lm(Average.Cost ~ logpop + check2int1 + rd_ind_check2)
average_cost_check_3 = lm(Average.Cost ~ logpop + check3int1 + rd_ind_check3)
average_cost_check_4 = lm(Average.Cost ~ logpop + check4int1 + rd_ind_check4)
average_cost_check_5 = lm(Average.Cost ~ logpop + check5int1 + rd_ind_check5)
average_cost_check_6 = lm(Average.Cost ~ logpop + check6int1 + rd_ind_check6)
average_cost_check_7 = lm(Average.Cost ~ logpop + check7int1 + rd_ind_check7)
average_cost_check_8 = lm(Average.Cost ~ logpop + check8int1 + rd_ind_check8)

item_variety_check_1 = lm(Item.Variety ~ logpop + check1int1 + rd_ind_check1)
item_variety_check_2 = lm(Item.Variety ~ logpop + check2int1 + rd_ind_check2)
item_variety_check_3 = lm(Item.Variety ~ logpop + check3int1 + rd_ind_check3)
item_variety_check_4 = lm(Item.Variety ~ logpop + check4int1 + rd_ind_check4)
item_variety_check_5 = lm(Item.Variety ~ logpop + check5int1 + rd_ind_check5)
item_variety_check_6 = lm(Item.Variety ~ logpop + check6int1 + rd_ind_check6)
item_variety_check_7 = lm(Item.Variety ~ logpop + check7int1 + rd_ind_check7)
item_variety_check_8 = lm(Item.Variety ~ logpop + check8int1 + rd_ind_check8)

operation_time_check_1 = lm(Operation.Time ~ logpop + check1int1 + rd_ind_check1)
operation_time_check_2 = lm(Operation.Time ~ logpop + check2int1 + rd_ind_check2)
operation_time_check_3 = lm(Operation.Time ~ logpop + check3int1 + rd_ind_check3)
operation_time_check_4 = lm(Operation.Time ~ logpop + check4int1 + rd_ind_check4)
operation_time_check_5 = lm(Operation.Time ~ logpop + check5int1 + rd_ind_check5)
operation_time_check_6 = lm(Operation.Time ~ logpop + check6int1 + rd_ind_check6)
operation_time_check_7 = lm(Operation.Time ~ logpop + check7int1 + rd_ind_check7)
operation_time_check_8 = lm(Operation.Time ~ logpop + check8int1 + rd_ind_check8)

order_count_check_1 = lm(Order.Count ~ logpop + check1int1 + rd_ind_check1)
order_count_check_2 = lm(Order.Count ~ logpop + check2int1 + rd_ind_check2)
order_count_check_3 = lm(Order.Count ~ logpop + check3int1 + rd_ind_check3)
order_count_check_4 = lm(Order.Count ~ logpop + check4int1 + rd_ind_check4)
order_count_check_5 = lm(Order.Count ~ logpop + check5int1 + rd_ind_check5)
order_count_check_6 = lm(Order.Count ~ logpop + check6int1 + rd_ind_check6)
order_count_check_7 = lm(Order.Count ~ logpop + check7int1 + rd_ind_check7)
order_count_check_8 = lm(Order.Count ~ logpop + check8int1 + rd_ind_check8)

volume_intensity_check_1 = lm(volume_intensity ~ logpop + check1int1 + rd_ind_check1)
volume_intensity_check_2 = lm(volume_intensity ~ logpop + check2int1 + rd_ind_check2)
volume_intensity_check_3 = lm(volume_intensity ~ logpop + check3int1 + rd_ind_check3)
volume_intensity_check_4 = lm(volume_intensity ~ logpop + check4int1 + rd_ind_check4)
volume_intensity_check_5 = lm(volume_intensity ~ logpop + check5int1 + rd_ind_check5)
volume_intensity_check_6 = lm(volume_intensity ~ logpop + check6int1 + rd_ind_check6)
volume_intensity_check_7 = lm(volume_intensity ~ logpop + check7int1 + rd_ind_check7)
volume_intensity_check_8 = lm(volume_intensity ~ logpop + check8int1 + rd_ind_check8)

variety_intensity_check_1 = lm(variety_intensity ~ logpop + check1int1 + rd_ind_check1)
variety_intensity_check_2 = lm(variety_intensity ~ logpop + check2int1 + rd_ind_check2)
variety_intensity_check_3 = lm(variety_intensity ~ logpop + check3int1 + rd_ind_check3)
variety_intensity_check_4 = lm(variety_intensity ~ logpop + check4int1 + rd_ind_check4)
variety_intensity_check_5 = lm(variety_intensity ~ logpop + check5int1 + rd_ind_check5)
variety_intensity_check_6 = lm(variety_intensity ~ logpop + check6int1 + rd_ind_check6)
variety_intensity_check_7 = lm(variety_intensity ~ logpop + check7int1 + rd_ind_check7)
variety_intensity_check_8 = lm(variety_intensity ~ logpop + check8int1 + rd_ind_check8)

volume_per_store_check_1 = lm(volume_per_store ~ logpop + check1int1 + rd_ind_check1)
volume_per_store_check_2 = lm(volume_per_store ~ logpop + check2int1 + rd_ind_check2)
volume_per_store_check_3 = lm(volume_per_store ~ logpop + check3int1 + rd_ind_check3)
volume_per_store_check_4 = lm(volume_per_store ~ logpop + check4int1 + rd_ind_check4)
volume_per_store_check_5 = lm(volume_per_store ~ logpop + check5int1 + rd_ind_check5)
volume_per_store_check_6 = lm(volume_per_store ~ logpop + check6int1 + rd_ind_check6)
volume_per_store_check_7 = lm(volume_per_store ~ logpop + check7int1 + rd_ind_check7)
volume_per_store_check_8 = lm(volume_per_store ~ logpop + check8int1 + rd_ind_check8)

operation_time_per_store_check_1 = lm(operation_time_per_store ~ logpop + check1int1 + rd_ind_check1)
operation_time_per_store_check_2 = lm(operation_time_per_store ~ logpop + check2int1 + rd_ind_check2)
operation_time_per_store_check_3 = lm(operation_time_per_store ~ logpop + check3int1 + rd_ind_check3)
operation_time_per_store_check_4 = lm(operation_time_per_store ~ logpop + check4int1 + rd_ind_check4)
operation_time_per_store_check_5 = lm(operation_time_per_store ~ logpop + check5int1 + rd_ind_check5)
operation_time_per_store_check_6 = lm(operation_time_per_store ~ logpop + check6int1 + rd_ind_check6)
operation_time_per_store_check_7 = lm(operation_time_per_store ~ logpop + check7int1 + rd_ind_check7)
operation_time_per_store_check_8 = lm(operation_time_per_store ~ logpop + check8int1 + rd_ind_check8)

summary(store_count_check_1)
summary(store_count_check_2)
summary(store_count_check_3)
summary(store_count_check_4)
summary(store_count_check_5)
summary(store_count_check_6)
summary(store_count_check_7)
summary(store_count_check_8)

summary(volume_check_1)
summary(volume_check_2)
summary(volume_check_3)
summary(volume_check_4)
summary(volume_check_5)
summary(volume_check_6)
summary(volume_check_7)
summary(volume_check_8)

summary(operation_time_check_1)
summary(operation_time_check_2)
summary(operation_time_check_3)
summary(operation_time_check_4)
summary(operation_time_check_5)
summary(operation_time_check_6)
summary(operation_time_check_7)
summary(operation_time_check_8)

summary(average_cost_check_1)
summary(average_cost_check_2)
summary(average_cost_check_3)
summary(average_cost_check_4)
summary(average_cost_check_5)
summary(average_cost_check_6)
summary(average_cost_check_7)
summary(average_cost_check_8)

summary(item_variety_check_1)
summary(item_variety_check_2)
summary(item_variety_check_3)
summary(item_variety_check_4)
summary(item_variety_check_5)
summary(item_variety_check_6)
summary(item_variety_check_7)
summary(item_variety_check_8)

summary(square_footage_check_1)
summary(square_footage_check_2)
summary(square_footage_check_3)
summary(square_footage_check_4)
summary(square_footage_check_5)
summary(square_footage_check_6)
summary(square_footage_check_7)
summary(square_footage_check_8)

summary(order_count_check_1)
summary(order_count_check_2)
summary(order_count_check_3)
summary(order_count_check_4)
summary(order_count_check_5)
summary(order_count_check_6)
summary(order_count_check_7)
summary(order_count_check_8)

summary(volume_intensity_check_1)
summary(volume_intensity_check_2)
summary(volume_intensity_check_3)
summary(volume_intensity_check_4)
summary(volume_intensity_check_5)
summary(volume_intensity_check_6)
summary(volume_intensity_check_7)
summary(volume_intensity_check_8)

summary(variety_intensity_check_1)
summary(variety_intensity_check_2)
summary(variety_intensity_check_3)
summary(variety_intensity_check_4)
summary(variety_intensity_check_5)
summary(variety_intensity_check_6)
summary(variety_intensity_check_7)
summary(variety_intensity_check_8)

summary(volume_per_store_check_1)
summary(volume_per_store_check_2)
summary(volume_per_store_check_3)
summary(volume_per_store_check_4)
summary(volume_per_store_check_5)
summary(volume_per_store_check_6)
summary(volume_per_store_check_7)
summary(volume_per_store_check_8)

summary(operation_time_per_store_check_1)
summary(operation_time_per_store_check_2)
summary(operation_time_per_store_check_3)
summary(operation_time_per_store_check_4)
summary(operation_time_per_store_check_5)
summary(operation_time_per_store_check_6)
summary(operation_time_per_store_check_7)
summary(operation_time_per_store_check_8)


#####################################
## Local Polynomials with 
## Normal Kernel Smoothing
#####################################

## Local Polynomial and Kernel parameters with bandwidth and degree of local polynomial
band = 350
deg = 2

## Cutoff for estimating the threshold
## Note that at 1506 there is an apparent outlier on volume, results are quite 
## sensitive to its inclusion/exclusion
threshlow = 1499
threshhigh = 1500
threshmax = 3000
threshmin = 0

## Volume
banda <-  dpill(Population[Population < 1500],  Volume[Population < 1500])
band2a <- dpill(Population[Population >= 1500], Volume[Population >= 1500])
rega = data.frame(locpoly(x = Population, y = Volume, degree = deg, kernel = "normal", range.x = c(threshmin,threshlow), bandwidth = banda))
#ggplot(rega, aes(y = rega$y, x = rega$x)) + geom_point()
reg2a = data.frame(locpoly(x = Population, y = Volume, degree = deg, kernel = "normal", range.x = c(threshhigh,threshmax), bandwidth = band2a))
#ggplot(reg2a, aes(y = reg2a$y, x = reg2a$x)) + geom_point()

## Order Count
bandb <-  dpill(Population[Population < 1500],  Order.Count[Population < 1500])
band2b <- dpill(Population[Population >= 1500], Order.Count[Population >= 1500])
regb = data.frame(locpoly(x = Population, y = Order.Count, degree = deg, kernel = "normal", range.x = c(threshmin,threshlow), bandwidth = bandb))
#ggplot(reg, aes(y = reg$y, x = reg$x)) + geom_point()
reg2b = data.frame(locpoly(x = Population, y = Order.Count, degree = deg, kernel = "normal", range.x = c(threshhigh,threshmax), bandwidth = band2b))
#ggplot(reg2, aes(y = reg2$y, x = reg2$x)) + geom_point()

## Square Footage
bandc <-  dpill(Population[Population < 1500],  Square.Footage[Population < 1500])
band2c <- dpill(Population[Population >= 1500], Square.Footage[Population >= 1500])
regc = data.frame(locpoly(x = Population, y = Square.Footage, degree = deg, kernel = "normal", range.x = c(threshmin,threshlow), bandwidth = bandc))
#ggplot(reg, aes(y = reg$y, x = reg$x)) + geom_point()
reg2c = data.frame(locpoly(x = Population, y = Square.Footage, degree = deg, kernel = "normal", range.x = c(threshhigh,threshmax), bandwidth = band2c))
#ggplot(reg2, aes(y = reg2$y, x = reg2$x)) + geom_point()

## Operation Days
bandd <-  dpill(Population[Population < 1500],  Operation.Time[Population < 1500])
band2d <- dpill(Population[Population >= 1500], Operation.Time[Population >= 1500])
regd = data.frame(locpoly(x = Population, y = Operation.Time, degree = deg, kernel = "normal", range.x = c(threshmin,threshlow), bandwidth = bandd))
#ggplot(reg, aes(y = reg$y, x = reg$x)) + geom_point()
reg2d = data.frame(locpoly(x = Population, y = Operation.Time, degree = deg, kernel = "normal", range.x = c(threshhigh,threshmax), bandwidth = band2d))
#ggplot(reg2, aes(y = reg2$y, x = reg2$x)) + geom_point()

## Item Variety
bande <-  dpill(Population[Population < 1500],  Item.Variety[Population < 1500])
band2e <- dpill(Population[Population >= 1500], Item.Variety[Population >= 1500])
rege = data.frame(locpoly(x = Population, y = Item.Variety, degree = deg, kernel = "normal", range.x = c(threshmin,threshlow), bandwidth = bande))
#ggplot(reg, aes(y = reg$y, x = reg$x)) + geom_point()
reg2e = data.frame(locpoly(x = Population, y = Item.Variety, degree = deg, kernel = "normal", range.x = c(threshhigh,threshmax), bandwidth = band2e))
#ggplot(reg2, aes(y = reg2$y, x = reg2$x)) + geom_point()

## Avg. Item Cost
bandf <-  dpill(Population[Population < 1500],  Average.Cost[Population < 1500])
band2f <- dpill(Population[Population >= 1500], Average.Cost[Population >= 1500])
regf = data.frame(locpoly(x = Population, y = Average.Cost, degree = deg, kernel = "normal", range.x = c(threshmin,threshlow), bandwidth = bandf))
#ggplot(reg, aes(y = reg$y, x = reg$x)) + geom_point()
reg2f = data.frame(locpoly(x = Population, y = Average.Cost, degree = deg, kernel = "normal", range.x = c(threshhigh,threshmax), bandwidth = band2f))
#ggplot(reg2, aes(y = reg2$y, x = reg2$x)) + geom_point()

## Store Count
band <-  dpill(Population[Population < 1500],  Store.Count.Address[Population < 1500])
band2 <- dpill(Population[Population >= 1500], Store.Count.Address[Population >= 1500])
reg = data.frame(locpoly(x = Population, y = Store.Count.Address, degree = deg, kernel = "normal", range.x = c(threshmin,threshlow), bandwidth = band))
#ggplot(reg, aes(y = reg$y, x = reg$x)) + geom_point()
reg2 = data.frame(locpoly(x = Population, y = Store.Count.Address, degree = deg, kernel = "normal", range.x = c(threshhigh,threshmax), bandwidth = band2))
#ggplot(reg2, aes(y = reg2$y, x = reg2$x)) + geom_point()

## Volume Intensity
bandg <-  dpill(Population[Population < 1500],  volume_intensity[Population < 1500])
band2g <- dpill(Population[Population >= 1500], volume_intensity[Population >= 1500])
reg = data.frame(locpoly(x = Population, y = volume_intensity, degree = deg, kernel = "normal", range.x = c(threshmin,threshlow), bandwidth = bandg))
#ggplot(reg, aes(y = reg$y, x = reg$x)) + geom_point()
reg2 = data.frame(locpoly(x = Population, y = volume_intensity, degree = deg, kernel = "normal", range.x = c(threshhigh,threshmax), bandwidth = band2g))
#ggplot(reg2, aes(y = reg2$y, x = reg2$x)) + geom_point()

## Variety Intensity
bandh <-  dpill(Population[Population < 1500],  variety_intensity[Population < 1500])
band2h <- dpill(Population[Population >= 1500], variety_intensity[Population >= 1500])
reg = data.frame(locpoly(x = Population, y = volume_intensity, degree = deg, kernel = "normal", range.x = c(threshmin,threshlow), bandwidth = bandh))
#ggplot(reg, aes(y = reg$y, x = reg$x)) + geom_point()
reg2 = data.frame(locpoly(x = Population, y = volume_intensity, degree = deg, kernel = "normal", range.x = c(threshhigh,threshmax), bandwidth = band2h))
#ggplot(reg2, aes(y = reg2$y, x = reg2$x)) + geom_point()

## Volume Per Store
bandi <-  dpill(Population[Population < 1500],  volume_per_store[Population < 1500])
band2i <- dpill(Population[Population >= 1500], volume_per_store[Population >= 1500])
reg = data.frame(locpoly(x = Population, y = volume_per_store, degree = deg, kernel = "normal", range.x = c(threshmin,threshlow), bandwidth = bandi))
#ggplot(reg, aes(y = reg$y, x = reg$x)) + geom_point()
reg2 = data.frame(locpoly(x = Population, y = volume_per_store, degree = deg, kernel = "normal", range.x = c(threshhigh,threshmax), bandwidth = band2i))
#ggplot(reg2, aes(y = reg2$y, x = reg2$x)) + geom_point()

#######################################
## Bootstrapping the kernel estimations
#######################################

## Store Count
iterations = 1000
bootdata = matrix(NA, nrow = length(data2$Store.Count.Address), ncol = iterations * 2)
regboot = matrix(NA,nrow = length(reg$x), ncol = iterations + 1)
regboot[,1] = reg$x
regboot2 = matrix(NA,nrow = length(reg2$x), ncol = iterations + 1)
regboot2[,1] = reg2$x
for(i in 1:iterations){
  for(j in 1:length(data2$Store.Count.Address)){
  sampler = sample(1:length(data2$Store.Count.Address),length(data2$Store.Count.Address), replace = TRUE)
  bootdata[j,i] = data2$Store.Count.Address[sampler[j]]
  bootdata[j,i + iterations] = data2$Population[sampler[j]]
  }
  regboot[,i+1] = data.frame(locpoly(x = bootdata[,i + iterations], y = bootdata[,i], degree = deg, kernel = "normal", range.x = c(threshmin,threshlow),bandwidth = band))$y
  regboot2[,i+1] = data.frame(locpoly(x = bootdata[,i + iterations], y = bootdata[,i], degree = deg, kernel = "normal", range.x = c(threshhigh,threshmax),bandwidth = band2))$y
}
regbootdata = data.frame(t(regboot))
colnames(regbootdata) = regbootdata[1,]
regbootdata = regbootdata[-1,]

regbootdata2 = data.frame(t(regboot2))
colnames(regbootdata2) = regbootdata2[1,]
regbootdata2 = regbootdata2[-1,]


bootdata = cbind(regbootdata, regbootdata2)
bootdataquant = data.frame(t(apply(bootdata,2,quantile,probs = c(.025, .05, .5, .95, .975))))
bootgraph = ggplot(data = bootdataquant, aes(x = as.numeric(row.names(bootdataquant)), y = bootdataquant$X2.5.)) + geom_point() + xlab("Population") + ylab("Store Count by Address") + ggtitle("Store Count Kernel Estimation and 95% CI")
bootgraph = bootgraph + geom_point(data = bootdataquant, aes(x = as.numeric(row.names(bootdataquant)), y = bootdataquant$X97.5.))
bootgraph = bootgraph + geom_point(data = bootdataquant, aes(x = as.numeric(row.names(bootdataquant)), y = bootdataquant$X50.), color = "red")
bootgraph = bootgraph + geom_vline(xintercept = 1500) + ylim(0,max(Store.Count.Address))
bootgraph

## Volume
iterations = 1000
bootvoldata = matrix(NA, nrow = length(data2$Volume), ncol = iterations * 2)
regbootvol = matrix(NA,nrow = length(rega$x), ncol = iterations + 1)
regbootvol[,1] = rega$x
regbootvol2 = matrix(NA,nrow = length(reg2a$x), ncol = iterations + 1)
regbootvol2[,1] = reg2a$x
for(i in 1:iterations){
  for(j in 1:length(data2$Volume)){
    sampler = sample(1:length(data2$Volume),length(data2$Volume), replace = TRUE)
    bootvoldata[j,i] = data2$Volume[sampler[j]]
    bootvoldata[j,i + iterations] = data2$Population[sampler[j]]
  }
  regbootvol[,i+1] = data.frame(locpoly(x = bootvoldata[,i + iterations], y = bootvoldata[,i], degree = deg, kernel = "normal", range.x = c(threshmin,threshlow),bandwidth = banda))$y
  regbootvol2[,i+1] = data.frame(locpoly(x = bootvoldata[,i + iterations], y = bootvoldata[,i], degree = deg, kernel = "normal", range.x = c(threshhigh,threshmax),bandwidth = band2a))$y
}
regbootvoldata = data.frame(t(regbootvol))
colnames(regbootvoldata) = regbootvoldata[1,]
regbootvoldata = regbootvoldata[-1,]

regbootvoldata2 = data.frame(t(regbootvol2))
colnames(regbootvoldata2) = regbootvoldata2[1,]
regbootvoldata2 = regbootvoldata2[-1,]


bootvoldata = cbind(regbootvoldata, regbootvoldata2)
bootvoldataquant = data.frame(t(apply(bootvoldata,2,quantile,probs = c(.025, .05, .5, .95, .975))))
bootgraphvol = ggplot(data = bootvoldataquant, aes(x = as.numeric(row.names(bootvoldataquant)), y = bootvoldataquant$X2.5.)) + geom_point() + xlab("Population") + ylab("Volume") + ggtitle("Volume Kernel Estimation and 95% CI")
bootgraphvol = bootgraphvol + geom_point(data = bootvoldataquant, aes(x = as.numeric(row.names(bootvoldataquant)), y = bootvoldataquant$X97.5.))
bootgraphvol = bootgraphvol + geom_point(data = bootvoldataquant, aes(x = as.numeric(row.names(bootvoldataquant)), y = bootvoldataquant$X50.), color = "red")
bootgraphvol = bootgraphvol + geom_vline(xintercept = 1500) + ylim(0,max(Volume))
bootgraphvol

## Order Count
iterations = 1000
bootordata = matrix(NA, nrow = length(data2$Order.Count), ncol = iterations * 2)
regbootor = matrix(NA,nrow = length(regb$x), ncol = iterations + 1)
regbootor[,1] = regb$x
regbootor2 = matrix(NA,nrow = length(reg2b$x), ncol = iterations + 1)
regbootor2[,1] = reg2b$x
for(i in 1:iterations){
  for(j in 1:length(data2$Order.Count)){
    sampler = sample(1:length(data2$Order.Count),length(data2$Order.Count), replace = TRUE)
    bootordata[j,i] = data2$Order.Count[sampler[j]]
    bootordata[j,i + iterations] = data2$Population[sampler[j]]
  }
  regbootor[,i+1] = data.frame(locpoly(x = bootordata[,i + iterations], y = bootordata[,i], degree = deg, kernel = "normal", range.x = c(threshmin,threshlow),bandwidth = bandb))$y
  regbootor2[,i+1] = data.frame(locpoly(x = bootordata[,i + iterations], y = bootordata[,i], degree = deg, kernel = "normal", range.x = c(threshhigh,threshmax),bandwidth = band2b))$y
}
regbootordata = data.frame(t(regbootor))
colnames(regbootordata) = regbootordata[1,]
regbootordata = regbootordata[-1,]

regbootordata2 = data.frame(t(regbootor2))
colnames(regbootordata2) = regbootordata2[1,]
regbootordata2 = regbootordata2[-1,]


bootordata = cbind(regbootordata, regbootordata2)
bootordataquant = data.frame(t(apply(bootordata,2,quantile,probs = c(.025, .05, .5, .95, .975))))
bootgraphor = ggplot(data = bootordataquant, aes(x = as.numeric(row.names(bootordataquant)), y = bootordataquant$X2.5.)) + geom_point() + xlab("Population") + ylab("Order Count") + ggtitle("Order Count Kernel Estimation and 95% CI")
bootgraphor = bootgraphor + geom_point(data = bootordataquant, aes(x = as.numeric(row.names(bootordataquant)), y = bootordataquant$X97.5.))
bootgraphor = bootgraphor + geom_point(data = bootordataquant, aes(x = as.numeric(row.names(bootordataquant)), y = bootordataquant$X50.), color = "red")
bootgraphor = bootgraphor + geom_vline(xintercept = 1500) + ylim(0,max(Order.Count))
bootgraphor

## Square Footage
iterations = 1000
bootsfdata = matrix(NA, nrow = length(data2$Square.Footage), ncol = iterations * 2)
regbootsf = matrix(NA,nrow = length(regb$x), ncol = iterations + 1)
regbootsf[,1] = regc$x
regbootsf2 = matrix(NA,nrow = length(reg2b$x), ncol = iterations + 1)
regbootsf2[,1] = reg2c$x
for(i in 1:iterations){
  for(j in 1:length(data2$Square.Footage)){
    sampler = sample(1:length(data2$Square.Footage),length(data2$Square.Footage), replace = TRUE)
    bootsfdata[j,i] = data2$Square.Footage[sampler[j]]
    bootsfdata[j,i + iterations] = data2$Population[sampler[j]]
  }
  regbootsf[,i+1] = data.frame(locpoly(x = bootsfdata[,i + iterations], y = bootsfdata[,i], degree = deg, kernel = "normal", range.x = c(threshmin,threshlow),bandwidth = bandc))$y
  regbootsf2[,i+1] = data.frame(locpoly(x = bootsfdata[,i + iterations], y = bootsfdata[,i], degree = deg, kernel = "normal", range.x = c(threshhigh,threshmax),bandwidth = band2c))$y
}
regbootsfdata = data.frame(t(regbootsf))
colnames(regbootsfdata) = regbootsfdata[1,]
regbootsfdata = regbootsfdata[-1,]

regbootsfdata2 = data.frame(t(regbootsf2))
colnames(regbootsfdata2) = regbootsfdata2[1,]
regbootsfdata2 = regbootsfdata2[-1,]


bootsfdata = cbind(regbootsfdata, regbootsfdata2)
bootsfdataquant = data.frame(t(apply(bootsfdata,2,quantile,probs = c(.025, .05, .5, .95, .975))))
bootgraphsf = ggplot(data = bootsfdataquant, aes(x = as.numeric(row.names(bootsfdataquant)), y = bootsfdataquant$X2.5.)) + geom_point() + xlab("Population") + ylab("Square Footage") + ggtitle("Square Footage Kernel Estimation and 95% CI")
bootgraphsf = bootgraphsf + geom_point(data = bootsfdataquant, aes(x = as.numeric(row.names(bootsfdataquant)), y = bootsfdataquant$X97.5.))
bootgraphsf = bootgraphsf + geom_point(data = bootsfdataquant, aes(x = as.numeric(row.names(bootsfdataquant)), y = bootsfdataquant$X50.), color = "red")
bootgraphsf = bootgraphsf + geom_vline(xintercept = 1500) + ylim(0,max(Square.Footage))
bootgraphsf

## Operation Days
iterations = 1000
bootoddata = matrix(NA, nrow = length(data2$Operation.Time), ncol = iterations * 2)
regbootod = matrix(NA,nrow = length(regd$x), ncol = iterations + 1)
regbootod[,1] = regd$x
regbootod2 = matrix(NA,nrow = length(reg2d$x), ncol = iterations + 1)
regbootod2[,1] = reg2d$x
for(i in 1:iterations){
  for(j in 1:length(data2$Operation.Time)){
    sampler = sample(1:length(data2$Operation.Time),length(data2$Operation.Time), replace = TRUE)
    bootoddata[j,i] = data2$Operation.Time[sampler[j]]
    bootoddata[j,i + iterations] = data2$Population[sampler[j]]
  }
  regbootod[,i+1] = data.frame(locpoly(x = bootoddata[,i + iterations], y = bootoddata[,i], degree = deg, kernel = "normal", range.x = c(threshmin,threshlow),bandwidth = bandd))$y
  regbootod2[,i+1] = data.frame(locpoly(x = bootoddata[,i + iterations], y = bootoddata[,i], degree = deg, kernel = "normal", range.x = c(threshhigh,threshmax),bandwidth = band2d))$y
}
regbootoddata = data.frame(t(regbootod))
colnames(regbootoddata) = regbootoddata[1,]
regbootoddata = regbootoddata[-1,]

regbootoddata2 = data.frame(t(regbootod2))
colnames(regbootoddata2) = regbootoddata2[1,]
regbootoddata2 = regbootoddata2[-1,]


bootoddata = cbind(regbootoddata, regbootoddata2)
bootoddataquant = data.frame(t(apply(bootoddata,2,quantile,probs = c(.025, .05, .5, .95, .975))))
bootgraphod = ggplot(data = bootoddataquant, aes(x = as.numeric(row.names(bootoddataquant)), y = bootoddataquant$X2.5.)) + geom_point() + xlab("Population") + ylab("Operation Days") + ggtitle("Operation Days Kernel Estimation and 95% CI")
bootgraphod = bootgraphod + geom_point(data = bootoddataquant, aes(x = as.numeric(row.names(bootoddataquant)), y = bootoddataquant$X97.5.))
bootgraphod = bootgraphod + geom_point(data = bootoddataquant, aes(x = as.numeric(row.names(bootoddataquant)), y = bootoddataquant$X50.), color = "red")
bootgraphod = bootgraphod + geom_vline(xintercept = 1500) + ylim(0,max(Operation.Time))
bootgraphod

## Item Variety
iterations = 1000
bootivdata = matrix(NA, nrow = length(data2$Item.Variety), ncol = iterations * 2)
regbootiv = matrix(NA,nrow = length(regd$x), ncol = iterations + 1)
regbootiv[,1] = regd$x
regbootiv2 = matrix(NA,nrow = length(reg2d$x), ncol = iterations + 1)
regbootiv2[,1] = reg2d$x
for(i in 1:iterations){
  for(j in 1:length(data2$Item.Variety)){
    sampler = sample(1:length(data2$Item.Variety),length(data2$Item.Variety), replace = TRUE)
    bootivdata[j,i] = data2$Item.Variety[sampler[j]]
    bootivdata[j,i + iterations] = data2$Population[sampler[j]]
  }
  regbootiv[,i+1] = data.frame(locpoly(x = bootivdata[,i + iterations], y = bootivdata[,i], degree = deg, kernel = "normal", range.x = c(threshmin,threshlow),bandwidth = bande))$y
  regbootiv2[,i+1] = data.frame(locpoly(x = bootivdata[,i + iterations], y = bootivdata[,i], degree = deg, kernel = "normal", range.x = c(threshhigh,threshmax),bandwidth = band2e))$y
}
regbootivdata = data.frame(t(regbootiv))
colnames(regbootivdata) = regbootivdata[1,]
regbootivdata = regbootivdata[-1,]

regbootivdata2 = data.frame(t(regbootiv2))
colnames(regbootivdata2) = regbootivdata2[1,]
regbootivdata2 = regbootivdata2[-1,]


bootivdata = cbind(regbootivdata, regbootivdata2)
bootivdataquant = data.frame(t(apply(bootivdata,2,quantile,probs = c(.025, .05, .5, .95, .975))))
bootgraphiv = ggplot(data = bootivdataquant, aes(x = as.numeric(row.names(bootivdataquant)), y = bootivdataquant$X2.5.)) + geom_point() + xlab("Population") + ylab("Item Variety") + ggtitle("Item Variety Kernel Estimation and 95% CI")
bootgraphiv = bootgraphiv + geom_point(data = bootivdataquant, aes(x = as.numeric(row.names(bootivdataquant)), y = bootivdataquant$X97.5.))
bootgraphiv = bootgraphiv + geom_point(data = bootivdataquant, aes(x = as.numeric(row.names(bootivdataquant)), y = bootivdataquant$X50.), color = "red")
bootgraphiv = bootgraphiv + geom_vline(xintercept = 1500) + ylim(0,max(Item.Variety))
bootgraphiv

## Average Cost
iterations = 1000
bootacdata = matrix(NA, nrow = length(data2$Average.Cost), ncol = iterations * 2)
regbootac = matrix(NA,nrow = length(regd$x), ncol = iterations + 1)
regbootac[,1] = regd$x
regbootac2 = matrix(NA,nrow = length(reg2d$x), ncol = iterations + 1)
regbootac2[,1] = reg2d$x
for(i in 1:iterations){
  for(j in 1:length(data2$Average.Cost)){
    sampler = sample(1:length(data2$Average.Cost),length(data2$Average.Cost), replace = TRUE)
    bootacdata[j,i] = data2$Average.Cost[sampler[j]]
    bootacdata[j,i + iterations] = data2$Population[sampler[j]]
  }
  regbootac[,i+1] = data.frame(locpoly(x = bootacdata[,i + iterations], y = bootacdata[,i], degree = deg, kernel = "normal", range.x = c(threshmin,threshlow),bandwidth = bandf))$y
  regbootac2[,i+1] = data.frame(locpoly(x = bootacdata[,i + iterations], y = bootacdata[,i], degree = deg, kernel = "normal", range.x = c(threshhigh,threshmax),bandwidth = band2f))$y
}
regbootacdata = data.frame(t(regbootac))
colnames(regbootacdata) = regbootacdata[1,]
regbootacdata = regbootacdata[-1,]

regbootacdata2 = data.frame(t(regbootac2))
colnames(regbootacdata2) = regbootacdata2[1,]
regbootacdata2 = regbootacdata2[-1,]


bootacdata = cbind(regbootacdata, regbootacdata2)
bootacdataquant = data.frame(t(apply(bootacdata,2,quantile,probs = c(.025, .05, .5, .95, .975))))
bootgraphac = ggplot(data = bootacdataquant, aes(x = as.numeric(row.names(bootacdataquant)), y = bootacdataquant$X2.5.)) + geom_point() + xlab("Population") + ylab("Average Cost") + ggtitle("Average Cost Kernel Estimation and 95% CI")
bootgraphac = bootgraphac + geom_point(data = bootacdataquant, aes(x = as.numeric(row.names(bootacdataquant)), y = bootacdataquant$X97.5.))
bootgraphac = bootgraphac + geom_point(data = bootacdataquant, aes(x = as.numeric(row.names(bootacdataquant)), y = bootacdataquant$X50.), color = "red")
bootgraphac = bootgraphac + geom_vline(xintercept = 1500) + ylim(0,max(Average.Cost))
bootgraphac

## Volume Intensity
iterations = 1000
bootvidata = matrix(NA, nrow = length(data2$volume_intensity), ncol = iterations * 2)
regbootvi = matrix(NA,nrow = length(regd$x), ncol = iterations + 1)
regbootvi[,1] = regd$x
regbootvi2 = matrix(NA,nrow = length(reg2d$x), ncol = iterations + 1)
regbootvi2[,1] = reg2d$x
for(i in 1:iterations){
  for(j in 1:length(data2$volume_intensity)){
    sampler = sample(1:length(data2$volume_intensity),length(data2$volume_intensity), replace = TRUE)
    bootvidata[j,i] = data2$volume_intensity[sampler[j]]
    bootvidata[j,i + iterations] = data2$Population[sampler[j]]
  }
  regbootvi[,i+1] = data.frame(locpoly(x = bootvidata[,i + iterations], y = bootvidata[,i], degree = deg, kernel = "normal", range.x = c(threshmin,threshlow),bandwidth = bandg))$y
  regbootvi2[,i+1] = data.frame(locpoly(x = bootvidata[,i + iterations], y = bootvidata[,i], degree = deg, kernel = "normal", range.x = c(threshhigh,threshmax),bandwidth = band2g))$y
}
regbootvidata = data.frame(t(regbootvi))
colnames(regbootvidata) = regbootvidata[1,]
regbootvidata = regbootvidata[-1,]

regbootvidata2 = data.frame(t(regbootvi2))
colnames(regbootvidata2) = regbootvidata2[1,]
regbootvidata2 = regbootvidata2[-1,]


bootvidata = cbind(regbootvidata, regbootvidata2)
bootvidataquant = data.frame(t(apply(bootvidata,2,quantile,probs = c(.025, .05, .5, .95, .975))))
bootgraphvi = ggplot(data = bootvidataquant, aes(x = as.numeric(row.names(bootvidataquant)), y = bootvidataquant$X2.5.)) + geom_point() + xlab("Population") + ylab("Volume Intensity") + ggtitle("Volume Intensity Kernel Estimation and 95% CI")
bootgraphvi = bootgraphvi + geom_point(data = bootvidataquant, aes(x = as.numeric(row.names(bootvidataquant)), y = bootvidataquant$X97.5.))
bootgraphvi = bootgraphvi + geom_point(data = bootvidataquant, aes(x = as.numeric(row.names(bootvidataquant)), y = bootvidataquant$X50.), color = "red")
bootgraphvi = bootgraphvi + geom_vline(xintercept = 1500) + ylim(0,max(volume_intensity))
bootgraphvi

## Variety Intensity
iterations = 1000
bootridata = matrix(NA, nrow = length(data2$variety_intensity), ncol = iterations * 2)
regbootri = matrix(NA,nrow = length(regd$x), ncol = iterations + 1)
regbootri[,1] = regd$x
regbootri2 = matrix(NA,nrow = length(reg2d$x), ncol = iterations + 1)
regbootri2[,1] = reg2d$x
for(i in 1:iterations){
  for(j in 1:length(data2$variety_intensity)){
    sampler = sample(1:length(data2$variety_intensity),length(data2$variety_intensity), replace = TRUE)
    bootridata[j,i] = data2$variety_intensity[sampler[j]]
    bootridata[j,i + iterations] = data2$Population[sampler[j]]
  }
  regbootri[,i+1] = data.frame(locpoly(x = bootridata[,i + iterations], y = bootridata[,i], degree = deg, kernel = "normal", range.x = c(threshmin,threshlow),bandwidth = bandh))$y
  regbootri2[,i+1] = data.frame(locpoly(x = bootridata[,i + iterations], y = bootridata[,i], degree = deg, kernel = "normal", range.x = c(threshhigh,threshmax),bandwidth = band2h))$y
}
regbootridata = data.frame(t(regbootri))
colnames(regbootridata) = regbootridata[1,]
regbootridata = regbootridata[-1,]

regbootridata2 = data.frame(t(regbootri2))
colnames(regbootridata2) = regbootridata2[1,]
regbootridata2 = regbootridata2[-1,]


bootridata = cbind(regbootridata, regbootridata2)
bootridataquant = data.frame(t(apply(bootridata,2,quantile,probs = c(.025, .05, .5, .95, .975))))
bootgraphri = ggplot(data = bootridataquant, aes(x = as.numeric(row.names(bootridataquant)), y = bootridataquant$X2.5.)) + geom_point() + xlab("Population") + ylab("Variety Intensity") + ggtitle("Variety Intensity Kernel Estimation and 95% CI")
bootgraphri = bootgraphri + geom_point(data = bootridataquant, aes(x = as.numeric(row.names(bootridataquant)), y = bootridataquant$X97.5.))
bootgraphri = bootgraphri + geom_point(data = bootridataquant, aes(x = as.numeric(row.names(bootridataquant)), y = bootridataquant$X50.), color = "red")
bootgraphri = bootgraphri + geom_vline(xintercept = 1500) + ylim(0,max(variety_intensity))
bootgraphri

## Volume Per Store
iterations = 1000
bootvsdata = matrix(NA, nrow = length(data2$volume_per_store), ncol = iterations * 2)
regbootvs = matrix(NA,nrow = length(regd$x), ncol = iterations + 1)
regbootvs[,1] = regd$x
regbootvs2 = matrix(NA,nrow = length(reg2d$x), ncol = iterations + 1)
regbootvs2[,1] = reg2d$x
for(i in 1:iterations){
  for(j in 1:length(data2$volume_per_store)){
    sampler = sample(1:length(data2$volume_per_store),length(data2$volume_per_store), replace = TRUE)
    bootvsdata[j,i] = data2$volume_per_store[sampler[j]]
    bootvsdata[j,i + iterations] = data2$Population[sampler[j]]
  }
  regbootvs[,i+1] = data.frame(locpoly(x = bootvsdata[,i + iterations], y = bootvsdata[,i], degree = deg, kernel = "normal", range.x = c(threshmin,threshlow),bandwidth = bandi))$y
  regbootvs2[,i+1] = data.frame(locpoly(x = bootvsdata[,i + iterations], y = bootvsdata[,i], degree = deg, kernel = "normal", range.x = c(threshhigh,threshmax),bandwidth = band2i))$y
}
regbootvsdata = data.frame(t(regbootvs))
colnames(regbootvsdata) = regbootvsdata[1,]
regbootvsdata = regbootvsdata[-1,]

regbootvsdata2 = data.frame(t(regbootvs2))
colnames(regbootvsdata2) = regbootvsdata2[1,]
regbootvsdata2 = regbootvsdata2[-1,]


bootvsdata = cbind(regbootvsdata, regbootvsdata2)
bootvsdataquant = data.frame(t(apply(bootvsdata,2,quantile,probs = c(.025, .05, .5, .95, .975))))
bootgraphvs = ggplot(data = bootvsdataquant, aes(x = as.numeric(row.names(bootvsdataquant)), y = bootvsdataquant$X2.5.)) + geom_point() + xlab("Population") + ylab("Volume Per Store Intensity") + ggtitle("Volume Per Store Kernel Estimation and 95% CI")
bootgraphvs = bootgraphvs + geom_point(data = bootvsdataquant, aes(x = as.numeric(row.names(bootvsdataquant)), y = bootvsdataquant$X97.5.))
bootgraphvs = bootgraphvs + geom_point(data = bootvsdataquant, aes(x = as.numeric(row.names(bootvsdataquant)), y = bootvsdataquant$X50.), color = "red")
bootgraphvs = bootgraphvs + geom_vline(xintercept = 1500) + ylim(0,max(volume_per_store))
bootgraphvs
#######################################
## LOESS analysis on whole dataset 
#######################################


data2a = subset(data2,Population <= 3000 & Volume > 0)
data2a$assignment = sample(1:6,length(data2a$Population), replace = TRUE)


# loess_store_count = matrix(nrow = length(data2a$Store.Count), ncol = 602)
# loess_store_count[,1] = data2a$assignment
# loess_store_count[,2] = data2a$Store.Count
# 
# alphas = seq(.1,10,.1)
# 
# for(j in alphas){
#     for(i in 2:6){
# df = subset(data2a,assignment != i)
# loess.store = loess(df$Store.Count ~ df$rd_ind + df$Popthou, data = df, span = j)
# c = ((j-1)*60+1+i)
# loess_store_count[,c] = predict(loess.store, type = "response", newdata = data.frame(Popthou = data2a$Popthou, rd_ind = data2a$rd_ind))
# print(c)
#     }
# }

span = .75

# loess_store_matrix = matrix(0, nrow = 835, ncol = 102)
# loess_store_matrix[,1] = data2a$assignment
# loess_store_matrix[,2] = data2a$Store.Count.Address
# loess_volume_matrix = matrix(0, nrow = 835, ncol = 102)
# loess_volume_matrix[,1] = data2a$assignment
# loess_volume_matrix[,2] = data2a$Volume
# loess_area_matrix = matrix(0, nrow = 835, ncol = 102)
# loess_area_matrix[,1] = data2a$assignment
# loess_area_matrix[,2] = data2a$Square.Footage
# 
# alpha = seq(.1,10,.1)
# 
# 
# for(j in alpha){
# loess.store = loess(Store.Count.Address ~ rd_ind + Popthou, span = j, data = data2a)
# loess1 = predict(loess.store, type = "response", newdata = data.frame(Popthou = data2a$Popthou, rd_ind = data2a$rd_ind))
# loess.volume = loess(Volume ~ rd_ind + Popthou, span = j, data = data2a)
# loess2 = predict(loess.volume, type = "response", newdata = data.frame(Popthou = data2a$Popthou, rd_ind = data2a$rd_ind))
# loess.area = loess(Square.Footage ~ rd_ind + Popthou, span = j, data = data2a)
# loess3 = predict(loess.area, type = "response", newdata = data.frame(Popthou = data2a$Popthou, rd_ind = data2a$rd_ind))
# loess_store_matrix[,j*10+2] = (loess1 - loess_store_matrix[,2]) ^ 2
# loess_volume_matrix[,j*10+2] = (loess2 - loess_volume_matrix[,2]) ^ 2
# loess_area_matrix[,j*10+2] = (loess3 - loess_area_matrix[,2]) ^ 2
# print(j*10+2)
# }


loess.store = loess(Store.Count.Address ~ rd_ind + Popthou, span = span, data = data2a)
loess1 = predict(loess.store, type = "response", newdata = data.frame(Popthou = data2a$Popthou, rd_ind = data2a$rd_ind))
p1 = ggplot(data2a, aes(y = Store.Count.Address, x = Population)) + geom_point()
p1 = p1 + geom_line(data = data2a, aes(y = loess1, x = Population), colour = "red")


plx<-predict(loess(Store.Count.Address ~ rd_ind + Popthou),span = span, data = data2a, se=T)

lines(Popthou,plx$fit)
lines(Popthou,plx$fit - qt(0.975,plx$df)*plx$se, lty=2)
lines(Popthou,plx$fit + qt(0.975,plx$df)*plx$se, lty=2)


loess.volume = loess(Volume ~ rd_ind + Popthou, span = span, data = data2a)
loess2 = predict(loess.volume, type = "response", newdata = data.frame(Popthou = data2a$Popthou, rd_ind = data2a$rd_ind))
p2 = ggplot(data2a, aes(y = Volume, x = Population)) + geom_point()
p2 = p2 + geom_line(data = data2a, aes(y = loess2, x = Population), colour = "red")

loess.area = loess(Square.Footage ~ rd_ind + Popthou, span = span, data = data2a)
loess3 = predict(loess.area, type = "response", newdata = data.frame(Popthou = data2a$Popthou, rd_ind = data2a$rd_ind))
p3 = ggplot(data2a, aes(y = Square.Footage, x = Population)) + geom_point()
p3 = p3 + geom_line(data = data2a, aes(y = loess3, x = Population), colour = "red")

loess.variety = loess(Item.Variety ~ rd_ind + Popthou, span = span, data = data2a)
loess4 = predict(loess.variety, type = "response", newdata = data.frame(Popthou = data2a$Popthou, rd_ind = data2a$rd_ind))
p4 = ggplot(data2a, aes(y = Item.Variety, x = Population)) + geom_point()
p4 = p4 + geom_line(data = data2a, aes(y = loess4, x = Population), colour = "red")

loess.cost = loess(Average.Cost ~ rd_ind + Popthou, span = span, data = data2a)
loess5 = predict(loess.cost, type = "response", newdata = data.frame(Popthou = data2a$Popthou, rd_ind = data2a$rd_ind))
p5 = ggplot(data2a, aes(y = Average.Cost, x = Population)) + geom_point()
p5 = p5 + geom_line(data = data2a, aes(y = loess5, x = Population), colour = "red")

loess.volume_intensity = loess(volume_intensity ~ rd_ind + Popthou, span = span, data = data2a)
loess6 = predict(loess.volume_intensity, type = "response", newdata = data.frame(Popthou = data2a$Popthou, rd_ind = data2a$rd_ind))
p6 = ggplot(data2a, aes(y = volume_intensity, x = Population)) + geom_point()
p6 = p6 + geom_line(data = data2a, aes(y = loess6, x = Population), colour = "red")

loess.variety_intensity = loess(variety_intensity ~ rd_ind + Popthou, span = span, data = data2a)
loess7 = predict(loess.variety_intensity, type = "response", newdata = data.frame(Popthou = data2a$Popthou, rd_ind = data2a$rd_ind))
p7 = ggplot(data2a, aes(y = variety_intensity, x = Population)) + geom_point()
p7 = p7 + geom_line(data = data2a, aes(y = loess7, x = Population), colour = "red")

loess.order_intensity = loess(order_intensity ~ rd_ind + Popthou, span = span, data = data2a)
loess8 = predict(loess.order_intensity, type = "response", newdata = data.frame(Popthou = data2a$Popthou, rd_ind = data2a$rd_ind))
p8 = ggplot(data2a, aes(y = order_intensity, x = Population)) + geom_point()
p8 = p8 + geom_line(data = data2a, aes(y = loess8, x = Population), colour = "red")

loess.operation_intensity = loess(operation_intensity ~ rd_ind + Popthou, span = span, data = data2a)
loess9 = predict(loess.operation_intensity, type = "response", newdata = data.frame(Popthou = data2a$Popthou, rd_ind = data2a$rd_ind))
p9 = ggplot(data2a, aes(y = operation_intensity, x = Population)) + geom_point()
p9 = p9 + geom_line(data = data2a, aes(y = loess9, x = Population), colour = "red")

loess.average_sqft = loess(average_sqft ~ rd_ind + Popthou, span = span, data = data2a)
loess10 = predict(loess.average_sqft, type = "response", newdata = data.frame(Popthou = data2a$Popthou, rd_ind = data2a$rd_ind))
p10 = ggplot(data2a, aes(y = average_sqft, x = Population)) + geom_point()
p10 = p10 + geom_line(data = data2a, aes(y = loess10, x = Population), colour = "red")

loess.volume_per_store = loess(volume_per_store ~ rd_ind + Popthou, span = span, data = data2a)
loess11 = predict(loess.volume_per_store, type = "response", newdata = data.frame(Popthou = data2a$Popthou, rd_ind = data2a$rd_ind))
p11 = ggplot(data2a, aes(y = volume_per_store, x = Population)) + geom_point()
p11 = p11 + geom_line(data = data2a, aes(y = loess11, x = Population), colour = "red")

loess.operation_time_per_store = loess(operation_time_per_store ~ rd_ind + Popthou, span = span, data = data2a)
loess12 = predict(loess.operation_time_per_store, type = "response", newdata = data.frame(Popthou = data2a$Popthou, rd_ind = data2a$rd_ind))
p12 = ggplot(data2a, aes(y = operation_time_per_store, x = Population)) + geom_point()
p12 = p12 + geom_line(data = data2a, aes(y = loess12, x = Population), colour = "red")


## LM with weights, instead of LOESS
data2a$tweights = 1-abs(1500-data2a$Population)/1500
triangle.weights = lm(Store.Count.Address ~ Popthou + Popthouint + Popthousq + Popthousqint + Popthoucu + Popthoucuint + rd_ind, weights = data2a$tweights, data = data2a)
summary(triangle.weights)



#######################################
## Using the RDD package
#######################################

rd_test <- RDestimate(Store.Count ~ Population, cutpoint = 1500, bw = 250)
plot(rd_test)











##Output regression results to LaTeX tables
stargazer(rd_volume1, rd_volume2, rd_volume3, rd_volume4, rd_volume5, rd_volume6, rd_volume7, rd_volume8, title = "Volume LM")
stargazer(rd_Order.Count1, rd_Order.Count2, rd_Order.Count3, rd_Order.Count4, rd_Order.Count5, rd_Order.Count6, rd_Order.Count7, rd_Order.Count8, title = "Order Count LM")
stargazer(rd_Item.Variety1, rd_Item.Variety2, rd_Item.Variety3, rd_Item.Variety4, rd_Item.Variety5, rd_Item.Variety6, rd_Item.Variety7, rd_Item.Variety8, title = "Item Variety LM")
stargazer(rd_Average.Cost1, rd_Average.Cost2, rd_Average.Cost3, rd_Average.Cost4, rd_Average.Cost5, rd_Average.Cost6, rd_Average.Cost7, rd_Average.Cost8, title = "Average Cost LM")
stargazer(rd_Square.Footage1, rd_Square.Footage2, rd_Square.Footage3, rd_Square.Footage4, rd_Square.Footage5, rd_Square.Footage6, rd_Square.Footage7, rd_Square.Footage8, title = "Square Footage LM")
stargazer(rd_Operation.Time1, rd_Operation.Time2, rd_Operation.Time3, rd_Operation.Time4, rd_Operation.Time5, rd_Operation.Time6, rd_Operation.Time7, rd_Operation.Time8, title = "Operation Time LM")
stargazer(rd_stores1, rd_stores2, rd_stores3, rd_stores4, rd_stores5, rd_stores6, rd_stores7, rd_stores8, title = "Store Count LM")
stargazer(poi1, poi2, poi3, poi4, poi5, poi6, poi7, poi8, title = "Store Count Poisson")
stargazer(nb1, nb2, nb3, nb4, nb5, nb6, nb7, nb8, title = "Store Count NB")
stargazer(probit1, probit2, probit3, probit4, probit5, probit6, probit7, probit8, title = "Store Count Ordered Probit")
