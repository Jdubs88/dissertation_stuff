data$col1 = rep(1,952)
data$col2 = rep(1,952)
data$col3 = rep(1,952)
data$col4 = rep(1,952)
data$col5 = rep(1,952)
data$col6 = rep(1,952)
data$col7 = rep(1,952)
data$col8 = rep(1,952)
data$col9 = rep(1,952)
attach(data)
for(i in 1:9){
  col = paste('col',i,sep="")
  data[,col] = 3
}
detach(data)


