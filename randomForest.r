##### cross validation of random forest 
ntrees.calc <- function(ntrees,max.nodes,train.data,test.data){
  rf <- randomForest(scaleLogSalary~.,ntree=ntrees,data=train.data,maxnodes=max.nodes)
  predict <- predict(rf,newdata=test.data)
  mse.test <- mean((test.data$scaleLogSalary-predict)^2)
  return(mse.test)
}


folds <- createFolds(1:nrow(train.data),k=5)

ntrees <- seq(10,200,by=10)
max.nodes <- seq(10,200,by=10)

train.matrix <- matrix(NA,ncol=length(ntrees),nrow = length(max.nodes))

temp.matrix <- matrix(NA,nrow=length(folds),ncol=length(ntrees))
row.count<-1
ptm <- proc.time()
for (n.nodes in max.nodes){
  print(row.count)
  for(i in 1:length(folds)){
    print(i*10)
  train.fold <- train.data[folds[[i]],]
  test.fold <- train.data[-folds[[i]],]
  temp.matrix[i,] <- sapply(ntrees,ntrees.calc,max.nodes=n.nodes,train.data=train.fold,test.data=test.fold)
  }
  train.matrix[row.count,] <- colMeans(temp.matrix)
  row.count <-row.count +1 
  
} 
proc.time() - ptm 
train.matrix
