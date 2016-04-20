##### cross validation of random forest 
ntrees.calc <- function(ntrees,max.nodes,train.data,test.data){
  rf <- randomForest(scaleLogSalary~.,ntree=ntrees,data=train.data,maxnodes=max.nodes)
  predict <- predict(rf,newdata=test.data)
  mse.test <- mean((test.data$scaleLogSalary-predict)^2)
  return(mse.test)
}




ntrees <- 200

max.nodes <- seq(6000,11000,by=1000)

train.matrix <- matrix(NA,ncol=2),nrow = length(max.nodes))

folds <- createFolds(1:nrow(train.data),k=3)
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

folds <- createFolds(1:nrow(train.data),k=5)
temp.matrix <- matrix(NA,nrow=length(folds),ncol=length(ntrees))
for (n.nodes in max.nodes){
  print(row.count)
  for(i in 1:length(folds)){
    print(i*10)
  train.fold <- train.data[folds[[i]],]
  test.fold <- train.data[-folds[[i]],]
  temp.matrix[i,] <- sapply(ntrees,ntrees.calc,max.nodes=n.nodes,train.data=train.fold,test.data=test.fold)
  }
  train.matrix[row.count,] <- colMeans(temp.matrix)
  
  
} 
rownames(train.matrix) <- c("3 Folds","5 Folds")
colnames(train.matrix) <- c("7000Nodes","8000Nodes","9000Nodes","10000Nodes","11000Nodes")

rf.model <- randomForest(scaleLogSalary~.,ntree=200,data=train.data,maxnodes=10000,importance=T,proximity=T,keep.forest=T,mtry=mtrys)
  predict <- predict(rf,newdata=test.data)
  mse.test <- mean((test.data$scaleLogSalary-predict)^2)

