library(caret)
library(randomForest)

set.seed(12345)

## load data. replace "#DIV/0!"
training <- read.table("pml-training.csv", sep=",", na.strings=c("#DIV/0!","","NA"), stringsAsFactors=FALSE, header=TRUE)
testing <- read.table("pml-testing.csv", sep=",", na.strings=c("#DIV/0!","","NA"), stringsAsFactors=FALSE, header=TRUE)


##preprocessing
# omit columns with all NA's
training_data <- training[,colSums(is.na(training))==0]
testing_data <- testing[,colSums(is.na(training))==0] 

# omit unnessesary columns
training_data <- training_data[,-c(1:7)]
testing_data <- testing_data[,-c(1:7)]

# make classe factor
training_data$classe <- as.factor(training_data$classe)

# prepair data for training and validation
inTrain <- createDataPartition(y=training_data$classe, p=0.7, list=FALSE)
training_sub <- training_data[inTrain,]
validate_sub <- training_data[-inTrain,]

## make model
model <- randomForest(classe~., data=training_sub, importance=TRUE)


# see accuracy
result <- predict(model, validate_sub)
confusionMatrix(result, validate_sub$classe)

## predict test set
predict(model, testing_data)


#plot randomForest


to.dendrogram <- function(dfrep,rownum=1,height.increment=0.1){
  
  if(dfrep[rownum,'status'] == -1){
    rval <- list()
    
    attr(rval,"members") <- 1
    attr(rval,"height") <- 0.0
    attr(rval,"label") <- dfrep[rownum,'prediction']
    attr(rval,"leaf") <- TRUE
    
  }else{##note the change "to.dendrogram" and not "to.dendogram"
    left <- to.dendrogram(dfrep,dfrep[rownum,'left daughter'],height.increment)
    right <- to.dendrogram(dfrep,dfrep[rownum,'right daughter'],height.increment)
    rval <- list(left,right)
    
    attr(rval,"members") <- attr(left,"members") + attr(right,"members")
    attr(rval,"height") <- max(attr(left,"height"),attr(right,"height")) + height.increment
    attr(rval,"leaf") <- FALSE
    attr(rval,"edgetext") <- dfrep[rownum,'split var']
    #To add Split Point in Dendrogram
    attr(rval,"edgetext") <- paste(dfrep[rownum,'split var'],"\n<",round(dfrep[rownum,'split point'], digits = 2),"=>", sep = " ")
  }
  
  class(rval) <- "dendrogram"
  
  return(rval)
}

tree <- getTree(model,1,labelVar=TRUE)

d <- to.dendrogram(tree,rownum=1)
str(d)
plot(d,center=TRUE,leaflab='none',edgePar=list(t.cex=1,p.col=NA,p.lty=0))
