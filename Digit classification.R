#Loading packages
library(readr)
library(caret)

#test and train
train <- read.csv("train.csv")
test <- read.csv("test.csv")

#Checking the dimensions
dim(train)

table(as.factor(train$label))

#ggplot of labels
ggplot(train,aes(x=as.factor(label),fill=label))+
  geom_bar(stat="count",color="yellow")+
  scale_fill_gradient(low="lightgreen",high="blue",guide=FALSE)+
  labs(title="Count of the labels",x="Labels (0-9)")

#Some images need roation like in the following example
m = matrix(unlist(train[10,-1]),nrow = 29,byrow = T)
image(m ,col=grey.colors(255)) #3 appears to be tilted 90 degrees anticlockwise

#We need to reverse the columns before transposing, this can be done in 2 methods:
#1 apply function
  #first reverse, then transpose, it's the same as rotate 90 degrees
  rotate_clockwise <- function(x) { t(apply(x, 2, rev))}
   n= rotate_clockwise(matrix(unlist(train[10,-1]),nrow = 28,byrow = T))
   image(n ,col=grey.colors(255))
#2 Use library pracma
library(pracma)
rotate_ninty <- function(x) { rot90(x, 1) }

#Plotting 20 randomly selected labels
par(mfrow=c(5,5),mar=c(1,1,1,1))
s <- sample(1:nrow(train), 25)
for (x in s)
  {m = rotate_clockwise(matrix(unlist(train[x,-1]),nrow = 28,byrow = T))
  image((m) ,col=grey.colors(255))
  }

#Setting the par to 1,1 so that plots appear one at a time
par(mfrow=c(1,1))

#Data Prepocessing
#Removing predictors which have almost zero variance
nz <- nearZeroVar(train[,-1], saveMetrics=T)
sum(nz$nzv) #532 near zero variance predictors

#Removing the 532 columns
toremove <- rownames(nz[nz$nzv==TRUE,])
difference <- setdiff(names(train),toremove)
train <- train[,difference]


#Creating covariance matrix
l <- as.factor(train[[1]])
train$l <- NULL
train <- train/255
covtrain <- cov(train)

#Apply pca to the covariance matrix
pca_train <- prcomp(covtrain)
r <- pca_train$sdev^2/sum(pca_train$sdev^2)
v <- cumsum(r)
result <- data.frame(num=1:length(pca_train$sdev),
                     ex=r,
                     cum=v)

plot(result$num,result$cum,type="b",xlim=c(0,50),
     main="Variance Explained by Top 50 Components",
     xlab="Component number",ylab="Variance Explained")
abline(v=25)

#Combining first 25 components to create new train
train <- cbind(l,as.data.frame(as.matrix(train) %*% pca_train$rotation[,1:25]))

#Support vector machine
svmfit <- train(l~.,data=train,
                 method="svmRadial",
                 trControl=trainControl(method="cv",
                                        number=10),
                 tuneGrid=data.frame(sigma = 0.01,
                                     C = 3.5))
svmfit


test <- test[,var[-1]]/255
test <- as.matrix(test) %*% pca_train$rotation[,1:25]
test <- as.data.frame(test)

pred <- predict(svmfit$finalModel,test,type="response")




