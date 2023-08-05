library(readxl)
w<- read_excel("C:/Users/tando/Downloads/winequality-white.xlsx")
head(w)
View(w)
str(w)
sum(is.na(w))
#install.packages("rpart")
library("rpart")
w$quality <- factor(w$quality)#integer variable will be converted into factor
str(w)

normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
wn <- as.data.frame(lapply(w[-12], normalize))
head(wn)

wn$quality <- w$quality
View(wn)
str(wn)

#traning and testing data
library("caTools")
set.seed(1234)
split <- sample.split(wn, SplitRatio=0.8)

train <- subset(wn, split==TRUE)
test <- subset(wn, split==FALSE)

tree<- rpart(quality~.,train)
tree
#install.packages("rpart.plot")
library("rpart.plot")
rpart.plot(tree)
pred= predict(tree, test, type = "vector")
pred

library("gmodels")
CrossTable(x = test$quality, y = pred, prop.chisq = FALSE)

tbl1 <- table(test$quality, pred)
tbl1

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tbl1)

#install.packages("caret")
library("caret")
confusionMatrix(pred,test$quality)
