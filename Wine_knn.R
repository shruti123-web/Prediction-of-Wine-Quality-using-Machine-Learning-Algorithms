library(readxl)
w<- read_excel("C:/Users/tando/Downloads/winequality-white.xlsx")
head(w)
View(w)
str(w)
sum(is.na(w))

w$quality <- factor(w$quality)
str(w)
table(w$quality)

normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
wn <- as.data.frame(lapply(w[-12], normalize))
head(wn)

wn$quality <- w$quality
View(wn)
str(wn)

library("caTools")
set.seed(1234)

split <- sample.split(wn$quality, SplitRatio=0.8)

train <- subset(wn, split==TRUE)
test <- subset(wn, split==FALSE)

trainl <- subset(wn$quality, split==TRUE)
testl <- subset(wn$quality, split==FALSE)

library(class)

pred <- knn(train = train, test = test, cl = trainl, k = 62)
pred

table(pred)

cm <- table(pred, test$quality)
cm

library("gmodels")
CrossTable(x = test$quality, y = pred, prop.chisq = FALSE)

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(cm)

install.packages("caret")
library("caret")
confusionMatrix(pred,test$quality)



#library("ggplot2")
#install.packages("GGally")
#library("GGally")
#ggpairs(wn, ggplot2::aes(colour = quality, alpha = 0.4))