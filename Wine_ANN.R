library(readxl)
w<- read_excel("C:/Users/tando/Downloads/winequality-white.xlsx")
head(w)
View(w)
str(w)
sum(is.na(w))

hist(w$quality)
summary(w$quality)
normalize <- function(x) {  return((x - min(x)) / (max(x) - min(x)))}
w_norm <- as.data.frame(lapply(w, normalize))
summary(w_norm$quality)

library("caTools")
set.seed(1234)

split <- sample.split(w_norm, SplitRatio=0.70)

w_train <- subset(w_norm, split==TRUE)
w_test <- subset(w_norm, split==FALSE)

#install.packages("neuralnet")
library(neuralnet)

wine_model <- neuralnet(quality~. ,
                            data = w_train)

plot(wine_model)

model_results = compute(wine_model,w_test[1:11])
predicted_quality <- model_results$net.result

cor(predicted_quality, w_test$quality)



wine_model2 <- neuralnet(quality~. ,
                             data = w_train, hidden = 5)

plot(wine_model2)

model_results2 <- compute(wine_model2, w_test[1:11])
predicted_quality2 <- model_results2$net.result

cor(predicted_quality2, w_test$quality)
