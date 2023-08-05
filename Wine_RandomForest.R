library(readxl)
w<- read_excel("C:/Users/tando/Downloads/winequality-white.xlsx")
head(w)
View(w)
str(w)
sum(is.na(w))

colnames(w)[1]<-"FA"
colnames(w)[2]<-"VA"
colnames(w)[3]<-"CA"
colnames(w)[4]<-"RS"
colnames(w)[5]<-"C"
colnames(w)[6]<-"FSD"
colnames(w)[7]<-"TSD"
colnames(w)[8]<-"D"
colnames(w)[9]<-"pH"
colnames(w)[10]<-"S"
colnames(w)[11]<-"A"

View(w)
#splitting the dataset
set.seed(1234)
split <- sample.split(w$D, SplitRatio=0.8)
train <- subset(w, split==TRUE)
test <- subset(w, split==FALSE)


#install.packages("randomForest")
library(randomForest)

# Create random forest for regression
density_rf <- randomForest(D ~ ., data = train)

# Print regression model
density_rf
summary(density_rf)

# Prediction
pred = predict(density_rf, test)
pred


plt = getTree(density_rf, k=1, labelVar = TRUE)
plt

# Plot the error vs the number of trees graph
plot(density_rf)

library(modelr)
R2 = rsquare(density_rf, data = test)
R2*100
