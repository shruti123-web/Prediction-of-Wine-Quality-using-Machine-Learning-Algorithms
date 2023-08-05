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

#install.packages('DataExplorer')
library('DataExplorer')
plot_correlation(w)
#create_report(w)
#handling categorical data
w$quality <- factor(w$quality)
str(w)

library(caTools)

#splitting the dataset
set.seed(1234)

split <- sample.split(w$D, SplitRatio=0.8)

train <- subset(w, split==TRUE)
test <- subset(w, split==FALSE)

#fitting multiple linear regression model
regressor=lm(formula = D~.,
             data=train)
y_pred=predict(regressor,newdata=test)
y_pred

#prediction
df <- data.frame(FA = 7.4,VA = 0.25, CA = 0.37, RS = 13.5,
                 C = 0.06, FSD = 52, TSD = 192, pH=3,
                 S = 0.44, A = 9.1, quality = as.factor(5))

result <- predict(regressor,df)
print(result)

#install.packages("modelr")
library(modelr)
R2 = rsquare(regressor, data = w)
R2*100

library(ggplot2)
ggplot(train, aes(FA, D)) +
  geom_smooth(method="lm") +
  geom_point(size=1) +
  theme_bw() + 
  xlab("Fixed Acidity") +
  ylab("Density") +
  ggtitle("FA vs D")
