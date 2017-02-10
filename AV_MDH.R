train <- read.csv('train.csv', header = T)
test <- read.csv('test.csv', header = T)

head(train)
str(train)

library(mice)
library(VIM)
library(ggplot2)

mice_plot <- aggr(train, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(train), cex.axis=.3,
                  gap=3, ylab=c("Missing data","Pattern"))

summary(train)
#removed Var1
train_new <- train[,-10]
test_new <- test[,-10]

table(train_new$Destination_Type, train_new$Surge_Pricing_Type)
table(train_new$Type_of_Cab, train_new$Surge_Pricing_Type)

table(train_new$Gender, train_new$Surge_Pricing_Type)
table(train_new$Customer_Since_Months, train_new$Surge_Pricing_Type)

table(cut(train_new$Trip_Distance,breaks = c(0,25,50,75,100)))
table(cut(train_new$Trip_Distance,breaks = c(0,30,60,120)))

##Feature engg 1
train_new$Trip_Distance <- cut(train_new$Trip_Distance, breaks = c(0,30,60,120),labels = c('Small','Avg','Long'))
test_new$Trip_Distance <- cut(test_new$Trip_Distance, breaks = c(0,30,60,120),labels = c('Small','Avg','Long'))

table(train_new$Trip_Distance, train_new$Surge_Pricing_Type)

table(sum(train_new$Life_Style_Index),train_new$Confidence_Life_Style_Index)
ggplot(train_new, aes(Confidence_Life_Style_Index,Life_Style_Index))+geom_boxplot()+labs(title='Boxplot',X='Sex',y='Count')

table(train_new$Surge_Pricing_Type, train_new$Destination_Type)

#From the Given exploration We are removing the Var1
summary(train_new$Life_Style_Index)
plot(density((train_new$Life_Style_Index)^2))
boxplot(train_new$Life_Style_Index)

sum(is.na(train_new$Life_Style_Index))
dim(train_new)

train_new$Life_Style_Index[is.na(train_new$Life_Style_Index)] <- mean(train_new$Life_Style_Index, na.rm = TRUE)
test_new$Life_Style_Index[is.na(test_new$Life_Style_Index)] <- mean(test_new$Life_Style_Index, na.rm = TRUE)

train_new$Confidence_Life_Style_Index <-as.character(train$Confidence_Life_Style_Index)
train_new$Confidence_Life_Style_Index[train_new$Confidence_Life_Style_Index==''] <- 'O'
train_new$Confidence_Life_Style_Index <- as.factor(train_new$Confidence_Life_Style_Index)

test_new$Confidence_Life_Style_Index <-as.character(test$Confidence_Life_Style_Index)
test_new$Confidence_Life_Style_Index[test_new$Confidence_Life_Style_Index==''] <- 'O'
test_new$Confidence_Life_Style_Index <- as.factor(test_new$Confidence_Life_Style_Index)



table(cut(train_new$Life_Style_Index, breaks = c(0,2.7,2.9,5)))

#Feature engg 2
train_new$Life_Style_Index <- cut(train_new$Life_Style_Index, breaks = c(0,2.7,2.9,5),labels = c('Low','Medium','High'))
test_new$Life_Style_Index <- cut(test_new$Life_Style_Index, breaks = c(0,2.7,2.9,5),labels = c('Low','Medium','High'))


summary(train_new$Confidence_Life_Style_Index)
table(train_new$Confidence_Life_Style_Index, train_new$Life_Style_Index)


summary(train_new$Life_Style_Index[train_new$Confidence_Life_Style_Index == 'A'])
summary(train_new$Life_Style_Index[train_new$Confidence_Life_Style_Index == 'B'])
summary(train_new$Life_Style_Index[train_new$Confidence_Life_Style_Index == 'C'])

summary(train_new)
summary(train_new$Customer_Since_Months)

table(train_new$Surge_Pricing_Type, train$Customer_Since_Months)

#Featuer Engg
train_new$Customer_Since_Months[is.na(train_new$Customer_Since_Months)] <- 0
test_new$Customer_Since_Months[is.na(test_new$Customer_Since_Months)] <- 0

new_train_1 <- train_new[,-c(1)]
new_test_1 <- test_new[,-1]

summary(new_test_1)

#multinomial model
library(nnet)

mdoelnames <- names(new_train_1)

model_ml <- multinom(Surge_Pricing_Type~.-Cancellation_Last_1Month-Customer_Since_Months,data = new_train_1)

table(predict(model_ml, newdata = new_train_1, type='class'), new_train_1$Surge_Pricing_Type)

prediction <- predict(model_ml, newdata = new_test_1, type='class')

submission <- data.frame(Trip_ID = test$Trip_ID, Surge_Pricing_Type = prediction)
write.csv(submission, 'Submission_MDH3.csv')
