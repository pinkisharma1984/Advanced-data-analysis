# load the new data
df_faoT = read.csv("proj_fao_T.csv")
str(df_faoT)
df_faoT$PSI_level <- factor(df_faoT$PSI_level, levels = c("VeryStable", "Stable", "Neutral", "Unstable", "VeryUnstable"))
set.seed(12345)
# from factor analysis re running lda
# production + import+ export +GDP + PSI
#GDP + PSI + VP + VI + VE + FP + FI + MP + MI
df_fao_factor <- df_faoT[,c(-1,-2,-3,-5,-6,-7,-13, -12)]
str(df_fao_factor)

# standardizing
df_fao1 = df_fao_factor
df_fao1[,c(-2)] = scale(df_fao1[,c(-2)])


# let's separate data set into training and  test data
s= sample(nrow(df_fao1), nrow(df_fao1)* 0.8)
train_fao1 = df_fao1[s,]
test_fao1 = df_fao1[-s,]
df_train1 <- train_fao1
df_test1 <- test_fao1
#set.seed(123)
#sample_size = round(nrow(df_fao)* 0.8)
#index <- sample(seq_len(nrow(df_fao)), size = sample_size)
#df_train <- df_fao[index, ]
#df_test <- df_fao[-index, ]

# initial LDA for everything 
train_df.lda = lda(PSI_level~ ., data=df_train1)
print(train_df.lda)

round(train_df.lda$scaling[order(train_df.lda$scaling[,1]),],2)
round(train_df.lda$scaling[order(train_df.lda$scaling[,2]),],2)

# predicting on the training set
train.lda1.values <- predict(train_df.lda)
# Look at the separation obtained by the two variates
par(mar=c(1,1,1,1))
ldahist(train.lda1.values$x[, 1], g=(df_train1$PSI_level))  
plot(train_df.lda)
library(MASS)
# Now, let's plot the transformed data so we can see the classification
train.lda1.values$x  # The scores are stored in the x-parameter
plot(train.lda1.values$x[, 1], train.lda1.values$x[, 2], col=as.factor(df_train1$PSI_level), pch=16)
library(ggplot2)
newdata <- data.frame(PSI_level= factor(df_train1$PSI_level), lda_tr =train.lda1.values$x)
ggplot(newdata) + geom_point(aes(lda_tr.LD1, lda_tr.LD2, colour = PSI_level), size=2.5)

#Compute a confusion matrix
conf_train1 <-table(df_train1$PSI_level, train.lda1.values$class) 
conf_train1
#library(rattle)
#install.packages("rattle")
#install.packages("rfUtilities")
library(rfUtilities)
accuracy(df_train1$PSI_level, train.lda1.values$class )
confusionMatrix(conf_train1)


# for test data
test.lda1.values = predict(train_df.lda, df_test1)

# Look at the separation obtained by the two variates
ldahist(data=test.lda1.values$x[, 1], g=df_test1$PSI_level)
ldahist(data=test.lda1.values$x[, 2], g=df_test1$PSI_level)

# Now, let's plot the transformed data so we can see the classification
test.lda1.values$x  # The scores are stored in the x-parameter
plot(test.lda1.values$x[, 1], test.lda1.values$x[, 2], col=as.factor(df_test1$PSI_level), pch=16)


newdata1 <- data.frame(PSI_level=(df_test1$PSI_level), lda_ts =test.lda1.values$x)
ggplot(newdata1) + geom_point(aes(lda_ts.LD1, lda_ts.LD2, colour = PSI_level), size=2.5)

# Compute a confusion matrix
conf1 <- table(df_test1$PSI_level, test.lda1.values$class)   
conf1
accuracy(df_test1$PSI_level, test.lda1.values$class)

install.packages("caret")
library(caret)
confusionMatrix(conf1)

# Compute a confusion matrix
conf <- table(df_test$PSI_level, test.lda1.values$class)

