
# load the new data
df_faot = read.csv("proj_fao_T.csv")
str(df_faot)

# one method to order the level, this will create atomic vectors
#levels_PSI <- c("VeryStable", "Stable", "Neutral", "Unstable", "VeryUnstable")
#df_faot <- factor(df_faot$PSI_level, levels = levels_PSI)


#arrange(df_faot, PSI_level, levels = c("VeryStable", "Stable", "Neutral", "Unstable", "VeryUnstable"))

#df_faot$PSI_level <- as.factor(df_faot$PSI_level)
df_faot$PSI_level <- factor(df_faot$PSI_level, levels = c("VeryStable", "Stable", "Neutral", "Unstable", "VeryUnstable"))


df_faoT <- df_faot[c(-13)]
str(df_faoT)
library(corrplot)
corrplot(cor(df_fao))

plot(df_faoT[,c(-8)], col=(df_faoT$PSI_level))
set.seed(123)


# standarizing
df_fao1 = df_faoT
str(df_fao1)
df_fao1[,c(-8)] = scale(df_fao1[,c(-8)])
str(df_fao1)
#df_fao1$PSI_level <- as.factor(df_fao1$PSI_level)
# let's separate data set into training and  test data
s= sample(nrow(df_fao1), nrow(df_fao1)* 0.6)
train_fao1 = df_fao1[s,]
test_fao1 = df_fao1[-s,]
df_train <- train_fao1
df_test <- test_fao1
#set.seed(123)
#sample_size = round(nrow(df_fao)* 0.8)
#index <- sample(seq_len(nrow(df_fao)), size = sample_size)
#df_train <- df_fao[index, ]
#df_test <- df_fao[-index, ]

# initial LDA for everything 
train_df.lda = lda(PSI_level ~ ., data=df_train)
print(train_df.lda)

#train_df.lda = lda(PSI_order   ~ ., data=df_train)
#print(train_df.lda)

round(train_df.lda$scaling[order(train_df.lda$scaling[,1]),],2)
round(train_df.lda$scaling[order(train_df.lda$scaling[,2]),],2)

# predicting on the training set
train.lda1.values <- predict(train_df.lda)
# Look at the separation obtained by the two variates
par(mar=c(1,1,1,1))
ldahist(train.lda1.values$x[, 1], g=(df_train$PSI_level))  
plot(train_df.lda)

library(MASS)
# Now, let's plot the transformed data so we can see the classification
train.lda1.values$x  # The scores are stored in the x-parameter
plot(train.lda1.values$x[, 1], train.lda1.values$x[, 2], col=(df_train$PSI_level), pch=16)

library(ggplot2)
newdata <- data.frame(PSI_level= factor(df_train$PSI_level), lda_tr =train.lda1.values$x)
ggplot(newdata) + geom_point(aes(lda_tr.LD1, lda_tr.LD2, colour = PSI_level), size=2.5)

#Compute a confusion matrix
conf_train1 <-table(df_train$PSI_level, train.lda1.values$class) 
conf_train1
#library(rattle)
#install.packages("rattle")
#install.packages("rfUtilities")
library(rfUtilities)
accuracy(df_train$PSI_level, train.lda1.values$class )
confusionMatrix(conf_train1)


# for test data
test.lda1.values = predict(train_df.lda, df_test)

# Look at the separation obtained by the two variates
ldahist(data=test.lda1.values$x[, 1], g=df_test$PSI_level)
ldahist(data=test.lda1.values$x[, 2], g=df_test$PSI_level)

# Now, let's plot the transformed data so we can see the classification
test.lda1.values$x  # The scores are stored in the x-parameter
plot(test.lda1.values$x[, 1], test.lda1.values$x[, 2], col=(df_test$PSI_level), pch=16)

mycolors <- c("black", "red", "blue", "green", "purple")
newdata1 <- data.frame(PSI_level= factor(df_test$PSI_level), lda_ts = test.lda1.values$x)
ggplot(newdata1) + geom_point(aes(lda_ts.LD1, lda_ts.LD2, colour = PSI_level), size=2.5)

# different method of creating plot using ggplot
newdata2 <- data.frame(PSI_level= df_test$PSI_level, X= test.lda1.values$x[,1], Y= test.lda1.values$x[,2])
ggplot(data = newdata2, aes(x= X, y= Y, color = factor(PSI_level)))+ geom_point() + scale_color_manual(values = mycolors) + theme_bw()
ggplot(data = newdata1, aes(lda_ts.LD1, lda_ts.LD2, color = PSI_level))+ geom_point() + scale_color_manual(values = mycolors)


# Compute a confusion matrix
conf1 <- table(df_test$PSI_level, test.lda1.values$class)   
conf1
accuracy(df_test$PSI_level, test.lda1.values$class)

install.packages("caret")
library(caret)
confusionMatrix(conf1)


#Compute a confusion matrix
conf_train <-table(df_train$PSI_level, train.lda1.values$class) 
conf_train

## analysing LDA
ggplot(df_fao, aes(x= PSI_level, y=GDP, color=PSI_level)) + geom_boxplot(outlier.colour = "", outlier.shape = 8, outlier.size = 4)

ggplot(df_fao, aes(x=GDP, y= PSI_level, fill = PSI_level)) + geom_boxplot()+ stat_summary(fun = "mean", geom = "point", shape=8, color="white")

TotalDomSupply <-t(prop.table(table(df_fao$TotalDomSupply))) /10000

PieChart(continent, data= cat2, hole=0, fill= cols, labels_cex = 0.9)
ggplot(data= df_fao, aes(x=, fill= PSI_level)) + geom_bar(position = "dodge") +ylab("GDP") # stacked bar graph
ggplot(data= df_fao, aes(x= PSI_level, fill= PSI_level)) + geom_bar(position = "dodge") +ylab("GDP")

############
###########logistic regression

# let's separate data set into training and  test 
df_faoT <- df_faot[c(-13)]
s= sample(nrow(df_faoT), nrow(df_faoT)* 0.6)
train_faot = df_faoT[s,]
test_faot = df_faoT[-s,]
dsTrain <- train_faot
dsTest <- test_faot
summary(df_faoT)
df_faoT$PSI_level <-as.factor(df_faoT$PSI_level)

model1 = polr(PSI_level ~., data = dsTrain, Hess = TRUE )

summary(model1)  

pred = predict(model1, newdata=dsTest)
#pred1 = predict(model, newdata=dsTest)
print(pred)   # We get predicted probabilities

mean(pred == dsTest$PSI_level)
table(predicted = pred, actual = dsTest$PSI_level)
table(actual = dsTest$PSI_level, predicted = pred)
summary_table <- coef(summary(model1))

pval <- pnorm(abs(summary_table[,"t value"]), lower.tail =FALSE)*2
summary_table <- cbind(summary_table, "p value"= round(pval, 3))
summary_table
# Let's use the .5 cutoff to classify them
pred = ifelse(pred > 0.5, 1, 0)
table(model1, pred)  # Look at false positives and false negatives


