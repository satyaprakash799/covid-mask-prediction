##### Needed packages and libraries
## Please install the packages if some package isn't there on your system
# install.packages("readxl")
# install.packages("rstudioapi")
# install.packages("Hmisc")
# install.packages("caret")
#install.packages("pls")
library(readxl) ; library(rstudioapi) ; library(Hmisc) ; library(dplyr)
library(car) ; library(leaps) ; library(glmnet) ; library(randomForest)
library(tree) ; library(MASS) ; library(rpart) ; library(gbm) ; library(caret)


##### This code block fetches training data excel file path name automatically.
## Please place both this R file and our training data file named 
## "Training_data.xlsx" in the same folder.
active_doc = getActiveDocumentContext()
file_path = active_doc$path
dir_path = dirname(file_path)
traindata_pathname = paste0(dir_path,"/Training_data.xlsx")


##### Finding the number of null values in the data.
traindata = read_excel(traindata_pathname,col_names = TRUE, sheet = "Sheet2")
summary(traindata)
dim(traindata)
sapply(traindata,class)
sum(is.na.data.frame(traindata))


##### Imputing missing values with median values for each column and sanity checks
traindata$Q68[traindata$Q68 == "18 years old."] = "18"
traindata$Q68 = strtoi(traindata$Q68)
traindata[] = lapply(traindata,as.integer)
summary(traindata)
head(traindata)
traindata[, sapply(traindata, is.numeric)] = impute(traindata[, sapply(traindata, is.numeric)], fun = median)
sum(is.na.data.frame(traindata))
traindata_omitna<-traindata
sapply(traindata_omitna,class)
summary(traindata_omitna$Q68)
dim(traindata_omitna)
data_len = nrow(traindata_omitna)
pred_len = ncol(traindata_omitna)-1



############# Final Bagging Model to run for testing ############# 
set.seed(300)
bag.survey_final = randomForest(Q46_2~., mtry=154, data=traindata_omitna, 
                                importance=TRUE, ntree = 100)
testdata_pathname = paste0(dir_path,"/Test_data-1.xlsx")
## Finding the number of null values in the data.
testdata = read_excel(testdata_pathname,col_names = TRUE, sheet = "Sheet2")
summary(testdata)
dim(testdata)
sapply(testdata,class)
testdata[] = lapply(testdata,as.integer)
sum(is.na.data.frame(testdata))

yhat.bag_test = predict(bag.survey_final, newdata=testdata)
mean((yhat.bag_test-testdata$Q46_2)^2)



############# Other Models #############

####### Forward Step-wise Selection
set.seed(300)
fss_fits = regsubsets(Q46_2~., data = traindata_omitna, nvmax = 154, method = "forward")
fss_fits_summary = summary(fss_fits)

par(mfrow=c(2,2))
plot(fss_fits_summary$rss, xlab="Number of Variables",ylab="RSS", type = "l")
which.min(fss_fits_summary$rss)
points(154, fss_fits_summary$rss[154], col ="red",cex =2, pch =20)

plot(fss_fits_summary$adjr2, xlab="Number of Variables",ylab="Adj-R2", type = "l")
which.max(fss_fits_summary$adjr2)
points(47, fss_fits_summary$adjr2[47], col ="red",cex =2, pch =20)

plot(fss_fits_summary$cp, xlab="Number of Variables",ylab="cp", type = "l")
which.min(fss_fits_summary$cp)
points(24, fss_fits_summary$cp[24], col ="red",cex =2, pch =20)

plot(fss_fits_summary$bic, xlab="Number of Variables",ylab="bic", type = "l")
which.min(fss_fits_summary$bic)
points(7, fss_fits_summary$bic[7], col ="red",cex =2, pch =20)

coef(fss_fits, 7)


# Predict Function for regsubsets()
predict.regsubsets = function(object, newdata, id,...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form,newdata)
  coefi = coef(object, id=id)
  xvars = names(coefi)
  mat[,xvars]%*%coefi
}


## Cross-Validation using k-fold for Forward Selection
k=10
set.seed(300)
folds = sample(1:k, data_len, replace = TRUE)
cv.errors = matrix(NA,k,pred_len, dimnames = list(NULL, paste(1:pred_len)))
for (j in 1:k) {
  fss.fit = regsubsets(Q46_2~., data = traindata_omitna[folds!=j,], nvmax = pred_len, method = "forward")
  for (i in 1:pred_len) {
    pred = predict(fss.fit, traindata_omitna[folds==j,], id=i)
    cv.errors[j,i] = mean((traindata_omitna$Q46_2[folds==j]-pred)^2)
  }
}
mean.cv.errors = apply(cv.errors,2,mean)
par(mfrow=c(1,1))
plot(mean.cv.errors, type='b', xlab="Number of Predictors",ylab="10-Fold Cross-Validation Error")
min(mean.cv.errors)
which.min(mean.cv.errors)
points(7, mean.cv.errors[7], col ="red",cex =2, pch =20)
coef(fss_fits, 7)

## Forward names
fss_coef = coef(fss_fits, 47)
fss_coef_noint = fss_coef[fss_coef[] != 0][-1]
fss_names = names(fss_coef_noint)
fss_params_sum = paste(fss_names, collapse = "+")
fss_params_sum


fss.fit.train = lm(Q46_2~Q5_1+Q25_6+Q29_4+Q38+Q42+Q100+Q66_3, data = traindata_omitna)
fss.pred.train = predict(fss.fit.train, newdata=testdata)
summary(fss.fit.train)
mean((fss.pred.train-testdata$Q46_2)^2)

# calculate R2 and Adj-R2
predicted_values = predict(fss.fit.train, traindata_omitna)
residuals = traindata_omitna$Q46_2 - predicted_values
RSS = sum(residuals^2)
TSS = sum((traindata_omitna$Q46_2 - mean(traindata_omitna$Q46_2))^2)
R.2 = 1-(RSS/TSS) ; R.2
adj.r.2 = 1-((RSS/(479-7-1))*(479-1)/TSS) ; adj.r.2
mean(residuals^2)


####### Backward Step-wise Selection
set.seed(300)
bkss_fits = regsubsets(Q46_2~., data = traindata_omitna, nvmax = 154, method = "backward")
bkss_fits_summary = summary(bkss_fits)

par(mfrow=c(2,2))
plot(bkss_fits_summary$rss, xlab="Number of Variables",ylab="RSS", type = "l")
which.min(bkss_fits_summary$rss)
points(154, bkss_fits_summary$rss[154], col ="red",cex =2, pch =20)

plot(bkss_fits_summary$adjr2, xlab="Number of Variables",ylab="Adj-R2", type = "l")
which.max(bkss_fits_summary$adjr2)
points(65, bkss_fits_summary$adjr2[65], col ="red",cex =2, pch =20)

plot(bkss_fits_summary$cp, xlab="Number of Variables",ylab="cp", type = "l")
which.min(bkss_fits_summary$cp)
points(29, bkss_fits_summary$cp[29], col ="red",cex =2, pch =20)

plot(bkss_fits_summary$bic, xlab="Number of Variables",ylab="bic", type = "l")
which.min(bkss_fits_summary$bic)
points(7, bkss_fits_summary$bic[7], col ="red",cex =2, pch =20)

coef(bkss_fits, 7)


## Cross-Validation using k-fold for Backward subset selection
k=10
set.seed(300)
folds = sample(1:k, data_len, replace = TRUE)
cv.errors = matrix(NA,k,pred_len, dimnames = list(NULL, paste(1:pred_len)))
for (j in 1:k) {
  bkss.fit = regsubsets(Q46_2~., data = traindata_omitna[folds!=j,], nvmax = pred_len, method = "backward")
  for (i in 1:pred_len) {
    pred = predict(bkss.fit, traindata_omitna[folds==j,], id=i)
    cv.errors[j,i] = mean((traindata_omitna$Q46_2[folds==j]-pred)^2)
  }
}
mean.cv.errors = apply(cv.errors,2,mean)
par(mfrow=c(1,1))
plot(mean.cv.errors, type='b', xlab="Number of Predictors", ylab="10-Fold Cross-Validation Error")
which.min(mean.cv.errors)
min(mean.cv.errors)
points(6, mean.cv.errors[6], col ="red",cex =2, pch =20)
coef(bkss_fits, 6)

## Backward names
bkss_coef = coef(bkss_fits, 6)
bkss_coef_noint = bkss_coef[bkss_coef[] != 0][-1]
bkss_names = names(bkss_coef_noint)
bkss_params_sum <- paste(bkss_names, collapse = "+")
bkss_params_sum
bkss_coef


bkss.fit.train = lm(Q46_2~Q5_1+Q29_4+Q38+Q42+Q100+Q66_3, data = traindata_omitna)
bkss.pred.train = predict(bkss.fit.train, newdata=testdata)
summary(bkss.fit.train)
mean((bkss.pred.train-testdata$Q46_2)^2)

# calculate R2 and Adj-R2
predicted_values = predict(bkss.fit.train, traindata_omitna)
residuals = traindata_omitna$Q46_2 - predicted_values
RSS = sum(residuals^2)
TSS = sum((traindata_omitna$Q46_2 - mean(traindata_omitna$Q46_2))^2)
R.2 = 1-(RSS/TSS) ; R.2
adj.r.2 = 1-((RSS/(479-6-1))*(479-1)/TSS) ; adj.r.2
mean(residuals^2)





##### Ridge Regression
x = model.matrix(Q46_2~., data = traindata_omitna,)[,-1]
y = traindata_omitna$Q46_2

grid = 10^seq(10,-2,length=100)
ridge_fit = glmnet(x,y, alpha = 0, lambda = grid)
dim(coef(ridge_fit))

set.seed(300)
cv.out = cv.glmnet(x, y, alpha=0)
par(mfrow=c(1,2))
plot(cv.out)
plot(cv.out$lambda,cv.out$cvm, xlab = "Lambda", ylab = "Mean Cross-Validated Error")
bestlam = cv.out$lambda.min ; bestlam
predict(ridge_fit, type = "coefficients", s=bestlam)[1:155,]


## Cross-Validation using k-fold for ridge regression
k=10
set.seed(300)
folds = sample(1:k, data_len, replace = TRUE)
cv.errors_r = rep(NA,k)
for (j in 1:k) {
  x_r = model.matrix(Q46_2~., data = traindata_omitna[folds!=j,],)[,-1]
  y_r = traindata_omitna[folds!=j,]$Q46_2
  ridge.fit_r = glmnet(x_r,y_r, alpha = 0, lambda = grid)
  set.seed(300)
  cv.out_r = cv.glmnet(x_r, y_r, alpha=0)
  bestlam_r = cv.out_r$lambda.min
  x_r_test = model.matrix(Q46_2~., data = traindata_omitna[folds==j,],)[,-1]
  y_r_test = traindata_omitna[folds==j,]$Q46_2
  ridge.pred_r = predict(ridge.fit_r, s=bestlam_r, newx = x_r_test)
  cv.errors_r[j] = mean((ridge.pred_r-y_r_test)^2)
}

cv.errors_r
mean_cv.errors_r = mean(cv.errors_r)
mean_cv.errors_r

x_test = model.matrix(Q46_2~., data = testdata,)[,-1]
y_test = testdata$Q46_2
ridge.pred = predict(ridge_fit, s=bestlam, newx = x_test)
mean((ridge.pred-testdata$Q46_2)^2)

# calculate R2 and Adj-R2
predicted_values = predict(ridge_fit, s=bestlam, newx = x)
residuals = traindata_omitna$Q46_2 - predicted_values
RSS = sum(residuals^2)
TSS = sum((traindata_omitna$Q46_2 - mean(traindata_omitna$Q46_2))^2)
R.2 = 1-(RSS/TSS) ; R.2
adj.r.2 = 1-((RSS/(479-154-1))*(479-1)/TSS) ; adj.r.2
mean(residuals^2)



##### Lasso Regression
lasso_fit = glmnet(x,y, alpha = 1, lambda = grid)
par(mfrow=c(1,1))
plot(lasso_fit)
set.seed(300)
cv.out = cv.glmnet(x, y, alpha=1)
par(mfrow=c(1,2))
plot(cv.out)
plot(cv.out$lambda,cv.out$cvm, xlab = "Lambda", ylab = "Mean Cross-Validated Error")
bestlam = cv.out$lambda.min ; bestlam
lasso_coef = predict(lasso_fit, type = "coefficients", s=bestlam)[1:155,]
lasso_coef


## Lasso Names
lasso_coef_noint = lasso_coef[lasso_coef[] != 0][-1]
lasso_coef_noint
lasso_names = names(lasso_coef_noint)
lasso_params_sum <- paste(lasso_names, collapse = "+")
lasso_params_sum
lasso_names

  
## Cross-Validation using k-fold for lasso regression
k=10
set.seed(300)
folds = sample(1:k, data_len, replace = TRUE)
cv.errors_l = rep(NA,k)
for (j in 1:k) {
  x_l = model.matrix(Q46_2~., data = traindata_omitna[folds!=j,],)[,-1]
  y_l = traindata_omitna[folds!=j,]$Q46_2
  lasso.fit_l = glmnet(x_l,y_l, alpha = 1, lambda = grid)
  set.seed(300)
  cv.out_l = cv.glmnet(x_l, y_l, alpha=1)
  bestlam_l = cv.out_l$lambda.min
  x_l_test = model.matrix(Q46_2~., data = traindata_omitna[folds==j,],)[,-1]
  y_l_test = traindata_omitna[folds==j,]$Q46_2
  lasso.pred_l = predict(lasso.fit_l, s=bestlam_l, newx = x_l_test)
  cv.errors_l[j] = mean((lasso.pred_l-y_l_test)^2)
}

cv.errors_l
mean_cv.errors_l = mean(cv.errors_l)
mean_cv.errors_l


x_test = model.matrix(Q46_2~., data = testdata,)[,-1]
y_test = testdata$Q46_2
lasso.pred = predict(lasso_fit, s=bestlam, newx = x_test)
mean((lasso.pred-testdata$Q46_2)^2)

# calculate R2 and Adj-R2
predicted_values = predict(lasso_fit, s=bestlam, newx = x)
residuals = traindata_omitna$Q46_2 - predicted_values
RSS = sum(residuals^2)
TSS = sum((traindata_omitna$Q46_2 - mean(traindata_omitna$Q46_2))^2)
R.2 = 1-(RSS/TSS) ; R.2
adj.r.2 = 1-((RSS/(479-21-1))*(479-1)/TSS) ; adj.r.2
mean(residuals^2)



## Re-assigning train and test - 04/29/23
train = traindata
test = testdata


tree.survey<-tree(Q46_2~.,data=train)
par(mfrow=c(1,1))
plot(tree.survey)
text(tree.survey,pretty=0)
summary(tree.survey)

cv.survey = cv.tree(tree.survey)
par(mfrow=c(1,2))
plot(cv.survey$size, cv.survey$dev, type="b", xlab = "Tree Size", ylab = "Deviance")
plot(cv.survey$k, cv.survey$dev, type="b", xlab = "Cost-Complexity Parameter (alpha)", ylab = "Deviance")

which.min(cv.survey$dev)
cv.survey$size[which.min(cv.survey$dev)]
cv.survey$dev[which.min(cv.survey$dev)]

set.seed(300)
prune.survey<-prune.tree(tree.survey,best=11)
par(mfrow=c(1,1))
plot(prune.survey)
text(prune.survey, pretty=0)

# Pruned Tree Test Error
tree.pred = predict(prune.survey,test)
mean((tree.pred-test$Q46_2)^2)

# Un-Pruned Tree Test Error
tree.pred<-predict(tree.survey,test)
mean((tree.pred-test$Q46_2)^2)



##### Tree based methods - Classification
tree.survey_class<-rpart(Q46_2~.,data=train, method = "class")
tree.pred_class<-predict(tree.survey_class, newdata = test, type = "class")
tree.pred_class
plot(tree.survey_class)
text(tree.survey_class, pretty=0)
mean(tree.pred_class != test$Q46_2)


##### Bagging & Random Forest - Regression
# Varying mtry values
mtry_val = 1:154
test_errors = rep(NA, 154)
for (i in mtry_val) {
  set.seed(300)
  rf.survey_mtry = randomForest(Q46_2~., data = train, mtry = i, 
                                ntree = 100,importance=TRUE)
  yhat.rf_sur_mtry = predict(rf.survey_mtry, newdata = test)
  test_errors[i] = mean((yhat.rf_sur_mtry-test$Q46_2)^2)
}
plot(mtry_val, test_errors, type = 'b', xlab = "mtry Values", ylab = "Test Errors")
which.min(test_errors)

# Varying ntree values
ntree_val = 5:500
test_errors_2 = rep(NA, length(ntree_val))
for (j in ntree_val) {
  set.seed(300)
  rf.survey_ntree = randomForest(Q46_2~., data = train, mtry = 128,ntree = j,importance=TRUE)
  yhat.rf_sur_ntree = predict(rf.survey_ntree, newdata = test)
  test_errors_2[j-4] = mean((yhat.rf_sur_ntree-test$Q46_2)^2)
}
plot(ntree_val, test_errors_2, type = 'b', xlab = "ntree Values", ylab = "Test Errors")
min(test_errors_2)
test_errors_2[380]



# Bagging - Regression
set.seed(300)
bag.survey = randomForest(Q46_2~., data=train, importance=TRUE, mtry = 128, 
                          ntree = 100, maxnodes = 50)
plot(bag.survey)
yhat.bag = predict(bag.survey, newdata=test)
mean((yhat.bag-test$Q46_2)^2)

varImpPlot(bag.survey)
vimportance(bag.survey)
plot(randomForest::importance(bag.survey))
library(vip)
vip(bag.survey)
vi(bag.survey, type = "gcv")
varImp(bag.survey)



# calculate R2 and Adj-R2
predicted_values = predict(bag.survey, newdata=train)
residuals = train$Q46_2 - predicted_values
RSS = sum(residuals^2)
TSS = sum((train$Q46_2 - mean(train$Q46_2))^2)
R.2 = 1-(RSS/TSS) ; R.2
adj.r.2 = 1-((RSS/(479-154-1))*(479-1)/TSS) ; adj.r.2
mean(residuals^2)



##### Bagging & Random Forest - Classification
traindata_Q46_2_Class = traindata_omitna
traindata_Q46_2_Class$Q46_2 = as.factor(traindata_Q46_2_Class$Q46_2)
testdata_Q46_2_Class = testdata
testdata_Q46_2_Class$Q46_2 = as.factor(testdata_Q46_2_Class$Q46_2)
# Varying mtry values
mtry_val = 1:154
test_errors_3 = rep(NA, 154)
for (i in mtry_val) {
  set.seed(300)
  rf.survey_mtry = randomForest(Q46_2~., data = traindata_Q46_2_Class, mtry = i, ntree = 500,importance=TRUE)
  yhat.rf_sur_mtry = predict(rf.survey_mtry, newdata = testdata_Q46_2_Class)
  test_errors_3[i] = mean(yhat.rf_sur_mtry != testdata_Q46_2_Class$Q46_2)
}
plot(mtry_val, test_errors_3, type = 'b', xlab = "mtry Values", ylab = "Test Errors")
which.min(test_errors_3)


# Bagging - Classification
set.seed(300)
bag.survey_class = randomForest(Q46_2~.,data=traindata_Q46_2_Class, mtry=136,
                                importance=TRUE, ntree = 100, maxnodes = 50)
yhat.bag_class = predict(bag.survey_class, newdata=testdata_Q46_2_Class)
mean(yhat.bag_class != testdata_Q46_2_Class$Q46_2)

yhat.bag_class
mean((as.integer(yhat.bag_class)- as.integer(testdata_Q46_2_Class$Q46_2))^2)


# calculate R2 and Adj-R2
predicted_values = predict(bag.survey_class, newdata=traindata_Q46_2_Class)
residuals = (as.integer(traindata_Q46_2_Class$Q46_2)) - (as.integer(predicted_values))
RSS = sum(residuals^2)
TSS = sum((train$Q46_2 - mean(train$Q46_2))^2)
R.2 = 1-(RSS/TSS) ; R.2
adj.r.2 = 1-((RSS/(479-154-1))*(479-1)/TSS) ; adj.r.2
mean(residuals^2)


set.seed(300)
rf.survey<-randomForest(Q46_2~.,data=train,importance=TRUE)
yhat.rf<-predict(rf.survey,newdata=test)
mean((yhat.rf-test$Q46_2)^2)
importance(rf.survey)
varImpPlot(rf.survey)
summary(rf.survey)



##### LDA
lda.fit = lda(Q46_2~Q5_1+Q25_6+Q29_4+Q38+Q42+Q100+Q66_3, data = train)
lda.pred = predict(lda.fit, test)
summary(lda.pred)
table(lda.pred$class, test$Q46_2)
mean(lda.pred$class != test$Q46_2)
mean(((as.integer(as.vector(lda.pred$class))) - test$Q46_2)^2)
plot(lda.fit)
lda.fit

lda.pred = predict(lda.fit, train)
mean(((as.integer(as.vector(lda.pred$class))) - train$Q46_2)^2)


# calculate R2 and Adj-R2
predicted_values = predict(lda.fit, train)
residuals = train$Q46_2 - as.integer(predicted_values$class)
RSS = sum(residuals^2)
TSS = sum((train$Q46_2 - mean(train$Q46_2))^2)
R.2 = 1-(RSS/TSS) ; R.2
adj.r.2 = 1-((RSS/(479-7-1))*(479-1)/TSS) ; adj.r.2
mean(residuals^2)


##### QDA
qda.fit = qda(Q46_2~Q5_1+Q25_6+Q29_4+Q38+Q42+Q100+Q66_3, data = train)
qda.pred = predict(qda.fit, test)
summary(qda.pred)
table(qda.pred$class, test$Q46_2)
mean(qda.pred$class != test$Q46_2)
mean(((as.integer(as.vector(qda.pred$class))) - test$Q46_2)^2)

# calculate R2 and Adj-R2
predicted_values = predict(qda.fit, train)
residuals = train$Q46_2 - as.integer(predicted_values$class)
RSS = sum(residuals^2)
TSS = sum((train$Q46_2 - mean(train$Q46_2))^2)
R.2 = 1-(RSS/TSS) ; R.2
adj.r.2 = 1-((RSS/(479-7-1))*(479-1)/TSS) ; adj.r.2
mean(residuals^2)

##### Boosting
set.seed(300)
help(gbm)
boost.survey = gbm(Q46_2~., data = train, distribution = "gaussian", n.trees=5000,
                   interaction.depth = 1)
summary(boost.survey)
yhat.boost = predict(boost.survey, newdata=test, n.trees = 5000)
mean((yhat.boost-test$Q46_2)^2)


library(e1071)
##### SVM - Classification #####
## Linear
set.seed(300)
tune.out = tune(svm, Q46_2~Q5_1+Q25_6+Q29_4+Q38+Q42+Q100+Q66_3, data = traindata_Q46_2_Class, kernel = "linear", 
                ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100, 125, 150, 200)))
summary(tune.out)
bestmod = tune.out$best.model
summary(bestmod)

# Train error - Tuned
svm_linear_trainpred = predict(bestmod, traindata_Q46_2_Class)
table(predict = svm_linear_trainpred, truth = traindata_Q46_2_Class$Q46_2)
mean(svm_linear_trainpred != traindata_Q46_2_Class$Q46_2)
mean((as.integer(svm_linear_trainpred) - as.integer(traindata_Q46_2_Class$Q46_2))^2)

# Test error - Tuned
svm_linear_testpred = predict(bestmod, testdata_Q46_2_Class)
table(predict = svm_linear_testpred, truth = testdata_Q46_2_Class$Q46_2)
mean(svm_linear_testpred != testdata_Q46_2_Class$Q46_2)
mean((as.integer(svm_linear_testpred) - as.integer(test$Q46_2))^2)

# calculate R2 and Adj-R2
predicted_values = svm_linear_trainpred
residuals = train$Q46_2 - as.integer(predicted_values)
RSS = sum(residuals^2)
TSS = sum((train$Q46_2 - mean(train$Q46_2))^2)
R.2 = 1-(RSS/TSS) ; R.2
adj.r.2 = 1-((RSS/(479-7-1))*(479-1)/TSS) ; adj.r.2
mean(residuals^2)



## Radial
set.seed(300)
tune.out = tune(svm, Q46_2~Q5_1+Q25_6+Q29_4+Q38+Q42+Q100+Q66_3, data = traindata_Q46_2_Class, kernel = "radial",
                ranges = list(cost=c(0.1, 0.5, 1, 10, 100, 1000),
                              gamma=c(0.05, 0.1, 0.3, 0.5, 1, 2, 3, 4)))
summary(tune.out)
bestmod = tune.out$best.model
summary(bestmod)

# Train error - Tuned
svm_radial_trainpred = predict(bestmod, traindata_Q46_2_Class)
table(predict = svm_radial_trainpred, truth = traindata_Q46_2_Class$Q46_2)
mean(svm_radial_trainpred != traindata_Q46_2_Class$Q46_2)
mean((as.integer(svm_radial_trainpred) - as.integer(traindata_Q46_2_Class$Q46_2))^2)

# Test error - Tuned
svm_radial_testpred = predict(bestmod, testdata_Q46_2_Class)
table(predict = svm_radial_testpred, truth = testdata_Q46_2_Class$Q46_2)
mean(svm_radial_testpred != testdata_Q46_2_Class$Q46_2)
mean((as.integer(svm_radial_testpred) - as.integer(testdata_Q46_2_Class$Q46_2))^2)

# calculate R2 and Adj-R2
predicted_values = svm_radial_trainpred
residuals = train$Q46_2 - as.integer(predicted_values)
RSS = sum(residuals^2)
TSS = sum((train$Q46_2 - mean(train$Q46_2))^2)
R.2 = 1-(RSS/TSS) ; R.2
adj.r.2 = 1-((RSS/(479-7-1))*(479-1)/TSS) ; adj.r.2
mean(residuals^2)

## Polynomial
set.seed(300)
tune.out = tune(svm, Q46_2~Q5_1+Q25_6+Q29_4+Q38+Q42+Q100+Q66_3, data = traindata_Q46_2_Class, kernel="polynomial",
                ranges = list(cost=c(0.1, 0.5, 1, 10, 100, 1000),
                              degree=c(2, 3, 4, 5)))
summary(tune.out)
bestmod = tune.out$best.model
summary(bestmod)

# Train error - Tuned
svm_poly_trainpred = predict(bestmod, traindata_Q46_2_Class)
table(predict = svm_poly_trainpred, truth = traindata_Q46_2_Class$Q46_2)
mean(svm_poly_trainpred != traindata_Q46_2_Class$Q46_2)

# Test error - Tuned
svm_poly_testpred = predict(bestmod, testdata_Q46_2_Class)
table(predict = svm_poly_testpred, truth = testdata_Q46_2_Class$Q46_2)
mean(svm_poly_testpred != testdata_Q46_2_Class$Q46_2)
mean((as.integer(svm_poly_testpred) - as.integer(test$Q46_2))^2)




##### SVM - Regression #####
## Linear
set.seed(300)
tune.out = tune(svm, Q46_2~Q5_1+Q25_6+Q29_4+Q38+Q42+Q100+Q66_3, data = train, kernel = "linear", 
                ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100, 150, 200)))
summary(tune.out)
bestmod = tune.out$best.model
summary(bestmod)

# Train error - Tuned
svm_linear_trainpred = predict(bestmod, train)
table(predict = svm_linear_trainpred, truth = train$Q46_2)
mean((svm_linear_trainpred - train$Q46_2)^2)

# Test error - Tuned
svm_linear_testpred = predict(bestmod, test)
table(predict = svm_linear_testpred, truth = test$Q46_2)
mean((svm_linear_testpred - test$Q46_2)^2)



## Radial
set.seed(300)
tune.out = tune(svm, Q46_2~Q5_1+Q25_6+Q29_4+Q38+Q42+Q100+Q66_3, data = train, kernel = "radial",
                ranges = list(cost=c(0.1, 0.5, 1, 10, 100, 1000),
                              gamma=c(0.05, 0.1, 0.3, 0.5, 1, 2, 3, 4)))
summary(tune.out)
bestmod = tune.out$best.model
summary(bestmod)

# Train error - Tuned
svm_radial_trainpred = predict(bestmod, train)
table(predict = svm_radial_trainpred, truth = train$Q46_2)
mean((svm_radial_trainpred - train$Q46_2)^2)

# Test error - Tuned
svm_radial_testpred = predict(bestmod, test)
table(predict = svm_radial_testpred, truth = test$Q46_2)
mean((svm_radial_testpred - test$Q46_2)^2)



## Polynomial
set.seed(300)
tune.out = tune(svm, Q46_2~., data = train, kernel="polynomial",
                ranges = list(cost=c(0.1, 0.5, 1, 10, 100, 1000),
                              degree=c(2, 3, 4, 5)))
summary(tune.out)
bestmod = tune.out$best.model
summary(bestmod)

# Train error - Tuned
svm_poly_trainpred = predict(bestmod, train)
table(predict = svm_poly_trainpred, truth = train$Q46_2)
mean((svm_poly_trainpred - train$Q46_2)^2)

# Test error - Tuned
svm_poly_testpred = predict(bestmod, test)
table(predict = svm_poly_testpred, truth = test$Q46_2)
mean((svm_poly_testpred - test$Q46_2)^2)





##### Principal Component Regression #####
library(pls)
set.seed(300)
pcr.fit = pcr(Q46_2~Q5_1+Q25_6+Q29_4+Q38+Q42+Q100+Q66_3, data = train, 
              scale = TRUE, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")
pcr.pred = predict(pcr.fit, test[,-155], ncomp = 2)
mean((pcr.pred - test$Q46_2)^2)

pcr_error = rep(NA, 7)
for (i in 1:7) {
  pcr.pred = predict(pcr.fit, test[,-155], ncomp = i)
  pcr_error[i] = mean((pcr.pred - test$Q46_2)^2)
}
min(pcr_error)
which.min(pcr_error)

pcr.pred = predict(pcr.fit, train[,-155], ncomp = 2)
mean((pcr.pred - train$Q46_2)^2)

# calculate R2 and Adj-R2
predicted_values = predict(pcr.fit, train[,-155], ncomp = 2)
residuals = train$Q46_2 - predicted_values
RSS = sum(residuals^2)
TSS = sum((train$Q46_2 - mean(train$Q46_2))^2)
R.2 = 1-(RSS/TSS) ; R.2
adj.r.2 = 1-((RSS/(479-2-1))*(479-1)/TSS) ; adj.r.2
mean(residuals^2)



##### Partial Least Squares #####
set.seed(300)
pls.fit = plsr(Q46_2~Q5_1+Q25_6+Q29_4+Q38+Q42+Q100+Q66_3, data = train, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")

pls_error = rep(NA, 7)
for (i in 1:7) {
  pls.pred = predict(pls.fit, test[,-155], ncomp = i)
  pls_error[i] = mean((pls.pred - test$Q46_2)^2)
}
min(pls_error)
which.min(pls_error)

# calculate R2 and Adj-R2
predicted_values = predict(pls.fit, train[,-155], ncomp = 2)
residuals = train$Q46_2 - predicted_values
RSS = sum(residuals^2)
TSS = sum((train$Q46_2 - mean(train$Q46_2))^2)
R.2 = 1-(RSS/TSS) ; R.2
adj.r.2 = 1-((RSS/(479-2-1))*(479-1)/TSS) ; adj.r.2
mean(residuals^2)
