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
## "Training_data_Group_smiles.xlsx" in the same folder.
active_doc = getActiveDocumentContext()
file_path = active_doc$path
dir_path = dirname(file_path)
traindata_pathname = paste0(dir_path,"/Training_data_Group_smiles.xlsx")


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


############# Final Model to run for phase-2 ############# 
set.seed(300)
bag.survey_final = randomForest(Q46_2~., mtry=154, data=traindata_omitna, 
                                importance=TRUE, ntree = 100)
testdata_pathname = paste0(dir_path,"/Test_data-1.xlsx")
##### Finding the number of null values in the data.
testdata = read_excel(testdata_pathname,col_names = TRUE, sheet = "Sheet2")
summary(testdata)
dim(testdata)
sapply(testdata,class)
testdata[] = lapply(testdata,as.integer)
sum(is.na.data.frame(testdata))

yhat.bag_test = predict(bag.survey_final, newdata=testdata)
mean((yhat.bag_test-testdata$Q46_2)^2)

# calculate R2 and Adj-R2
predicted_values = predict(fss.fit.train, traindata_omitna)
residuals = traindata_omitna$Q46_2 - predicted_values
RSS = sum(residuals^2)
TSS = sum((traindata_omitna$Q46_2 - mean(traindata_omitna$Q46_2))^2)
R.2 = 1-(RSS/TSS) ; R.2
adj.r.2 = 1-((RSS/(479-7-1))*(479-1)/TSS) ; adj.r.2
mean(residuals^2)



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
fss_coef = coef(fss_fits, 7)
fss_coef_noint = fss_coef[fss_coef[] != 0][-1]
fss_names = names(fss_coef_noint)
fss_params_sum = paste(fss_names, collapse = "+")
fss_params_sum


fss.fit.train = lm(Q46_2~Q5_1+Q25_6+Q29_4+Q38+Q42+Q100+Q66_3, data = traindata_omitna)
fss.pred.train = predict(fss.fit.train, newdata=testdata)
summary(fss.fit.train)
mean((fss.pred.train-testdata$Q46_2)^2)




################# Improved Model #################
## Re-assigning train and test - 04/29/23
train = traindata
test = testdata

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
help("randomForest")
varImp(bag.survey)



# calculate R2 and Adj-R2
predicted_values = predict(bag.survey, newdata=train)
residuals = train$Q46_2 - predicted_values
RSS = sum(residuals^2)
TSS = sum((train$Q46_2 - mean(train$Q46_2))^2)
R.2 = 1-(RSS/TSS) ; R.2
adj.r.2 = 1-((RSS/(479-154-1))*(479-1)/TSS) ; adj.r.2
mean(residuals^2)




##### THE END ####
