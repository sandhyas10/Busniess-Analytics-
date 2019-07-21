library(leaps) 
library(glmnet)

setwd("/Users/sandhyasriraman/Downloads/")
oj=read.csv("OrangeJuice.csv")

oj$Store1=ifelse(oj$StoreID==1,1,0)
oj$Store2=ifelse(oj$StoreID==2,1,0)
oj$Store3=ifelse(oj$StoreID==3,1,0)
oj$Store4=ifelse(oj$StoreID==4,1,0)

oj$StoreID = NULL
oj$X=NULL
oj$SalePriceCH=NULL
oj$SalePriceMM=NULL
oj$PriceDiff=NULL

#oj$oj_Purchase=ifelse(oj$Purchase=="MM",1,0)
#oj$Purchase=NULL


# Splitting into test, train and validation
set.seed(1337)
train = sample(1:nrow(oj),0.75*nrow(oj))
train_tot = oj[train,]
test_oj = oj[-train,]

train_1= sample(1:nrow(train_tot),(2/3)*nrow(train_tot))
train_oj = train_tot[train_1,]
validate_oj=train_tot[-train_1,]

summary(train_oj)

#Logistic Regression for probability of purchase
glm.fit=glm(train_oj$Purchase~.,data=train_oj,family ="binomial")
summary (glm.fit)


#Logitic with elastic net
grid=c(0.001, 0.01, 0.1, 1, 10, 100, 1000)
x1=model.matrix(Purchase~.,train_oj)[,-1]
y1=train_oj$Purchase
cvout = cv.glmnet(x1, y1, alpha=1, family='binomial', lambda=grid) 
lam = cvout$lambda.min
lam
#Running on train data
laregr = glmnet(x1, y1, alpha=1, family='binomial', lambda=lam)
coef(laregr)

#Decision tree to predict purchase
library(tree)
library(MASS)
d_tree = tree(train_oj$Purchase~., train_oj)
plot(d_tree)
text(d_tree, pretty=0)
summary(d_tree)

cv_data = cv.tree(d_tree, FUN = prune.misclass) 
plot(cv_data$size,cv_data$dev,type='b')

f_size = cv_data$size[which.min(cv_data$dev)]
prune_tree = prune.tree(d_tree, best=f_size)

plot(prune_tree)
text(prune_tree, pretty=0)
summary(prune_tree)

#Linear Discriminant Analysis for prob of purchase
library(MASS)

lda_fit = lda(train_oj$Purchase~.,train_oj)
lda_fit
names(lda_fit)

lda_pred = predict(lda_fit,train_oj)
lda_class = lda_pred$class

mean(lda_class!=train_oj[,'Purchase'])

#Finding best k for K-nn using k-fold cross validation

k_vec = 1:100
folds = 5

error_mat = matrix(0, ncol = length(k_vec), nrow = folds)

for (i in 1:folds) {
  cv.ind = sample(1:nrow(train_oj), .8*nrow(train_oj))
  train.cv = train_oj[cv.ind, ]
  test.cv = train_oj[-cv.ind, ]
  for (j in 1:length(k_vec)) {
    knnpred = knn(train.cv[,-1], test.cv[,-1], train.cv[,1], prob = TRUE, k=k_vec[j])
    error_mat[i,j] = sum(knnpred != test.cv[,1])/length(knnpred) 
  }
}

k_final = k_vec[which.min(apply(error_mat, 2, mean))]
k_final

knnpred = knn(train_oj[,-1], train_oj[,-1], train_oj[,1], prob = TRUE, k=k_final)

class_error = mean(knnpred != train_oj[,1])
class_error

# Least classification error

#Lasso&Logit

x_p=model.matrix(Purchase~.,validate_oj)[,-1]
y_p = validate_oj$Purchase

logpred=predict(laregr, x_p, type = 'class')

logerror=mean(logpred != y_p)
logerror

#Decision Tree

treepred=predict(prune_tree, validate_oj, type = 'class')
tree_err=mean(treepred != validate_oj[,1])
tree_err

#LDA

lda_pred=predict(lda_fit, validate_oj)
lda_class = lda_pred$class 
lda_error = 1 - (sum (lda_class == validate_oj[,1])/length(validate_oj[,1]))
lda_error

#K-nn using k from above

knnpre = knn(validate_oj[,-1], validate_oj[,-1], validate_oj[,1], prob = TRUE, k=k_final)
knnerror = mean(knnpre != validate_oj[,1])
knnerror

#K-nn performance testing on train+validation data

k_vec = 1:100
folds = 5
err_matr = matrix(0, ncol = length(k_vec), nrow = folds)
for (i in 1:folds) {
  cv.ind = sample(1:nrow(train_tot), .8*nrow(train_tot))
  train.cv = train_tot[cv.ind, ]
  test.cv = train_tot[-cv.ind, ]
  for (j in 1:length(k_vec)) {
    knnpreds = knn(train.cv[,-1], test.cv[,-1], train.cv[,1], prob = TRUE, k=k_vec[j])
    err_matr[i,j] = sum(knnpreds != test.cv[,1])/length(knnpreds) 
  }
}
k_new = k_vec[which.min(apply(err_matr, 2, mean))]
k_new

knn_test = knn(test_oj[,-1], test_oj[,-1], test_oj[,1], prob = TRUE, k=k_new)

class_error_ontest = mean(knn_test != test_oj[,1])
class_error_ontest


#Logitistic regression to find cut-off probablity
glm.fit1=glm(test_oj$oj_Purchase~test_oj$LoyalCH,data=train_oj,family ="binomial")
summary (glm.fit1)
predict(glm.fit1, test_oj)


#Decision tree to find affecting factors
count=ifelse(test_oj$LoyalCH < 0.482304, 1,0)
sum(count)/534


#knn- confusion matrix


test=test_oj
knnpred = knn(test[,-1], test[,-1], test[,1], prob = TRUE, k=k_final)
table(knnpred,test$Purchase)

