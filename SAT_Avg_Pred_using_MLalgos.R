library(leaps) 
library(glmnet) 
install.packages("leaps")


setwd("/Users/sandhyasriraman/Downloads")
c_data=read.csv("CollegeData.csv")
c_data=na.omit(c_data)
#Predicting university based on SAT score avg.
#Interaction terms
c_data$scostt4_A= sqrt(c_data$COSTT4_A)
c_data$sTUITIONFEE_OUT=sqrt(c_data$TUITIONFEE_OUT)
c_data$sAVGFACSAL=sqrt(c_data$AVGFACSAL)
c_data$sTUITFTE=sqrt(c_data$TUITFTE)

#adding interation terms
interact_f=model.matrix(~(COSTT4_A+TUITIONFEE_OUT+TUITFTE+AVGFACSAL)^2,c_data)[,c(-1,-2,-3,-4,-5)]
c_data=cbind(c_data,interact_f)
c_data=c_data[-1]
colMeans(x, na.rm = TRUE, dims = 1) #Mean of every column



#Splitting data
set.seed(4574)
train_data = sample(1:nrow(c_data),0.75*nrow(c_data))
test_data = -train_data
train_d = c_data[train_data,]
test_d = c_data[test_data,]
mean(train_d$SAT_AVG)
mean(test_d$SAT_AVG)


#k-fold forward stepwise

p = 8
predict.regsubsets=function(regfit.full,newdata,t){
  form=as.formula(regfit.full$call[[2]])
  mat=model.matrix(form,newdata) 
  coefi=coef(regfit.full,id=t) 
  xvars=names(coefi)
  pred = mat[,xvars]%*%coefi
  return(pred)
}

k=5

d_train = c_data[train_data,]

folds=sample(1:k,nrow(d_train),replace=TRUE)

errorcv=array(NA,dim=c(k,p)) 

for(j in 1:k){
  
  best.fit = regsubsets(SAT_AVG~.,data=d_train[folds!=j,],nvmax=p, method="forward")
  
  
  for(t in 1:p){
    pred = predict.regsubsets(best.fit,d_train[folds==j,],t)
    actual = d_train$SAT_AVG[folds==j]
    
    errorcv[j,t] = mean((actual-pred)^2)
  }
}


mean_errors=apply(errorcv,2,mean)
mean_errors

#best model
final_model = which.min(mean_errors)
final_model

#running on train data
regfit = regsubsets(SAT_AVG~.,data=d_train, nvmax=final_model, method="forward")
summary(regfit)
coef(regfit,id=final_model)

#running on test data for MSE
prediction = predict.regsubsets(regfit, c_data[test_data,], final_model)
actual = c_data$SAT_AVG[test_data];
mse_finalmodel_kfold = mean((actual - prediction)^2) 
mse_finalmodel_kfold






#*******LASSO REGRESSION********

grid= c(0, .001, .01, .1, 1, 10, 100, 1000)

x=model.matrix(c_data$SAT_AVG ~.,c_data)[,-1]
y=c_data$SAT_AVG

#lasso
c_out =cv.glmnet(train_data[x,], train_data[y,], alpha=1, lambda=grid, nlambda=8, nfolds=5) 

final_lam = c_out$lambda.min
final_lam
#Running on train data
laregr = glmnet(train_data[x,], train_data[y,], alpha=1, lambda=bestlam)
coef(laregr)

#running on test data to find MSE
prediction = predict(laregr, tes[y,])
actual = y[test_data]
mse_finalmodel_lasso= mean((actual-prediction)^2) 
mse_finalmodel_lasso


