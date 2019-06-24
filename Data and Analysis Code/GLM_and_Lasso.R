This file includes the data variables.
The analysis includes the Lasso and GLM Regressions. 



#Install the required libraries. 
library(lars)
library(glmnet)
library(caret)
library(Hmisc)
library(corrplot)

######Read the Data for all covariates######3
Data_Matrix <- read.table("Peru_Complete_Dataset.txt",sep =" ", header = TRUE)


#####Computing the Correlations between variables####33
res2 <- rcorr(as.matrix(Data_Matrix))
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank")


#######Generalized Linear Model#########
model_glm <- glm(Conflicts ~ ., data = Data_Matrix, family = poisson())
summary(model_glm)



############Lasso Regression########

#Seperating into dependent and independent variables. 
dep_var <- c("Conflicts")
dep <- data.matrix(Data_Matrix[dep_var]) 
ind <- data.matrix(Data_Matrix[, !names(Data_Matrix) %in% dep_var])

#Fitting a model without lamba
model_lasso <- glmnet(ind, dep, family = "poisson")
plot.glmnet(model_lasso, xvar = "norm", label = TRUE)
l2=log(model_lasso$lambda) #Regularization Path
matplot(as.matrix(l2),t(coef(model_lasso)[-1,]),type="l",lty=1,ylab="coef",xlab="log(lambda)",main="Regularization Path")

#Cross Validation
set.seed(1234) #Reproductibilty. 
cv_fit <- cv.glmnet(x=ind, y=dep, alpha = 1, nfolds = 10, family = "poisson")
plot.cv.glmnet(cv_fit)
cv_fit$lambda.min

##Fitting the lasso model at best lamda
fit <- glmnet(x=ind, y=dep, alpha = 1, lambda=cv_fit$lambda.min, family = "poisson") 
fit$beta   #These are the coefficients


##############END of PROGRAM############################





