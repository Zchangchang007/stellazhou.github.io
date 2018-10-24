#######################################
###Data Final Project
###
###Section B, Team 12
###
###Lingqi Nie, Stella Zhou, Mei Zhang, Joanna Sun, Dominique Vidjangni
###Load additional file to install packages
install.packages("pls")
library(pls)
installpkg <- function(x){
  if(x %in% rownames(installed.packages())==FALSE) {
    if(x %in% rownames(available.packages())==FALSE) {
      paste(x,"is not a valid package - please check again...")
    } else {
      install.packages(x)           
    }
    
  } else {
    paste(x,"package already installed...")
  }
}


installpkg <- function(x){
  if(x %in% rownames(installed.packages())==FALSE) {
    if(x %in% rownames(available.packages())==FALSE) {
      paste(x,"is not a valid package - please check again...")
    } else {
      install.packages(x)           
    }
    
  } else {
    paste(x,"package already installed...")
  }
}


### FPR_TPR
FPR_TPR <- function(prediction, actual){
  
  TP <- sum((prediction)*(actual))
  FP <- sum((prediction)*(!actual))
  FN <- sum((!prediction)*(actual))
  TN <- sum((!prediction)*(!actual))
  result <- data.frame( TP, FP, FN, TN, FPR = FP / (FP + TN), TPR = TP / (TP + FN), ACC = (TP+TN)/(TP+TN+FP+FN) )
  
  return (result)
}

BinaryAccuracy <- function(prediction, actual){
  
  TP <- sum((prediction)*(actual))
  FP <- sum((prediction)*(!actual))
  FN <- sum((!prediction)*(actual))
  TN <- sum((!prediction)*(!actual))
  result <-  (TP+TN)/(TP+TN+FP+FN) 
  
  return (result)
}

#################################################
###
#  
## deviance calculations

## pred must be probabilities (0<pred<1) for binomial
deviance <- function(y, pred, family=c("gaussian","binomial")){
  family <- match.arg(family)
  if(family=="gaussian"){
    return( sum( (y-pred)^2 ) )
  }else{
    if(is.factor(y)) y <- as.numeric(y)>1
    return( -2*sum( y*log(pred) + (1-y)*log(1-pred) ) )
  }
}

devianceQR <- function(y, pred, tau){
  return( sum(  tau*max(0, y-pred ) + (1-tau)*max(0, pred-y ) ) )
  
}

## get null devaince too, and return R2
R2 <- function(y, pred, family=c("gaussian","binomial")){
  fam <- match.arg(family)
  if(fam=="binomial"){
    if(is.factor(y)){ y <- as.numeric(y)>1 }
  }
  dev <- deviance(y, pred, family=fam)
  dev0 <- deviance(y, mean(y), family=fam)
  return(1-dev/dev0)
}

#  A collection of functions that are useful 
#  for lasso type of estimators
## 
### Returns the indices for which |x[i]| > tr
support<- function(x, tr = 10e-6) {
  m<- rep(0, length(x))
  for (i in 1:length(x)) if( abs(x[i])> tr ) m[i]<- i
  m <- m[m>0]
  m
}

### Penalty choice for Quantile Regression
lambda.BC<- function(X, R = 1000, tau = 0.5, c = 1, alpha = .05){
  n <- nrow(X)
  sigs <- apply(X,2,norm2n)
  U <- matrix(runif(n * R),n)
  R <- (t(X) %*% (tau - (U < tau)))/(sigs*sqrt(tau*(1-tau)))
  r <- apply(abs(R),2,max)
  c * quantile(r, 1 - alpha) * sqrt(tau*(1-tau))*c(1,sigs)
}


## Selects the Number of Clusters via an Information Criteria
## get AIC (option "A") and BIC (option "B") for the output of kmeans
kIC <- function(fit, rule=c("A","B")){
  df <- length(fit$centers) # K*dim
  n <- sum(fit$size)
  D <- fit$tot.withinss # deviance
  rule=match.arg(rule)
  if(rule=="A")
    #return(D + 2*df*n/max(1,n-df-1))
    return(D + 2*df)
  else
    return(D + log(n)*df)
}

####################Summary data
summary(dfp)
###Drop the only record in Utilities which is "NoSeWa" because we think this property is an outlier and can not be compared with others. 
dfp<-dfp[dfp$Utilities!="NoSeWa",]
###Check duplicated records
cat("The number of duplicated rows are", nrow(dfp) - nrow(unique(dfp)))
###Check N/A values
Check_missing<-sapply(dfp,function(y)length(which(is.na(y)==T)))
Check_missing
###Drop specific columns（which doesn't make sense and some duplicated columns,eg.garge and garage)
drop<-c("Id","Utilities","Street","Alley","MSSubClass","1stFlrSF","2ndFlrSF","MiscFeature","FireplaceQu","PoolQC","Fence","LotFrontage","GarageYrBlt","MasVnrArea","MasVnrType","GargeType","GargeFinish","GargeQual","GargeCond")
DATA <- dfp[,!(names(dfp) %in% drop)]
###DCHECKING
Check_missing<-sapply(DATA,function(y)length(which(is.na(y)==T)))
Check_missing
###Delect all missing value
DATA<-na.omit(DATA)
###Double check
Check_missing<-sapply(DATA,function(y)length(which(is.na(y)==T)))
Check_missing
###find character variable
cat_var <- names(DATA)[which(sapply(DATA, is.character))]
###factor all character variables
DATA$MSZoning<-factor(DATA$MSZoning)
DATA$LotShape<-factor(DATA$LotShape)
DATA$LandContour<-factor(DATA$LandContour)
DATA$LotConfig<-factor(DATA$LotConfig)
DATA$LandSlope<-factor(DATA$LandSlope)
DATA$Neighborhood<-factor(DATA$Neighborhood)
DATA$Condition1<-factor(DATA$Condition1)
DATA$Condition2<-factor(DATA$Condition2)
DATA$BldgType<-factor(DATA$BldgType)
DATA$HouseStyle<-factor(DATA$HouseStyle)
DATA$RoofStyle<-factor(DATA$RoofStyle)
DATA$RoofMatl<-factor(DATA$RoofMatl)
DATA$Exterior1st<-factor(DATA$Exterior1st)
DATA$Exterior2nd<-factor(DATA$Exterior2nd)
DATA$ExterQual<-factor(DATA$ExterQual)
DATA$ExterCond<-factor(DATA$ExterCond)
DATA$Foundation<-factor(DATA$Foundation)
DATA$BsmtQual<-factor(DATA$BsmtQual)
DATA$BsmtCond<-factor(DATA$BsmtCond)
DATA$BsmtExposure<-factor(DATA$BsmtExposure)
DATA$BsmtFinType1<-factor(DATA$BsmtFinType1)
DATA$BsmtFinType2<-factor(DATA$BsmtFinType2)
DATA$Heating<-factor(DATA$Heating)
DATA$HeatingQC<-factor(DATA$HeatingQC)
DATA$CentralAir<-factor(DATA$CentralAir)
DATA$Electrical<-factor(DATA$Electrical)
DATA$KitchenQual<-factor(DATA$KitchenQual)
DATA$Functional<-factor(DATA$Functional)
DATA$PavedDrive<-factor(DATA$PavedDrive)
DATA$SaleType<-factor(DATA$SaleType)
DATA$SaleCondition<-factor(DATA$SaleCondition)
DATA$GarageFinish<-factor(DATA$GarageFinish)
DATA$GarageQual<-factor(DATA$GarageQual)
DATA$GarageCond<-factor(DATA$GarageCond)

for (Var in names(DATA)) {
  missing <- sum(is.na(DATA[,Var]))
  if (missing > 0) {
    print(c(Var,missing))
  }
}

################################
######
###################RUN VIF TO CHECK MULTICOLLINEARITY
head(DATA)
installed.packages("car")
library(car)
lm.sol <- lm(SalePrice~., data=DATA)
summary(lm.sol)
#GOT SOME  VARIABLES CAUSING MULTICOLLINEARITY BY USING ALIAS
alias(lm(SalePrice~., data=DATA))
lm <- glm(SalePrice~., data=DATA)
summary(lm)
#DOUBLE CHECK AFRER DELETEING THOSE VARIABLES
alias(lm(SalePrice~.-Electrical-TotalBsmtSF-Exterior2nd, data=DATA))
lm <- glm(SalePrice~.-Electrical-TotalBsmtSF-Exterior2nd, data=DATA)
summary(lm)
vif(lm, singular.ok=TRUE)
#Looks good
##########after VIF
lm <- glm(SalePrice~., data=DATA)

###drop variables with multicollinearity issues and get a new data set
drop2<-c("GargeType","Electrical","TotalBsmtSF","Exterior2nd","Neighborhood","Exterior1st")
DATA <- DATA[,!(names(DATA) %in% drop2)]

#####################
##explore the correlation
install.packages("corrplot")
library(dplyr)
library(data.table)
library(ggplot2)
library(corrplot)
setDT(DATA)
numeric_var <- names(DATA)[which(sapply(DATA, is.numeric))]
DATA_cont <- DATA[,.SD,.SDcols = numeric_var]
correlations <- cor(na.omit(DATA_cont[,-1, with = FALSE]))
row_indic <- apply(correlations, 1, function(x) sum(x > 0.5 | x < -0.5) > 1)
correlations<- correlations[row_indic ,row_indic ]
corrplot(correlations, method="square")

corr.SalePrice <- as.matrix(sort(correlations[,'SalePrice'], decreasing = TRUE))
corr.idx <- names(which(apply(corr.SalePrice, 1, function(x) (x > 0.5 | x < -0.5))))
corrplot(as.matrix(correlations[corr.idx,corr.idx]), type = 'upper', method='color', addCoef.col = 'black', tl.cex = .7,cl.cex = .7, number.cex=.7)
View(DATA)

####Unsupervised Data Mining: PCA
SalePrice.pca <- prcomp(DATA_cont,center = TRUE,scale. = TRUE)
names(SalePrice.pca)
print(SalePrice.pca)
summary(SalePrice.pca)
install.packages("ggfortify")
library(ggfortify)
biplot(SalePrice.pca,scale=0, cex=.7)
plot(SalePrice.pca,main="PCA: Variance Explained by Factors")
mtext(side=1, "Factors",  line=1, font=2)

###Backward stepwise
step(glm(SalePrice~., data=DATA),direction="backward")


#glm(formula = SalePrice ~ MSZoning + LotArea + LandContour + 
#LotConfig + LandSlope + Condition1 + Condition2 + BldgType + 
#OverallQual + OverallCond + YearBuilt + RoofMatl + ExterQual + 
#Foundation + BsmtQual + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + 
#BsmtFinSF2 + BsmtUnfSF + LowQualFinSF + GrLivArea + BedroomAbvGr + 
#KitchenAbvGr + KitchenQual + Functional + Fireplaces + GarageType + 
#GarageCars + GarageArea + GarageQual + GarageCond + WoodDeckSF + 
#OpenPorchSF + ScreenPorch + PoolArea + SaleCondition, data = DATA)
View(DATA)
###Check category variables
table(DATA$MSZoning)
table(DATA$LandContour)
table(DATA$LotConfig)
table(DATA$LandSlope)
table(DATA$Condition1)
table(DATA$Condition2)
table(DATA$BldgType)
table(DATA$OverallQual)
table(DATA$RoofMatl)
table(DATA$ExterQual)
table(DATA$Foundation)
table(DATA$BsmtQual)
table(DATA$BsmtExposure)
table(DATA$BsmtFinType1)
table(DATA$LowQualFinSF)
table(DATA$BedroomAbvGr)
table(DATA$KitchenAbvGr)
table(DATA$KitchenQual)
table(DATA$Functional)
table(DATA$SaleCondition)
table(DATA$PoolArea)
table(DATA$GarageCond)
table(DATA$GarageQual)
table(DATA$GarageCars)
table(DATA$GarageType)
table(DATA$Fireplaces)
###select variables,delete outliers
###LotConfig,Condition1,Condition2,Foundation,LowQualFinSF,BedroomAbvGr,KitchenAbvGr,Functional,SaleCondition,PoolArea,GarageCond,GarageQual,Fireplaces
###Based on domain knowledge, we decided to remove the columns : LotConfig, Condition1, Condition2, LowQualFinSF, Functional, PoolArea, RoofMatl

###Remove specific rows 
DATA<-DATA[DATA$GarageCond!="Ex",]
DATA<-DATA[DATA$KitchenAbvGr!="3",]
DATA<-DATA[DATA$GarageQual!="Ex",]
DATA<-DATA[DATA$GarageQual!="Po",]
DATA<-DATA[DATA$SaleCondition!="AdjLand",]
DATA<-DATA[DATA$Foundation!="Wood",]
DATA<-DATA[DATA$Foundation!="Stone",]
DATA<-DATA[DATA$BedroomAbvGr!=0,]
DATA<-DATA[DATA$BedroomAbvGr!=6,]
DATA<-DATA[DATA$Fireplaces!=3,]
DATA<-DATA[DATA$GarageCars!=4,]
DATA<-DATA[DATA$GarageType != "2Types",]
DATA<-DATA[DATA$GarageType != "CarPort",]
DATA<-DATA[DATA$ExterQual != "Fa",]
DATA<-DATA[DATA$GarageCond!="Po",]
DATA<-DATA[DATA$MSZoning!="C (all)",]
View(DATA)
###Remove specific columns
DATA2<-as.data.frame.matrix(DATA) 
drop3<-c("LotConfig", "Condition1", "Condition2", "LowQualFinSF", "Functional", "PoolArea","RoofMatl")
DATA2 <- DATA2[,!(names(DATA2) %in% drop3)]

DATA <- DATA2

###Create train and test subdataset
set.seed(1)
holdout.indices <- sample(nrow(DATA),645)
dfp_train<-DATA[-holdout.indices,]
dfp_test<-DATA[holdout.indices,]
nrow(dfp_train)
nrow(dfp_test)

###
### K Fold Cross Validation
###
### create a vector of fold memberships (random order)
nfold <- 10
n<-nrow(DATA)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
### create an empty dataframe of results
OOS <- data.frame(linear=rep(NA,nfold),null=rep(NA,nfold)) 

install.packages("tree")
library(tree)
### Use a for loop to run through the nfold trails
for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  ## fit the two regressions and null model
  model.linear <-glm(SalePrice ~ MSZoning + LotArea + LandContour + 
                       LandSlope  + BldgType + 
                       OverallQual + OverallCond + YearBuilt + ExterQual + 
                       Foundation + BsmtQual + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + 
                       BsmtFinSF2 + BsmtUnfSF + GrLivArea + BedroomAbvGr + 
                       KitchenAbvGr + KitchenQual + Fireplaces + GarageType + 
                       GarageCars + GarageArea + GarageQual + GarageCond + WoodDeckSF + 
                       OpenPorchSF + ScreenPorch + SaleCondition,data=DATA, subset=train)
  model.tree <- tree(SalePrice~ MSZoning + LotArea + LandContour + 
                       LandSlope  + BldgType + 
                       OverallQual + OverallCond + YearBuilt + ExterQual + 
                       Foundation + BsmtQual + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + 
                       BsmtFinSF2 + BsmtUnfSF + GrLivArea + BedroomAbvGr + 
                       KitchenAbvGr + KitchenQual + Fireplaces + GarageType + 
                       GarageCars + GarageArea + GarageQual + GarageCond + WoodDeckSF + 
                       OpenPorchSF + ScreenPorch + SaleCondition, data=DATA, subset=train)
  model.null<-glm(SalePrice~1,data=DATA,subset=train)
  ## get predictions: so we have probabilities
  pred.linear <- predict(model.linear, newdata=DATA[-train,])
  pred.null <- predict(model.null, newdata=DATA[-train,])
  pred.tree <- predict(model.tree, newdata=DATA[-train,])
  ## calculate and log R2
  OOS$tree[k] <- R2(y=DATA$SalePrice[-train], pred=pred.tree)
  OOS$tree[k]
  # Linear
  OOS$linear[k] <- R2(y=DATA$SalePrice[-train], pred=pred.linear)
  OOS$linear[k]
  #Null
  OOS$null[k] <- R2(y=DATA$SalePrice[-train], pred=pred.null)
  OOS$null[k]
  #Null Model guess
  sum(dfp_train$SalePrice[train])/length(train)
  ## We will loop this nfold times (I setup for 10)
  ## this will print the progress (iteration that finished)
  print(paste("Iteration",k,"of",nfold,"(thank you for your patience)"))
}

### list the mean of the results stored in the dataframe OOS
### we have nfold values in OOS for each model, this computes the mean of them)
colMeans(OOS)
m.OOS <- as.matrix(OOS)
rownames(m.OOS) <- c(1:nfold)
barplot(t(as.matrix(OOS)), beside=TRUE, legend=FALSE, args.legend=c(xjust=1, yjust=0.5),
        ylab= bquote( "Out of Sample " ~ R^2), xlab="Fold", names.arg = c(1:10))
#if you put legend is TRUE in the previous command, you get the labels;
#but this seems to induce some formatting issues.
### If you kept at least 10 folds, we can plot a box blot 
### so see how OOS R2 fluctuates across fold
if (nfold >= 10){
  ### This plots a box plot with the performance of the three models
  names(OOS)[1] <-"xxx"
  ### Lets zoom in  to see better the performance of 
  ### the small and the null model
  boxplot(OOS, col="plum", las = 2, ylab=expression(paste("OOS ",R^2)), xlab="", main="10-fold Cross Validation")
  
}



### Next we will start with Regularization via Lasso
#### install package for Lasso
source("installpackages.R")
source("lasso_aux.R")
installpkg("glmnet")
library(glmnet)
#### Lets run Lasso
#### First lets set up the data for it
#### the features need to be a matrix ([,-1] removes the first column which is the intercept)
Mx<- model.matrix(SalePrice ~ MSZoning + LotArea + LandContour + 
                    LandSlope  + BldgType + 
                    OverallQual + OverallCond + YearBuilt + ExterQual + 
                    Foundation + BsmtQual + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + 
                    BsmtFinSF2 + BsmtUnfSF + GrLivArea + BedroomAbvGr + 
                    KitchenAbvGr + KitchenQual + Fireplaces + GarageType + 
                    GarageCars + GarageArea + GarageQual + GarageCond + WoodDeckSF + 
                    OpenPorchSF + ScreenPorch + SaleCondition, data=DATA)[,-1]
My<- DATA$SalePrice
### This defined the features we will use the matrix Mx (X) and the target My (Y)
###
#### Lasso requires a penalty parameter lambda
num.features <- ncol(Mx)
num.n <- nrow(Mx)
num.churn <- sum(My)
lasso <- glmnet(Mx,My)
lasso$lambda[1:5]
par(mar=c(1.5,1.5,0.75,1.5))
par(mai=c(1.5,1.5,0.75,1.5))
par(mfrow=c(1,2))
coef_ind <- 5
par(mar=c(1.5,0.5,0.75,0.5))
par(mai=c(1.5,0.5,0.75,0.5))
plot(log(lasso$lambda),lasso$beta[coef_ind,], ylab="Coefficient value", main=paste("Coefficient for",colnames(Mx)[coef_ind]),xlab = expression(paste("log(",lambda,")")))
coef_ind <- 2
par(mar=c(1.5,0.5,0.75,0.5))
par(mai=c(1.5,0.5,0.75,0.5))
plot(log(lasso$lambda),lasso$beta[coef_ind,], ylab="Coefficient value", main=paste("Coefficient for",colnames(Mx)[coef_ind]),xlab = expression(paste("log(",lambda,")")))
par(mfrow=c(1,1))
par(mar=c(1.5,1.5,1.5,1.5))
par(mai=c(1.5,1.5,1.5,1.5))
plot(lasso, xvar="lambda", main="# of non-zero coefficients", ylab ="Coefficient values", xlab = expression(paste("log(",lambda,")")))
lassoCV <- cv.glmnet(Mx,My)
par(mar=c(1.5,1.5,2,1.5))
par(mai=c(1.5,1.5,2,1.5))
plot(lassoCV, main="Fitting Graph for CV Lasso \n \n # of non-zero coefficients  ", xlab = expression(paste("log(",lambda,")")))
lassoCV$lambda[which.min(lassoCV$cvm)]
text(log(lassoCV$lambda.min), .95,"min",cex=1)
text(log(lassoCV$lambda.1se), 1,"1se",cex=1)

coef(lassoCV, s = "lambda.min")
lasso <- glmnet(Mx,My)
summary(lasso)
lassoCV$lambda[which.min(lassoCV$cvm)]
coef(lassoCV, s = "lambda.min")
#Final Model Selected
selected<-glm(SalePrice~LotArea+LandContour+LandSlope+BldgType+OverallQual+OverallCond+YearBuilt+ExterQual+Foundation+BsmtQual+BsmtExposure+BsmtFinType1+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+GrLivArea+KitchenAbvGr+KitchenQual+Fireplaces+GarageCars+GarageArea+WoodDeckSF+ScreenPorch+SaleCondition,data=DATA)
#Calculate OOS R-square for Final Model
model.logistic <-glm(SalePrice~LotArea+LandContour+LandSlope+BldgType+OverallQual+OverallCond+YearBuilt+ExterQual+Foundation+BsmtQual+BsmtExposure+BsmtFinType1+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+GrLivArea+KitchenAbvGr+KitchenQual+Fireplaces+GarageCars+GarageArea+WoodDeckSF+ScreenPorch+SaleCondition,data=dfp_train)
pred.logistic <- predict(model.logistic, newdata=dfp_test)
OOS <- R2(y=dfp_test$SalePrice, pred=pred.logistic)
OOS

