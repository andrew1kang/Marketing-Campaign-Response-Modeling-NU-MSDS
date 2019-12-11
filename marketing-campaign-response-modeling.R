#############
#############
####
#### SOLO 3
####
#############
#############

##############################################################################################
setwd('D:/OneDrive/MSDS/450/Solo 3')

load("XYZ_complete_customer_data_frame.RData")
ls()
mydata <- complete.customer.data.frame
str(mydata)
names(mydata)

###############################################################################################
###########  Step 0 - EDA - Getting insights about the data   #################################
###############################################################################################

table(mydata$BUYER_STATUS)
table(mydata$RESPONSE16)
table(mydata$ANY_MAIL_16)
xtabs(~RESPONSE16 + ANY_MAIL_16, data = mydata)

mydata$PRE2009SALES = mydata$LTD_SALES - mydata$YTD_SALES_2009
mydata$PRE2009TRANSACTIONS = mydata$LTD_TRANSACTIONS -
  mydata$YTD_TRANSACTIONS_2009
mydata$cum15QTY <- mydata$QTY0 + mydata$QTY1 + mydata$QTY2 + mydata$QTY3 +
  mydata$QTY4 + mydata$QTY5 + mydata$QTY6 + mydata$QTY7 + mydata$QTY8 +
  mydata$QTY9 + mydata$QTY10 + mydata$QTY11 + mydata$QTY12 + mydata$QTY13 +
  mydata$QTY14 + mydata$QTY15
mydata$cum15TOTAMT <- mydata$TOTAMT0 + mydata$TOTAMT1 + mydata$TOTAMT2 + mydata$TOTAMT3 +
  mydata$TOTAMT4 + mydata$TOTAMT5 + mydata$TOTAMT6 + mydata$TOTAMT7 + mydata$TOTAMT8 +
  mydata$TOTAMT9 + mydata$TOTAMT10 + mydata$TOTAMT11 + mydata$TOTAMT12 + mydata$TOTAMT13 +
  mydata$TOTAMT14 + mydata$TOTAMT15

mean(mydata$PRE2009SALES)
mean(mydata$PRE2009TRANSACTIONS)
mean(mydata$cum15TOTAMT)
mean(mydata$cum15QTY)

#mydata$salepercust <- mydata$PRE2009SALES/mydata$CENSUS_FACT1
meansalepercust <- mean(mydata$PRE2009SALES)
minsalepercust <- min(mydata$PRE2009SALES)
maxsalepercust <- max(mydata$PRE2009SALES)

mydata$salepertrans <- mydata$PRE2009SALES/mydata$PRE2009TRANSACTIONS
mydata$salepercamp <- mydata$cum15TOTAMT/mydata$TOTAL_MAIL_15

mean(mydata$salepertrans, trim = 0.01, na.rm = TRUE)
mean(mydata$salepercamp, trim = 0.05, na.rm = TRUE)


require(rpart)
require(rpart.plot)
require(tree)
require(rattle)
require(caTools)
require(ROCR)
require(ResourceSelection)

library(corrgram)
library(MASS)
library(randomForest)
library(inTrees)
library(pROC)
library(caret)
library(dplyr)

#############
#############
####
#### Correlation Plot
####
#############
#############

library(ggcorrplot)
library(corrplot)
library(ggplot2)
# library(testhat)
library(psych)

###############################################################
##########################map ################################
###############################################################
library(ggmap)
library(gridExtra)
library(ggplot2)


# lets make it a little easier to type
cc <- mydata
ccMap <- subset(cc,select=c(LONG,LAT,RESPONSE16))
ccMap$lon <- -(as.numeric(substr(ccMap$LONG,2,9))/1000000)
ccMap$lat <- (as.numeric(substr(ccMap$LAT,2,9))/1000000)
m1 <- qmplot(lon, lat, data =ccMap[ccMap$RESPONSE16 == 0,], color=I('red'),size = I(0.5), darken = .0) + ggtitle("Response16 = 0")
m2 <- qmplot(lon, lat, data =ccMap[ccMap$RESPONSE16 == 1,], color=I('green'),size = I(0.5), darken = .0) + ggtitle("Response16 = 1")
grid.arrange(m1, m2, nrow=2)


##################################################################################################
###########  Step 1 - prtend you are a domain expert and pick upto 50 'important' predictors  ####
##################################################################################################

# subdat <- subset(mydata, select=c("PRE2009SALES","PRE2009TRANSACTIONS","MED_INC","cum15QTY", "QTY15",
#                                   "cum15TOTAMT", "TOTAMT15","ESTHMVL","EXAGE", "INC_WOUTSCS_AMT_4","ZKITCHEN",
#                                   "SUM_MAIL_16","TOTAL_MAIL_16","salepercamp",
#                                   "ANY_MAIL_16","RESPONSE16","salepertrans"))

subdat <- subset(mydata,select=c(
    'ANY_MAIL_16',
    'cum15QTY',
    'cum15TOTAMT',
    'EXAGE',
    'HOMEOWNR',
    'MED_INC',
    'PRE2009SALES',
    'RESPONSE16',
    'salepercamp',
    'salepertrans',
    'TOTAMT15',
    # 'ZIP',
    'PRE2009TRANSACTIONS',
    'ZCREDIT',
    'NAT_INC',
    'LOR1',
    'NUMBADLT',
    'NUM_CHILD',
    'ZONLINE',
    'BUYER_STATUS'
    ))

str(subdat)
summary(subdat)
## Homeowner vs Renter
# testhat::describe(subdat$HOMEOWNR)
unique(subdat$HOMEOWNR)

## Age
# describe(subdat$EXAGE)
unique(subdat$EXAGE)

## Credit/Income
unique(subdat$NAT_INC)
unique(subdat$MED_INC)

## Family
unique(subdat$NUMBADLT)
unique(subdat$NUM_CHILD)
unique(subdat$LOR1)
unique(subdat$ZCREDIT)
unique(subdat$ZONLINE)
unique(subdat$BUYER_STATUS)

## Sales
# describe(subdat$PRE2009_TRANSACTIONS)
unique(subdat$PRE2009_TRANSACTIONS)
# describe(subdat$PRE2009_SALES)
unique(subdat$PRE2009_SALES)
# describe(subdat$ANY_MAIL_16)
unique(subdat$ANY_MAIL_16)
# describe(subdat$cum15QTY)
unique(subdat$cum15QTY)
# describe(subdat$cum15TOTAMT)
unique(subdat$cum15TOTAMT)
# describe(subdat$TOTAMT15)
unique(subdat$TOTAMT15)
# names(mydata)

#############################################################################################
############### Step 2 - Pick only the people who were sent mailers in Campaign 16 ##########
#############################################################################################

subdat2 <- subset(subdat, ANY_MAIL_16 > 0)
str(subdat2)

#subdat2 <- subset(subdat, EXAGE > 0)
###subdat$ANY_MAIL_16 <- NULL
##unitamtt <- mean(subdat2$TOTAMT)
#subdat2$FRESPONSE16 <- factor(as.factor(subdat2$RESPONSE16))

###########################################################################################
################### Step 3 - EDA - Cleaning up of the subsetted data  #####################
###########################################################################################
head(subdat2,20)
subdat2$EXAGE[subdat2$EXAGE=="U"] <- NA
subdat2$HOMEOWNR[subdat2$HOMEOWNR=="U"] <- NA
subdat2$HOMEOWNR[subdat2$HOMEOWNR==""] <- 0
subdat2$HOMEOWNR[subdat2$HOMEOWNR=="Y"] <- 1
subdat2$ZCREDIT[subdat2$ZCREDIT=="U"] <- NA
subdat2$ZCREDIT[subdat2$ZCREDIT==""] <- 0
subdat2$ZCREDIT[subdat2$ZCREDIT=="Y"] <- 1
subdat2$ZONLINE[subdat2$ZONLINE=="U"] <- NA
subdat2$ZONLINE[subdat2$ZONLINE==""] <- 0
subdat2$ZONLINE[subdat2$ZONLINE=="Y"] <- 1
subdat2[subdat2 == ""] <- NA

subdat2$salepercamp[subdat2$salepercamp=="NaN"] <- 0
subdat2$salepercamp[subdat2$salepercamp=="Inf"] <- 0

subdat2$ACTIVE <- 0
subdat2$ACTIVE[subdat2$BUYER_STATUS == 'ACTIVE'] <- 1
subdat2$LAPSED <- 0
subdat2$LAPSED[subdat2$BUYER_STATUS == 'LAPSED'] <- 1
subdat2$INACTIVE <- 0
subdat2$INACTIVE[subdat2$BUYER_STATUS == 'INACTIVE'] <- 1

subdat2 <- subdat2[,-which(names(subdat2)=='BUYER_STATUS')]

colSums(is.na(subdat2))

subdat2 <- data.frame(lapply(subdat2, function(x) as.numeric(as.character(x))))
subdat2
colSums(is.na(subdat2))

#
# library(missForest)
# subdat2.mis <- prodNA(subdat2,noNA=.1)
# summary(subdat2.mis)
# subdat2.imp <- missForest(subdat2.mis,ntree=32,maxiter=3)
# subdat2.imp
# write.csv(subdat2.imp,file="imputed_solo_3_data.csv")
#
# head(subdat2,20)
# describe(subdat2)

subdat3 <- na.omit(subdat2)
head(subdat3)
str(subdat3)
colSums(is.na(subdat3))

# subdat3$EXAGE <- as.numeric(subdat3$EXAGE)
str(subdat3)
summary(subdat3)
mean(subdat3$EXAGE,na.rm=TRUE)

subdat4 <- subdat3
subdat4$PRE2009TRANSACTIONS <- log(subdat4$PRE2009TRANSACTIONS)
subdat4$cum15QTY <- log(subdat4$cum15QTY)
subdat4$cum15TOTAMT <- log(subdat4$cum15TOTAMT)
subdat4$PRE2009SALES <- log(subdat4$PRE2009SALES)
subdat4$MED_INC <- log(subdat4$MED_INC)
subdat4$salepercamp <- log(subdat4$salepercamp)
subdat4$salepertrans <- log(subdat4$salepertrans)
subdat4$TOTAMT15 <- log(subdat4$TOTAMT15)
subdat4$LOR1 <- log(subdat4$LOR1)
subdat4$NUM_CHILD <- log(subdat4$NUM_CHILD)
subdat4$NUMBADLT <- log(subdat4$NUMBADLT)
subdat4$EXAGE <- log(subdat4$EXAGE)
# subdat4$NAT_INC <- log(subdat4$NAT_INC)
subdat4[subdat4=="-Inf"] = 0

head(subdat4)
str(subdat4)
names(subdat4)
colSums(is.na(subdat4))

subdat3 <- subset(subdat3,select=c(
  # 'ANY_MAIL_16',
  'cum15QTY',
  'cum15TOTAMT',
  'EXAGE',
  'HOMEOWNR',
  'MED_INC',
  'PRE2009SALES',
  'RESPONSE16',
  'salepercamp',
  'salepertrans',
  'TOTAMT15',
  # 'ZIP',
  'PRE2009TRANSACTIONS',
  'ZCREDIT',
  'NAT_INC',
  'LOR1',
  'NUMBADLT',
  'NUM_CHILD',
  'ZONLINE'
  # 'LEARNING_TEST'
  # 'ACTIVE',
  # 'LAPSED',
  # 'INACTIVE'
  ))

subdat4 <- subset(subdat4,select=c(
  # 'ANY_MAIL_16',
  'cum15QTY',
  'cum15TOTAMT',
  'EXAGE',
  'HOMEOWNR',
  'MED_INC',
  'PRE2009SALES',
  'RESPONSE16',
  'salepercamp',
  'salepertrans',
  'TOTAMT15',
  # 'ZIP',
  'PRE2009TRANSACTIONS',
  'ZCREDIT',
  'NAT_INC',
  'LOR1',
  'NUMBADLT',
  'NUM_CHILD',
  'ZONLINE'
  # 'LEARNING_TEST'
  # 'ACTIVE',
  # 'LAPSED',
  # 'INACTIVE'
))
# subdat3 <- na.omit(subdat3)
# str(subdat3)
table(subdat3$RESPONSE16)
table(subdat4$RESPONSE16)
### note: 8705/9784 = 88.97%


### note: 8562/9712 = 88% did not respond####

#########################################################################
######## Step 4 build models - Logistic Regression Model #############
##########################################################################
library(glm2)
library(pROC)
library(ROSE)
#### TRAIN/TEST SPLIT
trainIndex <- createDataPartition(subdat3$RESPONSE16,p=.7,list=FALSE)
train_subdat3 <- subdat3[trainIndex,]
train_subdat3 <- ROSE(RESPONSE16 ~.,N=nrow(subdat3),data=train_subdat3,seed=123)$data
table(train_subdat3$RESPONSE16)
test_subdat3 <- subdat3[-trainIndex,]
nrow(train_subdat3)
nrow(test_subdat3)
nrow(subdat3)
colSums(is.na(train_subdat3))
colSums(is.na(test_subdat3))

train_subdat4 <- subdat4[trainIndex,]
train_subdat4 <- ROSE(RESPONSE16 ~.,N=nrow(subdat4),data=train_subdat4,seed=123)$data
train_subdat4
test_subdat4 <- subdat4[-trainIndex,]
nrow(train_subdat4)
nrow(test_subdat4)
nrow(subdat4)

########
########
####
#### Upper Model
####
########
########
mylogit <- glm(RESPONSE16 ~ .,
               data = train_subdat3, family = "binomial")
summary(mylogit)## AIC: 6356.5

# mylogit.log <- glm(RESPONSE16 ~ .,
#                  data = train_subdat4, family = "binomial")
# summary(mylogit.log)## AIC: 6176.2

#pvalue <- 1 - pchisq(425, df=8)
#pvalue
###7065-6640=425; 9711-9703=8; deviance significant##
###Another GOF tets - Hosmer & lemeshow ###
hoslem.test(train_subdat3$RESPONSE16, fitted(mylogit)) ## p-value: 4.608e-10

# hoslem.test(train_subdat4$RESPONSE16, fitted(mylogit.log)) ##p-value: 0.04243

# test.subdat3[-RESPONSE16]
pred2 <- predict(mylogit,newdata=test_subdat3, type="response")
head(pred2)
# help(predict)
str(pred2)

pred2round <- round(pred2,0)
str(pred2round)
length(pred2round)
# nrow(subdat3)

## create confusion matrix
cm <- xtabs(~RESPONSE16 + pred2round, data = test_subdat3)
cm
accuracy <- sum(diag(cm))/sum(cm)
accuracy ## Accuracy: 88.49693%


pred2 <- predict(mylogit.log,newdata=test_subdat4, type="response")
head(pred2)
str(pred2)

pred2round <- round(pred2,0)
## create confusion matrix
cm <- xtabs(~RESPONSE16 + pred2round, data = test_subdat4)
cm
accuracy <- sum(diag(cm))/sum(cm)
accuracy ## AccuracY: 88.65031%

# (8679+14)/9784
## My accuracy log = 89.00245%
## My accuracy 2 = 88.72649%
## My accuracy 1= 88.84914%

#### accuracy = 8510+35/9712 = 88%####
#### accuracy among responded: 35/(1115+35)= 3%

########
########
####
#### Subdat3
####
########
########
mylogit.lower <- glm(RESPONSE16 ~ 1, data=train_subdat3, family="binomial" )
summary(mylogit.lower) ## AIC: 6794.1

# hoslem.test(subdat3$RESPONSE16, fitted(mylogit.lower))
pred2 <- predict(mylogit.lower,newdata=test_subdat3, type="response")
head(pred2)
str(pred2)

pred2round <- round(pred2,0)
## create confusion matrix
cm <- xtabs(~RESPONSE16 + pred2round, data = test_subdat3)
cm
accuracy <- sum(diag(cm))/sum(cm)
accuracy ## Accuracy: 88.80368%

##Stepwise
stepAIC(mylogit, scope = list(upper=mylogit), direction="both", validation="Chisq", data=train_subdat3)
## Results: subdat4 = AIC 5075
mylogit <- glm(RESPONSE16 ~ cum15QTY + cum15TOTAMT + EXAGE + MED_INC +
                 PRE2009SALES + salepercamp + salepertrans + TOTAMT15 + PRE2009TRANSACTIONS +
                 LOR1 + NUM_CHILD,
               data=train_subdat3, family="binomial" )
summary(mylogit)

# hoslem.test(subdat3$RESPONSE16, fitted(mylogit.lower))
pred2 <- predict(mylogit,newdata=test_subdat3, type="response")
head(pred2)
str(pred2)

pred2round <- round(pred2,0)
## create confusion matrix
cm <- xtabs(~RESPONSE16 + pred2round, data = test_subdat3)
cm
accuracy <- sum(diag(cm))/sum(cm)
accuracy ## Accuracy: 88.59918%

### ROCR curve
ROCRpred <- prediction(pred2,test_subdat3$RESPONSE16)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
abline(a=0,b=1)
auc(test_subdat3$RESPONSE16,pred2) ## AUC: 69.52%

hist(pred2)

######################
######################
## PREDICT ON EVERYTHING
######################
######################

pred2 <- predict(mylogit,newdata=subdat3, type="response")
head(pred2)
str(pred2)

pred2round <- round(pred2,0)
## create confusion matrix
cm <- xtabs(~RESPONSE16 + pred2round, data = subdat3)
cm
accuracy <- sum(diag(cm))/sum(cm)
accuracy ## Accuracy: 88.59918%

### ROCR curve
ROCRpred <- prediction(pred2,subdat3$RESPONSE16)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
abline(a=0,b=1)
auc(subdat3$RESPONSE16,pred2) ## AUC: 69.52%
# performance(ROCRpred,"auc")
hist(pred2)

# library(MLmetrics)

pred2df <- as.data.frame(pred2)
data_all <- cbind(subdat3,pred2)
str(data_all)
mean(data_all$salepertrans)
mean(data_all$salepertrans [data_all$pred2>0.8]) ##mean = 365.477

####################################################################
######## Step 5 Random Forest model ###############################
##################################################################

### putting '.' means includes all the variables
##rf1 <- randomForest(RESPONSE16 ~ .
##                    ,data=subdat3,importance=TRUE,ntree=100)
# rf1 <- randomForest(RESPONSE16 ~ PRE2009SALES+MED_INC+TOTAMT15+
#                       PRE2009TRANSACTIONS+EXAGE+cum15QTY
#                     ,data=subdat3,importance=TRUE,ntree=100)

########
########
####
#### RF1
####
########
########


rf1 <- randomForest(RESPONSE16 ~ .
                    ,data=train_subdat3,
                    importance=TRUE,
                    ntree=100)
summary(rf1)

##getTree(rf1,1,labelVar=TRUE)
##?getTree
print(rf1)
plot(rf1)
importance(rf1)
varImpPlot(rf1)
##how much each variable improves the prediction of its tree##
##compared to the exact same tree without that variable##


#get the prediction probabilities##
rf1p  <- predict(rf1, newdata=test_subdat3,type="response")
head(rf1p)
hist(rf1p)
rf1pdf <- as.data.frame(rf1p)
# rf1pdf
rf1.pred = prediction(rf1p, test_subdat3$RESPONSE16)
rf1.perf = performance(rf1.pred,"tpr","fpr")
plot(rf1.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

auc(test_subdat3$RESPONSE16,rf1p) ## AUC: .6758

#Make a confusion matrix and compute accuracy
##confusion.matrix <- table(rf1p, subdat3$RESPONSE16)
##confusion.matrix

rf1pround <- round(rf1p,0)
cm <- xtabs(~RESPONSE16 + rf1pround, data = subdat3)
cm
accuracy <- sum(diag(cm))/sum(cm)
accuracy ## Accuracy: 84.122266%

## Predict on Everything
rf1p  <- predict(rf1, newdata=subdat3,type="response")
head(rf1p)
hist(rf1p)
rf1pdf <- as.data.frame(rf1p)

rf1.pred = prediction(rf1p, subdat3$RESPONSE16)
rf1.perf = performance(rf1.pred,"tpr","fpr")
plot(rf1.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

auc(subdat3$RESPONSE16,rf1p) ## AUC: .6758

#Make a confusion matrix and compute accuracy
##confusion.matrix <- table(rf1p, subdat3$RESPONSE16)
##confusion.matrix

rf1pround <- round(rf1p,0)
cm <- xtabs(~RESPONSE16 + rf1pround, data = subdat3)
cm
accuracy <- sum(diag(cm))/sum(cm)
accuracy ## Accuracy: 85.01635%

data_alldf <- cbind(subdat3,rf1pdf)
str(data_alldf)
head(data_alldf)
mean(data_alldf$salepertrans)
mean(data_alldf$salepertrans [data_alldf$rf1p>0.1])

#################################################################

########
########
####
#### RF2
####
########
########

rf2 <- randomForest(RESPONSE16 ~ TOTAMT15 + salepercamp + cum15TOTAMT +
                      PRE2009TRANSACTIONS + cum15QTY + PRE2009SALES +
                      MED_INC + NAT_INC + EXAGE + salepertrans + NUM_CHILD + LOR1 + NUMBADLT,
                    data=train_subdat3,importance=TRUE,ntree=100)
summary(rf2)

##getTree(rf1,1,labelVar=TRUE)
##?getTree
print(rf2)
plot(rf2)
importance(rf2)
varImpPlot(rf2)
##how much each variable improves the prediction of its tree##
##compared to the exact same tree without that variable##


#get the prediction probabilities##
rf2p  <- predict(rf2, newdata=test_subdat3,type="response")
head(rf2p)
hist(rf2p)
rf2pdf <- as.data.frame(rf2p)
# rf1pdf
rf2.pred = prediction(rf2p, test_subdat3$RESPONSE16)
rf2.perf = performance(rf2.pred,"tpr","fpr")
plot(rf2.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

auc(test_subdat3$RESPONSE16,rf2p)## AUC: 68.29%

#Make a confusion matrix and compute accuracy
##confusion.matrix <- table(rf1p, subdat3$RESPONSE16)
##confusion.matrix

rf2pround <- round(rf2p,0)
cm <- xtabs(~RESPONSE16 + rf2pround, data = test_subdat3)
cm
accuracy <- sum(diag(cm))/sum(cm)
accuracy ## Accuracy = 84.32709%

#### Predict on Everything
#get the prediction probabilities##

rf2p  <- predict(rf2, newdata=subdat3,type="response")
head(rf2p)
hist(rf2p)
rf2pdf <- as.data.frame(rf2p)
# rf1pdf
rf2.pred = prediction(rf2p, subdat3$RESPONSE16)
rf2.perf = performance(rf2.pred,"tpr","fpr")
plot(rf2.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

auc(subdat3$RESPONSE16,rf2p)## AUC: 68.29%

#Make a confusion matrix and compute accuracy
##confusion.matrix <- table(rf1p, subdat3$RESPONSE16)
##confusion.matrix

rf2pround <- round(rf2p,0)
cm <- xtabs(~RESPONSE16 + rf2pround, data = subdat3)
cm
accuracy <- sum(diag(cm))/sum(cm)
accuracy ## Accuracy = 85.34342%

data_alldf <- cbind(subdat3,rf2pdf)
str(data_alldf)
head(data_alldf)
mean(data_alldf$salepertrans)
mean(data_alldf$salepertrans [data_alldf$rf2p>0.1])


###############################################################
################### CUSTOMER CALCULATIONS
###############################################################

## TARGETED
nrow(data_alldf[data_alldf$rf2p>.1,])
nrow(data_alldf[data_alldf$rf2p>.2,])
nrow(data_alldf[data_alldf$rf2p>.3,])
nrow(data_alldf[data_alldf$rf2p>.4,])
nrow(data_alldf[data_alldf$rf2p>.5,])
nrow(data_alldf[data_alldf$rf2p>.6,])
nrow(data_alldf[data_alldf$rf2p>.7,])
nrow(data_alldf[data_alldf$rf2p>.8,])
nrow(data_alldf[data_alldf$rf2p>.9,])

## SUM REV / TARGETED
sum(data_alldf$salepertrans[data_alldf$rf2p>.1])/nrow(data_alldf[data_alldf$rf2p>.1,])
sum(data_alldf$salepertrans[data_alldf$rf2p>.2])/nrow(data_alldf[data_alldf$rf2p>.2,])
sum(data_alldf$salepertrans[data_alldf$rf2p>.3])/nrow(data_alldf[data_alldf$rf2p>.3,])
sum(data_alldf$salepertrans[data_alldf$rf2p>.4])/nrow(data_alldf[data_alldf$rf2p>.4,])
sum(data_alldf$salepertrans[data_alldf$rf2p>.5])/nrow(data_alldf[data_alldf$rf2p>.5,])
sum(data_alldf$salepertrans[data_alldf$rf2p>.6])/nrow(data_alldf[data_alldf$rf2p>.6,])
sum(data_alldf$salepertrans[data_alldf$rf2p>.7])/nrow(data_alldf[data_alldf$rf2p>.7,])
sum(data_alldf$salepertrans[data_alldf$rf2p>.8])/nrow(data_alldf[data_alldf$rf2p>.8,])
sum(data_alldf$salepertrans[data_alldf$rf2p>.9])/nrow(data_alldf[data_alldf$rf2p>.9,])

nrow(subdat3)
sum(mydata$PRE2009SALES)/sum(mydata$PRE2009TRANSACTIONS)

nrow(mydata[mydata$ANY_MAIL_16==0,])
nrow(mydata[mydata$ANY_MAIL_16>0,])
sum(mydata$TOTAMT16)/nrow(mydata[mydata$ANY_MAIL_16>0,])
nrow(mydata[mydata$ANY_MAIL_16>0,])

###############################################################
################### NO MAILER CUSTOMER CALCULATIONS
###############################################################

############################################################################################
############### Step 2 - Pick only the people who were sent mailers in Campaign 16 ##########
#############################################################################################

subdat2 <- subset(subdat, ANY_MAIL_16 == 0)
str(subdat2)

#subdat2 <- subset(subdat, EXAGE > 0)
###subdat$ANY_MAIL_16 <- NULL
##unitamtt <- mean(subdat2$TOTAMT)
#subdat2$FRESPONSE16 <- factor(as.factor(subdat2$RESPONSE16))

###########################################################################################
################### Step 3 - EDA - Cleaning up of the subsetted data  #####################
###########################################################################################
head(subdat2,20)
subdat2$EXAGE[subdat2$EXAGE=="U"] <- NA
subdat2$HOMEOWNR[subdat2$HOMEOWNR=="U"] <- NA
subdat2$HOMEOWNR[subdat2$HOMEOWNR==""] <- 0
subdat2$HOMEOWNR[subdat2$HOMEOWNR=="Y"] <- 1
subdat2$ZCREDIT[subdat2$ZCREDIT=="U"] <- NA
subdat2$ZCREDIT[subdat2$ZCREDIT==""] <- 0
subdat2$ZCREDIT[subdat2$ZCREDIT=="Y"] <- 1
subdat2$ZONLINE[subdat2$ZONLINE=="U"] <- NA
subdat2$ZONLINE[subdat2$ZONLINE==""] <- 0
subdat2$ZONLINE[subdat2$ZONLINE=="Y"] <- 1
subdat2[subdat2 == ""] <- NA

subdat2$salepercamp[subdat2$salepercamp=="NaN"] <- 0
subdat2$salepercamp[subdat2$salepercamp=="Inf"] <- 0

subdat2$ACTIVE <- 0
subdat2$ACTIVE[subdat2$BUYER_STATUS == 'ACTIVE'] <- 1
subdat2$LAPSED <- 0
subdat2$LAPSED[subdat2$BUYER_STATUS == 'LAPSED'] <- 1
subdat2$INACTIVE <- 0
subdat2$INACTIVE[subdat2$BUYER_STATUS == 'INACTIVE'] <- 1

subdat2 <- subdat2[,-which(names(subdat2)=='BUYER_STATUS')]

colSums(is.na(subdat2))

subdat2 <- data.frame(lapply(subdat2, function(x) as.numeric(as.character(x))))
subdat2
colSums(is.na(subdat2))

#
# library(missForest)
# subdat2.mis <- prodNA(subdat2,noNA=.1)
# summary(subdat2.mis)
# subdat2.imp <- missForest(subdat2.mis,ntree=32,maxiter=3)
# subdat2.imp
# write.csv(subdat2.imp,file="imputed_solo_3_data.csv")
#
# head(subdat2,20)
# describe(subdat2)

subdat3 <- na.omit(subdat2)
head(subdat3)
str(subdat3)
colSums(is.na(subdat3))

# subdat3$EXAGE <- as.numeric(subdat3$EXAGE)
str(subdat3)
summary(subdat3)
mean(subdat3$EXAGE,na.rm=TRUE)

#### Predict on Everything
#get the prediction probabilities##

rf2p  <- predict(rf2, newdata=subdat3,type="response")
head(rf2p)
hist(rf2p)
rf2pdf <- as.data.frame(rf2p)
# rf1pdf
rf2.pred = prediction(rf2p, subdat3$RESPONSE16)
rf2.perf = performance(rf2.pred,"tpr","fpr")
plot(rf2.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

auc(subdat3$RESPONSE16,rf2p)## AUC: 68.29%

#Make a confusion matrix and compute accuracy
##confusion.matrix <- table(rf1p, subdat3$RESPONSE16)
##confusion.matrix

rf2pround <- round(rf2p,0)
cm <- xtabs(~RESPONSE16 + rf2pround, data = subdat3)
cm
accuracy <- sum(diag(cm))/sum(cm)
accuracy ## Accuracy = 85.34342%

data_alldf <- cbind(subdat3,rf2pdf)
str(data_alldf)
head(data_alldf)
mean(data_alldf$salepertrans)
mean(data_alldf$salepertrans [data_alldf$rf2p>0.1])

## TARGETED
nrow(data_alldf[data_alldf$rf2p>.1,])
nrow(data_alldf[data_alldf$rf2p>.2,])
nrow(data_alldf[data_alldf$rf2p>.3,])
nrow(data_alldf[data_alldf$rf2p>.4,])
nrow(data_alldf[data_alldf$rf2p>.5,])
nrow(data_alldf[data_alldf$rf2p>.6,])
nrow(data_alldf[data_alldf$rf2p>.7,])
nrow(data_alldf[data_alldf$rf2p>.8,])
nrow(data_alldf[data_alldf$rf2p>.9,])

## SUM REV / TARGETED
sum(data_alldf$salepertrans[data_alldf$rf2p>.1])/nrow(data_alldf[data_alldf$rf2p>.1,])
sum(data_alldf$salepertrans[data_alldf$rf2p>.2])/nrow(data_alldf[data_alldf$rf2p>.2,])
sum(data_alldf$salepertrans[data_alldf$rf2p>.3])/nrow(data_alldf[data_alldf$rf2p>.3,])
sum(data_alldf$salepertrans[data_alldf$rf2p>.4])/nrow(data_alldf[data_alldf$rf2p>.4,])
sum(data_alldf$salepertrans[data_alldf$rf2p>.5])/nrow(data_alldf[data_alldf$rf2p>.5,])
sum(data_alldf$salepertrans[data_alldf$rf2p>.6])/nrow(data_alldf[data_alldf$rf2p>.6,])
sum(data_alldf$salepertrans[data_alldf$rf2p>.7])/nrow(data_alldf[data_alldf$rf2p>.7,])
sum(data_alldf$salepertrans[data_alldf$rf2p>.8])/nrow(data_alldf[data_alldf$rf2p>.8,])
sum(data_alldf$salepertrans[data_alldf$rf2p>.9])/nrow(data_alldf[data_alldf$rf2p>.9,])
