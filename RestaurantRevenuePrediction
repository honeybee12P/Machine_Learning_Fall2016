# Import Libraries
library(caret)
library(e1071)
library(randomForest)
require(e1071)
library(mice)
library(gbm)


#Getting the arguments from command line
args = commandArgs(trailingOnly=TRUE)
train <- read.table(args[1], header=TRUE,stringsAsFactors = FALSE,sep=",")
test<- read.table(args[2], header=TRUE,stringsAsFactors = FALSE,sep=",")
#Getting the arguments from command line

##########SVM 10-K fold && Random Forest with 10-K Fold###################
paste('Running 10-Fold Split SVM and Random Forest')

# TRAIN PREPROCESS
trainFiltered <- train[c(2,6:43)]
trainFiltered <- trainFiltered[trainFiltered$revenue < 10000000,]
kFolds <- createFolds(trainFiltered$revenue, k = 10, list = TRUE, returnTrain = FALSE)
#set open date to number days since may 1st, 2015
end <- as.Date("2015-03-23")
trainFiltered$Open.Date <- as.numeric(end - as.Date(trainFiltered$Open.Date,"%m/%d/%Y"))
maxs = apply(trainFiltered, MARGIN = 2, max)
mins = apply(trainFiltered, MARGIN = 2, min)
trainFiltered = as.data.frame(scale(trainFiltered, center = mins, scale = maxs - mins))
# TRAIN PREPROCESS


# TEST PREPROCESS
testFiltered <- test[c(2,6:42)]
testFiltered$Open.Date <- as.numeric(end - as.Date(testFiltered$Open.Date,"%m/%d/%Y"))
maxs = apply(testFiltered, MARGIN = 2, max)
mins = apply(testFiltered, MARGIN = 2, min)
testFiltered = as.data.frame(scale(testFiltered, center = mins, scale = maxs - mins))
# TEST PREPROCESS


# Final Model Generation and Prediction
svmModelFin <- svm(formula=revenue~., data=trainFiltered, kernel="radial")            		   # SVM Model Generation
rf <- randomForest(formula=revenue~., data = trainFiltered, importance = TRUE, ntree = 600)    # Random Forest Model Generation


predFinSVM <- predict(svmModelFin, testFiltered, type="response")  # SVM prediction
predFinRF <- predict(rf, testFiltered, type="response")            # Random Forest Prediction

results = matrix(nrow = 10, ncol = 2)
 
for(i in 1:10){
	testK <- trainFiltered[kFolds[[i]],]
	trainK <- trainFiltered[-(kFolds[[i]]),]
	svmModel <- svm(formula=revenue~., data=trainK, kernel="radial")
	rf <- randomForest(formula=revenue~., data = trainK, importance = TRUE, ntree = 600)
	pred <- predict(svmModel,testK, type="response")
	predrf <- predict(rf, testK, type="response")
	results[i, 1] <- sqrt(sum((testK[,39] - pred)^2)/nrow(testK))
	results[i, 2] <- sqrt(sum((testK[,39] - predrf)^2)/nrow(testK))
}

SVMRMSE <- (sum(results[,1])/10*(max(train$revenue)-min(train$revenue))+min(train$revenue))
RFRMSE <- (sum(results[,2])/10*(max(train$revenue)-min(train$revenue))+min(train$revenue))
paste("    RMSE of SVM with K-Fold split: ", SVMRMSE)
paste("    RMSE of Random Forest with K-Fold split: ", RFRMSE)

#kFoldSVMACC <- sqrt(sum((testK[,39] - pred)^2)/nrow(testK))
#rmsesvm8020 <- sqrt(sum((te[,39] - predSVM)^2)/nrow(te))
#paste("RMSE of SVM with 80/20 split: ", rmsesvm8020)

predFinSVM.unscaled <- (predFinSVM)*(max(train$revenue)-min(train$revenue))+min(train$revenue)  # SVM Descaling
predFinRF.unscaled <- (predFinRF)*(max(train$revenue)-min(train$revenue))+min(train$revenue)    # Random Forest Descaling
#Final Model Generation and Prediction #

# Generate Prediction File
finalSVM = data.frame(ID = test$Id, Prediction = predFinSVM.unscaled)
colnames(finalSVM)[2] = "Prediction"
write.csv(finalSVM, "SVM_k_10fold.csv", row.names = F)


finalRF = data.frame(ID = test$Id, Prediction = predFinRF.unscaled)
colnames(finalRF)[2] = "Prediction"
write.csv(finalRF, "RF_k_fold.csv", row.names = F)
paste('       SVM_k_10fold.csv and RF_k_fold.csv files created')
# Generate Prediction File

##########SVM 10-K fold && Random Forest with 10-K Fold###################



##########SVM 80/20 split##############
paste('Running 80/20-split SVM')

#set open date to number days since may 1st, 2015
end <- as.Date("2015-03-23")
train$Open.Date <- as.numeric(end - as.Date(train$Open.Date,"%m/%d/%Y"))
test$Open.Date <- as.numeric(end - as.Date(test$Open.Date,"%m/%d/%Y"))

filtered <- train[c(2,6:43)]
filteredTest <- test[c(2,6:42)]

n <- names(filtered)
f <- as.formula(paste("revenue~",paste(n[!n%in%"revenue"], collapse = "+")))

trainIndex <- sample(1:nrow(filtered), .8*nrow(filtered))
tr <- filtered[trainIndex,]
te <- filtered[-trainIndex,]

svm <- svm(f, data = tr, kernel = "radial")
predSVM <- predict(svm, te[,-39])
rmsesvm8020 <- sqrt(sum((te[,39] - predSVM)^2)/nrow(te))
paste("    RMSE of SVM with 80/20 split: ", rmsesvm8020)


predSVMTEST <- predict(svm, filteredTest)
predScaled <- predSVMTEST*(max(train$revenue)-min(train$revenue))+min(train$revenue)
results = data.frame(Id = test$Id, Prediction = predScaled)
write.csv(results, "SVM_80_20.csv", row.names = F)
paste('       SVM_80_20.csv file created')
##########SVM 80/20 split##############




##########Random Forest with no split#############
paste('Running Random Forest with no split')
names(train) <- c("Id","Open Date","City","City Group","Type","P1","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11","P12","P13","P14","P15","P16","P17","P18","P19","P20","P21","P22","P23","P24","P25","P26","P27","P28","P29","P30","P31","P32","P33","P34","P35","P36","P37","revenue")
names(test) <- c("Id","Open Date","City","City Group","Type","P1","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11","P12","P13","P14","P15","P16","P17","P18","P19","P20","P21","P22","P23","P24","P25","P26","P27","P28","P29","P30","P31","P32","P33","P34","P35","P36","P37")


cols <- sapply(train, is.logical)
train[,cols] <- lapply(train[,cols], as.numeric)


#omitting columns - Open Date, City, Type, City Group
train$`Open Date` <- train$City <- train$Type <- train$`City Group`<- NULL
test$`Open Date` <- test$City <- test$Type <- test$`City Group`<- NULL


#model
rffit <- randomForest(formula=revenue~.,data=train, importance = TRUE)


#performing the predictions
pred <- predict(rffit, test, type="response")


#preparing to write to the output csv file
final = data.frame(ID = test$Id, Prediction = pred)


#naming the columns in the output csv file
colnames(final)[2] = "Prediction"

#Calculating the root mean squared error
paste('    RMSE of Random Forest with no split ',mean(sqrt(rffit$mse)))

#writing the predictions to the csv file
write.csv(final, "RF_no_split.csv", row.names = F)
paste('       RF_no_split.csv file created')



##########Random Forest with no split##############



##########Boosting################### 
paste('Running Boosting')

train <- read.table(args[1], header=TRUE,stringsAsFactors = FALSE,sep=",")
test<- read.table(args[2], header=TRUE,stringsAsFactors = FALSE,sep=",")

#REMOVING ZERO VALUES
train[train==0]<-NA
t <- mice(train,meth='rf',printFlag=FALSE)
train <- complete(t, 1)


#PROCESSING DATE COLUMN 
train$year <- as.numeric(substr(as.character(train$Open.Date),7,10))
train$mon <- as.numeric(substr(as.character(train$Open.Date),1,2))
train$day <- as.numeric(substr(as.character(train$Open.Date),4,5))
train$Open.Date <- as.Date(strptime(train$Open.Date, "%m/%d/%Y"))
train$days <- as.numeric(train$Open.Date - as.Date(paste0(min(train$year),"-01-01")))
test$year <- as.numeric(substr(as.character(test$Open.Date),7,10))
test$mon <- as.numeric(substr(as.character(test$Open.Date),1,2))
test$day <- as.numeric(substr(as.character(test$Open.Date),4,5))
test$Open.Date <- as.Date(strptime(test$Open.Date, "%m/%d/%Y"))
test$days <- as.numeric(test$Open.Date - as.Date(paste0(min(test$year),"-01-01")))


#PROCESSING TYPE AND CITY.GROUP COLUMNS
train$Type = factor(train$Type, levels=c("DT","FC","IL","MB"))
train$City.Group = as.factor(train$City.Group)
test$Type = factor(test$Type, levels=c("DT","FC","IL","MB"))
test$City.Group = as.factor(test$City.Group)


#REMOVING CITY NAMES AND DATE COULMNS
train = train[, -(3)]
test = test[, -(3)]


#MODELLING GRADIENT BOOSTING
tc = trainControl(method = "cv", number = 10)
gG <- expand.grid(interaction.depth = 3,n.trees = 500,shrinkage = c(0.01, 0.1),n.minobsinnode = c(5,10))
gbmFit <- train(revenue~.,data=train,method = "gbm",tuneGrid = gG,verbose = FALSE,trControl = tc)


#GTTING THE REVENUE FOR TEST DATA
result = data.frame(Id = test$Id)
result$Prediction = predict(gbmFit,test)

#RMSE FOR THE ETRAIN DATA
paste("    RMSE on Training Data:", min(gbmFit$results$RMSE))

#WRITING RESULT ON TO THE OUTPUT FILE 
write.csv(result, file = 'Boosting.csv',quote = FALSE, row.names = FALSE)
paste('       Boosting.csv file created')
##########Boosting###################








