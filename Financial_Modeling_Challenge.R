rm(list=ls())
setwd("~/Documents/KaggleProject")

# Load the libraries used
suppressMessages(library(rhdf5))
suppressMessages(library(dplyr))
suppressMessages(library(caret))
suppressMessages(library(corrplot))
suppressMessages(library(rpart))

# show the hdf structure
# h5ls("train.h5")

# read h5
#axis0  <- h5read("train.h5",name = "train/axis0")
#axis1  <- h5read("train.h5",name = "train/axis1")
#block0_items  <- h5read("train.h5",name = "train/block0_items")
#block0_values  <- h5read("train.h5",name = "train/block0_values")
block1_items  <- h5read("train.h5",name = "train/block1_items")
block1_values  <- h5read("train.h5",name = "train/block1_values")

# Extract the data set "Train"
Train <- as.matrix(block1_values)
Train <- t(Train)
colnames(Train) <- block1_items
Train <- as.data.frame(Train)
dim(Train)

# Remove the observation that contains the NA or NaN values
good <- apply(Train,1,function(x) sum(is.na(x)))==0
Train <- Train[good,]
# Split trainSet and testSet for the modeling train and test
set.seed(2017-01-10)
inTrain <- createDataPartition(Train$y,p=0.7, list=FALSE)
trainSet <- Train[inTrain,]
testSet <- Train[-inTrain,]

# Remove the variables with small variance
#NZV <- nearZeroVar(trainSet)
#trainSet <- trainSet[, -NZV]
#testSet <- testSet[,-NZV]
#dim(trainSet)

# replace the NaN by NA
# trainSet[which(is.nan(trainSet))] <- NA

# replace the NaN by NA
# trainSet <- replace(trainSet, is.nan(trainSet), NA)

# replace the NaN by NA
# is.nan.data.frame <- function(x)
#        do.call(cbind, lapply(x, is.nan))
# trainSet[is.nan(trainSet)] <- NA


# Remove the variables that contain more than 95% NA or NaN values
#AllNA <- sapply(trainSet, function(x) mean(is.na(x))) > 0
#AllNAN <- sapply(trainSet, function(x) mean(is.nan(x))) > 0.1
#trainSet <- trainSet[, AllNA==FALSE ]
#testSet <- testSet[,AllNA==FALSE ]
#dim(trainSet)

# Check out the correlation between each variables
#png(filename = "FinancialCh.png", width = 4800,height = 4800,units = "px")
#rrow <- sample(1:dim(trainSet)[1],50)
# corGraph <- cor(trainSet[rrow, ])
# corrplot(corGraph, order = "FPC", method = "number", type = "lower", 
#         tl.cex = 2.0, tl.col = rgb(0, 0, 0),number.cex = 1.5, number.digits = 2)
#dev.off()

# Remove the correlated variables. This can save time and computing source.
trainSet <- trainSet %>%
        select(-c(fundamental_61,fundamental_11,fundamental_56,fundamental_26,fundamental_10,fundamental_15,fundamental_57,fundamental_41,fundamental_30,fundamental_53,fundamental_42,fundamental_26,fundamental_10,fundamental_60,fundamental_48,fundamental_55,fundamental_11,fundamental_45,fundamental_16,fundamental_34,fundamental_12,fundamental_51,fundamental_43,fundamental_1,fundamental_42,fundamental_30,fundamental_53))

testSet <- testSet %>%
        select(-c(fundamental_61,fundamental_11,fundamental_56,fundamental_26,fundamental_10,fundamental_15,fundamental_57,fundamental_41,fundamental_30,fundamental_53,fundamental_42,fundamental_26,fundamental_10,fundamental_60,fundamental_48,fundamental_55,fundamental_11,fundamental_45,fundamental_16,fundamental_34,fundamental_12,fundamental_51,fundamental_43,fundamental_1,fundamental_42,fundamental_30,fundamental_53))

Train <- Train %>%
        select(-c(fundamental_61,fundamental_11,fundamental_56,fundamental_26,fundamental_10,fundamental_15,fundamental_57,fundamental_41,fundamental_30,fundamental_53,fundamental_42,fundamental_26,fundamental_10,fundamental_60,fundamental_48,fundamental_55,fundamental_11,fundamental_45,fundamental_16,fundamental_34,fundamental_12,fundamental_51,fundamental_43,fundamental_1,fundamental_42,fundamental_30,fundamental_53))


# Modeling fit
R_val <- rep(0,50)

for (i in 51:100) {
        set.seed(i)
        inTrain <- createDataPartition(Train$y,p=0.7, list=FALSE)
        trainSet <- Train[inTrain,]
        testSet <- Train[-inTrain,]
        
        fit <- lm(y~., data = trainSet, na.action=na.exclude)
        
        # Predict the y of testSet to compare with the real y of testSet
        pred_lm <-predict.lm(fit, newdata = testSet,interval = "none")
        
        
        # R value 
        R_sq <- 1- (sum((pred_lm-testSet$y)^2))/(sum((testSet$y-mean(testSet$y))^2))
        R_val[i] <- sign(R_sq)*sqrt(abs(R_sq))
        
}

# Try different varialbes. Maybe some single variables can help.
n <- names(Train)
R_val <- matrix(50,dim(Train)[2]-1)
for (var in 1:(dim(Train)[2]-1)) {
        f <- as.formula(paste("y ~", paste(n[1:var], collapse = " + "))) # y~. does not work in neuralnet model
        for (seed in 1:50) {
                set.seed(seed)
                inTrain <- createDataPartition(Train$y,p=0.7, list=FALSE)
                trainSet <- Train[inTrain,]
                testSet <- Train[-inTrain,]
                
                fit <- lm(f, data = trainSet)
                
                # Predict the y of testSet to compare with the real y of testSet
                pred_lm <-predict.lm(fit, newdata = testSet,interval = "none")
                
                
                # R value 
                R_sq <- 1- (sum((pred_lm-testSet$y)^2))/(sum((testSet$y-mean(testSet$y))^2))
                R_val[seed,var] <- sign(R_sq)*sqrt(abs(R_sq))
                
        }
}

# Try different varialbes. Maybe some single variables can help.
set.seed(2017-01-10)
inTrain <- createDataPartition(Train$y,p=0.7, list=FALSE)
trainSet <- Train[inTrain,]
testSet <- Train[-inTrain,]

n <- names(Train)
R_val <- matrix(50,dim(Train)[2]-1)
lm.ls <- NULL
lm.ls <- as.list(lm.ls) 
for (var in 1:(dim(Train)[2]-1)) {
        f <- as.formula(paste("y ~", paste(n[1:var], collapse = " + "))) # y~. does not work in neuralnet model
           
                lm.ls[[var]] <- lm(f, data = trainSet)
                
                # Predict the y of testSet to compare with the real y of testSet
                #pred_lm <-predict.lm(fit, newdata = testSet,interval = "none")
                
                
                # R value 
                #R_sq <- 1- (sum((pred_lm-testSet$y)^2))/(sum((testSet$y-mean(testSet$y))^2))
                #R_val[seed,var] <- sign(R_sq)*sqrt(abs(R_sq))
        
}

trainSet <- Train[sample(1:223040, 5000),]
inifit<- lm(y~., data=trainSet)
#do.call(anova,lm.ls)
bestfit <- step(inifit,direction = "both")


# check the R-val
pred_lm <-predict.lm(fit_final, newdata = testSet,interval = "none")

R_sq <- 1- (sum((pred_lm-testSet$y)^2))/(sum((testSet$y-mean(testSet$y))^2))
R_val<- sign(R_sq)*sqrt(abs(R_sq))
R_val

# 
fit_final <- bestfit$call
fit_final <- lm(formula = y ~ fundamental_2 + fundamental_3 + fundamental_8 + 
           fundamental_14 + fundamental_17 + fundamental_18 + fundamental_20 + 
           fundamental_21 + fundamental_28 + fundamental_35 + fundamental_36 + 
           fundamental_38 + fundamental_39 + fundamental_46 + fundamental_52 + 
           fundamental_59 + technical_1 + technical_7 + technical_10 + 
           technical_16 + technical_30 + technical_37, data = trainSet)


# conf_lm <- confusionMatrix(data=as.numeric(pred_lm), reference=as.numeric(testSet$y))
# conf_lm

# check the MSE of lm prediction
library(boot)
set.seed(200)
lm.fit <- glm(y~.,data=trainSet)
cv.glm(trainSet,lm.fit,K=10)$delta[1]

# check R_val
        


