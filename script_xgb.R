time.1 <- Sys.time()
format(time.1, "%d-%m-%Y--%H:%M:%S")

library(readr)
library(xgboost)
library(digest)

# Load train and test datasets
train <- read_csv("./input/train.csv")
test  <- read_csv("./input/test.csv")

feature.names <- names(train)[2:ncol(train)-1]

# Pre-processing
# --------------

# remove the id and target
target = train$target 
test_id <- test$ID
train <- train[,!(names(train) %in% c("ID", "target"))]
test <- test[,!(names(train) == "ID")]

#Lets look at the columns with only one unique value.
col_ct = sapply(train, function(x) length(unique(x)))
cat("Constant features:", length(col_ct[col_ct==1]))
col_ct2 <- unlist(col_ct[unlist(col_ct)==2])

delete_const <- names(col_ct[col_ct==1])
delete_NA56 <- names(which(sapply(train[,(names(train) %in% names(col_ct2))], 
                                         function(x) max(table(x,useNA='always')))==145175))
delete_NA89 <- names(which(sapply(train[,(names(train) %in% names(col_ct2))], 
                                  function(x) max(table(x,useNA='always')))==145142))
delete_NA918 <- names(which(sapply(train[,(names(train) %in% names(col_ct2))], 
                                   function(x) max(table(x,useNA='always')))==144313))

# VARS to delete
# safe to remove VARS with 56, 89 and 918 NA's as they are covered by other VARS
# VAR_0227 = VAR_0228 - delete, they are ids
toDelete <- c(delete_const, delete_NA56, delete_NA89, delete_NA918, "VAR_0227", "VAR_0228")
cat("Features to remove:", length(toDelete))

# delete feature from the list
train <- train[,!(names(train) %in% toDelete)]
test <- test[,!(names(test) %in% toDelete)]

# Find and remove duplicates
train$VAR_0044[train$VAR_0044 == "[]"] <- "false"
dups <- duplicated(lapply(train, digest))
train <- train[!dups]
test <- test[!dups]


#Identify and separate the numeric and non numeric rows.
train_num <- train[, sapply(train, is.numeric)]
train_char <- train[, sapply(train, is.character)]

test_num <- test[, sapply(test, is.numeric)]
test_char <- test[, sapply(test, is.character)]

# Let's work on the character features first

# Convert -1 or [] or blank values to NA
train_char[train_char==-1] = NA
train_char[train_char==""] = NA
train_char[train_char=="[]"] = NA

test_char[test_char==-1] = NA
test_char[test_char==""] = NA
test_char[test_char=="[]"] = NA

# We place the date columns into a new dataframe and parse the dates
train_date = train_char[,grep("JAN1|FEB1|MAR1", train_char),]
test_date = test_char[,grep("JAN1|FEB1|MAR1", test_char),]

# Now lets separate out the dates from the character columns and look at them further.
train_char = train_char[, !colnames(train_char) %in% colnames(train_date)]
train_date = sapply(train_date, function(x) strptime(x, "%d%B%y:%H:%M:%S"))
train_date = do.call(cbind.data.frame, train_date)

test_char = test_char[, !colnames(test_char) %in% colnames(test_date)]
test_date = sapply(test_date, function(x) strptime(x, "%d%B%y:%H:%M:%S"))
test_date = do.call(cbind.data.frame, test_date)


# Create a new DF with time difference between dates and current date
train_tin <- sapply(train_date, function(x) as.numeric(difftime(time.1, x)))
trainDT <- as.data.frame(train_tin)
train_tin <- NULL
test_tin <- sapply(test_date, function(x) as.numeric(difftime(time.1, x)))
testDT <- as.data.frame(test_tin)
test_tin <- NULL

# Create new time and hour variables from VAR_0204
train_time = train_date[, "VAR_0204"]
train_time = sapply(train_date[, "VAR_0204"], function(x) strftime(x, "%H:%M:%S"))
train_time = sapply(train_time, function(x) as.numeric(as.character(substr(x ,1, 2))))
train_time <- as.data.frame(train_time)
trainDT <- cbind(trainDT, train_time)
train_time <- NULL

test_time = test_date[, "VAR_0204"]
test_time = sapply(test_date[, "VAR_0204"], function(x) strftime(x, "%H:%M:%S"))
test_time = sapply(test_time, function(x) as.numeric(as.character(substr(x ,1, 2))))
test_time <- as.data.frame(test_time)
testDT <- cbind(testDT, test_time)
test_time <- NULL

# Split dates into year and month columns
train_date <- sapply(train_date, function(x) strftime(x, "%Y-%m"))
train_date <- as.data.frame(train_date)
for (feature in colnames(train_date)) {
  spltfeat <- strsplit(as.character(train_date[, feature]), '-', fixed=TRUE)
  spltfeat <- sapply(spltfeat, as.numeric)  
  spltfeat <- data.frame(do.call('rbind',spltfeat))
  names(spltfeat) <- c(paste0(feature, "_y"), paste0(feature, "_m"))
  trainDT <- cbind(trainDT, spltfeat)
}
train_date <- NULL

test_date <- as.data.frame(test_date)
for (feature in colnames(test_date)) {
  spltfeat <- strsplit(as.character(test_date[, feature]), '-', fixed=TRUE)
  spltfeat <- sapply(spltfeat, as.numeric)  
  spltfeat <- data.frame(do.call('rbind',spltfeat))
  names(spltfeat) <- c(paste0(feature, "_y"), paste0(feature, "_m"))
  testDT <- cbind(testDT, spltfeat)
}
test_date <- NULL


# find and remove low NZV features
nzv_train <- nearZeroVar(train_char, uniqueCut = 5)
train_char <- train_char[, -nzv_train]
test_char <- test_char[, -nzv_train]

# Fix VAR_0467
train_char$VAR_0467[train_char$VAR_0467 == "Dismissed"] = "Discharged"
train_char$VAR_0467[train_char$VAR_0467 == "Discharge NA"] = "Discharged"
test_char$VAR_0467[test_char$VAR_0467 == "Dismissed"] = "Discharged"
test_char$VAR_0467[test_char$VAR_0467 == "Discharge NA"] = "Discharged"

# Find high level features and delete them:
levels <- lapply(train_char, function(x) length(unique(x)))
highLevels <- names(train_char[, levels >=100])
train_char <- train_char[, -highLevels]
test_char <- test_char[, -highLevels]

# fill out NA data with "missed" value because NA is some information as well
train_char[is.na(train_char)] = "none"
test_char[is.na(test_char)] = "none"

# Assuming text variables are categorical & replacing them with numeric ids
for (f in names(train_char)) {
  if (class(train_char[[f]])=="character") {
    levels <- unique(c(train_char[[f]]))#, test_char[[f]])
    train_char[[f]] <- as.integer(factor(train_char[[f]], levels=levels))
  }
}

for (f in names(test_char)) {
  if (class(test_char[[f]])=="character") {
    levels <- unique(c(test_char[[f]]))#, test_char[[f]])
    test_char[[f]] <- as.integer(factor(test_char[[f]], levels=levels))
  }
}

# ---------------------
# We're done with char datasets and go to numerical ones

# First find number of unique values
# Let's look at the columns with only two unique values.
col_ct_num = sapply(train_num, function(x) length(unique(x)))
col_2 <- names(train_num[col_ct_num==2])

# Replace -99999 in col_2 by -1
train_num[,col_2][train_num[, col_2] == -99999] = -1
test_num[,col_2][test_num[, col_2] == -99999] = -1

# print unique values
# str(lapply(train_num[,col_2], unique), vec.len =6)

# Find NZV for numerical values
nzv_num <- nearZeroVar(train_num, uniqueCut = 5, saveMetrics = TRUE)
nzv100 <- which(nzv_num$freqRatio>100)
train_num <- train_num[, -nzv100]
test_num <- test_num[, -nzv100]

# Replace NAs by -1
train_num[is.na(train_num)] = -1
test_num[is.na(test_num)] = -1

# Find highly correlated variables
# numr_corr <- cor(train_num, use="complete.obs")

# Filter features with corr >= 90%
# highlyCor <- findCorrelation(numr_corr, cutoff = .9)
# train_num_2 <- train_num[, -highlyCor]
# test_num_2 <- test_num[, -highlyCor]

# Remove linear dependent features
# comboInfo <- findLinearCombos(train_num)
# train_num_2 <- train_num_2[, -comboInfo$remove]
# test_num_2 <- test_num_2[, -comboInfo$remove]

# Combine all features together into one dataset
inTrain <- cbind(target, train_num, train_char, trainDT)
inTest <- cbind(test_num, test_char, testDT)
features <- names(inTrain[,-target])

# save some RAM
spltfeat <- NULL
train_num <- NULL
train_char <- NULL
trainDT <- NULL
train <- NULL

test_num <- NULL
test_char <- NULL
testDT <- NULL
test <- NULL

# ========================
# Make a xgboost model

set.seed(285)

h <- sample(nrow(inTrain), 130000)
X <- inTrain[h,]
dtrain <- xgb.DMatrix(data.matrix(X[,features]), label=X$target, missing = NA)
val<-inTrain[-h,]
dval <- xgb.DMatrix(data.matrix(val[,features]), label=val$target, missing = NA)
inTrain <- NULL

watchlist <- list(eval = dval, train = dtrain)

# Training a XGBoost classifier
param <- list(  objective           = "binary:logistic", 
                #booster             = "gbtree", 
                eta                 = 0.005,
                max_depth           = 30,  
                subsample           = 0.7,
                colsample_bytree    = 0.8,
                eval_metric         = "auc",
                min_child_weight    = 6,
                alpha               = 4
                # lambda = 1
                )

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 5000, 
                    verbose             = 1, 
                    early.stop.round    = 10,
                    watchlist           = watchlist,
                    maximize            = TRUE)


# ==============
# Test data and predict

submission <- data.frame(ID=test_id)
submission$target <- NA 

for (rows in split(1:nrow(inTest), ceiling((1:nrow(inTest))/10000))) {
  submission[rows, "target"] <- predict(clf, data.matrix(inTest[rows,features]))
}

cat("saving the submission file\n")
if (!file.exists("output")) {
  dir.create("output")
}
write_csv(submission, "./output/output.csv")
