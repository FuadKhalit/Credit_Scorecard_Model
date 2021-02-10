#----------------Library Initialisation--------------------
library(ggplot2)
library (grid)
library(DMwR)
library(data.table)
library(Information)
library(gridExtra)
library(class)
library(caret)
library(dplyr)
library(skimr)


#------------Reading the dataset---------------------

application_train <- read.csv("C:/Users/Fuad/Documents/APU Stuff/Kaggle Dataset-Home Credit Default Risk/application_train.csv", header=TRUE,stringsAsFactors=FALSE)


#Preporcessing Part

#-----------Initial Data Exploration----------------------
skim(application_train)

#-----------Imputing the missing/NA values----------------


# Categorizing the OCCUPATION_TYPE column in application_train
application_train$OCCUPATION_TYPE <- as.integer(as.numeric(as.factor(application_train$OCCUPATION_TYPE)))
# Categorizing the ORGANIZATION_TYPE COLUMN in application_train
application_train$ORGANIZATION_TYPE <- as.integer(as.numeric(as.factor(application_train$ORGANIZATION_TYPE)))
# Categorizing the NAME_CONTRACT_TYPE column in application_train
application_train$NAME_CONTRACT_TYPE <- as.integer(as.numeric(as.factor(application_train$NAME_CONTRACT_TYPE)))
# Categorizing the CODE_GENDER column in application_train
application_train$CODE_GENDER <- as.integer(as.numeric(as.factor(application_train$CODE_GENDER)))
# Categorizing the EMERGENCYSTATE_MODE column in application_train
application_train$EMERGENCYSTATE_MODE <- as.integer(as.numeric(as.factor(application_train$EMERGENCYSTATE_MODE)))
# Categorizing the EMERGENCYSTATE_MODE column in application_train
application_train$EMERGENCYSTATE_MODE <- as.integer(as.numeric(as.factor(application_train$EMERGENCYSTATE_MODE)))
# Categorizing the FLAG_OWN_CAR  column in application_train
application_train$FLAG_OWN_CAR  <- as.integer(as.numeric(as.factor(application_train$FLAG_OWN_CAR)))
# Categorizing the FLAG_OWN_REALTY  column in application_train
application_train$FLAG_OWN_REALTY  <- as.integer(as.numeric(as.factor(application_train$FLAG_OWN_REALTY)))
# Categorizing the NAME_TYPE_SUITE  column in application_train
application_train$NAME_TYPE_SUITE  <- as.integer(as.numeric(as.factor(application_train$NAME_TYPE_SUITE)))
# Categorizing the NAME_INCOME_TYPE  column in application_train
application_train$NAME_INCOME_TYPE  <- as.integer(as.numeric(as.factor(application_train$NAME_INCOME_TYPE)))
# Categorizing the NAME_EDUCATION_TYPE    column in application_train
application_train$NAME_EDUCATION_TYPE   <- as.integer(as.numeric(as.factor(application_train$NAME_EDUCATION_TYPE)))
# Categorizing the NAME_FAMILY_STATUS    column in application_train
application_train$NAME_FAMILY_STATUS   <- as.integer(as.numeric(as.factor(application_train$NAME_FAMILY_STATUS)))
# Categorizing the NAME_HOUSING_TYPE    column in application_train
application_train$NAME_HOUSING_TYPE   <- as.integer(as.numeric(as.factor(application_train$NAME_HOUSING_TYPE)))
# Categorizing the WEEKDAY_APPR_PROCESS_START    column in application_train
application_train$WEEKDAY_APPR_PROCESS_START   <- as.integer(as.numeric(as.factor(application_train$WEEKDAY_APPR_PROCESS_START)))
# Converting the NULL Values to NA
application_train$HOUSETYPE_MODE <- ifelse(application_train$HOUSETYPE_MODE=="","NA",application_train$HOUSETYPE_MODE)
# Converting the NULL Values to NA
application_train$WALLSMATERIAL_MODE<- ifelse(application_train$WALLSMATERIAL_MODE=="","NA",application_train$WALLSMATERIAL_MODE)
# Converting the NULL Values to NA
application_train$EMERGENCYSTATE_MODE<- ifelse(application_train$EMERGENCYSTATE_MODE=="","NA",application_train$EMERGENCYSTATE_MODE)
# Subsetting the Numeric class columns
subset_colclasses <- function(DF, colclasses="numeric") {
  DF[,sapply(DF, function(vec, test) class(vec) %in% test, test=colclasses)]
}
# Subsetting the data into numeric, integer, character
subset_numeric <- subset_colclasses(application_train,colclasses="numeric")
subset_integer <- subset_colclasses(application_train,colclasses="integer")
subset_character <- subset_colclasses(application_train,colclasses="character")
f=function(x){
  x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
  x[is.na(x)] =mean(x, na.rm=TRUE) #convert the item with NA to mean value from the column
  x #display the column
}
ss=data.frame(apply(subset_numeric,2,f))
f_median=function(x){
  x<-as.integer(as.character(x)) #first convert each column into numeric if it is from factor
  x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
  x #display the column
}
ss_int=data.frame(apply(subset_integer,2,f))
uniqueX <- unique(subset_character$FONDKAPREMONT_MODE)
uniqueX <- uniqueX[uniqueX!="NA"]
subset_character$FONDKAPREMONT_MODE[subset_character$FONDKAPREMONT_MODE=="NA"] = sample(uniqueX,1) #convert the item with NA to median value from the column
uniqueY <- unique(subset_character$HOUSETYPE_MODE)
uniqueY <- uniqueY[uniqueY!="NA"]
subset_character$HOUSETYPE_MODE[subset_character$HOUSETYPE_MODE=="NA"] = sample(uniqueY,1) #convert the item with NA to median value from the column
uniqueZ <- unique(subset_character$WALLSMATERIAL_MODE)
uniqueZ <- uniqueZ[uniqueZ!="NA"]
subset_character$WALLSMATERIAL_MODE[subset_character$WALLSMATERIAL_MODE=="NA"] = sample(uniqueZ,1) #convert the item with NA to median value from the column
subset_master_character <- apply(subset_character,2,function(x)as.integer(as.numeric(as.factor(x))))
subset_master_character <- data.frame(subset_master_character)


#---------Creating the cleaned Master data file--------------


# Master Data
imputed_master <- cbind(ss,ss_int,subset_master_character)
#write.csv(imputed_master,"C:/Users/Fuad/Documents/APU Stuff/Kaggle Dataset-Home Credit Default Risk/TrainDataCleaned.csv")


#--------Dimension Reduction -Principle Component Analysis (PCA)---------------------

TrainDataCleaned = imputed_master
TrainDataCleaned =TrainDataCleaned[,-1]
#str(TrainDataCleaned)
#names(TrainDataCleaned)
Target= TrainDataCleaned[,67]
TrainDataCleaned = TrainDataCleaned[,-c(66,67)]
#str(TrainDataCleaned)
TrainDataCleaned <- scale(TrainDataCleaned,center=apply(TrainDataCleaned,2,mean),scale=apply(TrainDataCleaned,2,sd))

Project.Data.PCA <- princomp(TrainDataCleaned)

barplot(Project.Data.PCA$sdev^2/sum(Project.Data.PCA$sdev^2), ylim=c(0,1))


cumsum(Project.Data.PCA$sdev^2/sum(Project.Data.PCA$sdev^2))
plot(cumsum(Project.Data.PCA$sdev^2/sum(Project.Data.PCA$sdev^2)))

#Selecting the first 65 components to get about 90% of the total variance, and setting the target as the first variable

TrainDataReducedPCA <- Project.Data.PCA$scores[,1:65] 
TrainDataReducedPCA <- cbind.data.frame(Target,TrainDataReducedPCA)
str(TrainDataReducedPCA)


#Splitting this data into train and test

set.seed(3)
n=round(length(TrainDataReducedPCA[,1])*0.7)
totaln=length(TrainDataReducedPCA[,1])

totaln
n

all =1:n
train = sample(1:totaln,size =n,replace = FALSE)
train =sort(train)
hold= all[-train]

TrainPCA <- as.data.frame(TrainDataReducedPCA[train,])
TestPCA <- as.data.frame(TrainDataReducedPCA[hold,])
str(TrainPCA)


#converting the Target into factors
TrainPCA$Target =factor(TrainPCA$Target)
TestPCA$Target <- factor(TestPCA$Target)


#----Applying SMOTE on the train and test samples to fixed the imbalance in the response------------

TrainDataSmotePCA <- SMOTE(Target~., data=TrainPCA,perc.over = 200,k=5,learner=NULL) 
TestDataSmotePCA <-  SMOTE(Target~., data=TestPCA,perc.over = 200,k=5,learner=NULL)
#write.csv(TrainDataSmotePCA,"C:/Users/Fuad/Documents/APU Stuff/Kaggle Dataset-Home Credit Default Risk/TrainDataSmotePCA.csv")
#write.csv(TestDataSmotePCA,"C:/Users/Fuad/Documents/APU Stuff/Kaggle Dataset-Home Credit Default Risk/TestDataSmotePCA.csv")



#Modeling PART 1
#on PART 1 we will use dataset TrainSmotePCA and TestSmotePCA
#We will see the result based on AUC on 3 model Naive Bayes, Random Forest & ANN Deep Learning
#Part 2 we will used data TrainDataCleaned to see different effect on model
#----------Loading H20-----------------


library(h2o)
localH2O = h2o.init()

#load the dataset if you choose to restart r session 

nn1<-read.csv("C:/Users/Fuad/Documents/APU Stuff/Kaggle Dataset-Home Credit Default Risk/TrainDataSmotePCA.csv")
nn2<-read.csv("C:/Users/Fuad/Documents/APU Stuff/Kaggle Dataset-Home Credit Default Risk/TestDataSmotePCA.csv")

----#Deleting identifier on test data--------------
nn1<-nn1[,-1]
print(nn1)

nn2<-nn2[,-1]
print(nn2)

#------------COnverting the dataframe to an H2O frameusing as.h2o------------

data_h2o <- as.h2o(nn1)
data1_h2o<- as.h2o(nn2)


#-------change target variable to factor----------------------
# Since we want to train a binary classification model, 
# we must ensure that the response is coded as a factor
# If the response is 0/1, H2O will assume it's numeric,
# which means that H2O will train a regression model instead
data_h2o$Target <- as.factor(data_h2o$Target)  #encode the binary repsonse as a factor
h2o.levels(data_h2o$Target)  #optional: after encoding, this shows the two factor levels, '0' and '1'
# [1] "0" "1"
data1_h2o$Target <- as.factor(data1_h2o$Target)  #encode the binary repsonse as a factor
h2o.levels(data1_h2o$Target)  #optional: after encoding, this shows the two factor levels, '0' and '1'
# [1] "0" "1"


#-----------Naive Bayes Model------------------------------


# Setting target variable & training classifier 
y <- "Target"
x <- setdiff(names(data_h2o), y)
print(x)


# The Naive Bayes (NB) algorithm does not usually beat an algorithm like a Random Forest 
# or GBM, however it is still a popular algorithm, especially in the text domain (when your 
# input is text encoded as "Bag of Words", for example).  The Naive Bayes algorithm is for 
# binary or multiclass classification problems only, not regression.  Therefore, your response 
# must be a factor instead of a numeric.

# First we will train a basic NB model with default parameters. 
nb_fit1 <- h2o.naiveBayes(x = x,
                          y = y,
                          training_frame = data_h2o,
                          model_id = "nb_fit1")

# Train a NB model with Laplace Smoothing
# One of the few tunable model parameters for the Naive Bayes algorithm is the amount of Laplace 
# smoothing. The H2O Naive Bayes model will not use any Laplace smoothing by default.
nb_fit2 <- h2o.naiveBayes(x = x,
                          y = y,
                          training_frame = data_h2o,
                          model_id = "nb_fit2",
                          laplace = 6)

# compare the performance of the two NB models
nb_perf1 <- h2o.performance(model = nb_fit1,
                            newdata = data1_h2o)
nb_perf2 <- h2o.performance(model = nb_fit2,
                            newdata = data1_h2o )

# Print model performance
nb_perf1
nb_perf2

# Retreive test set AUC
h2o.auc(nb_perf1)  # 0.7948077
h2o.auc(nb_perf2)  # 0.7948291

h2o.scoreHistory(nb_fit2)

#------------------------Random Forest----------------------------------------

# Spliting the training data into train, validation & test sets(70/15/15) split
splits_h2o <- h2o.splitFrame(data_h2o, ratios = 0.7)

train_h2o <- splits_h2o[[1]]
valid_h2o <- splits_h2o[[2]]
test_h2o  <- data1_h2o


# Setting target variable & training set 
y <- "Target"
x <- setdiff(names(train_h2o), y)
print(x)

# H2O's Random Forest (RF) implements a distributed version of the standard 
# Random Forest algorithm and variable importance measures.
# First we will train a basic Random Forest model with default parameters. 
# The Random Forest model will infer the response distribution from the response encoding. 
# A seed is required for reproducibility.
rf_fit1 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = train_h2o,
                            model_id = "rf_fit1",
                            seed = 1)

# Next we will increase the number of trees used in the forest by setting `ntrees = 100`.  
# The default number of trees in an H2O Random Forest is 50, so this RF will be twice as 
# big as the default.  Usually increasing the number of trees in a RF will increase 
# performance as well.  Unlike Gradient Boosting Machines (GBMs), Random Forests are fairly 
# resistant (although not free from) overfitting.

rf_fit2 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = train_h2o,
                            model_id = "rf_fit2",
                            validation_frame = valid_h2o,  #only used if stopping_rounds > 0
                            ntrees = 100,
                            seed = 1)

# compare the performance of the two RFs
rf_perf1 <- h2o.performance(model = rf_fit1,
                            newdata = test_h2o)
rf_perf2 <- h2o.performance(model = rf_fit2,
                            newdata = test_h2o)

# Print model performance
rf_perf1
rf_perf2

# Retreive test set AUC
h2o.auc(rf_perf1)  # 0.9671316
h2o.auc(rf_perf2)  # 0.9698009

#-------retrieve time to execute----------------

h2o.scoreHistory(rf_fit2) #25 second to execute


#-----------Artifical Neural Network (Deep Learning)------------------------

dl_fit3 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = train_h2o,
                            model_id = "dl_fit3",
                            validation_frame = valid_h2o,  #in DL, early stopping is on by default
                            epochs = 20,
                            hidden = c(10,10),
                            score_interval = 1,           #used for early stopping
                            stopping_rounds = 3,          #used for early stopping
                            stopping_metric = "AUC",      #used for early stopping
                            stopping_tolerance = 0.0005,  #used for early stopping
                            seed = 1)
?h2o.deeplearning
# Checking performance
dl_perf3 <- h2o.performance(model = dl_fit3,
                            newdata = test_h2o)

# Print model performance
dl_perf3

# Retreive test set AUC
h2o.auc(dl_perf3) #0.9845605

# time for execution
h2o.scoreHistory(dl_fit3) #8 second to execute



-----------------#experiment PART 2---------------------------------------------------
#Continue from PART 1, This experiment using dataset TrainDataCleaned.csv
#this experiment to see effect on performance where pca and smote are not applied to the data


library(h2o)
localH2O = h2o.init()

#load the dataset if you choose to restart r session instead of clearing unwanted dataset

ff1<-read.csv("C:/Users/Fuad/Documents/APU Stuff/Kaggle Dataset-Home Credit Default Risk/TrainDataCleaned.csv")


----#Deleting identifier on test data--------------
ff1<-ff1[,-1]
print(ff1)

#------------COnverting the dataframe to an H2O frameusing as.h2o------------

ff_h2o <- as.h2o(ff1)




#---check if target variable is factor-------------

h2o.getTypes(ff_h2o) #its integer. need to convert to factor

#-------change target variable to factor----------------------
# Since we want to train a binary classification model, 
# we must ensure that the response is coded as a factor
# If the response is 0/1, H2O will assume it's numeric,
# which means that H2O will train a regression model instead
ff_h2o$TARGET <- as.factor(ff_h2o$TARGET)  #encode the binary repsonse as a factor
h2o.levels(ff_h2o$TARGET)  #optional: after encoding, this shows the two factor levels, '0' and '1'
# [1] "0" "1"



# Spliting the training data into train, validation & test sets(70/15/15) split
splitx_h2o <- h2o.splitFrame(ff_h2o, ratios = c(0.7, 0.15), seed = 1234)

training_h2o <- splitx_h2o[[1]]
validation_h2o <- splitx_h2o[[2]]
testing_h2o  <- splitx_h2o[[3]]




# Setting target variable & training set 
y <- "TARGET"
x <- setdiff(names(training_h2o), y)
print(x)




#------------------Naive Bayes----------------------------------------
# The Naive Bayes (NB) algorithm does not usually beat an algorithm like a Random Forest 
# or GBM, however it is still a popular algorithm, especially in the text domain (when your 
# input is text encoded as "Bag of Words", for example).  The Naive Bayes algorithm is for 
# binary or multiclass classification problems only, not regression.  Therefore, your response 
# must be a factor instead of a numeric.

# First we will train a basic NB model with default parameters. 
nb_fit <- h2o.naiveBayes(x = x,
                         y = y,
                         training_frame = training_h2o,
                         model_id = "nb_fit")

# Train a NB model with Laplace Smoothing
# One of the few tunable model parameters for the Naive Bayes algorithm is the amount of Laplace 
# smoothing. The H2O Naive Bayes model will not use any Laplace smoothing by default.
nb_fitx <- h2o.naiveBayes(x = x,
                          y = y,
                          training_frame = training_h2o,
                          model_id = "nb_fitx",
                          laplace = 6)

# Let's compare the performance of the two NB models
nb_fix1 <- h2o.performance(model = nb_fit,
                           newdata = testing_h2o)
nb_fix2 <- h2o.performance(model = nb_fitx,
                           newdata = testing_h2o )

# Print model performance
nb_fix1
nb_fix2

# Retreive test set AUC
h2o.auc(nb_fix1)  # 0.6238857535
h2o.auc(nb_fix2)  # 0.6261947778

h2o.scoreHistory(nb_fix2)



#--------------------Random Forest----------------------------------------------


# H2O's Random Forest (RF) implements a distributed version of the standard 
# Random Forest algorithm and variable importance measures.
# First we will train a basic Random Forest model with default parameters. 
# The Random Forest model will infer the response distribution from the response encoding. 
# A seed is required for reproducibility.
rfd_fit1 <- h2o.randomForest(x = x,
                             y = y,
                             training_frame = training_h2o,
                             model_id = "rfd_fit1",
                             seed = 1)

# Next we will increase the number of trees used in the forest by setting `ntrees = 100`.  
# The default number of trees in an H2O Random Forest is 50, so this RF will be twice as 
# big as the default.  Usually increasing the number of trees in a RF will increase 
# performance as well.  Unlike Gradient Boosting Machines (GBMs), Random Forests are fairly 
# resistant (although not free from) overfitting.

rfd_fit2 <- h2o.randomForest(x = x,
                             y = y,
                             training_frame = training_h2o,
                             model_id = "rfd_fit2",
                             validation_frame = validation_h2o,  #only used if stopping_rounds > 0
                             ntrees = 100,
                             seed = 1)

# Let's compare the performance of the two RFs
rfd_perf1 <- h2o.performance(model = rfd_fit1,
                             newdata = testing_h2o)
rfd_perf2 <- h2o.performance(model = rfd_fit2,
                             newdata = testing_h2o)

# Print model performance
rfd_perf1
rfd_perf2

# Retreive test set AUC
h2o.auc(rfd_perf1)  # 0.7223952672
h2o.auc(rfd_perf2)  # 0.7301016207

#-------retrieve time to execute----------------

h2o.scoreHistory(rfd_fit2) #1 min  6.134 sec 


#-------------Artificial Neural Network (Deep Learning)-----------------------------

dlx_fit3 <- h2o.deeplearning(x = x,
                             y = y,
                             training_frame = training_h2o,
                             model_id = "dlx_fit3",
                             validation_frame = validation_h2o,  #in DL, early stopping is on by default
                             epochs = 100, #increase epoch for better performance on the prediction
                             hidden = c(150,100,50),
                             score_interval = 1,           #used for early stopping
                             stopping_rounds = 3,          #used for early stopping
                             stopping_metric = "AUC",      #used for early stopping
                             stopping_tolerance = 0.0005,  #used for early stopping
                             seed = 1)


# Checking performance
dlx_perf3 <- h2o.performance(model = dlx_fit3,
                             newdata = testing_h2o)

# Print model performance
dlx_perf3

# Retreive test set AUC
h2o.auc(dlx_perf3) #0.7016050934(30epoch/2 hidden layer(10,10)), 0.7065203151 (100e/2hl(10,10))
#0.7099537338(100epoch/3hl(150,100,50))

# time for execution
h2o.scoreHistory(dlx_fit3) #7.556 second to execute, 56.7sec (3hl)


