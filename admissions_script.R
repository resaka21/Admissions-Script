### Rei Esaka
### Admissions Assignment
### November 20, 2021

### Referenced Dr. Jorge Colazo's code (Trinity University)

### Objective: We were given a university's admissions data, and we were to predict who is going
### to take the offer of admission from them This can allow admissions officers to follow up and
### increase the yield (accepted/admitted).


###### Before Data Cleaning

# libraries
library(dplyr) # for select, filter, summarize
library(Metrics) # for Kappa score
library(e1071) # for SVM
library(randomForest) # for rf
library(gbm) # for boosting
library(ggplot2) # for plots
library(class) # for knn


# set the seed to make partition reproducible
set.seed(123)

# read raw data
raw_data <- read.csv('TU.csv')


# glimpse into the dataset shows that the original dataset has 15,143 rows and 69 columns. 
# Mostly integers and characters, some logical and double as well.
glimpse(raw_data)


# looks like no duplicates
count(raw_data[duplicated(raw_data) == TRUE, ])




###### Dealing with NA/Blank Values

### To see which columns contain NA values
sapply(raw_data[,1:69], function(x) sum(is.na(x)))


### Binarizing columns

# legacy = 1, not a legacy = 0
raw_data$Legacy <- ifelse(raw_data$Legacy != "",1,0)


# athlete = 1, not an athlete = 0
raw_data$Athlete <- ifelse(raw_data$Athlete != "",1,0)


# Only one blank in Permanent Geomarket, so replacing with the most frequent value
raw_data$Permanent.Geomarket[raw_data$Permanent.Geomarket == ""] <- names(which.max(table(raw_data$Permanent.Geomarket)))

# change to in-state = 1, out of state = 0
raw_data$Permanent.Geomarket <- ifelse(startsWith(raw_data$Permanent.Geomarket, "TX"),"1","0")
raw_data$Permanent.Geomarket <- as.numeric(raw_data$Permanent.Geomarket)


# Creating new columns

# If a student participated in an event, they are probably more likely to accept the offer
raw_data["Participated_in_event"] <- ifelse(raw_data$Total.Event.Participation > 0, 1, 0)

# If a student did a campus, they are probably more likely to accept the offer   
raw_data["Campus_Visit"] <- ifelse(raw_data$Count.of.Campus.Visits > 0, 1, 0)    



### Replacing NA with 0
raw_data$School.1.Class.Rank..Numeric.[is.na(raw_data$School.1.Class.Rank..Numeric.)] <- 0



### Special cases

# ACT/SAT 
# There are blanks in the ACT Composite column. Where there are blanks, there IS a value in the ACT Concordance Score of SAT/SAT R (unless
# submitted test scores were optional). So I'll create a new variable called ACT_combined that has the ACT Concordance Score
# moved over to ACT Composite, and where the test was optional, I'll fill in with the mean score. Any remainder blanks will also
# be filled in with the mean score
summary(raw_data$ACT.Composite) #7502 NAS

raw_data["ACT_combined"] <- ifelse(is.na(raw_data$ACT.Composite) & !is.na(raw_data$ACT.Concordance.Score..of.SAT.R.), raw_data$ACT.Concordance.Score..of.SAT.R., raw_data$ACT.Composite)
summary(raw_data$ACT_combined) #2618 NAs

raw_data$ACT_combined <- ifelse(is.na(raw_data$ACT_combined) & !is.na(raw_data$ACT.Concordance.Score..of.SAT.), raw_data$ACT.Concordance.Score..of.SAT., raw_data$ACT_combined)
summary(raw_data$ACT_combined) #2615 NAs

raw_data %>% filter(ACT_combined != "") %>% summarize(ACT_Avg = mean(ACT_combined)) # 30.48, Rounding to 30

raw_data$ACT_combined <- ifelse(is.na(raw_data$ACT_combined) & raw_data$Test.Optional == 1, 30, raw_data$ACT_combined)
summary(raw_data$ACT_combined) #1179 NAs

raw_data$ACT_combined <- ifelse(is.na(raw_data$ACT_combined), 30, raw_data$ACT_combined)
summary(raw_data$ACT_combined) #Nice, 0 NAs


# Academic Index
# Academic Index has some blanks. According to the Admissions features doc, one of the major factors that influences 
# the index is GPA, so for each index #, I will find the mean GPA, which will act as parameters. For the blanks, 
# I will be assigning an index depending on their GPA
raw_data %>% filter(Academic.Index == 1) %>% summarize(index1_Avg = mean(School.1.GPA.Recalculated)) # 3.97
raw_data %>% filter(Academic.Index == 2) %>% summarize(index2_Avg = mean(School.1.GPA.Recalculated)) # 3.85
raw_data %>% filter(Academic.Index == 3) %>% summarize(index3_Avg = mean(School.1.GPA.Recalculated)) # 3.61
raw_data %>% filter(Academic.Index == 4) %>% summarize(index4_Avg = mean(School.1.GPA.Recalculated)) # 3.25
raw_data %>% filter(Academic.Index == 5) %>% summarize(index5_Avg = mean(School.1.GPA.Recalculated)) # 3.02

raw_data$Academic.Index <- ifelse(is.na(raw_data$Academic.Index) & raw_data$School.1.GPA.Recalculated >= 3.97, 1,
                                  ifelse(is.na(raw_data$Academic.Index) & raw_data$School.1.GPA.Recalculated >= 3.85 & raw_data$School.1.GPA.Recalculated < 3.97, 2,
                                         ifelse(is.na(raw_data$Academic.Index) & raw_data$School.1.GPA.Recalculated >= 3.61 & raw_data$School.1.GPA.Recalculated < 3.85, 3,
                                                ifelse(is.na(raw_data$Academic.Index) & raw_data$School.1.GPA.Recalculated >= 3.25 & raw_data$School.1.GPA.Recalculated < 3.61, 4,
                                                       ifelse(is.na(raw_data$Academic.Index) & raw_data$School.1.GPA.Recalculated >= 0 & raw_data$School.1.GPA.Recalculated < 3.25, 5,raw_data$Academic.Index)))))






###### Editing the Contents of a Column

# Right now, Merit Award is categorical, but we can turn them into numeric by referring to the "Admissions Data_Merit
# Award Codes" spreadsheet. 
raw_data$Merit.Award[raw_data$Merit.Award == "I10"] <- 10000
raw_data$Merit.Award[raw_data$Merit.Award == "I12"] <- 12000
raw_data$Merit.Award[raw_data$Merit.Award == "I12.5"] <- 12500
raw_data$Merit.Award[raw_data$Merit.Award == "I15"] <- 15000
raw_data$Merit.Award[raw_data$Merit.Award == "I17"] <- 17000
raw_data$Merit.Award[raw_data$Merit.Award == "I18"] <- 18000
raw_data$Merit.Award[raw_data$Merit.Award == "I19"] <- 19000
raw_data$Merit.Award[raw_data$Merit.Award == "I20"] <- 20000
raw_data$Merit.Award[raw_data$Merit.Award == "I21"] <- 21000
raw_data$Merit.Award[raw_data$Merit.Award == "I24"] <- 24000
raw_data$Merit.Award[raw_data$Merit.Award == "I25"] <- 25000
raw_data$Merit.Award[raw_data$Merit.Award == "I26"] <- 26000
raw_data$Merit.Award[raw_data$Merit.Award == "I27"] <- 27000
raw_data$Merit.Award[raw_data$Merit.Award == "I28"] <- 28000
raw_data$Merit.Award[raw_data$Merit.Award == "I30"] <- 30000
raw_data$Merit.Award[raw_data$Merit.Award == "I32"] <- 32000
raw_data$Merit.Award[raw_data$Merit.Award == "I33"] <- 33000
raw_data$Merit.Award[raw_data$Merit.Award == "I35"] <- 35000
raw_data$Merit.Award[raw_data$Merit.Award == "I38"] <- 38000
raw_data$Merit.Award[raw_data$Merit.Award == "I5"] <- 5000
raw_data$Merit.Award[raw_data$Merit.Award == "I9"] <- 9000
raw_data$Merit.Award[raw_data$Merit.Award == "D12.5"] <- 12500
raw_data$Merit.Award[raw_data$Merit.Award == "Z0"] <- 0
raw_data$Merit.Award[raw_data$Merit.Award == "I0"] <- 0
raw_data$Merit.Award[raw_data$Merit.Award == "M24"] <- 24000
raw_data$Merit.Award[raw_data$Merit.Award == "P17"] <- 17000
raw_data$Merit.Award[raw_data$Merit.Award == "TT9"] <- 9000
raw_data$Merit.Award[raw_data$Merit.Award == "T21"] <- 21000
raw_data$Merit.Award[raw_data$Merit.Award == "X0"] <- 0
raw_data$Merit.Award[raw_data$Merit.Award == "Y0"] <- 0

# For the full ride scholarships, I'll take the average university tuition from the past 5 years
# source: https://www.collegetuitioncompare.com/trends/trinity-university/cost-of-attendance/ 
# 2016-17 $39,560
# 2017-18 $41,344
# 2018-19 $42,976
# 2019-20 $44,680
# 2020-21 $46,456
tuitions <- c(39560, 41344, 42976, 44680, 46456) 
mean(tuitions) # 43003.2

raw_data$Merit.Award[raw_data$Merit.Award == "SEM"] <- 43003
raw_data$Merit.Award[raw_data$Merit.Award == "TTS"] <- 43003

# there were some merit awards that weren't on the spreadsheet, but you could assume the amount based on the code
raw_data$Merit.Award[raw_data$Merit.Award == "TT125"] <- 12500
raw_data$Merit.Award[raw_data$Merit.Award == "TT12"] <- 12000
raw_data$Merit.Award[raw_data$Merit.Award == "TT10"] <- 10000
raw_data$Merit.Award[raw_data$Merit.Award == "T25"] <- 25000
raw_data$Merit.Award[raw_data$Merit.Award == "T23"] <- 23000
raw_data$Merit.Award[raw_data$Merit.Award == "T22"] <- 22000
raw_data$Merit.Award[raw_data$Merit.Award == "P23"] <- 23000
raw_data$Merit.Award[raw_data$Merit.Award == "P18"] <- 18000
raw_data$Merit.Award[raw_data$Merit.Award == "M30"] <- 30000
raw_data$Merit.Award[raw_data$Merit.Award == "M27"] <- 27000
raw_data$Merit.Award[raw_data$Merit.Award == "M26"] <- 26000
raw_data$Merit.Award[raw_data$Merit.Award == "M25"] <- 25000
raw_data$Merit.Award[raw_data$Merit.Award == "I7.5"] <- 7500
raw_data$Merit.Award[raw_data$Merit.Award == "I52"] <- 52000
raw_data$Merit.Award[raw_data$Merit.Award == "I50"] <- 50000
raw_data$Merit.Award[raw_data$Merit.Award == "I45"] <- 45000
raw_data$Merit.Award[raw_data$Merit.Award == "I43"] <- 43000
raw_data$Merit.Award[raw_data$Merit.Award == "I40"] <- 40000
raw_data$Merit.Award[raw_data$Merit.Award == "I23"] <- 23000
raw_data$Merit.Award[raw_data$Merit.Award == "I22"] <- 22000
raw_data$Merit.Award[raw_data$Merit.Award == "D20"] <- 20000
raw_data$Merit.Award[raw_data$Merit.Award == "D18"] <- 18000

raw_data$Merit.Award <- as.numeric(raw_data$Merit.Award)





###### Dropped Columns 

# Admit.Type can be dropped since the only value in this column is "FY". This only creates "noise" in the data
raw_data <- raw_data %>% dplyr::select(-Admit.Type)        


# Drop the date columns since it's creating way too many factors, and it likely does not heavily influence whether the student
# accepts or declines the offer
raw_data <- raw_data %>% dplyr::select(-First_Source.Origin.First.Source.Date) 
raw_data <- raw_data %>% dplyr::select(-Inquiry.Date) 
raw_data <- raw_data %>% dplyr::select(-Submitted) 


# Total.Event.Participation can be dropped since it was binarized
raw_data <- raw_data %>% dplyr::select(-Total.Event.Participation)


# Count.of.Campus.Visits can be dropped since it was binarized
raw_data <- raw_data %>% dplyr::select(-Count.of.Campus.Visits)


# Delete the School 1 GPA column because there's already a recalculated column
raw_data <- raw_data %>% dplyr::select(-School.1.GPA)


# Delete the GPA Scale columns that are creating noise because the GPA recalculated column has no blank values. 
raw_data <- raw_data %>% dplyr::select(-School.1.GPA.Scale)
raw_data <- raw_data %>% dplyr::select(-School.2.GPA.Scale)
raw_data <- raw_data %>% dplyr::select(-School.3.GPA.Scale)


# I don't think School Code adds any value in predicting the acceptance of an offer, so deleting
raw_data <- raw_data %>% dplyr::select(-School.1.Code)


# There are no values in School 2 and 3 columns, so drop bc they just add noise
raw_data <- raw_data %>% dplyr::select(-School.2.Class.Rank..Numeric.)
raw_data <- raw_data %>% dplyr::select(-School.2.Class.Size..Numeric.)
raw_data <- raw_data %>% dplyr::select(-School.2.GPA)
raw_data <- raw_data %>% dplyr::select(-School.2.GPA.Recalculated)
raw_data <- raw_data %>% dplyr::select(-School.3.Class.Rank..Numeric.)
raw_data <- raw_data %>% dplyr::select(-School.3.Class.Size..Numeric.)
raw_data <- raw_data %>% dplyr::select(-School.3.GPA)
raw_data <- raw_data %>% dplyr::select(-School.3.GPA.Recalculated)


# The University, being a liberal arts college, should only care about the ACT composite scores rather than the scores in the separate
# sections of the exam. Therefore, dropping ACT English, Reading, Math, Science Reasoning, Writing
raw_data <- raw_data %>% dplyr::select(-ACT.English)
raw_data <- raw_data %>% dplyr::select(-ACT.Reading)
raw_data <- raw_data %>% dplyr::select(-ACT.Math)
raw_data <- raw_data %>% dplyr::select(-ACT.Science.Reasoning)
raw_data <- raw_data %>% dplyr::select(-ACT.Writing)


# Delete the ACT Composite and ACT Concordance Score (of SAT/SAT R) since these columns were combined into ACT_combined
raw_data <- raw_data %>% dplyr::select(-ACT.Composite)
raw_data <- raw_data %>% dplyr::select(-ACT.Concordance.Score..of.SAT.R.)
raw_data <- raw_data %>% dplyr::select(-ACT.Concordance.Score..of.SAT.)


# Delete the SAT-related columns because on the Admissions features doc, it describes that the university uses "ACT scores or a
# SAT converted to ACT score to calculate the Merit Award, in addition to the Recalculated GPA".
raw_data <- raw_data %>% dplyr::select(-SAT.I.CR...M)
raw_data <- raw_data %>% dplyr::select(-SAT.R.Evidence.Based.Reading.and.Writing.Section...Math.Section)
raw_data <- raw_data %>% dplyr::select(-SAT.Concordance.Score..of.SAT.R.)
raw_data <- raw_data %>% dplyr::select(-SAT.I.Critical.Reading)
raw_data <- raw_data %>% dplyr::select(-SAT.I.Math)
raw_data <- raw_data %>% dplyr::select(-SAT.I.Writing)
raw_data <- raw_data %>% dplyr::select(-SAT.R.Evidence.Based.Reading.and.Writing.Section)
raw_data <- raw_data %>% dplyr::select(-SAT.R.Math.Section)



# similar situation with first source origin first source summary
raw_data <- raw_data %>% dplyr::select(-First_Source.Origin.First.Source.Summary)

##### Columns I decided to delete when optimizing my model. Looked at the number of stars (*) showing significnace next to the variable
##### when I ran the summary on my model. 

raw_data <- raw_data %>% dplyr::select(-Permanent.Country) # permanent geo does the same
raw_data <- raw_data %>% dplyr::select(-Permanent.Postal) # permanent geo does the same
raw_data <- raw_data %>% dplyr::select(-Sex) # gender shouldn't matter
raw_data <- raw_data %>% dplyr::select(-Religion) # university is nonreligious
raw_data <- raw_data %>% dplyr::select(-Staff.Assigned.Name) # doesn't matter
raw_data <- raw_data %>% dplyr::select(-Academic.Interest.1) # doesn't matter
raw_data <- raw_data %>% dplyr::select(-Academic.Interest.2) # doesn't matter
raw_data <- raw_data %>% dplyr::select(-Entry.Term..Application.) # all enter in the fall
raw_data <- raw_data %>% dplyr::select(-School.1.Class.Size..Numeric.) # doesn't matter
raw_data <- raw_data %>% dplyr::select(-Test.Optional) # don't need after creating ACT_combined
raw_data <- raw_data %>% dplyr::select(-Sport.1.Sport) # significant if they're an athlete or not
raw_data <- raw_data %>% dplyr::select(-Sport.2.Sport)
raw_data <- raw_data %>% dplyr::select(-Sport.3.Sport)
raw_data <- raw_data %>% dplyr::select(-Sport.1.Rating)
raw_data <- raw_data %>% dplyr::select(-Sport.2.Rating)
raw_data <- raw_data %>% dplyr::select(-Sport.3.Rating)
raw_data <- raw_data %>% dplyr::select(-School..1.Organization.Category) # doesn't matter
raw_data <- raw_data %>% dplyr::select(-Citizenship.Status) # doesn't matter
raw_data <- raw_data %>% dplyr::select(-Intend.to.Apply.for.Financial.Aid.)  
raw_data <- raw_data %>% dplyr::select(-Race) 
raw_data <- raw_data %>% dplyr::select(-Ethnicity)




##### character to factor
str(raw_data) #checking the data types

# Source: https://statisticsglobe.com/convert-character-to-factor-in-r
raw_data <- as.data.frame(unclass(raw_data), stringsAsFactors = TRUE) 


##### Integer to numeric       
raw_data$Decision <- as.numeric(raw_data$Decision)
str(raw_data) #double checking the data types

##### need to filter out ID and train.test
raw_data2  <- raw_data[, !names(raw_data) %in% c("ID", "train.test")]


##### Onehot encode all categorical          
# dummify the data
dummy <- dummyVars(" ~ .", data = raw_data2)
transformed <- data.frame(predict(dummy, newdata = raw_data2))
transformed

# add to raw_data
raw_data <- cbind(raw_data,transformed)

# drop duplicated columns
raw_data <- raw_data[, !duplicated(colnames(raw_data))]

# drop the columns that got dummified
raw_data <- raw_data %>% dplyr::select(-Application.Source)
raw_data <- raw_data %>% dplyr::select(-Decision.Plan)


##### The Final dataset           
str(raw_data) #15143 rows, 23 variables



###### Modeling
# Your metric for each case will be Cohen's Kappa on the test set.

# 5143 NAs. This will be where I split into train/test 
summary(raw_data$Decision) 


# splitting the data
train <- raw_data[raw_data$train.test == "train",] # includes Decision
train <- train %>% dplyr::select(-train.test)

test <- raw_data[raw_data$train.test == "test",] # does NOT include Decision
test <- test %>% dplyr::select(-train.test)

# this tells me columns that I should drop since they only have one unique value
which(sapply(train, function(x) (is.numeric(x)) & length(unique(x))<2))
which(sapply(test, function(x) (is.numeric(x)) & length(unique(x))<2))



##### Logistic Regression  

# Runs the model as logistic regression with ALL X, predicting decision
model <- glm(Decision ~.,family=binomial(link='logit'),data=train)

# get predicted results
fit_lr <- predict(model,newdata=test,type='response')

# binarize the results
binary_fitted.results.lr <- ifelse(fit_lr > 0.5,1,0)

predict_lr <- data.frame(ID = test$ID, Decision = binary_fitted.results.lr)

# Export into csv file
write.csv(predict_lr, file = "Esaka_10.csv", row.names = FALSE)

# actual kappa score [1] 0.3991326



#### splitting the data   
x_test <- subset(test, select = -c(Decision, ID))
x_test <- data.matrix(x_test)
x_train<-subset(train, select = -c(Decision, ID))
x_train <- data.matrix(x_train)
y_train <- train$Decision



##### KNN  

# make y into data matrix before testing model
y_train <- data.matrix(y_train)   

# Create the model
knn_model <- as.numeric(knn(x_train[,],x_test[,],y_train[,1],k=3))

# recode into 0 and 1
knn_adjusted <-as.numeric(knn_model)-1

# predict response for the test set   
predict_knn <- data.frame(ID = test$ID, Decision = knn_adjusted)

# export into csv file
write.csv(predict_knn, file = "Esaka_8.csv", row.names = FALSE)

# actual kappa score  [1] 0.1029955




##### SVM classification with different kernels

### SVM for classification - linear kernel, cost=10 (works!)

# Create the SVM model, linear kernel, cost=10
svmfit<-svm(x_train, y_train, type="C-classification",kernel="linear", cost=10)

# now predict response for the test set
fit_linear <-predict(svmfit,newdata = x_test)

predict_linear <- data.frame(ID = test$ID, Decision = fit_linear)

# Export into csv file
write.csv(predict_linear, file = "Esaka_2.csv", row.names = FALSE)

# actual kappa score [1] 0.2290798



### SVM for classification - polynomial kernel, cost=10 (works!)

# Create the SVM model, polynomial kernel, cost=10
svmfit2<-svm(x_train, y_train, type="C-classification",kernel="polynomial", cost=10)

# now predict response for the test set
fit_poly <-predict(svmfit2,newdata = x_test)

predict_poly <- data.frame(ID = test$ID, Decision = fit_poly)

# Export into csv file
write.csv(predict_poly, file = "Esaka_3.csv", row.names = FALSE)

# actual kappa score [1] 0.2864655



### SVM for classification - radial, cost-10

# Create the SVM model, radial kernel
svmfit3<-svm(x_train, y_train, type="C-classification",kernel="radial", cost=10)

# now predict response for the test set
fit_rad <-predict(svmfit3,newdata = x_test)

predict_rad <- data.frame(ID = test$ID, Decision = fit_rad)

# Export into csv file
write.csv(predict_rad, file = "Esaka_4.csv", row.names = FALSE)

# actual Kappa score [1] 0.3479243





##### Classification trees with Random forests (works, waiting for kappa score)
# create the model     
rf<-randomForest(Decision ~. -ID, train, na.action = na.exclude)

# predict response for test set   
fit_rf <- predict(rf, newdata=test)

# binarize the results
binary_fitted.results <- ifelse(fit_rf > 0.5,1,0)

# predict response    
predict_rf <- data.frame(ID = test$ID, Decision = binary_fitted.results)

# export into csv file   
write.csv(predict_rf, file = "Esaka_10.csv", row.names = FALSE)


#  kappa score [1] 0.3924479


##### Bagging (works, waiting for kappa score)

# create model
bagging <- randomForest(Decision ~.-ID, train, importance = TRUE, ntrees = 100)

# predict response for test set  
fit_bag <- predict(bagging, newdata = test)

# binarize the results
binary_fitted.results <- ifelse(fit_bag > 0.5,1,0)

# predict response
predict_bag <- data.frame(ID = test$ID, Decision = binary_fitted.results)

# export into csv file   
write.csv(predict_bag, file = "Esaka_9.csv", row.names = FALSE)

# actual kappa score [1] 0.411888



##### Boosting (works, waiting for kappa score)
# create model
boosting <- gbm(Decision~.-ID,data=train,distribution="gaussian") 

# predict response for test set  
fit_boost <- predict(boosting, newdata = test, n.trees = 100)

# binarize the results
binary_fitted.results <- ifelse(fit_boost > 0.5,1,0) 

# predict response
predict_boost <- data.frame(ID = test$ID, Decision = binary_fitted.results)

# export into csv file   
write.csv(predict_boost, file = "Esaka_7.csv", row.names = FALSE)

# actual kappa score [1] 0.3036898




