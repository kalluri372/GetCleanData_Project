
## 
## "run_analysis.R"
##
## Generates a tidy data set from raw data with smartphone 
## accelerometer measurements during 6 different activities 
## of 30 subjects, available as training and test data sets.
##


## read in all necessary raw data sets
##
        # features
features <- read.table( "./features.txt", 
                        colClasses = c( "integer", "character"), 
                        stringsAsFactors = FALSE )
names(features) <- c( "feature_code", "feature_name" )

features$feature_name <- 
 sapply( 1:561, 
         function(i) { paste0( features[i, "feature_name"], 
                               "-", 
                               as.character(i) ) } )

        # activity labels
activity_labels <- read.table( "./activity_labels.txt", 
                               colClasses = c( "integer", "character"), 
                               stringsAsFactors = FALSE )
names(activity_labels) <- c( "activity_code", "activity_name" )

        # training data
subject_train <- read.table( "./subject_train.txt", colClasses = "integer" )
X_train <- read.table( "./X_train.txt", colClasses = "numeric" )
y_train <- read.table( "./y_train.txt", colClasses = "integer" )

        # test data
subject_test <- read.table( "./subject_test.txt", colClasses = "integer" )
X_test <- read.table( "./X_test.txt", colClasses = "numeric" )
y_test <- read.table( "./y_test.txt", colClasses = "integer" )


## Step 1: merge training and test data sets
##
train_test_df <- rbind( cbind( cbind(subject_train, X_train), y_train ), 
                        cbind( cbind(subject_test, X_test), y_test ) )
any( colSums(is.na(train_test_df)) == 1 )
# [1] FALSE

names(train_test_df) <- c( "subject_code", 
                           features$feature_name, 
                           "activity_code" )


## Step 2: extract variables with mean() and std() 
##
## NOTE: 
##  * ignoring variables with "Mean" in their name 
##  * "features.txt" has no overlap of mean() & std() 
##
mean_variable_indices <- grep( "mean()", features$feature_name )
std_variable_indices <- grep( "std()", features$feature_name )

length( intersect(mean_variable_indices, std_variable_indices) )
# [1] 0

desired_indices <- c( mean_variable_indices, std_variable_indices ) + 1

library(dplyr)

train_test_df <- select( train_test_df, 
                         subject_code, desired_indices, activity_code )


## Step 3: descriptive activity names
##
train_test_df <- mutate( train_test_df, 
                         activity_label = activity_labels[activity_code, 2] )


## Step 4: names of variables
##
## NOTE: done in Step 1 above


## Step 5: tidy data set with averages
##
train_test_df <- train_test_df[, -81]

split_df <- split(train_test_df, 
                  list(train_test_df$subject_code, 
                       train_test_df$activity_label))

tidy_dataset <- sapply( split_df, 
                        function(x) { colMeans(x[, 2:80]) } )

## need to reshape this

