run_analysis <- function(){
  
  ## function to create tidy data set from
  
  ## load in data sets
  ## assumes downloaded data is in current working directory
  ## .\UCI HAR Dataset
  
  
  ## read in data features
  projectDir  <- "UCI Har Dataset"
  fileName  <- "\\features.txt"
  features <- read.table(paste(projectDir,fileName,sep="")) 
  
  ## training data
  dataDir  <- paste(projectDir,"\\train\\",sep="")
  fileName <- "X_train.txt"
  ## actual data
  dataTrainX <- read.table(paste(dataDir,fileName,sep=""))
  ## add column names
  colnames(dataTrainX)  <- features[,2]
  ## subjects
  fileName <- "subject_train.txt"
  dataTrainSubj <- read.table(paste(dataDir,fileName,sep=""))
  ## add subject to data
  dataTrainXSubj <- cbind(dataTrainSubj,dataTrainX)
  ## activity
  fileName <- "Y_train.txt"
  dataTrainY <- read.table(paste(dataDir,fileName,sep=""))
  ## add activity to data
  trainFull <- cbind(dataTrainY,dataTrainXSubj)
                         
  ## test data
  dataDir  <- paste(projectDir,"\\test\\",sep="")
  fileName <- "X_test.txt"
  ## actual data
  dataTestX <- read.table(paste(dataDir,fileName,sep=""))
  ## add column names
  colnames(dataTestX)  <- features[,2]
  ## subjects
  fileName <- "subject_test.txt"
  dataTestSubj <- read.table(paste(dataDir,fileName,sep=""))
  ## add subject to data
  dataTestXSubj <- cbind(dataTestSubj,dataTestX)
  ## activity
  fileName <- "Y_test.txt"
  dataTestY <- read.table(paste(dataDir,fileName,sep=""))
  ## add activity to data
  testFull <- cbind(dataTestY,dataTestXSubj)
  
  
  ## merge the training and test data sets
  ## note the two data sets are identically formatted
  combinedData <- rbind(trainFull,testFull)
  
  ## find which columns contain mean & std measurements
  measName <- "mean()"
  meanColumns <- grep(measName,features[,2],fixed=TRUE)
  measName <- "std()"
  stdColumns <- grep(measName,features[,2],fixed=TRUE)
  ## combine and sort the mean & std columns
  measColumns <- sort(rbind(meanColumns,stdColumns))
  
  ## extract only mean & std columns
  selectedData <- subset(combinedData,select=measColumns)
  
  ## rename the activities to descriptive labels
  ## training data
  fileName <- "\\activity_labels.txt"
  ## actual data
  activityLabels <- read.table(paste(projectDir,fileName,sep=""))
  ## assigne labels to data
  ## this should be done via Lapply but have not figured out that yet :(
  selectedData$V1[which(selectedData$V1 == "1")] = as.character(activityLabels[1,2])
  selectedData$V1[which(selectedData$V1 == "2")] = as.character(activityLabels[2,2])
  selectedData$V1[which(selectedData$V1 == "3")] = as.character(activityLabels[3,2])
  selectedData$V1[which(selectedData$V1 == "4")] = as.character(activityLabels[4,2])
  selectedData$V1[which(selectedData$V1 == "5")] = as.character(activityLabels[5,2])
  selectedData$V1[which(selectedData$V1 == "6")] = as.character(activityLabels[6,2])
#    for (i in 1:6)
#    {selectedData$V1[which(as.integer(selectedData$V1) == i)] = activityLabels[i,2]}
#   
  ## assign activity/subject colnames
  colnames(selectedData)[1]  = "Activity"
  colnames(selectedData)[2]  = "Subject"

  ## going to leave the measurement descriptors as the features as I'm used to
  ## seeing terms like this

##  return(selectedData)

  ## this is the tidy part
  ## this technique is taken from discussion forums, I'm not sure I clearly understand it
  ## the selected data table is piped to group by which yield the grouping of the table
  ## summarize+each then applies mean() to each group
  tidyData <- selectedData %>% group_by(Subject, Activity) %>% summarise_each(funs(mean))

  return(tidyData)
}