rm(list = ls()) #clear environment


#import features and activity labels
features <- read.table('features.txt',header=FALSE);
activityLabels <- read.table('activity_labels.txt',header=FALSE); 
colnames(activityLabels) <- c("activityId","activity");

#function to read a data set
readDataSet <- function(name)
{
	subject <- read.table(file.path(name,paste("subject_",name,".txt",sep='')),header=FALSE);
	x <- read.table(file.path(name,paste("x_",name,".txt",sep='')),header=FALSE);
	y <- read.table(file.path(name,paste("y_",name,".txt",sep='')),header=FALSE);

	colnames(subject) <- "subjectId";
	colnames(x) = features[,2]; 
	colnames(y) <- "activityId";	
	data <- cbind(subject,y,x);
	return(data);
}
trainData<-readDataSet("train");
testData<-readDataSet("test");
data<-rbind(trainData,testData);

colNames<-colnames(data);
validCols<-grep("activityId|subjectId|-mean\\(\\)|-std\\(\\)", colNames);
data<-data[,validCols];
colnames(data) <- gsub("\\(|\\)", "", colnames(data));

tidyData <- aggregate(data[,!(colnames(data) %in% c('activityId','subjectId'))],by=list(activityId=data$activityId,subjectId=data$subjectId),FUN=mean);
tidyData <- merge(activityLabels,tidyData,by='activityId',all.x=TRUE);
write.table(tidyData,"tidy_data.txt",row.name=FALSE);

