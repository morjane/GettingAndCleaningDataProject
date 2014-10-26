## run_calc is a function that extracts only the measurements on the mean and standard deviation for each measurement,
## from the file that is merged using the run_analysis function.
## This function must be called after the run_analysis function
run_calc <- function(directory) {
  if (file.exists(directory)){  
    mypath <- paste(directory,"/data_bind.txt",sep="") 
    mydata<-read.csv(mypath) 
    
    print(dim(mydata))
    
    mydata <-na.omit(mydata[,1:9])
    
    mymean <- sapply(mydata,mean,na.rm=TRUE)
    mystd  <- sapply(mydata,sd,na.rm=TRUE)
    
    myoutput <- rbind(mean=mymean, stdev=mystd)
    colnames(myoutput) <- colnames(mydata[1:9])
    
    return(myoutput)
  }
  else return("Directory Not Found")  
}

## run_analysis is a function that merges the training and the test sets to create one data set.
## This data set is wrote in the data_bind.txt file 
run_analysis <- function(directory=getwd()) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the Samsung data files
  ## the directory must be the "UCI HAR Dataset" directory
  
  
  if (file.exists(directory)){
    
    test_folder <- paste(directory,"/test/Inertial Signals/",sep="")
    train_folder<- paste(directory,"/train/Inertial Signals/",sep="")
    
    mytestfiles<-list.files(test_folder,pattern="*.txt")
    mytrainfiles<-list.files(train_folder,pattern="*.txt")
    
    #########################
    ## Working on test files
    #########################
    
    mypath <- paste(test_folder,mytestfiles[1],sep="")
    mytestdata<-read.csv(mypath)    
    mydata <- matrix(nrow=nrow(mytestdata),ncol=length(mytestfiles)+1)
    
    i<-1
    
    mynames <-character(0)
    
    for(mytestfile in mytestfiles)
    {
      mypath <- paste(test_folder,mytestfile,sep="")
      print(mypath)
      mytestdata<-read.csv(mypath)
      
      print(dim(mytestdata))
      
      mynames <- c(mynames,substring(mytestfile, 1,nchar(mytestfile)-9))
      
      mydata[,i] <- mytestdata[,1]
      
      
      i<-i+1
    }
    
  
    mydata[,ncol(mydata)]<-rep("test",nrow(mydata)) 
    
    mynames <- c(mynames,"test_or_train")
    colnames(mydata)<-mynames
    
  
    outputtest <- paste(directory,"/data_bind.txt",sep="")  
    write.table(mydata,outputtest,append=TRUE, sep=",", row.names=FALSE, quote = FALSE)
    print(paste(i-1,"Test Done!"))
    
    #########################
    ## Working on train files
    #########################
    
    mypath <- paste(train_folder,mytrainfiles[1],sep="")
    mytraindata<-read.csv(mypath)
    mydata <- matrix(nrow=nrow(mytraindata),ncol=length(mytrainfiles)+1)
    i<-1
    
    for(mytrainfile in mytrainfiles)
    {
      mypath <- paste(train_folder,mytrainfile,sep="")
      print(mypath)
      mytraindata<-read.csv(mypath)
      
      print(dim(mytraindata))
      
      mydata[,i] <- mytraindata[,1]
      
      
      i<-i+1
    }
    
    
    mydata[,ncol(mydata)]<-rep("train",nrow(mydata))  
    
    write.table(mydata,outputtest,append=TRUE, sep=",", col.names = FALSE,row.names=FALSE, quote = FALSE)
    print(paste(i-1,"Train Done!"))
    
  }
  else return("Directory Not Found")
  
}
