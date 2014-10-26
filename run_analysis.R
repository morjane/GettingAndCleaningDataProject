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


run_analysis <- function(directory) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  
  if (file.exists(directory)){
    
    ##myactivity_labels<- read.csv("activity_labels.txt")
    
    test_folder <- paste(directory,"/test/Inertial Signals/",sep="")
    train_folder<- paste(directory,"/train/Inertial Signals/",sep="")
    
    mytestfiles<-list.files(test_folder,pattern="*.txt")
    mytrainfiles<-list.files(train_folder,pattern="*.txt")
    
    mypath <- paste(test_folder,mytestfiles[1],sep="")
    mytestdata<-read.csv(mypath)    
    mydata <- matrix(nrow=nrow(mytestdata),ncol=length(mytestfiles)+1)
    ##names(mydata)<-substring(mytestfiles, 1,nchar(mytestfiles)-4)
    
    ##print(dim(mydata))
    
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
      ##if (i==2) break
    }
    
    ##print(mynames)
    ##print(dim(mydata))
    
    mydata[,ncol(mydata)]<-rep("test",nrow(mydata)) 
    
    mynames <- c(mynames,"test_or_train")
    colnames(mydata)<-mynames
    
    ##print(colnames(mydata))
    
    outputtest <- paste(directory,"/data_bind.txt",sep="")  
    write.table(mydata,outputtest,append=TRUE, sep=",", row.names=FALSE, quote = FALSE)
    print(paste(i-1,"Test Done!"))
    
    ################
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
      
      ##mynames <- c(mynames,substring(mytestfile, 1,nchar(mytestfile)-4))
      
      mydata[,i] <- mytraindata[,1]
      
      
      i<-i+1
      ##if (i==2) break
    }
    
    ##print(mynames)
    ##print(dim(mydata))
    
    mydata[,ncol(mydata)]<-rep("train",nrow(mydata))  
    
    ##colnames(mydata)<-mynames
    
    ##print(colnames(mydata))
    
    ##outputtest <- paste(directory,"/data_bind.csv",sep="")  
    write.table(mydata,outputtest,append=TRUE, sep=",", col.names = FALSE,row.names=FALSE, quote = FALSE)
    print(paste(i-1,"Train Done!"))
    
  }
  else return("Directory Not Found")
  
}
