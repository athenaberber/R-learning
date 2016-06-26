complete<-function(directory,id=1:300){
   dataname<-dir(directory)
   n<-length(id)
   # contruct a dataframe to store the result
   dataset<-matrix(rep(0,n*2),nrow=n,ncol=2)
   dataset<-as.data.frame(dataset)
   colnames(dataset)<-c("id","nobs")
   for(i in 1:n){
      datapath<-paste("specdata",dataname[id[i]],sep="/")
      data<-read.table(datapath,sep=",",header=TRUE)
      data$sul<-is.na(data$sulfate)
      data$nit<-is.na(data$nitrate)
      data$complete<-data$sul+data$nit
      datasub<-data[(data$complete==0),]
      total<-nrow(datasub)
      dataset[i,1]<-id[i]   ## notice that we cannot write as dataset[,i],for this every row will have the same number
      dataset[i,2]<-total
   }
   return(dataset)
}