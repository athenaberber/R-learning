corr<-function(directory,threshold=0){
   dataname<-dir(directory)
   correlation<-c()
   # contruct a dataframe to store the result
   for(i in 1:332){
      datapath<-paste("specdata",dataname[i],sep="/")
      data<-read.table(datapath,sep=",",header=TRUE)
      data$sul<-is.na(data$sulfate)
      data$nit<-is.na(data$nitrate)
      data$complete<-data$sul+data$nit
      datasub<-data[(data$complete==0),]
      total<-nrow(datasub)
      if(total>threshold){
         correlation<-c(correlation,cor(datasub$sulfate,datasub$nitrate))
      }
   }
   return(correlation)
}