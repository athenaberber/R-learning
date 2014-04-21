pollutantmean<-function(directory,pollutant,id=1:332){
   data_name<-dir(directory)   
   n<-length(id)
   # judge sulfate or 
   index<-ifelse(pollutant=="sulfate",2,3) # index is for choosing the column that used in the later 
   count<-0     # give an initial values
   total<-0    
   for(i in 1:n){
      datapath<-paste("specdata",data_name[id[i]],sep="/")   # datapath must have / cannot use other symbols
      #if(i==1) print(datapath)
      data<-read.table(datapath,sep=",",header=TRUE)
      temp<-!is.na(data[,index])  # exclude the NA
      count<-count+sum(temp)
      total<-total+sum(data[,index][temp])  # gradually add up and calculate their as one to get mean 
   }
   return(total/count)
}