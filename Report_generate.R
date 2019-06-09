#install.packages("dplyr")
library("dplyr")

file_name=c('現金比率.csv','十大持股.csv','產業.csv','國家.csv','S&P Rating.csv','Security Type.csv','幣別.csv')
test<-read.csv(file_name[1],header = FALSE)
name=unique(test[,1])

for (i in c(2:length(name))) {
  range=which(test[,1]==as.character(name[i]))
  for (j in c(1:length(range))) {
    if(j==1)
      total=rbind(test[1,],as.data.frame(test[range[j],]))
    else
      total=rbind(total,as.data.frame(test[range[j],]))
  }
  
   for (k in c(2:6)) {
    file<-read.csv(file_name[k],header = FALSE)
    range_temp=which(file[,1]==as.character(name[i]))
    for (j in c(1:length(range_temp))) {
      blank=data.frame("")
      names(blank)<-NULL
      if(j==1&&k==2)
        total=plyr::rbind.fill(total,blank,as.data.frame(file[1,]),as.data.frame(file[range_temp[j],]))
    else if(j==1&&k!=2)
      total=plyr::rbind.fill(total,blank,as.data.frame(file[1,]),as.data.frame(file[range_temp[j],]))
    else
      total=plyr::rbind.fill(total,as.data.frame(file[range_temp[j],]))
    }
    write.table(total, file = paste0(name[i], '.csv'), row.names = FALSE,col.names = FALSE,na="",sep=",")
  }
}
remove(file_name,i,j,k,name,range,range_temp,blank,file,test,total)