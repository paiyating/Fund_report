#install.packages("dplyr")
library("dplyr")
test=read.csv('產業.csv')
name=unique(test[,1])
file_name=c('十大持股.csv','現金比率.csv','國家.csv','S&P Rating.csv','Security Type.csv','幣別.csv')

for (i in c(1:6)) {
  temp <- read.csv(file_name[i])
  if(i==1)
    names(temp)<-c("基金名稱","公司","國家",paste0("產業-",file_name[i]),"次產業",paste0("比率-",file_name[i]))
  else if(i==3)
    names(temp)<-c("基金名稱","國家別",paste0("比率-",file_name[i]))
  else  
    names(temp)[ncol(temp)]<-paste0("比率-",file_name[i])
  test=bind_rows(test,temp)
}
remove(temp)

for (i in c(1:length(name))) {
  range=which(test$基金名稱==as.character(name[i]))
  total=as.data.frame(test[range[1],])
  for (j in c(2:length(range))) {
    total=rbind(total,as.data.frame(test[range[j],]))
  }
  names(total)<-names(test)
  write.csv(total, file = paste0(name[i], '.csv'), row.names = FALSE,na="")
}
remove(i,j,test,total,range,file_name,name)