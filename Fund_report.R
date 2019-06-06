#install.packages(c("dplyr","readxl"))
library("dplyr")
library("readxl")
test <- readxl::read_excel("201905.xlsx")

for (i in c(7:15)) {
  temp<-cbind(test$基金名稱,test[,i],test$權重)
  temp=aggregate(as.numeric(temp[,3]), by=list(temp[,1],temp[,2]), FUN=sum)
  temp=arrange(temp,temp[,1])
  write.csv(temp, file = paste0(names(test[i]), '.csv'), row.names = FALSE,fileEncoding = "utf-8")
}