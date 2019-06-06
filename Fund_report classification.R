#install.packages(c("dplyr","readxl"))
library("dplyr")
library("readxl")
test <- readxl::read_excel("201905.xlsx")
test$權重=round(test$權重,4)

for (i in c(1:ncol(test))) {
  for (j in c(1:nrow(test))) {
    if(grepl("N/A",test[j,i])){
      test[j,i]="N/A";
    }
  }
}

final<-cbind(test$基金名稱,test$月底規模)
colnames(final)<-c("基金名稱","規模") 
temp<-cbind(test$基金名稱,test$權重)
temp=aggregate(as.numeric(temp[,2]), by=list(temp[,1]), FUN=sum)

for (i in c(1:nrow(temp))) {
  if(0<temp[i,2]&&temp[i,2]<1&&(!is.na(temp[i,2]))){
    temp[i,2]=round(1-temp[i,2],4);
  }
}
colnames(temp)<-c("基金名稱","現金")
final=merge(final,temp,by="基金名稱")
final=unique(final)

mer=cbind(test$基金名稱,test[,7],test$權重)
mer=aggregate(as.numeric(mer[,3]), by=list(mer[,1],mer[,2]), FUN=sum)
colnames(mer)<-c("基金名稱","國家","比率")
final=merge(final,mer,all = TRUE)
final=do.call(rbind, lapply(split(final, final$基金名稱), function(x) x[order(x$比率,decreasing = TRUE),]))
write.csv(final, file = "現金&國家.csv", row.names = FALSE)

mer=cbind(test$基金名稱,test[,8],test$權重)
mer=aggregate(as.numeric(mer[,3]), by=list(mer[,1],mer[,2]), FUN=sum)
colnames(mer)<-c("基金名稱","產業","比率")
mydf <- as.data.frame(mer)
mydf=do.call(rbind, lapply(split(mydf, mydf$基金名稱), function(x) x[order(x$比率,decreasing = TRUE)[],]))
write.csv(mydf, file = "產業.csv", row.names = FALSE)

mer=cbind(test$基金名稱,test$股票名稱,test[,16],test$`GICS Sector Name`,test$`GICS Sub-Industry Name`,test$權重)
colnames(mer)<-c("基金名稱","公司","國家","產業","次產業","比率")
mydf <- as.data.frame(mer)
mydf=do.call(rbind, lapply(split(mydf, mydf$基金名稱), function(x) x[order(x$比率,decreasing = TRUE)[1:10],]))
write.csv(mydf, file = "十大持股.csv", row.names = FALSE)

for (i in c(10,11,15)) {
  temp<-cbind(test$基金名稱,test[,i],test$權重)
  temp=aggregate(as.numeric(temp[,3]), by=list(temp[,1],temp[,2]), FUN=sum)
  colnames(temp)<-c("基金名稱",colnames(test[i]),"比率")
  mydf <- as.data.frame(temp)
  if(i==10)
    mydf=do.call(rbind, lapply(split(mydf, mydf$基金名稱), function(x) x[order(x[,"S&P Rating"],decreasing = FALSE)[],]))
  else
    mydf=do.call(rbind, lapply(split(mydf, mydf$基金名稱), function(x) x[order(x$比率,decreasing = TRUE)[],]))
  write.csv(mydf, file = paste0(names(test[i]), '.csv'), row.names = FALSE)
}

# mer=cbind(test$基金名稱,test[,8],test[,9],test$權重)
# mer=aggregate(as.numeric(mer[,4]), by=list(mer[,1],mer[,2],mer[,3]), FUN=sum)
# colnames(mer)<-c("基金名稱","產業","次產業","比率")
# mydf <- as.data.frame(mer)
# mydf=do.call(rbind, lapply(split(mydf, mydf$基金名稱), function(x) x[order(x$比率,decreasing = TRUE),]))
# write.csv(mer, file = "subindustry.csv", row.names = FALSE)