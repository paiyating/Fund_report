#install.packages(c("dplyr","readxl"))
library("dplyr")
library("readxl")
test <- readxl::read_excel("201905.xlsx")
test$權重=round(test$權重,4)
#轉換成中文
# currency <- readxl::read_excel("貨幣.xlsx")
# test$幣別=plyr::mapvalues(test$幣別,from = t(currency[,1]),to=t(currency[,2]))
# # country <- readxl::read_excel("國家別.xlsx")
# test$國家=plyr::mapvalues(test$國家,from = t(country[,1]),to=t(country[,2]))
# issue <- readxl::read_excel("發行國.xlsx")
# test$`ISO Country of Issue`=plyr::mapvalues(test$`ISO Country of Issue`,from=t(issue[,1]),to=t(issue[,2]))
#remove(currency,country,issue)

for (i in c(7:ncol(test))) {
  for (j in c(1:nrow(test))) {
    if(grepl("N/A",test[j,i])){
      test[j,i]="N/A";
    }
  }
}
for (i in c(1:nrow(test))) {
  if(is.na(test$權重[i])){
    test$權重[i]=0;
  }
}
final<-cbind(test$基金名稱,test$月底規模)
colnames(final)<-c("基金名稱","規模") 
final=unique(final)
temp<-cbind(test$基金名稱,test$權重)
holding=aggregate(as.numeric(temp[,2]), by=list(temp[,1]), FUN=sum)
colnames(holding)<-c("基金名稱","持股")
holding=mutate(as.data.frame(holding),"現金"=round(1-as.numeric(holding[,2]),4))
for (i in c(1:nrow(holding))) {
  if(((0>holding$持股[i])|(holding$持股[i]>1))&&(!is.na(holding$持股[i]))){
    holding$現金[i]=0;
  }
}
final=merge(final,holding,by="基金名稱")
write.csv(final, file = "現金比率.csv", row.names = FALSE)
remove(holding,j,final,temp)

mer=cbind(test$基金名稱,test[,7],test$權重)
mer=aggregate(as.numeric(mer[,3]), by=list(mer[,1],mer[,2]), FUN=sum)
colnames(mer)<-c("基金名稱","國家","比率")
mer=do.call(rbind, lapply(split(mer, mer$基金名稱), function(x) x[order(x$比率,decreasing = TRUE),]))
write.csv(mer, file = "國家.csv", row.names = FALSE)
mer<-NULL

mer=cbind(test$基金名稱,test[,8],test$權重)
mer=aggregate(as.numeric(mer[,3]), by=list(mer[,1],mer[,2]), FUN=sum)
colnames(mer)<-c("基金名稱","產業","比率")
mer <- as.data.frame(mer)
mer=do.call(rbind, lapply(split(mer, mer$基金名稱), function(x) x[order(x$比率,decreasing = TRUE)[],]))
write.csv(mer, file = "產業.csv", row.names = FALSE)
mer<-NULL

mer=cbind(test$基金名稱,test$股票名稱,test[,16],test$`GICS Sector Name`,test$`GICS Sub-Industry Name`,test$權重)
colnames(mer)<-c("基金名稱","公司","國家","產業","次產業","比率")
mer <- as.data.frame(mer)
mer=do.call(rbind, lapply(split(mer, mer$基金名稱), function(x) x[order(x$比率,decreasing = TRUE)[1:10],]))
mer=mer[complete.cases(mer[,1]),]
write.csv(mer, file = "十大持股.csv", row.names = FALSE)
mer<-NULL

for (i in c(10,11,15)) {
  mer<-cbind(test$基金名稱,test[,i],test$權重)
  mer=aggregate(as.numeric(mer[,3]), by=list(mer[,1],mer[,2]), FUN=sum)
  colnames(mer)<-c("基金名稱",colnames(test[i]),"比率")
  mer <- as.data.frame(mer)
  if(i==10)
    mer=do.call(rbind, lapply(split(mer, mer$基金名稱), function(x) x[order(x[,"S&P Rating"],decreasing = FALSE)[],]))
  else
    mer=do.call(rbind, lapply(split(mer, mer$基金名稱), function(x) x[order(x$比率,decreasing = TRUE)[],]))
  write.csv(mer, file = paste0(names(test[i]), '.csv'), row.names = FALSE)
}
remove(i,mer,test)
# mer=cbind(test$基金名稱,test[,8],test[,9],test$權重)
# mer=aggregate(as.numeric(mer[,4]), by=list(mer[,1],mer[,2],mer[,3]), FUN=sum)
# colnames(mer)<-c("基金名稱","產業","次產業","比率")
# mydf <- as.data.frame(mer)
# mydf=do.call(rbind, lapply(split(mydf, mydf$基金名稱), function(x) x[order(x$比率,decreasing = TRUE),]))
# write.csv(mer, file = "subindustry.csv", row.names = FALSE)