rm(list=ls(all=TRUE))
setwd("C:/Users/vijeth8/Desktop/insofe/ECLS_Project/ECLS Project")
wave1data<-read.csv(file="wave1data.csv",sep=",",header = T)

numeric<-c()

for(i in 1:74)
{ if (class(wave1data[,i]) == "numeric") {numeric[i]<-i}
}
numeric <- na.omit(numeric)
wave1num<-wave1data[,numeric]
wave1fact<-wave1data[,-numeric]

library(vegan)
wave1<- decostand(wave1num, "range")
#wave1_std <- decostand(wave1num,"standardize")
wave1data_range<- data.frame(wave1,wave1fact)
#wave1data_std<-merge(wave1_std,wave1fact)
table(wave1data_range$WKPOV_R)

y<-split(wave1data_range,wave1data_range[,72])
below <- as.data.frame(y[2])
colnames(below) = colnames(wave1data_range)
write.table(x = below,file = "wave1below.csv",quote = TRUE,sep=",",col.names = T)

library(randomForest)
math_belowdata <- subset(below, select = -c(  C1R4RSCL,  C1RGSCAL))
reading_belowdata <-subset(below, select = -c(  C1R4MSCL,  C1RGSCAL))
gk_belowdata <- subset(below, select = -c(  C1R4RSCL,  C1R4MSCL))
#library("randomForest")
important20 <- function(rf)
{
   Imp_rf <- data.frame(rf$importance)
   Imp_rf$Attr = row.names(Imp_rf)
   Imp_rf <- data.frame(row.names(Imp_rf),Imp_rf[,1])
   colnames(Imp_rf) = c('Attributes','Importance')
   Imp_rf <- Imp_rf[order(-Imp_rf$Importance),]
   Imp_rf<- Imp_rf$Attributes[1:20]
   Imp_rf
}

set.seed(1234)
rf <- randomForest(  C1RGSCAL ~ ., data=gk_belowdata, keep.forest=TRUE, ntree=30) 
imp1gk_below <- important20(rf)
rf <- randomForest(  C1R4MSCL ~ ., data=math_belowdata, keep.forest=TRUE, ntree=30)
imp1math_below<- important20(rf)
rf <- randomForest(  C1R4RSCL ~ ., data=reading_belowdata, keep.forest=TRUE, ntree=30)
imp1reading_below <- important20(rf)


remove(below,gk_belowdata,math_belowdata,reading_belowdata,wave1,wave1data,wave1data_range,wave1num,wave1fact,
       i,numeric,rf,y)  
#----------------------------------------------------------------------

wave2data<-read.csv(file="wave2data.csv",sep=",",header = T)

numeric<-c()

for(i in 1:48)
{ if (class(wave2data[,i]) == "numeric") {numeric[i]<-i}
}
numeric <- na.omit(numeric)
wave2num<-wave2data[,numeric]
wave2fact<-wave2data[,-numeric]

library(vegan)
wave2<- decostand(wave2num, "range")
#wave1_std <- decostand(wave1num,"standardize")
wave2data_range<- data.frame(wave2,wave2fact)
#wave1data_std<-merge(wave1_std,wave1fact)

y<-split(wave2data_range,wave2data_range[,46])

below <- as.data.frame(y[2])
colnames(below) = colnames(wave2data_range)
write.table(x = below,file = "wave2below.csv",quote = TRUE,sep=",",col.names = T)

library(randomForest)
math_belowdata <- subset(below, select = -c(  C2R4RSCL,  C2RGSCAL))
reading_belowdata <-subset(below, select = -c(  C2R4MSCL,  C2RGSCAL))
gk_belowdata <- subset(below, select = -c(  C2R4RSCL,  C2R4MSCL))
#library("randomForest")
set.seed(1234)
rf <- randomForest(  C2R4MSCL ~ ., data=math_belowdata, keep.forest=TRUE, ntree=30)
imp2math_below2<- important20(rf)
rf <- randomForest(  C2R4RSCL ~ ., data=reading_belowdata, keep.forest=TRUE, ntree=30)
imp2reading_below2 <- important20(rf)
rf<- randomForest(  C2RGSCAL ~ ., data=gk_belowdata, keep.forest=TRUE, ntree=30) 
imp2gk_below2 <-important20(rf)

remove(below,gk_belowdata,math_belowdata,reading_belowdata,wave2,wave2data,
       wave2data_range,wave2num,wave2fact,
       i,numeric,rf,y)       

#-------------------------------------------------------------------
wave3data<-read.csv(file="wave3data.csv",sep=",",header = T)

numeric<-c()

for(i in 1:16)
{ if (class(wave3data[,i]) == "numeric") {numeric[i]<-i}
}
numeric <- na.omit(numeric)
wave3num<-wave3data[,numeric]
wave3fact<-wave3data[,-numeric]

library(vegan)
wave3<- decostand(wave3num, "range")
#wave1_std <- decostand(wave1num,"standardize")
wave3data_range<- data.frame(wave3,wave3fact)
#wave1data_std<-merge(wave1_std,wave1fact)

y<-split(wave3data_range,wave3data_range[,13])

below <- as.data.frame(y[2])
colnames(below) = colnames(wave3data_range)
write.table(x = below,file = "wave3below.csv",quote = TRUE,sep=",",col.names = T)

library(randomForest)
math_belowdata <- subset(below, select = -c(  C3R4RSCL,  C3RGSCAL))
reading_belowdata <-subset(below, select = -c(  C3R4MSCL,  C3RGSCAL))
gk_belowdata <- subset(below, select = -c(  C3R4RSCL,  C3R4MSCL))
#library("randomForest")
set.seed(1234)
rf <- randomForest(  C3R4MSCL ~ ., data=math_belowdata, keep.forest=TRUE, ntree=30)
imp3math_below<- important20(rf)
rf <- randomForest(  C3R4RSCL ~ ., data=reading_belowdata, keep.forest=TRUE, ntree=30)
imp3reading_below <- important20(rf)
rf <- randomForest(  C3RGSCAL ~ ., data=gk_belowdata, keep.forest=TRUE, ntree=30) 
imp3gk_below <-important20(rf)

remove(below,gk_belowdata,math_belowdata,reading_belowdata,wave3,wave3data,
       wave3data_range,wave3num,wave3fact,
       i,numeric,rf,y)       
#------------------------------------------------------------------

wave4data<-read.csv(file="wave4data.csv",sep=",",header = T)

numeric<-c()

for(i in 1:42)
{ if (class(wave4data[,i]) == "numeric") {numeric[i]<-i}
}
numeric <- na.omit(numeric)
wave4num<-wave4data[,numeric]
wave4fact<-wave4data[,-numeric]

library(vegan)
wave4<- decostand(wave4num, "range")
#wave1_std <- decostand(wave1num,"standardize")
wave4data_range<- data.frame(wave4,wave4fact)
#wave1data_std<-merge(wave1_std,wave1fact)

y<-split(wave4data_range,wave4data_range[,39])

below <- as.data.frame(y[2])
colnames(below) = colnames(wave4data_range)
write.table(x = below,file = "wave4below.csv",quote = TRUE,sep=",",col.names = T)

library(randomForest)
math_belowdata <- subset(below, select = -c(  C4R4RSCL,  C4RGSCAL))
reading_belowdata <-subset(below, select = -c(  C4R4MSCL,  C4RGSCAL))
gk_belowdata <- subset(below, select = -c(  C4R4RSCL,  C4R4MSCL))
#library("randomForest")
set.seed(1244)
rf <- randomForest(  C4R4MSCL ~ ., data=math_belowdata, keep.forest=TRUE, ntree=40)
imp4math_below<- important20(rf)
rf <- randomForest(  C4R4RSCL ~ ., data=reading_belowdata, keep.forest=TRUE, ntree=40)
imp4reading_below <- important20(rf)
rf <- randomForest(  C4RGSCAL ~ ., data=gk_belowdata, keep.forest=TRUE, ntree=40) 
imp4gk_below <-important20(rf)

remove(below,gk_belowdata,math_belowdata,reading_belowdata,wave4,wave4data,
       wave4data_range,wave4num,wave4fact,
       i,numeric,rf,y)   
#------------------------------------------------------
   
   wave5data<-read.csv(file="wave5data.csv",sep=",",header = T)

numeric<-c()

for(i in 1:50)
{ if (class(wave5data[,i]) == "numeric") {numeric[i]<-i}
}
numeric <- na.omit(numeric)
wave5num<-wave5data[,numeric]
wave5fact<-wave5data[,-numeric]

library(vegan)
wave5<- decostand(wave5num, "range")
#wave1_std <- decostand(wave1num,"standardize")
wave5data_range<- data.frame(wave5,wave5fact)
#wave1data_std<-merge(wave1_std,wave1fact)

y<-split(wave5data_range,wave5data_range[,47])

below <- as.data.frame(y[2])
colnames(below) = colnames(wave5data_range)
write.table(x = below,file = "wave5below.csv",quote = TRUE,sep=",",col.names = T)

library(randomForest)
math_belowdata <- subset(below, select = -c(  C5R4RSCL,  C5R2SSCL))
reading_belowdata <-subset(below, select = -c(  C5R4MSCL,  C5R2SSCL))
gk_belowdata <- subset(below, select = -c(  C5R4RSCL,  C5R4MSCL))
#library("randomForest")
set.seed(1255)
rf <- randomForest(  C5R4MSCL ~ ., data=math_belowdata, keep.forest=TRUE, ntree=50)
imp5math_below<- important20(rf)
rf <- randomForest(  C5R4RSCL ~ ., data=reading_belowdata, keep.forest=TRUE, ntree=50)
imp5reading_below <- important20(rf)
rf <- randomForest(  C5R2SSCL ~ ., data=gk_belowdata, keep.forest=TRUE, ntree=50) 
imp5gk_below <- important20(rf)

remove(below,gk_belowdata,math_belowdata,reading_belowdata,wave5,wave5data,
       wave5data_range,wave5num,wave5fact,
       i,numeric,rf,y)   
#-------------------------------------------------
   
   
   wave6data<-read.csv(file="wave6data.csv",sep=",",header = T)

numeric<-c()

for(i in 1:48)
{ if (class(wave6data[,i]) == "numeric") {numeric[i]<-i}
}
numeric <- na.omit(numeric)
wave6num<-wave6data[,numeric]
wave6fact<-wave6data[,-numeric]

library(vegan)
wave6<- decostand(wave6num, "range")
#wave1_std <- decostand(wave1num,"standardize")
wave6data_range<- data.frame(wave6,wave6fact)
#wave1data_std<-merge(wave1_std,wave1fact)

y<-split(wave6data_range,wave6data_range[,45])

below <- as.data.frame(y[2])
colnames(below) = colnames(wave6data_range)
write.table(x = below,file = "wave6below.csv",quote = TRUE,sep=",",col.names = T)

library(randomForest)
math_belowdata <- subset(below, select = -c(  C6R4RSCL,  C6R2SSCL))
reading_belowdata <-subset(below, select = -c(  C6R4MSCL,  C6R2SSCL))
gk_belowdata <- subset(below, select = -c(  C6R4RSCL,  C6R4MSCL))
#library("randomForest")
set.seed(1266)
rf <- randomForest(  C6R4MSCL ~ ., data=math_belowdata, keep.forest=TRUE, ntree=60)
imp6math_below<- important20(rf)
rf <- randomForest(  C6R4RSCL ~ ., data=reading_belowdata, keep.forest=TRUE, ntree=60)
imp6reading_below <- important20(rf)
rf <- randomForest(  C6R2SSCL ~ ., data=gk_belowdata, keep.forest=TRUE, ntree=60) 
imp6gk_below <- important20(rf)

remove(below,gk_belowdata,math_belowdata,reading_belowdata,wave6,wave6data,
       wave6data_range,wave6num,wave6fact,
       i,numeric,rf,y)   
#----------------------------------------------------
   
   
   
   wave7data<-read.csv(file="wave7data.csv",sep=",",header = T)

numeric<-c()

for(i in 1:61)
{ if (class(wave7data[,i]) == "numeric") {numeric[i]<-i}
}
numeric <- na.omit(numeric)
wave7num<-wave7data[,numeric]
wave7fact<-wave7data[,-numeric]

library(vegan)
wave7<- decostand(wave7num, "range")
#wave1_std <- decostand(wave1num,"standardize")
wave7data_range<- data.frame(wave7,wave7fact)
#wave1data_std<-merge(wave1_std,wave1fact)

y<-split(wave7data_range,wave7data_range[,58])

below <- as.data.frame(y[2])
colnames(below) = colnames(wave7data_range)
write.table(x = below,file = "wave7below.csv",quote = TRUE,sep=",",col.names = T)

library(randomForest)
math_belowdata <- subset(below, select = -c(  C7R4RSCL,  C7R2SSCL))
reading_belowdata <-subset(below, select = -c(  C7R4MSCL,  C7R2SSCL))
gk_belowdata <- subset(below, select = -c(  C7R4RSCL,  C7R4MSCL))
#library("randomForest")
set.seed(1277)
rf <- randomForest(  C7R4MSCL ~ ., data=math_belowdata, keep.forest=TRUE, ntree=70)
imp7math_below<- important20(rf)
rf <- randomForest(  C7R4RSCL ~ ., data=reading_belowdata, keep.forest=TRUE, ntree=70)
imp7reading_below <- important20(rf)
rf <- randomForest(  C7R2SSCL ~ ., data=gk_belowdata, keep.forest=TRUE, ntree=70) 
imp7gk_below <- important20(rf)

remove(below,gk_belowdata,math_belowdata,reading_belowdata,wave7,wave7data,
       wave7data_range,wave7num,wave7fact,
       i,numeric,rf,y)   

