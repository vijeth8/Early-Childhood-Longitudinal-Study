#loading data
rm(list=ls(all=TRUE))
setwd("C:/Users/vijeth8/Desktop/insofe/ECLS_Project/ECLS Project")
wave1data<-read.csv(file="wave1data.csv",sep=",",header = T)

#splitting into numeric and categorical 
numeric<-c()
for(i in 1:74)
{ if (class(wave1data[,i]) == "numeric") {numeric[i]<-i}
}
numeric <- na.omit(numeric)
wave1num<-wave1data[,numeric]
wave1fact<-wave1data[,-numeric]

#standardizing the numeric data
library(vegan)
wave1<- decostand(wave1num, "range")
wave1data_range<- data.frame(wave1,wave1fact)
table(wave1data_range$WKPOV_R)

#splitting data and etracting the abov poverty data. Writing it as a csv file
y<-split(wave1data_range,wave1data_range[,72])
above <- as.data.frame(y[1])
colnames(above) = colnames(wave1data_range)
write.table(x = above,file = "wave1above.csv",quote = TRUE,sep=",",col.names = T)


#removing reading and science scores from the math data as they bring some bias when extracting 
#top 20 attributes. Similarly for other attributes also
math_abovedata <- subset(above, select = -c(C1R4RSCL,
                                            C1RGSCAL))
reading_abovedata <-subset(above, select = -c(C1R4MSCL,
                                              C1RGSCAL))
gk_abovedata <- subset(above, select = -c(C1R4RSCL,
                                          C1R4MSCL))
#creating a function to extract the top 20 attributes

important20 <- function(x)
{
   Imp_x <- data.frame(x$importance)
   Imp_x$Attr = row.names(Imp_x)
   Imp_x <- data.frame(row.names(Imp_x),Imp_x[,1])
   colnames(Imp_x) = c('Attributes','Importance')
   Imp_x <- Imp_x[order(-Imp_x$Importance),]
   Imp_x<- Imp_x$Attributes[1:20]
   Imp_x
}

#running random forest and extracting top 20 attibutes for math, reading and gk
library("randomForest")
set.seed(1234)
rf <- randomForest(  C1RGSCAL ~ .,
                     data=gk_abovedata, keep.forest=TRUE, ntree=30) 
imp1gk_above <- important20(rf)
rf <- randomForest(  C1R4MSCL ~ .,
                     data=math_abovedata, keep.forest=TRUE, ntree=30)
imp1math_above<- important20(rf)
rf <- randomForest(  C1R4RSCL ~ .,
                     data=reading_abovedata, keep.forest=TRUE, ntree=30)
imp1reading_above <- important20(rf)

#removing unnecessary objects
remove(above,gk_abovedata,math_abovedata,reading_abovedata,wave1,wave1data,
       wave1data_range,wave1num,wave1fact,i,numeric,rf,y)       
#-------------------------------------------------------------
   wave2data<-read.csv(file="wave2data.csv",sep=",",header = T)

#dividing the data into numeric and categoical data
numeric<-c()

for(i in 1:48)
{ if (class(wave2data[,i]) == "numeric") {numeric[i]<-i}
}
numeric <- na.omit(numeric)
wave2num<-wave2data[,numeric]
wave2fact<-wave2data[,-numeric]


#standardizing the numeric data and  combining it with categorical data to form the original data
library(vegan)
wave2<- decostand(wave2num, "range")
wave2data_range<- data.frame(wave2,wave2fact)

#splitting the data to extract only the above poverty data and write it
y<-split(wave2data_range,wave2data_range[,46])
above <- as.data.frame(y[1])
colnames(above) = colnames(wave2data_range)
write.table(x = above,file = "wave2above.csv",quote = TRUE,sep=",",col.names = T)

#removing reading and science foe math, math and science for redding..etc. to avoid bias
math_abovedata <- subset(above, select = -c(  C2R4RSCL,
                                              C2RGSCAL))
reading_abovedata <-subset(above, select = -c(  C2R4MSCL,
                                                C2RGSCAL))
gk_abovedata <- subset(above, select = -c(  C2R4RSCL,
                                            C2R4MSCL))

#running random forest and extracting the top20 attributes
library("randomForest")
set.seed(1234)
rf <- randomForest(  C2R4MSCL ~ .,data=math_abovedata, 
                     keep.forest=TRUE, ntree=30)
imp2math_above<- important20(rf)
rf <- randomForest(  C2R4RSCL ~ .,data=reading_abovedata,
                     keep.forest=TRUE, ntree=30)
imp2reading_above <- important20(rf)
rf<- randomForest(  C2RGSCAL ~ ., data=gk_abovedata, 
                    keep.forest=TRUE, ntree=30) 
imp2gk_above <-important20(rf)


#removing unnecessary objects
remove(above,gk_abovedata,math_abovedata,reading_abovedata,wave2,wave2data,
       wave2data_range,wave2num,wave2fact,
       i,numeric,rf,y)       
#-------------------------------------------------------------------
##loading wave 3 data
   wave3data<-read.csv(file="wave3data.csv",sep=",",header = T)

#dividing wave3 data into factor and categoric data
numeric<-c()

for(i in 1:16)
{ if (class(wave3data[,i]) == "numeric") {numeric[i]<-i}
}
numeric <- na.omit(numeric)
wave3num<-wave3data[,numeric]
wave3fact<-wave3data[,-numeric]

#standrdizing the numeric data
library(vegan)
wave3<- decostand(wave3num, "range")
wave3data_range<- data.frame(wave3,wave3fact)

#splitting the data and extracting the above poverty data
y<-split(wave3data_range,wave3data_range[,13])
above <- as.data.frame(y[1])
colnames(above) = colnames(wave3data_range)
write.table(x = above,file = "wave3above.csv",quote = TRUE,sep=",",col.names = T)

#removing the attributes that cause bias in the prediction for each of math, reading and gk
math_abovedata <- subset(above, select = -c(  C3R4RSCL,
                                              C3RGSCAL))
reading_abovedata <-subset(above, select = -c(  C3R4MSCL,
                                                C3RGSCAL))
gk_abovedata <- subset(above, select = -c(  C3R4RSCL,
                                            C3R4MSCL))

#running random forests and extracting the top 20 attributes
library("randomForest")
set.seed(1234)
rf <- randomForest(  C3R4MSCL ~ ., data=math_abovedata,
                     keep.forest=TRUE, ntree=30)
imp3math_above<- important20(rf)
rf <- randomForest(  C3R4RSCL ~ .,data=reading_abovedata,
                     keep.forest=TRUE, ntree=30)
imp3reading_above <- important20(rf)
rf <- randomForest(  C3RGSCAL ~ ., data=gk_abovedata,
                     keep.forest=TRUE, ntree=30) 
imp3gk_above <-important20(rf)

#removing the unnecessary objects
remove(above,gk_abovedata,math_abovedata,reading_abovedata,wave3,wave3data,
       wave3data_range,wave3num,wave3fact,
       i,numeric,rf,y)       

#------------------------------------------------------------------
   
#importing wave 4 data
   wave4data<-read.csv(file="wave4data.csv",sep=",",header = T)

#splitting he data into factor and categorical
numeric<-c()

for(i in 1:42)
{ if (class(wave4data[,i]) == "numeric") {numeric[i]<-i}
}
numeric <- na.omit(numeric)
wave4num<-wave4data[,numeric]
wave4fact<-wave4data[,-numeric]

#stadardizing the numeric data
library(vegan)
wave4<- decostand(wave4num, "range")
wave4data_range<- data.frame(wave4,wave4fact)

#splitting the data and extracting the above poverty data
y<-split(wave4data_range,wave4data_range[,39])
above <- as.data.frame(y[1])
colnames(above) = colnames(wave4data_range)
write.table(x = above,file = "wave4above.csv",quote = TRUE,sep=",",col.names = T)

#removing the attributes that cause bias to the target. for each of mah reading and scince
math_abovedata <- subset(above, select = -c(  C4R4RSCL,
                                              C4RGSCAL))
reading_abovedata <-subset(above, select = -c(  C4R4MSCL,
                                                C4RGSCAL))
gk_abovedata <- subset(above, select = -c(  C4R4RSCL,
                                            C4R4MSCL))
#running random forests
library("randomForest")
set.seed(1244)
rf <- randomForest(  C4R4MSCL ~ ., data=math_abovedata,
                     keep.forest=TRUE, ntree=40)
imp4math_above<- important20(rf)
rf <- randomForest(  C4R4RSCL ~ ., data=reading_abovedata,
                     keep.forest=TRUE, ntree=40)
imp4reading_above <- important20(rf)
rf <- randomForest(  C4RGSCAL ~ ., data=gk_abovedata,
                     keep.forest=TRUE, ntree=40) 
imp4gk_above <-important20(rf)

#removing th objects that are unnecessary
remove(above,gk_abovedata,math_abovedata,reading_abovedata,wave4,wave4data,
       wave4data_range,wave4num,wave4fact,
       i,numeric,rf,y)   
#------------------------------------------------------
   #loading wave 5 data
   wave5data<-read.csv(file="wave5data.csv",sep=",",header = T)

#splitting the data into numeric and categorical
numeric<-c()

for(i in 1:50)
{ if (class(wave5data[,i]) == "numeric") {numeric[i]<-i}
}
numeric <- na.omit(numeric)
wave5num<-wave5data[,numeric]
wave5fact<-wave5data[,-numeric]

#standardizing the numerc data
library(vegan)
wave5<- decostand(wave5num, "range")
wave5data_range<- data.frame(wave5,wave5fact)

#splitting the data and extracting the above poverty data
y<-split(wave5data_range,wave5data_range[,47])
above <- as.data.frame(y[1])
colnames(above) = colnames(wave5data_range)
write.table(x = above,file = "wave5above.csv",quote = TRUE,sep=",",col.names = T)

#running random forests
math_abovedata <- subset(above, select = -c(  C5R4RSCL,
                                              C5R2SSCL))
reading_abovedata <-subset(above, select = -c(  C5R4MSCL,
                                                C5R2SSCL))
gk_abovedata <- subset(above, select = -c(  C5R4RSCL,
                                            C5R4MSCL))
library("randomForest")
set.seed(1255)
rf <- randomForest(  C5R4MSCL ~ ., data=math_abovedata, 
                     keep.forest=TRUE, ntree=50)
imp5math_above<- important20(rf)
rf <- randomForest(  C5R4RSCL ~ .,data=reading_abovedata,
                     keep.forest=TRUE, ntree=50)
imp5reading_above <- important20(rf)
rf <- randomForest(  C5R2SSCL ~ ., data=gk_abovedata,
                     keep.forest=TRUE, ntree=50) 
imp5gk_above <- important20(rf)

remove(above,gk_abovedata,math_abovedata,reading_abovedata,wave5,wave5data,
       wave5data_range,wave5num,wave5fact,
       i,numeric,rf,y)   
#-------------------------------------------------
   
#loading wave 6 data
   wave6data<-read.csv(file="wave6data.csv",sep=",",header = T)

#splitting the data into numeric and categorical
numeric<-c()

for(i in 1:48)
{ if (class(wave6data[,i]) == "numeric") {numeric[i]<-i}
}
numeric <- na.omit(numeric)
wave6num<-wave6data[,numeric]
wave6fact<-wave6data[,-numeric]

#standardizing the numeric data
library(vegan)
wave6<- decostand(wave6num, "range")
wave6data_range<- data.frame(wave6,wave6fact)

#splitting the data and extracting above povery data
y<-split(wave6data_range,wave6data_range[,45])
above <- as.data.frame(y[1])
colnames(above) = colnames(wave6data_range)
write.table(x = above,file = "wave6above.csv",quote = TRUE,sep=",",col.names = T)


#removing the attributes that cause bias to the target attribtes
math_abovedata <- subset(above, select = -c(  C6R4RSCL,
                                              C6R2SSCL))
reading_abovedata <-subset(above, select = -c(  C6R4MSCL,
                                                C6R2SSCL))
gk_abovedata <- subset(above, select = -c(  C6R4RSCL,
                                            C6R4MSCL))
#running random forests and extracting the top 20 attributes
library("randomForest")
set.seed(1266)
rf <- randomForest(  C6R4MSCL ~ ., data=math_abovedata, 
                     keep.forest=TRUE, ntree=60)
imp6math_above<- important20(rf)
rf <- randomForest(  C6R4RSCL ~ ., data=reading_abovedata, 
                     keep.forest=TRUE, ntree=60)
imp6reading_above <- important20(rf)
rf <- randomForest(  C6R2SSCL ~ ., data=gk_abovedata, 
                     keep.forest=TRUE, ntree=60) 
imp6gk_above <- important20(rf)

remove(above,gk_abovedata,math_abovedata,reading_abovedata,wave6,wave6data,
       wave6data_range,wave6num,wave6fact,
       i,numeric,rf,y)   
#----------------------------------------------------
   
#loading wave 7 data
   
   wave7data<-read.csv(file="wave7data.csv",sep=",",header = T)

#splitting the data into numeric and categorical

numeric<-c()

for(i in 1:61)
{ if (class(wave7data[,i]) == "numeric") {numeric[i]<-i}
}
numeric <- na.omit(numeric)
wave7num<-wave7data[,numeric]
wave7fact<-wave7data[,-numeric]

#syandardizing the numric data
library(vegan)
wave7<- decostand(wave7num, "range")
wave7data_range<- data.frame(wave7,wave7fact)

#split the data and extract the abovepoverty data
y<-split(wave7data_range,wave7data_range[,58])
above <- as.data.frame(y[1])
colnames(above) = colnames(wave7data_range)
write.table(x = above,file = "wave7above.csv",quote = TRUE,sep=",",col.names = T)

#removing the attributes that create a bias to the target, for each of math,reading and science
math_abovedata <- subset(above, select = -c(  C7R4RSCL,
                                              C7R2SSCL))
reading_abovedata <-subset(above, select = -c(  C7R4MSCL,
                                                C7R2SSCL))
gk_abovedata <- subset(above, select = -c(  C7R4RSCL,
                                            C7R4MSCL))

#running random forests and extracting top 20 attributes 
library("randomForest")
set.seed(1277)
rf <- randomForest(  C7R4MSCL ~ ., data=math_abovedata, 
                     keep.forest=TRUE, ntree=70)
imp7math_above<- important20(rf)
rf <- randomForest(  C7R4RSCL ~ ., data=reading_abovedata, 
                     keep.forest=TRUE, ntree=70)
imp7reading_above <- important20(rf)
rf <- randomForest(  C7R2SSCL ~ ., data=gk_abovedata, 
                     keep.forest=TRUE, ntree=70) 
imp7gk_above <- important20(rf)

#removing unnecessary objects
remove(above,gk_abovedata,math_abovedata,reading_abovedata,wave7,wave7data,
       wave7data_range,wave7num,wave7fact,
       i,numeric,rf,y)   
#---------------------------------------------------------------------------------
   #combining the top 20 attributes for all of the 7 waves
   math_analysis<- data.frame(imp1math_above,imp2math_above,imp3math_above,
                              imp4math_above,imp5math_above,imp6math_above,imp7math_above)
gk_analysis <- data.frame(imp1gk_above,imp2gk_above,imp3gk_above,imp4gk_above,
                          imp5gk_above,imp6gk_above,imp7gk_above)
reading_analysis <- data.frame(imp1reading_above,imp2reading_above,imp3reading_above,
                               imp4reading_above,imp5reading_above,imp6reading_above,imp7gk_above)


## WKSESL- SES MEASURE
##AGE - CHILD ASSESSMENT 
##T1INTERN - INTERNALIZING PROBLEM BEHAVIOR --5
##C1BMI - BMI
##WKPARED - PARENT HIGHEST EDUCATIONAL LEVEL
## RACE 
## CONTRO - SELF CONTROL -- 5
#5# MINOR - MINORITY STUDENTS -6
#5#INTERP - INTERPERsonal