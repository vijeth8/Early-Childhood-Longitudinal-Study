rm(list=ls(all=T))
setwd("C:/Users/vijeth8/Desktop/insofe/ECLS_Project/ECLS Project")
data<-read.csv(file="ECLSData.csv",header=T,sep=',',na.strings = c("SUPPRESSED","NOT AVAILABLE","NOT APPLICABLE","DON'T KNOW","NOT ASCERTAINED","REFUSED","NA","OTHER","(other)","-1","-9","-8","-2","-7"))
   data2<-read.csv(file="ECLSData.csv",header=T,sep=',',na.strings = c("SUPPRESSED","NOT AVAILABLE","NOT APPLICABLE","DON'T KNOW","NOT ASCERTAINED","REFUSED","NA","OTHER","(other)"))
sum(is.na(data))

z <- c( "W1SESL","W3SESL","W5SESL","W8SESL","WKSESL")
data2<-data2[,colnames(data2) %in% z]
data[,colnames(data) %in% colnames(data2)] <- data2[,colnames(data2) %in% colnames(data)]

v<-c()
for(i in 1:ncol(data))
{
   if( sum(is.na(data[,i])/3143)*100 >= 40)
      {
      v<-append(v,i)
   }   }
#remove(v)
data <- data[,-v]
ncol(data)

#remove(data1)

#lapply(data[,1:468], 2, function(x) identical(x,x[,+1]))
x<-c()
#x<-duplicated(t(data))
#which
#identical(data[,21],data[,23])
#names(data[,21])
#colnames(data)[23]

#length(grep("-1",data$W3SESL))
names<-colnames(data)
names<-as.data.frame(names)

#n <- data[,which(grepl("^2+", names)==TRUE)]
library(sqldf)

datan<-sqldf("select * from names where names LIKE '%0'")
#data$P1THER10
#datan<-datan[-72,]
datan<-as.data.frame(datan)
datan<-as.vector(datan$names)
#data10<- subset(data,select = -c(datan))
#wave1data<- subset(data,select = c(wave1))
data<-data[ , !(colnames(data) %in% datan)]

library(DMwR)
data<-data[-manyNAs(data,0.55),]
data1<-data
#data<-knnImputation(data)
#sum(is.na(data))

remove(i,v,names,datan,data2,z)

x<-colnames(data)
x <- as.data.frame(x)
#str(data$S4ENRLS)
special<-sqldf("select * from x where x LIKE 'W%' ")
special<-as.vector(special$x)
data<-data[ , !(colnames(data) %in% special)]
#data<-subset(data,select = c(special_12),drop=T)

y<-colnames(data1)
y<-as.data.frame(y)

special_12<-sqldf("select * from y where y LIKE 'WK%'")
special_12<-as.vector(special_12$y)

   
x<-colnames(data)
x <- as.data.frame(x)
wave1<-sqldf("select * from x where x LIKE '_1%'")
wave1<- as.vector(wave1$x)
wave1data<- subset(data1,select = c(wave1,special_12,"GENDER","RACE"))
wave1data<-wave1data[-manyNAs(wave1data,0.70),]
wave1data<-knnImputation(wave1data)
write.table(x = wave1data,file = "wave1data.csv",quote = TRUE,sep=",",col.names = T)


wave2<-sqldf("select * from x where x LIKE '_2%'")
wave2<- as.vector(wave2$x)
x <- as.data.frame(x)
wave2data<- subset(data1,select = c(wave2,special_12,"GENDER","RACE"))
wave2data<-wave2data[-manyNAs(wave2data,0.70),]
wave2data<-knnImputation(wave2data)
write.table(x = wave2data,file = "wave2data.csv",quote = TRUE,sep=",",col.names = T)


special_34<-sqldf("select * from y where y LIKE 'W1%'")
special_34<-as.vector(special_34$y)
wave3<-sqldf("select * from x where x LIKE '_3%'")
wave3<- as.vector(wave3$x)
x <- as.data.frame(x)
wave3data<- subset(data1,select = c(wave3,special_34,"GENDER","RACE"))
wave3data<-wave3data[-manyNAs(wave3data,0.70),]
wave3data<-knnImputation(wave3data)
write.table(x = wave3data,file = "wave3data.csv",quote = TRUE,sep=",",col.names = T)

wave4<-sqldf("select * from x where x LIKE '_4%'")
wave4<- as.vector(wave4$x)
x <- as.data.frame(x)
wave4data<- subset(data1,select = c(wave4,special_34,"GENDER","RACE"))
wave4data<-wave4data[-manyNAs(wave4data,0.70),]
wave4data<-knnImputation(wave4data)
write.table(x = wave4data,file = "wave4data.csv",quote = TRUE,sep=",",col.names = T)

special_5<-sqldf(" select * from y where y LIKE 'W3%'")
special_5<-as.vector(special_5$y)
wave5<-sqldf("select * from x where x LIKE '_5%'")
wave5<- as.vector(wave5$x)
x <- as.data.frame(x)
wave5data<- subset(data1,select = c(wave5,special_5,"GENDER","RACE"))
wave5data<-wave5data[-manyNAs(wave5data,0.70),]
wave5data<-knnImputation(wave5data)
write.table(x = wave5data,file = "wave5data.csv",quote = TRUE,sep=",",col.names = T)

wave6<-sqldf("select * from x where x LIKE '_6%'")
wave6<- as.vector(wave6$x)
x <- as.data.frame(x)
special_6<-sqldf("select * from y where y LIKE 'W5%'")
special_6<-as.vector(special_6$y)
wave6data<- subset(data1,select = c(wave6,special_6,"GENDER","RACE"))
#wave6data<-wave6data[-manyNAs(wave6data,0.70),]
wave6data<-knnImputation(wave6data)
write.table(x = wave6data,file = "wave6data.csv",quote = TRUE,sep=",",col.names = T)

wave7<-sqldf("select * from x where x LIKE '_7%'")
wave7<- as.vector(wave7$x)
x <- as.data.frame(x)
special_7<-sqldf("select * from y where y LIKE 'W8%'")
special_7<-as.vector(special_7$y)
wave7data<- subset(data1,select = c(wave7,special_7,"GENDER","RACE"))
wave7data<-wave7data[-manyNAs(wave7data,0.70),]
wave7data<-knnImputation(wave7data)
write.table(x = wave7data,file = "wave7data.csv",quote = TRUE,sep=",",col.names = T)


#sapply(data[,1:397],MARGIN=2, function(x) table(x))
#table(data[,1])
