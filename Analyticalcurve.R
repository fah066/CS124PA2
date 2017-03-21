library(data.table)
setwd("C:/Users/H00037258/OneDrive/Documents/17 Spring/CS124/PA2")
len=10
result<-matrix(NA,ncol = 7,nrow = len)
for(s in 4:len)
{
  n<-2^s
  ##pure strassen algorithm
  time1<-((7/4)^(log2(n))-1)*6*n^2+7^(log2(n))
  ##pure naive algortihem
  time2<-n^2*(2*n-1)
  ##basic cache =2*2
  time3<-((7/4)^(log2(n/2))-1)*6*n^2+7^(log2(n/2))*(2^2*(2*2-1))
  ##basic cache= 4*4
  time4<-((7/4)^(log2(n/4))-1)*6*n^2+7^(log2(n/4))*(4^2*(2*4-1))
  #basic cache= 8*8
  time5<-((7/4)^(log2(n/8))-1)*6*n^2+7^(log2(n/8))*(8^2*(2*8-1))
  #basic cache=16*16
  time6<-((7/4)^(log2(n/16))-1)*6*n^2+7^(log2(n/16))*(16^2*(2*16-1))
  result[s,2:7]<-c(time1,time3,time4,time5,time6,time2)
  #result[s,2:7]<-log2(result[s,2:7])
  result[s,1]<-n
}
result<-as.data.frame(result[])
names(result)<-c("n","Strassen's","2*2","4*4","8*8","16*16","Conventional")
data_long <- melt(result, id="n")  
names(data_long)[3]<-"time"
library(ggplot2)
library(data.table)
ggplot(data=data_long,
       aes(x=n, y=time, colour=variable,size=5)) +
  geom_line(size=2)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
library(xtable)
print(xtable(result))


len=8
n<-2^len
result<-matrix(NA,ncol = 2,nrow = len)
for(i in 1:len)
{
  temp=2^i
  result[i,1]<-temp
  result[i,2]<-(((7/4)^(log2(n/temp))-1)*6*n^2+7^(log2(n/temp))*(temp^2*(2*temp-1)))
}
result<-as.data.frame(result)
names(result)<-c("cross","runtime")
ggplot(data=result,
       aes(x=cross, y=runtime)) +
  geom_line(size=1)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

data.1025<-read.table("record_1025",header = F)
data.1025$V1<-as.numeric(gsub("cross_over:", "", data.1025$V1))
data.1025<-aggregate(data.1025,list(data.1025$V1),mean)
data.1025<-as.data.frame(cbind(data.1025$V1,data.1025$V2))
names(data.1025)<-c("cross","time")
ggplot(data=data.1025,aes(x=cross, y=time))+geom_line(size=1)

data.1024<-data.1024[data.1024$cross<300,]
data.1025<-data.1025[data.1025$cross<300,]
data.2048<-data.2048[data.2048$cross<400,]
data.2049<-data.2049[data.2049$cross<400,]
data.1024$time<-round(data.1024$time,2)
data.1025$time<-round(data.1025$time,2)
data.2048$time<-round(data.2048$time,2)
data.2049$time<-round(data.2049$time,2)

data.2000<-cbind((data.2048$cross),(data.2048$time),(data.2049$time))
data.1000<-cbind((data.1024$cross),data.1024$time,data.1025$time)
