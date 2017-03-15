
n<-2^16
setwd("C:/Users/H00037258/OneDrive/Documents/17 Spring/CS124/PA2")

result<-matrix(NA,ncol = 6,nrow = 100)
for(s in 4:100)
{
  n<-2^s
  ##pure strassen algorithm
  time1<-((7/4)^(log2(n))-1)*6*n^2+7^(log2(n))
  ##pure naive algortihem
  time2<-n^2*(2*n-1)
  ##basic cache =2*2
  time3<-((7/4)^(log2(n/2))-1)*6*n^2+7^(log2(n/2))*(2^2*(2*2-1))
  ##basic cache= 4*4
  time4<-((7/4)^(log2(n/4))-1)*6*n^2+7^(log2(n/4))*(4^2*(4*4-1))
  #basic cache= 8*8
  time5<-((7/4)^(log2(n/8))-1)*6*n^2+7^(log2(n/8))*(8^2*(8*8-1))
  #basic cache=16*16
  time6<-((7/4)^(log2(n/16))-1)*6*n^2+7^(log2(n/16))*(16^2*(16*16-1))
  result[s,]<-c(time1,time3,time4,time5,time6,time2)
  print(s)
}