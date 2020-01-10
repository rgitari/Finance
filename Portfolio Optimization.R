load(file="B1.RData")
library(CVXR)
library(tidyverse)

mean=B1$mean
sd=B1$sd
sd
t(sd)
cor=B1$cor
VCV=sd%*%t(sd)*cor
vec1=rep(1,length=length(mean))
rf=B1$rf
#MSR Allocation
alloc.r.MSR=t(solve(VCV))%*%(mean-rf)/
  as.numeric(t(vec1)%*%solve(VCV)%*%(mean-rf))
alloc.r.MSR=t(alloc.r.MSR)
colnames(alloc.r.MSR)<-c('short-term bonds', 'medium-term bonds', 'long-term bonds', 'Canadian equity', 'US equity', 'non-North American equity')
#add the weight of risk-free asset
(alloc.MSR=transform(alloc.r.MSR,rf=1-sum(alloc.r.MSR)))

#GMV allocation
alloc.r.GMV=t(solve(VCV)%*%vec1)/as.numeric(t(vec1)%*%solve(VCV)%*%vec1)
colnames(alloc.r.GMV)<-colnames(alloc.r.MSR)
(alloc.GMV=transform(alloc.r.GMV,rf=1-sum(alloc.r.GMV)))

#combine the allocation results
alloc=as.matrix(rbind(alloc.MSR,alloc.GMV))
rownames(alloc)=c('MSR','GMV')
alloc 


nasset=6
TargetER=seq(0.012,0.071,by=0.001)
w=Variable(nasset)
r=t(w) %*% mean
var=quad_form(w,VCV)
obj=var
alloc.frontier=matrix(0,nrow=length(TargetER),ncol=nasset)
Std.frontier=rep(0,length(TargetER))

for(i in seq_along(TargetER)){
  constraints=list(w>0,sum(w)==1,r==TargetER[i])
  prob=Problem(Minimize(obj),constraints)
  result=solve(prob)
  alloc.frontier[i,]=result$getValue(w)
  Std.frontier[i]=result$getValue(sqrt(var))
}

ER.frontier=TargetER

#draw
plot_original<-plot(Std.frontier,ER.frontier
     ,type='l',col='blue',lwd=2
     ,main="MVF constrained"
     ,xlab='Portfolio standard deviation',ylab='portfolio expected return'
     ,xlim=c(0,0.25),ylim=c(0,0.08)
     ,xaxt='n',yaxt='n'
) +axis(1, at=seq(0,0.25,by=0.05),labels=paste0(seq(0,25,by=5),".00%"))+axis(2, at=seq(0,0.08,by=0.01),labels=paste0(seq(0,8,by=1),'.00%'),las=2)

mean_port_returns <- c(1.5, 3.0, 3.5 , 4.0, 4.5, 5.0, 5.5, 6.0, 6.5, 7.0)
ER.frontier[which(plot_original[ER.frontier] == mean_port_returns)]




###target 5% return
nasset=6
TargetER3=seq(0.012,0.071,by=0.001)
w3=Variable(nasset)
r=t(w3) %*% mean
var3=quad_form(w3,VCV)
obj3=var3
alloc.frontier3=matrix(0,nrow=length(TargetER3),ncol=nasset)
Std.frontier3=rep(0,length(TargetER3))




#Sensitivity
#if we change means
mean1=mean*1.1
sd1=B1$sd
t(sd1)
cor1=B1$cor
VCV1=sd1%*%t(sd1)*cor1
vec1=rep(1,length=length(mean1))

nasset=6
TargetER1=seq(0.015,0.080,by=0.001)
w1=Variable(nasset)
r=t(w1) %*% mean1
var1=quad_form(w1,VCV1)
obj1=var1
alloc.frontier1=matrix(0,nrow=length(TargetER1),ncol=nasset)
Std.frontier1=rep(0,length(TargetER1))

for(i in seq_along(TargetER1)){
  constraints=list(w1>0,sum(w1)==1,r==TargetER1[i])
  prob=Problem(Minimize(obj1),constraints)
  result=solve(prob)
  alloc.frontier1[i,]=result$getValue(w1)
  Std.frontier1[i]=result$getValue(sqrt(var1))
}

ER.frontier1=TargetER1

#if we change sd
sd2<-sd*1.1
mean2=B1$mean
t(sd2)
cor2=B1$cor
VCV2=sd2%*%t(sd2)*cor2
vec2=rep(1,length=length(mean2))

nasset=6
TargetER2=seq(0.012,0.0871,by=0.001)
w2=Variable(nasset)
r=t(w2) %*% mean2
var2=quad_form(w2,VCV2)
obj2=var2
alloc.frontier2=matrix(0,nrow=length(TargetER2),ncol=nasset)
Std.frontier2=rep(0,length(TargetER2))

for(i in seq_along(TargetER2)){
  constraints=list(w2>0,sum(w2)==1,r==TargetER2[i])
  prob=Problem(Minimize(obj2),constraints)
  result=solve(prob)
  alloc.frontier2[i,]=result$getValue(w2)
  Std.frontier2[i]=result$getValue(sqrt(var2))
}

ER.frontier2=TargetER2

#if we change correlation
sd3=B1$sd
mean3=B1$mean
t(sd3)
cor3=cor*1.1
VCV3=sd3%*%t(sd3)*cor3
vec3=rep(1,length=length(mean2))

nasset=6
TargetER3=seq(0.012,0.0871,by=0.001)
w3=Variable(nasset)
r=t(w3) %*% mean3
var3=quad_form(w3,VCV3)
obj3=var3
alloc.frontier3=matrix(0,nrow=length(TargetER3),ncol=nasset)
Std.frontier3=rep(0,length(TargetER3))

for(i in seq_along(TargetER3)){
  constraints=list(w3>0,sum(w3)==1,r==TargetER3[i])
  prob=Problem(Minimize(obj3),constraints)
  result=solve(prob)
  alloc.frontier3[i,]=result$getValue(w3)
  Std.frontier3[i]=result$getValue(sqrt(var3))
}

ER.frontier3=TargetER3


#draw
plot(Std.frontier1,ER.frontier1
     ,type='l',col='red',lwd=1.5
     ,main="MVF constrained"
     ,xlab='Portfolio standard deviation',ylab='portfolio expected return'
     ,xlim=c(0,0.25),ylim=c(0,0.08)
     ,xaxt='n',yaxt='n'
) 
axis(1, at=seq(0,0.25,by=0.05),labels=paste0(seq(0,25,by=5),".00%"))
axis(2, at=seq(0,0.08,by=0.01),labels=paste0(seq(0,8,by=1),'.00%'),las=2) # red, mean increased by 10%
lines(Std.frontier,ER.frontier,col='blue',lwd=1.5)  ##original
lines(Std.frontier2,ER.frontier2,col='green',lwd=1.5) ##sd increased by 10%
lines(Std.frontier3,ER.frontier3,col='pink',lwd=1.5)  ##cor increased by 10%
legend(0.13, 0.03, legend=c("Increased mean by 10%","Original","Increased sd by 10%","Increased cor by 10%"),col=c("red","blue","green","pink"),lty=1:2,cex=0.8)
text(0.04411756, 0.01456309,labels=c("GMR"))
text(0.09582467,0.045,labels=c("MSR"))

result<-data.frame(Std.frontier,ER.frontier)
result
result$sharpe_ratio=(result$ER.frontier-0.01)/result$Std.frontier


mean_returns_test <- c(0.015, 0.030, 0.035 , 0.040, 0.045, 0.050, 0.055, 0.060, 0.065, 0.070)
sh<-result%>%
  filter(ER.frontier==0.015,ER.frontier==0.030)
sh

which.max((ER.frontier-0.01)/Std.frontier)

