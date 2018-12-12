library(parallel) 

d<- read.csv("C:/Users/Desktop/merged_9_2.csv")

ID_Alone <- d$SAID

n <- length(ID_Alone)
time.start<-proc.time()[3] 

cl<-makeCluster(3) 

clusterExport(cl=cl,ls(),envir=.GlobalEnv) 

ffff<-function(par){
  a<-par[1]
  b<-par[2]
  beta<-par[3]
  re<-par[4]
  rm<-par[5]
  rp<-par[6]
  rrm<-par[7]  
  
  
  
  y<-rep(0,n)
  z<-rep(0,n)
  p<-rep(0,n)
  x<-rep(0,n)
  
  NN<- exp((-1)*beta*d$t)*log(1+d$number_of_Cases)*(1+re*log(1+d$number_of_escalation)
                                                    +rm*log(1+d$number_of_single_visit_missed)
                                                    +rp*log(1+d$number_of_parts_used)
                                                    +rrm*log(1+d$number_of_response_missed))
  y<- NN
  #print(y)
  #z<- a+b* y
  
  for (j in 1:n) {
    z[j]<-a+b* y[j]
    p[j]<- 1/(1+exp((-1)*z[j]))
    x[j]<- d$status[j]
  }
  
  
  pp<-rep(0,n)
  for(k in 1:n) { if(x[k]==1) pp[k]<- p[k] else pp[k]<- 1-p[k]}
  logproductpp<- sum(log(pp))
  return (-logproductpp)
}
ans<-parSapply(cl=cl, 1:1000,ffff) 

MLE<-nlminb(c(0,0,0,0,0,0,0),objective=ffff, control=list(eval.max=1000, iter.max=1500))


ffff1<-function(par){
  a<-par[1]
  b<-par[2]
  re<-par[3]
  rm<-par[4]
  rp<-par[5]
  rrm<-par[6]  
  
  
  
  y<-rep(0,n)
  z<-rep(0,n)
  p<-rep(0,n)
  x<-rep(0,n)
  
  NN<- rm*log(1+d$number_of_single_visit_missed)+rp*log(1+d$number_of_parts_used)
  +rrm*log(1+d$number_of_response_missed)
  
  y<- NN
  #print(y)
  #z<- a+b* y
  
  for (j in 1:n) {
    z[j]<-a+b* y[j]
    p[j]<- 1/(1+exp((-1)*z[j]))
    x[j]<- d$status[j]
  }
  
  pp<-rep(0,n)
  for(k in 1:n) { if(x[k]==1) pp[k]<-p[k] else pp[k]<-1-p[k]}
  logproductpp<-sum(log(pp))
  return(-logproductpp)
}

ans1<-parSapply(cl=cl, 1:1000,ffff1) 
MLE1<-nlminb(c(0,0,0,0,0,0),objective=ffff1, control=list(eval.max=1000, iter.max=1500))

MLE1

#p<- 1-pchisq((-MLEforFull+MLEforRestricted)*2,1)

ffff2<-function(par){
  a<-par[1]
  b<-par[2]
  beta<-par[3]
  rm<-par[4]
  rp<-par[5]
  rrm<-par[6]  
  
  
  
  y<-rep(0,n)
  z<-rep(0,n)
  p<-rep(0,n)
  x<-rep(0,n)
  
  
  NN<- NN<- exp((-1)*beta*d$t)*log(1+d$number_of_Cases)*(1+rm*log(1+d$number_of_single_visit_missed)
                                                         +rp*log(1+d$number_of_parts_used)
                                                         +rrm*log(1+d$number_of_response_missed))
  y<- NN
  #print(y)
  #z<- a+b* y
  
  for (j in 1:n) {
    z[j]<-a+b* y[j]
    p[j]<- 1/(1+exp((-1)*z[j]))
    x[j]<- d$status[j]
  }
  
  pp<-rep(0,n)
  for(k in 1:n) { if(x[k]==1) pp[k]<-p[k] else pp[k]<-1-p[k]}
  logproductpp<-sum(log(pp))
  return(-logproductpp)
}

ans2<-parSapply(cl=cl, 1:1000,ffff2) 
MLE2<-nlminb(c(0,0,0,0,0,0),objective=ffff2, control=list(eval.max=1000, iter.max=1500)) 
MLE2


ffff3<-function(par){
  a<-par[1]
  b<-par[2]
  beta<-par[3]
  re<-par[4]
  rp<-par[5]
  rrm<-par[6]  
  
  
  
  y<-rep(0,n)
  z<-rep(0,n)
  p<-rep(0,n)
  x<-rep(0,n)
  
  NN<- exp((-1)*beta*d$t)*log(1+d$number_of_Cases)*(1+re*log(1+d$number_of_escalation)
                                                    +rp*log(1+d$number_of_parts_used)
                                                    +rrm*log(1+d$number_of_response_missed))
  y<- NN
  #print(y)
  #z<- a+b* y
  
  for (j in 1:n) {
    z[j]<-a+b* y[j]
    p[j]<- 1/(1+exp((-1)*z[j]))
    x[j]<- d$status[j]
  }
  
  pp<-rep(0,n)
  for(k in 1:n) { if(x[k]==1) pp[k]<-p[k] else pp[k]<-1-p[k]}
  logproductpp<-sum(log(pp))
  return(-logproductpp)
}

ans3<-parSapply(cl=cl, 1:1000,ffff3) 
MLE3<-nlminb(c(0,0,0,0,0,0),objective=ffff3, control=list(eval.max=1000, iter.max=1500)) 
MLE3


ffff4<-function(par){
  a<-par[1]
  b<-par[2]
  beta<-par[3]
  re<-par[4]
  rm<-par[5]
  rrm<-par[6]  
  
  
  
  y<-rep(0,n)
  z<-rep(0,n)
  p<-rep(0,n)
  x<-rep(0,n)
  
  
  
  NN<- exp((-1)*beta*d$t)*log(1+d$number_of_Cases)*(1+re*log(1+d$number_of_escalation)
                                                    +rm*log(1+d$number_of_single_visit_missed)
                                                    +rrm*log(1+d$number_of_response_missed))
  
  y<- NN
  #print(y)
  #z<- a+b* y
  
  for (j in 1:n) {
    z[j]<-a+b* y[j]
    p[j]<- 1/(1+exp((-1)*z[j]))
    x[j]<- d$status[j]
  }
  
  pp<-rep(0,n)
  for(k in 1:n) { if(x[k]==1) pp[k]<-p[k] else pp[k]<-1-p[k]}
  logproductpp<-sum(log(pp))
  return(-logproductpp)
}

ans4<-parSapply(cl=cl, 1:1000,ffff4) 
MLE4<-nlminb(c(0,0,0,0,0,0),objective=ffff4, control=list(eval.max=1000, iter.max=1500)) 
MLE4

ffff5<-function(par){
  a<-par[1]
  b<-par[2]
  beta<-par[3]
  re<-par[4]
  rm<-par[5]
  rp<-par[6]
  
  
  
  y<-rep(0,n)
  z<-rep(0,n)
  p<-rep(0,n)
  x<-rep(0,n)
  
  NN<- exp((-1)*beta*d$t)*log(1+d$number_of_Cases)*(1+re*log(1+d$number_of_escalation)
                                                    +rm*log(1+d$number_of_single_visit_missed)
                                                    +rp*log(1+d$number_of_parts_used))
  y<- NN
  #print(y)
  #z<- a+b* y
  
  for (j in 1:n) {
    z[j]<-a+b* y[j]
    p[j]<- 1/(1+exp((-1)*z[j]))
    x[j]<- d$status[j]
  }
  
  pp<-rep(0,n)
  for(k in 1:n) { if(x[k]==1) pp[k]<-p[k] else pp[k]<-1-p[k]}
  logproductpp<-sum(log(pp))
  return(-logproductpp)
}

ans5<-parSapply(cl=cl, 1:1000,ffff5) 
MLE5<-nlminb(c(0,0,0,0,0,0),objective=ffff5, control=list(eval.max=1000, iter.max=1500)) 
MLE5


ffff6<-function(par){
  
  b<-par[1]
  beta<-par[2]
  re<-par[3]
  rm<-par[4]
  rp<-par[5]
  rrm<-par[6]  
  
  
  
  y<-rep(0,n)
  z<-rep(0,n)
  p<-rep(0,n)
  x<-rep(0,n)
  
  NN<- exp((-1)*beta*d$t)*log(1+d$number_of_Cases)*(1+re*log(1+d$number_of_escalation)
                                                    +rm*log(1+d$number_of_single_visit_missed)
                                                    +rp*log(1+d$number_of_parts_used)
                                                    +rrm*log(1+d$number_of_response_missed))
  
  y<- NN
  #print(y)
  #z<- b* y
  
  for (j in 1:n) {
    z[j]<- b* y[j]
    p[j]<- 1/(1+exp((-1)*z[j]))
    x[j]<- d$status[j]
  }
  
  pp<-rep(0,n)
  for(k in 1:n) { if(x[k]==1) pp[k]<-p[k] else pp[k]<-1-p[k]}
  logproductpp<-sum(log(pp))
  return(-logproductpp)
}

ans6<-parSapply(cl=cl, 1:1000,ffff6) 
MLE6<-nlminb(c(0,0,0,0,0,0),objective=ffff6, control=list(eval.max=1000, iter.max=1500)) 
MLE6

ffff7<-function(par){
  a<-par[1]
  
  beta<-par[2]
  re<-par[3]
  rm<-par[4]
  rp<-par[5]
  rrm<-par[6]  
  
  y<-rep(0,n)
  z<-rep(0,n)
  p<-rep(0,n)
  x<-rep(0,n)
  
  NN<- exp((-1)*beta*d$t)*log(1+d$number_of_Cases)*(1+re*log(1+d$number_of_escalation)
                                                    +rm*log(1+d$number_of_single_visit_missed)
                                                    +rp*log(1+d$number_of_parts_used)
                                                    +rrm*log(1+d$number_of_response_missed))
  y<- NN
  #print(y)
  #z<- a+ y
  
  for (j in 1:n) {
    z[j]<- a 
    p[j]<- 1/(1+exp((-1)*z[j]))
    x[j]<- d$status[j]
  }
  pp<-rep(0,n)
  for(k in 1:n) { if(x[k]==1) pp[k]<-p[k] else pp[k]<-1-p[k]}
  logproductpp<-sum(log(pp))
  return(-logproductpp)
}

ans7<-parSapply(cl=cl, 1:1000,ffff7) 
MLE7<-nlminb(c(0,0,0,0,0,0),objective=ffff7, control=list(eval.max=1000, iter.max=1500)) 
MLE7

pa <- 1-pchisq((-MLE$objective+MLE6$objective)*2,1)
pb <- 1-pchisq((-MLE$objective+MLE7$objective)*2,1)
pbeta <- 1-pchisq((-MLE$objective+MLE1$objective)*2,1)
pre <- 1-pchisq((-MLE$objective+MLE2$objective)*2,1)
prm <- 1-pchisq((-MLE$objective+MLE3$objective)*2,1)
prp <- 1-pchisq((-MLE$objective+MLE4$objective)*2,1)
prrm <- 1-pchisq((-MLE$objective+MLE5$objective)*2,1)


time.end<-proc.time()[3] 
dt<-time.end-time.start 

cat("time spent = ",dt,"s \n") 

stopCluster(cl) 
