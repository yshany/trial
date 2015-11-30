cat2freq<-function(x,key)
{
  m=as.data.frame(table(x[,key]))
  names(m)[1]=key
  x=merge(m,x,by=key)
  x[,key]=NULL
  return (x)  
  
  
}
cat2logfreq<-function(x,key)
{
  m=as.data.frame(table(x[,key]))
  m[,2]=log(m[,2])
  names(m)[1]=key
  x=merge(m,x,by=key)
  x[,key]=NULL
  return (x)  
  
  
}
cat2freqrank<-function(x,key)
{
  m=as.data.frame(sort(table(x[,key])))
  
  m[,1]=rev(c(1:length(m[,1])))
  m[,key]=row.names(m)
  
  x=merge(m,x,by=key)
  x[,key]=NULL
  return (x)  
  
  
}
x=event_type
key='event_type'
ne=cat2logfreq(event_type,'event_type')

scaling<-function(x)
{
  return(x/sum(x))
}

library(readr)
library(plyr)
library(reshape2)
library(caret)
library(xgboost)
setwd('E:/tei')
di=getwd()
d=list()
i=1
for (f in dir(di))
{
d[[i]]=read_csv(paste0(f))  
  i=i+1
  
}

event_type=read_csv('event_type.csv')
log_feature=read_csv('log_feature.csv')
resource_type=read_csv('resource_type.csv')
sampe_submission=read_csv('sample_submission.csv')
severity_type=read_csv('severity_type.csv')
train=read_csv('train.csv')
test=read_csv('test.csv')
event_type=cat2freqrank(event_type,'event_Type')
log_feature=cat2freqrank(log_feature,'log_feature')
resource_type=cat2freqrank(resource_type,'resource_type')
severity_type=cat2freqrank(severity_type,'severity_type')
event=ddply(event_type,.(id),mutate,index=paste0('v',1:length(id)))
event=reshape(event,timevar='index',idvar='id',direction='wide')
log=ddply(log_feature,.(id),mutate,index=paste0('v',1:length(id)))
log=reshape(log,timevar='index',idvar='id',direction='wide')
resource=ddply(resource_type,.(id),mutate,index=paste0('v',1:length(id)))
resource=reshape(resource,timevar='index',idvar='id',direction='wide')
severity=ddply(severity_type,.(id),mutate,index=paste0('v',1:length(id)))
severity=reshape(severity,timevar='index',idvar='id',direction='wide')
unique(log_feature$log_feature) #380+
unique(resource_type$resource_type)#10
unique(severity_type$severity_type)#5
unique(event_type$event_type)#53
train=merge(train,severity,by='id')
test=merge(test,severity,by='id')
train=merge(train,resource,by='id')
test=merge(test,resource,by='id')
train=merge(train,event,by='id')
test=merge(test,event,by='id')
train=merge(train,log,by='id')
test=merge(test,log,by='id')
train=cat2freqrank(train,'location')
test=cat2freqrank(test,'location')
length(table(test$location))
train$location=as.numeric(factor(train$location))
test$location=as.numeric(factor(test$location))
fea<-names(qtest)[2:59]
m<-dummyVars(~.,data=qtrain[,fea])
mtrain=predict(m,qtrain[,fea])
mtest=predict(m,qtest[,fea])
mtrain[is.na(mtrain)]=-1
mtest[is.na(mtest)]=-1
param <- list(  objective           = "multi:softprob", 
                # booster = "gblinear",
                eta                 = 0.004, # 0.06, #0.01,
                max_depth           = 7, #changed from default of 8
                subsample           = 0.7, # 0.7
                colsample_bytree    = 0.7, # 0.7
                
                eval_metric         = "mlogloss",
                num_class = 3
                # alpha = 0.0001, 
                # lambda = 1
)
clf <- xgboost(   params              = param, 
                    data                = xgb.DMatrix(mtrain,label=train$fault_severity), 
                    nrounds             = 1200, #300, #280, #125, #250, # changed from 300
                    verbose             = 1,
                   maximize            = FALSE)

pred=predict(clf,mtest)
sampe_submission[,2:4]=pred
write_csv(sampe_submission,"beatthebenchmark3.csv")
etrain=merge(train,event_type,by='id')

eventq=t(as.data.frame.matrix(table(etrain$fault_severity,etrain$event_type)))
eventq=data.frame(t(apply(eventq,1,scaling)))
View(eventq)
eventq$event_type=row.names(eventq)
eventq=merge(eventq,event_type,by='event_type',all.y=T)
eventq=ddply(eventq[,c(2,3,5)],.(id),mutate,index=paste0('v',1:length(id)))
eventq=reshape(eventq,timevar='index',idvar='id',direction='wide')
train=merge(train,eventq,by='id')
test=merge(test,eventq,by='id')

ltrain=merge(train,log_feature,by='id')
logq=t(as.data.frame.matrix(table(ltrain$fault_severity,ltrain$log_feature)))
logq=data.frame(t(apply(logq,1,scaling)))
View(logq)
logq$log_feature=row.names(logq)
logq=merge(logq,log_feature,by='log_feature',all.y=T)
logq=ddply(logq[,c(2,3,5,6)],.(id),mutate,index=paste0('v',1:length(id)))
logq=reshape(logq,timevar='index',idvar='id',direction='wide')
train=merge(train,logq,by='id')
test=merge(test,logq,by='id')
log(0.33333333)
qtrain=merge(train,event,by='id')
qtrain=merge(qtrain,severity,by='id')
qtrain=merge(qtrain,log,by='id')
qtrain=merge(qtrain,resource,by='id')
qtest=merge(test,event,by='id')
qtest=merge(qtest,severity,by='id')
qtest=merge(qtest,log,by='id')
qtest=merge(qtest,resource,by='id')
for (f in names(qtrain)) {
  if (class(qtrain[[f]])=="character") {
   
    
    levels <- unique(c(qtrain[[f]], qtrain[[f]]))
    qtrain[[f]] <- as.integer(factor(qtrain[[f]], levels=levels))
    qtest[[f]]  <- as.integer(factor(qtest[[f]],  levels=levels))
  }
  
}
