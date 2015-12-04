library(plyr)
library(reshape)
library(dplyr)
library(tidyr)
library(tm)
library(stringr)
trial=train[,c(2,6)]
trial$DepartmentDescription=gsub(' ','',trial$DepartmentDescription)
unit2<-function(x,key)
{
  return(paste(x[,key][1:length(x)],collapse=" "))  
  
  
}

head(trial)
t=ddply(trial,.(VisitNumber),unit2,"DepartmentDescription")
corpus <- Corpus(VectorSource(t$V1))
dtm <- DocumentTermMatrix(corpus)
dtmsparse <- as.data.frame(as.matrix(dtm))
gc()
trial=test[,c(1,5)]
trial$DepartmentDescription=gsub(' ','',trial$DepartmentDescription)
t=ddply(trial,.(VisitNumber),unit2,"DepartmentDescription")
corpus <- Corpus(VectorSource(t$V1))
dtm <- DocumentTermMatrix(corpus)
dtmsparse2 <- as.data.frame(as.matrix(dtm))
dtmsparse2$healthandbeautyaids=rep(0,95674)
dtmsparse2=dtmsparse2[,names(dtmsparse)]

trial=test[,c(1,2)]
t=ddply(trial,.(VisitNumber),unit2,"Weekday")
corpus <- Corpus(VectorSource(t$V1))
dtm <- DocumentTermMatrix(corpus)
dtmsparse4 <- as.data.frame(as.matrix(dtm))

trial=train[,c(2,3)]
t=ddply(trial,.(VisitNumber),unit2,"Weekday")
corpus <- Corpus(VectorSource(t$V1))
dtm <- DocumentTermMatrix(corpus)
dtmsparse3 <- as.data.frame(as.matrix(dtm))

sumsc<-function(x)
{
  sum(x$ScanCount,na.rm=T)  
  
  
  
}
trial=test[,c(1,4,5)]
t=ddply(trial,.(VisitNumber,DepartmentDescription),sumsc)
sctest=reshape(t,idvar='VisitNumber',timevar='DepartmentDescription',direction='wide')
sctest[is.na(sctest)]=0
trial=test[,c(1,4,5)]
t=ddply(trial,.(VisitNumber,DepartmentDescription),sumsc)
sctest=reshape(t,idvar='VisitNumber',timevar='DepartmentDescription',direction='wide')
sctest[is.na(sctest)]=0
sctest[,paste(setdiff(names(sctrain),names(sctest)))]=rep(0,95674)
trial=train[,c(2,5,6)]
t=ddply(trial,.(VisitNumber,DepartmentDescription),sumsc)
sctrain=reshape(t,idvar='VisitNumber',timevar='DepartmentDescription',direction='wide')
sctrain[is.na(sctrain)]=0
trainadd=cbind(dtmsparse,dtmsparse3,sctrain)
testadd=cbind(dtmsparse2,dtmsparse4,sctest)
