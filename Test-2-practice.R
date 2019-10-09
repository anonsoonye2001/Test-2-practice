# Lab 4 ; summaring and merging

load(file="fish_data.Rdata")
fish
f=fish
library("tidyverse")
# tapply ----( a tidyverse function)
# mean of parcel.density.m3
tapply(X=f$parcel.density.m3, INDEX=list(f$transect.id), FUN=mean)
pd.mean<-tapply(X=f$parcel.density.m3, INDEX=list(f$transect.id), FUN=mean)
pd.mean
head(pd.mean)

library(reshape2)
#convert object to data frame
mean.df = data.frame(tid = names(pd.mean), mean = as.numeric(pd.mean))
mean.df

#renaming the column with density values
fs=mean.df[,c("mean")]
fs=rename(.data=mean.df,density.mean=mean)
fs

mean.df
transect<-rownames(mean.df)
rownames(mean.df)=NULL
bind=cbind(transect,mean.df)



# standard deviation of parcel.density.m3
sd.pd=tapply(X=f$parcel.density.m3,INDEX=f$transect.id, FUN = sd)
sd.pd
#convert object to data frame
sd.df = data.frame(tid = names(sd.pd), sd = as.numeric(sd.pd))
sd.df

#renaming the column with standard deviation values
qs=sd.df[,c("sd")]
qs=rename(.data=sd.df,standard.deviation=sd)
qs
