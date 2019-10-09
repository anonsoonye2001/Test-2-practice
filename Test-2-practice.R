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

sd.df
transect<-rownames(sd.df)
rownames(sd.df)=NULL
bind2=cbind(transect,sd.df)
bind2

# merging the two data frames of mean and standard deviation

merge.sd.mean=merge(x=bind,y=bind2,by="transect")
merge.sd.mean

# counts of observations of parcel.density.m3

count.pd=tapply(X=f$parcel.density.m3,INDEX=f$transect.id, FUN = length)
count.pd
#convert object to data frame
count.df = data.frame(tid = names(count.pd), sd = as.numeric(count.pd))
count.df

#renaming the column with standard deviation values
cs=sd.df[,c("sd")]
cs=rename(.data=sd.df,standard.deviation=sd)
cs

count.df
transect<-rownames(count.df)
rownames(count.df)=NULL
bind3=cbind(transect,count.df)
bind3

# merging the two data frames of mean and standard deviation to create a new data frame with 3 columns of 
# mean density, sd density, count and transect.

merge.sd.mean2=merge(x=merge.sd.mean,y=bind3,by="transect")
merge.sd.mean2

## summarize and join
# mean function

mean=f%>%group_by(transect.id)%>%summarise(parcel.density.m3=mean(parcel.density.m3,na.rm=T))
mean
# convert the object into data frame

mean.group.df=as.data.frame(mean)
mean.group.df

# rename the column with the density value to something more descriptive

colnames(mean.group.df)[2]="group.mean.dp"
mean.group.df

# assign row names of the data frame to the value in a new field called transect
names(mean.group.df)
colnames(mean.group.df)[1]="transect"
mean.group.df
names(mean.group.df)

# standard deviation function

sd.group=f%>%group_by(transect.id)%>%summarise(parcel.density.m3=sd(parcel.density.m3,na.rm=T))
sd.group

# forming data frame

sd.group.df=as.data.frame(sd.group)
sd.group.df

colnames(sd.group.df)[2]="group.sd.density"
sd.group.df

colnames(sd.group.df)[1]="transect"
sd.group.df

# using the join function of  tidyverse, combine the data frames with the mean,and standard deviation to create 
# one new data frame that has three columns (mean density, sd density, transect)

sd.merge3=full_join(mean.group.df,sd.group.df,by="transect")
sd.merge3

## find the count of observations using group by and summarize

c.pd=f%>%group_by(transect.id)%>%summarise(parcel.density.m3=length(parcel.density.m3))
c.pd
cpd.df=as.data.frame(c.pd)
cpd.df

colnames(cpd.df)[2]="group.count.density"
cpd.df

colnames(cpd.df)[1]="transect"
cpd.df

# using the function join, combine the data frames with mean and standard deviation to create one new
# data frame that has three columns(mean density, sd density, count and transect)

sd.merge4=full_join(cpd.df, sd.merge3, by="transect")
sd.merge4

## free style

#select an 2 fields(e.g. area, depth, year, transect) in the fish data to group by. Find the minimum,
#lower 95%,median, mean, upper 95% and maximum values for parcel.length.m

#group by area_fac
a=tapply(X=f$parcel.length.m,INDEX=list(f$area_fac),FUN=fivenum)
a

#group by depth_fac

b=tapply(X=f$parcel.length.m,INDEX=list(f$depth_fac),FUN=fivenum)
b

# group by area_fac and depth_fac combined
c=tapply(X=f$parcel.length.m,INDEX=list(f$area_fac,f$depth_fac),FUN=fivenum)
c

c[1,1]
c[1,3]
