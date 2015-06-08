#=================================================
#   open plotrix library
#   this library has the ability to make population pyramid plots
#

library("plotrix")

source(file = "./Census/readcsv_sex_age.R")

#=================================================
#   select your column of interest
#
col<-"Estate Golden Grove"   #sex_age$Estate[1]

#=================================================
#   get females and males into separate columns
#   and 

xf<-sex_age[sex_age$Sex=="Female" & sex_age$Estate==col,c("Age","Population")]

names(xf)<-c("Age","Female")
xm<-sex_age[sex_age$Sex=="Male" & sex_age$Estate==col,c("Age","Population")]

names(xm)<-c( "Age","Male")
mrg<-merge(xf,xm,by = "Age" )
mrg<-mrg[c(18,9,1:8,10:17),]
#print(levels(mrg$Age))
#mrg$Age<-factor(mrg$Age,levels(mrg$Age) [c(3:10,2,11:18,1)],ordered = TRUE)
#mrg$Age<-reorder(mrg$Age,c(3:10,2,11:18,1))
#print(levels(mrg$Age))

maxn<-max(c(mrg$Male,mrg$Female))
maxn<-(as.integer(maxn/4) + 1)*4+4

data <- data.frame(mrg$Male,mrg$Female,mrg$Age)
title<-gsub("."," ",col,fixed=TRUE)  
title<-gsub(" Island","",title,fixed=TRUE)  
title<-gsub("St ","St.",title,fixed=TRUE)  

title<-paste("Population (2010 Census)","\n",title)
#pyramid(data,Lcol = "cornflowerblue",main = title) #,xlim=c(50,50))

par(xaxt = "n")

print(mrg$Age)
pyramid.plot(mrg$Male,mrg$Female,labels=mrg$Age,top.labels=c("Male","Age","Female"),
             main=title,laxlab=NULL,raxlab=NULL,unit="",
             lxcol="cornflowerblue", rxcol="pink",
             gap=maxn/6,space=0.5,
             ppmar=c(0,3,4,3),labelcex=0.65,
             add=FALSE,show.values=TRUE,ndig=1,
             do.first=NULL)

