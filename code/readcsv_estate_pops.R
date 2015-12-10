folder<-"./data/"

file<-"estates_vi.csv"
estates_file<-paste(folder,file,sep="")
estates_vi <- read.csv(estates_file,)

#sex_age$Age = factor(sex_age$Age,levels=sex_age$Age[1:18],ordered=TRUE)
#drops<-c("Status","CategoryID")

#icd10<-icd10[,!(names(icd10)%in% drops)]

rm(folder)
rm(file)
rm(estates_file)
#rm(drops)
