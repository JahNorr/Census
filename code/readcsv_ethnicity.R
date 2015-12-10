folder<-"./data/"

file<-"ethnicity.csv"
ethnicity_file<-paste(folder,file,sep="")
ethnicity <- read.csv(ethnicity_file,)

#sex_age$Age = factor(sex_age$Age,levels=sex_age$Age[1:18],ordered=TRUE)
#drops<-c("Status","CategoryID")

#icd10<-icd10[,!(names(icd10)%in% drops)]

rm(folder)
rm(file)
rm(ethnicity_file)
#rm(drops)

