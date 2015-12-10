folder<-"./data/"

file<-"race.csv"
race_file<-paste(folder,file,sep="")
race <- read.csv(race_file,)

#sex_age$Age = factor(sex_age$Age,levels=sex_age$Age[1:18],ordered=TRUE)
#drops<-c("Status","CategoryID")

#icd10<-icd10[,!(names(icd10)%in% drops)]

rm(folder)
rm(file)
rm(race_file)
#rm(drops)

