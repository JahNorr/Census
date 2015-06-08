#=================================================
#   set folder for data files
#
folder<-"./data/"

#=================================================
#   set file for data files
#
file<-"sex_age.csv"

#=================================================
#   put folder and  file together
#
sexage_file<-paste(folder,file,sep="")

#=================================================
#   read sex and age population totals
#
sex_age <- read.csv(sexage_file, stringsAsFactors = FALSE)

#=================================================
#   remove unneeded objects
#
rm(folder)
rm(file)
rm(sexage_file)
#rm(drops)

