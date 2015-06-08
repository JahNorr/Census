
library("plotrix")

col<-"St..Croix.Island"

coi1<-regexpr(pattern = "Island",text=names(ethnicity))>-1
coi2<-regexpr(pattern = "^(St)",text=names(ethnicity))>-1
coi3<-regexpr(pattern = "Short",text=names(ethnicity))>-1
coi<-coi1&coi2|coi3
coi
#--------------------------
eoi1<-regexpr(pattern = "Not Hispanic,",text=ethnicity$Short)>-1  #ethnicity$Short)
eoi2<-regexpr(pattern = "^[^N]*Hispanic$",text=ethnicity$Short)>-1  #ethnicity$Short)


eth<-ethnicity[eoi1|eoi2,c("Short",col)]
ethall<-ethnicity[eoi1|eoi2,coi]

ethall


pctall<-ethall


names(eth)[2]<-"Total"
pct<-eth
pct$Total<-pct$Total/sum(pct$Total)
# 

title_main<-"Hispanic Population (2010 Census)"
title_island<-gsub("."," ",col,fixed=TRUE)  
title_island<-gsub(" Island","",title_island,fixed=TRUE)  
title_island<-gsub("St ","St.",title_island,fixed=TRUE)  

#pyramid(data,Lcol = "cornflowerblue",main = title) #,xlim=c(50,50))

#par(mar=c(2.0, 2.0, 1.5, 1.5))

title<-title_main
barp(ethall[c(2:4)],main = title,names.arg = names(ethall[c(2:4)]),
     col = rep("cornflowerblue",each=4))

title<-paste(title_main,"\n",title_island)
barp(pct$Total*100,names.arg = pct$Short,main = title,ylab = "Percent")

