
library("plotrix")

island<-"St..Croix.Island"

roi1<-regexpr(pattern = "Island",text=ethnicity$Geographic.Area)>-1
roi2<-regexpr(pattern = "^(St)",text=ethnicity$Geographic.Area)>-1

roi3<-regexpr(pattern = "Not Hispanic,",text=ethnicity$Category)>-1  #ethnicity$Short)
roi4<-regexpr(pattern = "^[^N]*Hispanic$",text=ethnicity$Category)>-1  #ethnicity$Short)

rois<-roi1&roi2& (roi3|roi4)
rois
#--------------------------


eth<-ethnicity[rois,c(-2,-3,-4)]
eth$Geographic.Area<-as.character(eth$Geographic.Area)

eth$Geographic.Area<-sub(pattern = " Island","",eth$Geographic.Area,fixed=TRUE)
colnames(eth)[2]<-"Group"
islands<-unique(x = eth$Geographic.Area)

island_total<-function(x) sum(eth[eth$Geographic.Area==x,"Population"])
island_pops<-sapply(islands,island_total)

island_ptots<-sapply(eth$Geographic.Area,island_total)
eth$Total.Population<-NULL
eth$Total.Population<-as.vector(island_ptots)
eth$Pct<-eth$Population/eth$Total.Population    

pctall<-ethall

eth_tbl<-table(eth[c("Geographic.Area","Pct")],deparse.level = 1)
eth_tbl


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
barp(eth[c("Geographic.Area","Pct")],main = title,names.arg = c("Geographic.Area","Pct"),
     col = rep("cornflowerblue",each=4))

title<-paste(title_main,"\n",title_island)
barp(pct$Total*100,names.arg = pct$Short,main = title,ylab = "Percent")

