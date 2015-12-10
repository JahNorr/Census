#===============================================
#
# mapping
#
#
library(maps)
library(mapdata)
library(maptools)  #for shapefiles
library(scales)  #for transparency

#=====================================
#   best St. Croix limits

# latmax<-17.78
# latmin<-17.68
# 
# lonmax<--64.57
# lonmin<--64.9


#=====================================
#   best St. Thomas/St. John limits

# latmax<-18.39
# latmin<-18.31
# 
# lonmax<--64.66
# lonmin<--65.03

#=====================================
#   best St. Thomas limits
# 
# latmax<-18.4
# latmin<-18.3
# 
# lonmax<--64.926
# lonmin<--64.937

#=====================================
#   best St. John limits
#   needs some tweaking
# 
# latmax<-18.38
# latmin<-18.31
# 
# lonmax<--64.741
# lonmin<--64.7428

#=============================================================
#
#   get the shp file for the vi ... it includes all the estates
#
#

estates <- readShapePoly("../mapping/data_raw/tl_2014_78_estate.shp")
#=============================================================
#
#   get the list of all the estate geog codes
#   rename the column
#   cast them as integers
#
estates_fp<-as.data.frame(as.character(estates$ESTATEFP),stringsAsFactors = FALSE)
colnames(estates_fp)[1]<-"ESTATEFP"
estates_fp$ESTATEFP<-as.integer(estates_fp$ESTATEFP)

#=============================================================
#   get a copy of estates_vi
#   add any missing codes
# "Geographic.Area","StateCode" ,"CountyCode", "Estate.Code","X2010.Population"
#  
my_estates_vi<-estates_vi
my_estates_vi$Geographic.Area<-as.character(my_estates_vi$Geographic.Area)

missing<-setdiff(estates_fp$ESTATEFP,my_estates_vi$Estate.Code)
newrow<-c("Unknown",0,0,missing,as.integer(0))
my_estates_vi<-rbind(my_estates_vi,newrow)
#=============================================================
#
#   merge the codes
#  

estates_mrg<-merge(estates_fp,my_estates_vi,by.x = "ESTATEFP", by.y = "Estate.Code",all.x = TRUE,sort = FALSE)

#=============================================================
#
#   set limits for vi plots
#  
lim_stx<-c(17.729,17.731,-64.90,-64.57)
lim_stt<-c(18.3,18.4,-65.047,-64.823)
lim_stj<-c(18.298,18.373,-64.743,-64.735)
l<-c(lim_stx,lim_stt,lim_stj)

limits<-matrix(l,4,3)
colnames(limits)<- c("St. Croix","St. Thomas","St. John")
rownames(limits)<- c("minlat","maxlat","minlon","maxlon")

rm(lim_stx,lim_stt,lim_stj)
#=============================================================
#
#   choose island
#   set plot limits of that island

island<-"St. Croix"
maplims<-limits[,c(island)]


#=============================================================
#
#   set colors based on the alpha the larger the estate value of interest
#    (population) or log10(population), the larger the alpha 
#     represented as a percent of the max
#  

max<-log10(max(estates_mrg$X2010.Population,na.rm = TRUE))
blue<-0
green<-1
red<-0


logpop<-log10(as.integer(estates_mrg$X2010.Population))
logpop[is.na(logpop)|is.infinite(logpop)] <- 0

alpha<-(logpop/max)
colors<-rgb(red,green,blue,alpha)


#=============================================================
#
#   set any plot parameters and ... plot it
#
show.axes<-FALSE
par(mar=c(2.0, 2.0, 1.5, 1.5))

plot(estates,axes=show.axes,xlim=c(maplims["minlon"],maplims["maxlon"]),ylim=c(maplims["minlat"],maplims["maxlat"]), col=colors, border=TRUE)  #plot the species range
