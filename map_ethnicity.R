

source(file = "./readcsv_ethnicity.R")
source(file = "./readcsv_estate_pops.R")
source(file = "./census_funcs.R")


#=============================================================
#
#   choose island
#   choose category of interest


cois<-c("Not Hispanic, White","Not Hispanic, Black","Hispanic")
islands<-c("St. Croix","St. Thomas","St. John")
color<-"green"
color_zero_pop<-"dark grey"

df<-ethnicity

map_census_percent(dfin = df,islands = islands,cois = cois,  color = color ,colNA = color_zero_pop)

suppressWarnings(rm(my_ethnicity_vi, estates_mrg, missing,newrow,red,green,blue,colors,alpha))

