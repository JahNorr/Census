
load("../mapping/vi_estates.RData")

source(file = "../mapping/mapping_vi_estates.R")
source(file = "../mapping/color_funcs.R")

load(file= "./data/census_estate_ethnicity.RData")

#=============================================================
#   get a copy of estates_vi
#   add any missing codes
# "Geographic.Area","StateCode" ,"CountyCode", "Estate.Code","X2010.Population"
#  

fix_census_file<-function(df,category) {
  census_vi<-as.data.frame(df[ethnicity$Category==category,])
  census_vi<-merge(census_vi,estates_vi[c("Estate.Code","X2010.Population")],by="Estate.Code")

  census_vi$Geographic.Area<-as.character(census_vi$Geographic.Area)
  census_vi$Category<-as.character(census_vi$Category)

  estates_map<-data.frame(as.character(estates$ESTATEFP),stringsAsFactors = FALSE)
  colnames(estates_map)[1]<-"Estate.Code"
  estates_map$Estate.Code<-as.integer(estates_map$Estate.Code)
  
#====================================================================
#
#    add missing estates to the data.frame
#

  missing<-setdiff(estates_map$Estate.Code,census_vi$Estate.Code)
  mlen<-length(missing)
  if (mlen>0) {
    df_missing<-data.frame(missing)
    colnames(df_missing)[1]<-"Estate.Code"
  
    df_missing$Geographic.Area<-"Unknown"
    df_missing$StateCode<-78
    df_missing$CountyCode<-0
    df_missing$Category<-"Unknown"
    df_missing$Population<-0
    df_missing$X2010.Population<-0
    #df_missing[,2:7]<-c("Unknown",78,0,"Unknown",0,0)
    
    
    census_vi<-rbind(census_vi,df_missing)

  }
  final_df<-merge(estates_map,census_vi,by="Estate.Code",sort = FALSE)

  return (final_df)
}


pct<-function(x,y) {
  if (y==0) ret<-0 else ret<-x/y
  return (ret)
}

census_pcts<-function(df) {
  pcts<-mapply(pct,as.integer(df$Population),as.integer(df$X2010.Population))
  return (pcts)
}

set_colors_pct<-function(color,pcts) {
  colors<-vector(length = length(pcts))
  colors[]<-paste(substr(color,start = 1,7),as.hexmode(as.integer(pcts*255)),sep = "") #color
  
  return (colors)
}

apply_recolor<-function(x,y,col2) {
  if(x==0) y=col2 
  return(y)
}

recolor_zero_pop<-function(colors,dfin,col2){
  dfpop<-as.integer(dfin$X2010.Population)
  new_colors<-mapply(apply_recolor ,dfpop,colors,col2)#
  return (new_colors)
}


map_census_percent<-function(dfin,islands,cois,color,colNA) {
  
  for (island in islands) for (coi in cois)  map_census_percents(dfin,island,coi,color,colNA)
}
#========================================================
#
#
#
map_census_percents<-function(dfin,island,coi,color=col2rgb("green"),colNA="grey") {

  df_map<-fix_census_file(df = dfin,category = coi)
  
  map_pcts<-census_pcts(df =df_map)

#=============================================================
#
#   set colors based on the alpha the larger the estate value of interest
#    (population) or log10(population), the larger the alpha 
#     represented as a percent of the max
#  
  color<-resolve_color(color)

  colors<-set_colors_pct(color = color,pcts = map_pcts)


  new_colors<-recolor_zero_pop(colors,df_map,colNA)
#=============================================================
#
#   set any plot parameters and ... plot it
# 
  main<-paste(coi, island, sep="\n")
  main<-paste("Percent " , main)

  show_island_plot(island_name = island , colors = new_colors, main=main)
}