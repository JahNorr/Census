
totals_geog_area<-race[race$Short=="Total",c(-1,-2,-3)]
names_col<-colnames(totals_geog_area)

fix_names<-function(x) {
  x0000=gsub(pattern="..","$.",x=x,fixed = TRUE)
  x0000=gsub(pattern="."," ",x=x0000,fixed = TRUE,)
  x0000=gsub(pattern="$",".",x=x0000,fixed = TRUE,)
  return(x0000)
}

names_fixed<-lapply(names_col,fix_names)


totals_geog_area<-unlist(totals_geog_area)
df.totals<-data.frame(totals_geog_area,row.names = names_fixed)
df.totals$Estate<-names_fixed
is.estate<-substring(rownames(df.totals),first = 1,last = 6)=="Estate"
estate_only<-df.totals[is.estate&df.totals$totals_geog_area>0,]

colnames(estate_only)[1]<-"Population"
estate_stx<-estate_only[1:211,]
estate_stj<-estate_only[212:262,]
estate_stt<-estate_only[263:335,]

sum(estate_stx$Population)

pops<-c(sum(estate_stx$Population),sum(estate_stt$Population),sum(estate_stj$Population))
isles<-c("St. Croix","St. Thomas","St. John")

mean(estate_stj$Population)
median(estate_stj$Population,na.rm = TRUE)
quantile(estate_stj$Population)
summary(estate_stj$Population)
