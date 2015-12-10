
totals_geog_area<-race[race$Short=="Total",c(-1,-2,-3)]
names_col<-colnames(totals_geog_area)

fix_names<-function(x) {
  x0000<-gsub(pattern="..","$.",x=x,fixed = TRUE)
  x0000<-gsub(pattern="."," ",x=x0000,fixed = TRUE,)
  x0000<-gsub(pattern="$",".",x=x0000,fixed = TRUE,)
  return(x0000)
}

names_fixed<-lapply(names_col,fix_names)


totals_geog_area<-unlist(totals_geog_area)
df.totals<-data.frame(totals_geog_area,row.names = names_fixed)
df.totals$Subdistrict<-names_fixed
is.subdistrict<-gregexpr(pattern = "subdistrict$",text = rownames(df.totals))>-1
subdistrict_only<-df.totals[is.subdistrict&df.totals$totals_geog_area>0,]

colnames(subdistrict_only)[1]<-"Population"
subdistrict_stx<-subdistrict_only[1:9,]
subdistrict_stj<-subdistrict_only[10:12,]
subdistrict_stt<-subdistrict_only[13:18,]

pops<-c(sum(subdistrict_stx$Population),sum(subdistrict_stt$Population),sum(subdistrict_stj$Population))
isles<-c("St. Croix","St. Thomas","St. John")

subdistricts<-data.frame(pops,row.names = isles )
colnames(subdistricts)[1]<-"Population"

subdistricts


mean(subdistrict_stx$Population)
median(subdistrict_stx$Population,na.rm = TRUE)
quantile(subdistrict_stx$Population)
summary(subdistrict_stx$Population)
