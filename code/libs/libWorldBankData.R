
#filename<-"./data_raw/Population_World_Development_Indicators_Data.csv"

getWorldBankData<-function(filename,series=".*") {
  df_population_in<-read.csv(filename,stringsAsFactors = F)
  df_population_wb<-df_population_in[grep(series, df_population_in$Series.Name),]
  
  cols<-colnames(df_population_wb)
  
  colnames(df_population_wb)<-gsub("X([0-9]{4}).*","\\1",colnames(df_population_wb))
  colnames(df_population_wb)[1]<-"CountryName"
  colnames(df_population_wb)[2]<-"CountryCode"
  df_population_wb<-df_population_wb[nchar(df_population_wb$Series.Name)>0,]
  
  # df_pops<-df_population_wb[grep("^[Pp]opul.*total$"   ,df_population_wb$Series.Name),]
  # grep("^[Pp]opul.*female.*% of total\\)$"   ,df_population_wb$Series.Name)
  
  cols_yr<-grep("^[0-9]{4}$",colnames(df_population_wb))
  
  cols_ind<-grep("^[0-9]{4}$",colnames(df_population_wb),invert =T)
  
  df_pop_data<-df_population_wb[0,c(cols_ind)]
  df_pop_data$Year<-integer(0)
  df_pop_data$Value<-numeric(0)
  
  #colnames(df_pop_data)[length(cols_ind)+1]<-"Year"
  wrn<-getOption("warn")
  options(warn = -1)
  dummy<-sapply(cols_yr,function(x) {
    
    df_add<-df_population_wb[,c(cols_ind)]
    df_add$Year<-as.integer(colnames(df_population_wb)[x])
    df_add$Value<-as.numeric(df_population_wb[,x])
    
    df_pop_data<<-rbind(df_pop_data,df_add)
  })
  options(warn = wrn)
  
  df_pop_data
  
}
