
source("./code/libs/libWorldBankData.R")

filename<-"./data_raw/Population_World_Development_Indicators_Data.csv"

df_annual_pop<- getWorldBankData(filename,series="^[Pp]opul.*,.*total$")

df<-data.frame(Year=df_annual_pop$Year,Population=df_annual_pop$Value)

write.csv(df,file = "./data/population_vi_wb.csv",row.names = F)


