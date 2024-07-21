
download_midyear <- function(year, geog = "VI") {

  url <- paste0("https://api.census.gov/data/timeseries/idb/1year?get=NAME,AGE,POP&",
                "GENC=[GEOG]&YR=[YEAR]&SEX=[SEX]")

  url <- url %>%
    gsub("[YEAR]",year,., fixed = TRUE) %>%
    gsub("[GEOG]",geog,., fixed = TRUE)


  url1 <- url %>%
    gsub("[SEX]","1",., fixed = TRUE)
  url2 <- url %>%
    gsub("[SEX]","2",., fixed = TRUE)

  midyear_convert(url1) %>%
    bind_rows(midyear_convert(url2)) %>%
    rename(GEOG = GENC) %>%
    mutate(SEX = ifelse(SEX==2,"Female","Male")) %>%
    reshape2::dcast(formula = GEOG+NAME+AGE+YR ~ SEX, value.var = "POP") %>%
    mutate(Total = Male + Female)

}

midyear_convert <- function(url) {

  destfile <- tempfile()
  download.file(url=url,destfile = destfile, quiet = TRUE)

  lns <- readLines(destfile,warn = FALSE) %>%
    gsub(", ","-",.) %>%
    gsub("\"","",.) %>%
    gsub("\\]|\\[","",.) %>%
    gsub(",$","",.)

  lns

  file <- tempfile()

  write.table(lns, file=file,
              row.names=FALSE, col.names=FALSE, sep=",", quote = F)

  read.csv(file)

}

build_mid_year<-function() {

  df_midyear_csv<-read.csv("./misc/midyear/mid_year_census_vi.csv",stringsAsFactors = F,skip = 1)

  df_midyear_vi<-reformat_midyear(df_midyear_csv)

  save(df_midyear_vi,file="./data/midyear_census_vi.RData")

  ########################################################################################

  df_midyear_csv<-read.csv("./misc/midyear/mid_year_census_us.csv",stringsAsFactors = F)

  df_midyear_us<-reformat_midyear(df_midyear_csv)

  save(df_midyear_us,file="./data/midyear_census_us.RData")


}

reformat_midyear<-function(df_midyear) {

  df_midyear<-df_midyear[!df_midyear$Age%in%"Total",]
  df_midyear<-df_midyear[,grep("Percent|Ratio",colnames(df_midyear),invert = T)]

  colnames(df_midyear)[which(grepl("^Both",colnames(df_midyear)))]<-"Total"
  colnames(df_midyear)[which(grepl("^Male",colnames(df_midyear)))]<-"Male"
  colnames(df_midyear)[which(grepl("^Female",colnames(df_midyear)))]<-"Female"


  df_midyear$age_start<-midyear_range(as.character(df_midyear$Age),range = "start")
  df_midyear$age_end<-midyear_range(as.character(df_midyear$Age),range = "end")

  df_midyear<-df_midyear[order(df_midyear$Year,df_midyear$age_start),c("Country","Year","Age","age_start","age_end","Total","Male","Female")]
}

midyear_range<-function(ages,range=c("start","end")) {
  range<-range[1]

  age_int<-sapply(as.character(ages),function(age) {
    age<-as.character(age)
    #browser()
    if(grepl("^[0-9]{1,}$",age)) {
      x<-as.integer(age)
    } else if(grepl(pattern = "[+]",x = age)) {
      if(range=="start") {
        x<-as.integer(gsub("[+]","",age))
      } else {
        x<-999
      }
    } else if(grepl(pattern = "[-]",x = age)){
      if(range=="start") {
        x<-as.integer(gsub("(.*)[-](.*)","\\1",age))
      } else {
        x<-as.integer(gsub("(.*)[-](.*)","\\2",age))
      }
    } else {
      x<-1
    }
    x
  })
  age_int
}

#' Midyear census values
#'
#' The data frame returned includes population counts for ages by each year up to 84, and then 85-89, 90-94, 95-99, and 100+. Population is also broken down by gender.
#'
#'
#' \url{https://www.census.gov/data-tools/demo/idb/}
#'
#'
#' @param year integer - the year of interest
#' @param geog character - "vi"(default) or "us"
#' @param minage integer - mainimum age to return data for
#' @param maxage integer - maximum age to return data for
#' @param totals logical - instead of a dataframe, return a list of population totals for total, male, and female
#'
#' @return if \code{totals} = FALSE, returns data frame containing population counts else if \code{totals} = TRUE, returns a list of population totals for total, male, and female
#' @export
#'
#' @examples
# #' midyear_census(2005,"us")
#'

midyear_census<-function(year,geog="vi",minage=0,maxage=9999,totals = F, sex = NULL) {

  e<-new.env()
  fil<-paste("midyear_census",geog,sep="_")
  #item<-paste("df_midyear",geog,sep="_")

  data(list=fil,envir = e)
  df<-get(x = ls(e),envir = e)
  rownames(df)<-NULL

  df<-df[df$Year==year & df$age_start>=minage & df$age_end<=maxage,]

  if(totals) {
    return (list(total=sum(df$Total),male=sum(df$Male),female=sum(df$Female)))
  } else {
    if(!is.null(sex)) {
      df <- df %>% select(c(1:5,starts_with(sex)))
    }
    return (df)
  }
}

#' Population totals from midyear Census in age bins
#'
#' @param year integer - year of interest
#' @param df_age_bins data frame - returned from \code{get_standard_age_groups}
#' @param geog character - "vi" (default) or "us"
#' @param cols character vector - "Total" (default), "Male", or "Female"
#'
#' @return data frame - df_age_bins with an added column for each column selected in \code{cols} containing the total for that bin.
#' @export
#'
# #' @examples
# #' df_age_bins<-get_standard_age_groups(years=10, bins=11)
# #' midyear_census_bins(2012,df_age_bins=df_age_bins,cols="Male")
#'
midyear_census_bins<-function(year,df_age_bins,geog="vi",cols=c("Total")) {

  e<-new.env()

  fil<-paste("midyear_census",geog,sep="_")
  #item<-paste("df_midyear",geog,sep="_")

  data(list=fil,envir = e)

  df_census<-get(x = ls(e),envir = e)
  # df_census<-get(x = "df_midyear_vi",envir = e)
  df_census<-df_census[df_census$Year==year,]

  df_age_bins[,cols]<-0

  sapply(cols,function(col) {
    df_age_bins[,col]<<-mapply(function(mina,maxa) {

      sum(df_census[(which(df_census$age_start>=mina & df_census$age_end<=maxa)),col])

    },df_age_bins$min_age,df_age_bins$max_age)
  })
  df_age_bins
}

plot_sex_ratio<-function(where="vi") {
  require(ggplot2)

  df_cen<-midyear_census(2010,"vi")

  df_cen$Age<-as.character(df_cen$Age)
  df_cen$Age <- factor(df_cen$Age, levels=unique(df_cen$Age))
  df_cen$Sex.Ratio=df_cen$Male/df_cen$Total

  # ages<-df_cen$Age
  # ages<-as.integer(gsub("([0-9]*).*","\\1",ages))
  #
  # levels(df_cen$Age)<-levels(df_cen$Age)[order(ages)]
  ggplot2::ggplot(df_cen,aes(x=Age,y=Sex.Ratio)) + geom_point() + stat_identity() #+
  # geom_line(data = data.frame(x = c(0,100), y = c(100,100)),
  #           aes = aes(x = x, y = y), colour = "red")

  # theme(panel.grid.major.y = element_line(colour = "black", size = 0.5)) +
  #   scale_y_continuous(breaks = seq(0, 100, 100), minor_breaks = seq(0, 110, 10)) +

}

adjust_midyear_2020 <- function(act_pop) {
  require(dplyr)

  e <- new.env()
  fname <- "./data/midyear_census_vi.rda"
  load(file = fname ,envir = e)

  dfname <- ls(e)

  df <- get(x = dfname,envir = e)

  df_2020 <- df %>%
    filter(Year==2020)

  tot_pop <- sum(df_2020$Total)
  pct <- act_pop/tot_pop

  df_2020 <- df_2020 %>%
    mutate(Male = round(Male * pct,0)) %>%
    mutate(Female = round(Female * pct,0)) %>%
    mutate(Total = Male + Female)

  df<-df %>%
    filter(Year != 2020) %>%
    rbind(df_2020) %>%
    arrange(Year,age_start)

  assign(dfname,df)
  save(list = dfname,file = fname)

}
