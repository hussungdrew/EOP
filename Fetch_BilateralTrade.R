get_bilatTrade <- function(download.new, dest.path, file.name, countries, since.date){
  
  #' @title Grab Census Bureau data for US bilateral trade balance and load into environment
  #' @description This function goes and fetches the xlsx file hosted on the US Census Bureau's website at
  #' https://www.census.gov/foreign-trade/balance/index.html. This dataset contains the US bilateral trade
  #' data for a host of countries. With the function, the data is imported and the net trade balance is
  #' calculated. Basically, gets data ready for plotting on the EOP website. 
  #' @param download.new True/False: Do you wish to download a new file from the Census Bureau? If so, goes and 
  #' fetches file and if not, user must navigate to the preexisting xlsx file to use
  #' @param dest.path A character string giving the path for the location data is downloaded. Defaults to current working directory
  #' @param file.name Character string giving the name of the file you wish to save the downloaded xlsx file under. 
  #' Defaults to \code{'bilatTrade_yyyymmdd.xlsx'} where 'yyyymmdd' is today's date.
  #' @param countries Character vector giving the names of the countries whose trade balance you wish to inspect.
  #' Defaults to \code{c('Brazil', 'Canada', 'China', 'Germany', 'Japan', 'Mexico')}
  #' @param since.date A character string giving the earliest date you want to report. Must be in form \code{'yyyy-mm-dd'} or coercible to such.
  #' Attempts to default to grab latest six months of data
  #' @examples 
  #' bilat <- get_bilatTrade(dest.path = 'C:/Users/blahblah')
  #' bilat <- get_bilatTrade(dest.path = 'C:/Users/blahblah', countries = c('China', 'Zimbabwe'))
  #' @export
  
  library(readxl)
  library(data.table)
  library(lubridate)
  
  today.date <- gsub(pattern = '-', replacement = '', as.character(Sys.Date())) 
  schedule <- Census_Release()
  stat.month <- paste0(month.name[month(Sys.Date() - months(2))], ' ', year(Sys.Date()))
  
  if(missing(dest.path)) dest.path <- getwd()
  if(missing(countries)) countries <- c("Brazil", "Canada", 'China', "Germany", "Japan", "Mexico")
  if(missing(since.date)) since.date <- Sys.Date() - (day(Sys.Date()) - 1) - months(7) - months(1*(day(Sys.Date()) < day(schedule[stat.month, `Release Date`])))
  if(missing(file.name)) file.name <- paste0('bilatTrade', '_', today.date, '.xlsx')
  if(missing(download.new)) download.new <- T

  since.date <- as.Date(since.date)
  month.lookup <- data.table(month.abb = tolower(month.abb), month.name)
  
  if (download.new == T){
  ## Go download latest version of Excel file
    download.file("https://www.census.gov/foreign-trade/balance/country.xlsx", 
                destfile = paste0(dest.path, '/', file.name), cacheOK = F, mode = 'wb')
  
  ##Read in the Excel file
    bilat <- readxl::read_xlsx(path = file.path(dest.path, file.name), sheet = 'country') %>% as.data.table() %>%
      .[CTYNAME %in% countries] %>%
    ##Take from wide to long form
      melt(data = ., id.vars = c('year', 'CTY_CODE', 'CTYNAME'), measure.vars = union(colnames(.)[startsWith(colnames(.), 'I')], 
                                                                                    colnames(.)[startsWith(colnames(.), 'E')]),
         variable.name = 'month', value.name = 'amount') %>%
    ##Housekeeping
      .[ , month.abbr := tolower(substr(month, 2, 4))] %>%
      .[substr(month, 1, 1) == 'I', value := "Imports"] %>%
      .[substr(month, 1, 1) == 'E', value := 'Exports'] %>%
    ##Get month name from month.lookup
      merge(.,
            month.lookup[], by.x = 'month.abbr', by.y = 'month.abb', sort = F) %>%
      .[ , -c('month.abbr', 'CTY_CODE', 'month')] %>%
      .[ , month.num := c(1,2,3,4,5,6,7,8,9,10,11,12)[(month.name == "January")*1 + (month.name == "February") * 2 + 
                                                      (month.name == "March")*3 + (month.name == "April")*4 + 
                                                      (month.name == 'May')*5 + (month.name == "June")*6 + (month.name == "July")*7 + 
                                                      (month.name == "August")*8 + (month.name == "September")*9 +
                                                      (month.name == 'October')*10 + (month.name == "November")*11 + (month.name == "December")*12]] %>%
      .[ , day.num := 1] %>%
      ##Months not reported are listed as 0 rather than NA
      .[!amount == 0] %>%
      .[ , date := as.Date(paste0(year, '-', month.num, '-', '0', day.num))] %>%
      .[date >= since.date] %>%
    ##Make Exports and Imports two separate cols
      dcast(., year + CTYNAME + month.name + month.num + day.num  + date ~ value, value.var = 'amount') %>%
      .[ , `Net Goods Trade` := Exports - Imports] %>%
      .[order(CTYNAME, date)] %>%
      setnames(., old = 'CTYNAME', new = 'Country')
    
  }
  
    else if (download.new == F){
      ## If user doesn't wish to download a new file, have them navigate to the preexisting 
       #  xlsx file
      bilat <- readxl::read_xlsx(path = choose.files(caption = 'Navigate to XLSX from Census Bureau', multi = F), sheet = 'country') %>% 
        as.data.table() %>%
        .[CTYNAME %in% countries] %>%
        ##Take from wide to long form
        melt(data = ., id.vars = c('year', 'CTY_CODE', 'CTYNAME'), measure.vars = union(colnames(.)[startsWith(colnames(.), 'I')], 
                                                                                        colnames(.)[startsWith(colnames(.), 'E')]),
             variable.name = 'month', value.name = 'amount') %>%
        ##Housekeeping
        .[ , month.abbr := tolower(substr(month, 2, 4))] %>%
        .[substr(month, 1, 1) == 'I', value := "Imports"] %>%
        .[substr(month, 1, 1) == 'E', value := 'Exports'] %>%
        ##Get month name from month.lookup
        merge(.,
              month.lookup[], by.x = 'month.abbr', by.y = 'month.abb', sort = F) %>%
        .[ , -c('month.abbr', 'CTY_CODE', 'month')] %>%
        .[ , month.num := c(1,2,3,4,5,6,7,8,9,10,11,12)[(month.name == "January")*1 + (month.name == "February") * 2 + 
                                                          (month.name == "March")*3 + (month.name == "April")*4 + 
                                                          (month.name == 'May')*5 + (month.name == "June")*6 + (month.name == "July")*7 + 
                                                          (month.name == "August")*8 + (month.name == "September")*9 +
                                                          (month.name == 'October')*10 + (month.name == "November")*11 + (month.name == "December")*12]] %>%
        .[ , day.num := 1] %>%
        .[ , date := as.Date(paste0(year, '-', month.num, '-', '0', day.num))] %>%
        .[date >= since.date] %>%
        ##Months not reported are listed as 0 rather than NA
        .[!amount == 0] %>%
        ##Make Exports and Imports two separate cols
        dcast(., year + CTYNAME + month.name + month.num + day.num  + date ~ value, value.var = 'amount') %>%
        .[ , `Net Goods Trade` := Exports - Imports] %>%
        .[order(CTYNAME, date)] %>%
        setnames(., old = 'CTYNAME', new = 'Country')
    }
  return(bilat)
}








