get_bilatTrade <- function(dest.path, countries, since.date, file.name){
  #' @title Grab Census Bureau data for US bilateral trade balance and load into environment
  #' @description This function goes and fetches the xlsx file hosted on the US Census Bureau's website at
  #' https://www.census.gov/foreign-trade/balance/index.html. This dataset contains the US bilateral trade
  #' data for a host of countries. With the function, the data is imported and the net trade balance is
  #' calculated. Basically, gets data ready for plotting on the EOP website. Must pass \code{dest.path} for 
  #' operation
  #' @param dest.path A character string giving the path for the location data is downloaded. No default.
  #' @param countries Character vector giving the names of the countries whose trade balance you wish to inspect.
  #' Defaults to \code{c('Brazil', 'Canada', 'China', 'Germany', 'Japan', 'Mexico')}
  #' @param since.date A character string giving the earliest date you want to report. Must be in form \code{'yyyy-mm-dd'} or coercible to such.
  #' Defaults to grab latest six months of data
  #' @param file.name Character string giving the name of the file you wish to save the downloaded xlsx file under. 
  #' Defaults to \code{'bilatTrade_yyyymmdd.xlsx'}
  #' @examples 
  #' bilat <- get_bilatTrade(dest.path = 'C:/Users/blahblah')
  #' bilat <- get_bilatTrade(dest.path = 'C:/Users/blahblah', countries = c('China', 'Zimbabwe'))
  #' @export
  
  library(readxl)
  library(data.table)
  
  if(missing(countries)) countries <- c("Brazil", "Canada", 'China', "Germany", "Japan", "Mexico")
  if(missing(since.date)) since.date <- date(Sys.Date()) - (day(date(Sys.Date())) - 1) - months(7)
  if(missing(file.name)) file.name <- file.name <- paste0('bilatTrade', '_', date, '.xlsx')
  
  since.date <- as.Date(since.date)
  date <- gsub(pattern = '-', replacement = '', as.character(date(Sys.Date()))) 
  month.lookup <- data.table(month.abb = tolower(month.abb), month.name)

  download.file("https://www.census.gov/foreign-trade/balance/country.xlsx", 
                destfile = paste0(dest.path, '/', file.name), cacheOK = F, mode = 'wb')
  
  bilat <- readxl::read_xlsx(path = file.path(dest.path, file.name), sheet = 'country') %>% as.data.table() %>%
    .[CTYNAME %in% countries] %>%
    melt(data = ., id.vars = c('year', 'CTY_CODE', 'CTYNAME'), measure.vars = union(colnames(.)[startsWith(colnames(.), 'I')], 
                                                                                    colnames(.)[startsWith(colnames(.), 'E')]),
         variable.name = 'month', value.name = 'amount') %>%
    .[ , month.abbr := tolower(substr(month, 2, 4))] %>%
    .[substr(month, 1, 1) == 'I', value := "Imports"] %>%
    .[substr(month, 1, 1) == 'E', value := 'Exports'] %>%
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
    .[!amount == 0] %>%
    dcast(., year + CTYNAME + month.name + month.num + day.num  + date ~ value, value.var = 'amount') %>%
    .[ , `Net Goods Trade` := Exports - Imports] %>%
    .[order(CTYNAME, date)]
  
  return(bilat)
  
}




