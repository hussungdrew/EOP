get_bilatTrade <- function(dest.path, countries, since.date){
  
  library(readxl)
  
  if(missing(countries)) countries <- c("Brazil", "Canada", 'China', "Germany", "Japan", "Mexico")
  if(missing(since.date)) since.date <- date(Sys.Date()) - months(9)
  
  since.date <- as.Date(since.date)
  date <- gsub(pattern = '-', replacement = '', as.character(date(Sys.Date()))) 
  file.name <- paste0('bilatTrade', '_', date, '.xlsx')
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




