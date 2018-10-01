Census_Release <- function(){
  #' @title Fetch Census Bureau release schedule for bilateral trade data
  #' @description Goes and fetches Census Bureau's release schedule for bilateral trade data
  #' @examples 
  #' schedule <- Census_Release()
  #' @export
  library(data.table)
  library(rvest)
  # Data table of months and their corresponding numbers
  month.lookup <- data.table(month.num = 1:12, month.name)
  
  # Data table of release schedule for Census Trade data
 release.sched <- read_html('https://www.census.gov/foreign-trade/reference/release_schedule.html') %>%
  html_node('table') %>%
  html_table(header = F, fill = T) %>%
  as.data.table %>%
  .[-c(1, nrow(.)), c('X1', 'X2', 'X3')]
 
  release.sched <- setnames(release.sched, old = names(release.sched), new = c(release.sched[1]$X1, release.sched[1]$X2, release.sched[1]$X3)) %>%
  .[-1] %>%
    .[ , month.name := tstrsplit(`Release Date`, split = ' ', fixed = T, keep = 1)] %>%
    .[ , day := tstrsplit(`Release Date`, split = ' ', fixed = T, keep = 2)] %>%
    .[ , day := gsub(pattern = ',', replacement = '', day)] %>%
    .[nchar(day) == 1, day := paste0('0', day)] %>%
    .[ , year:= tstrsplit(`Release Date`, split = ' ', fixed = T, keep = 3)] %>%
  merge(.,
        month.lookup,
        by = 'month.name', all.x = T, sort = F) %>%
   .[ , `Release Date` := as.Date(paste0(year, '-', month.num, '-', day))] %>%
   .[ , c('Statistical Month', 'Release Date', 'Day')] %>%
   setkey('Statistical Month')
 
 return(release.sched)
}

