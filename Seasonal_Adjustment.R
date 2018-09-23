SA_EOP <- function(data.path, file.name, since.date, fields, return.format){
  
  #' @title Import and seasonally adjust CPS data for the EOP
  #' @description Imports and seasonally adjusts Dr. Even's CPS data, for making nicer plots.
  #' User can simply call this function and have all the necessary series to recreate http://fsb.muohio.edu/eop/ home page plots. Note:
  #' must pass \code{data.path} and \code{file.name} for base operation. Note: As you are likely adjusting upwards
  #' of 1000 individual time series, please allow lots of computing time. By lots of time, we're talking run and
  #' go grab some coffee and a snack and then come back and check and see that it still needs more time.
  #' 
  #' Returns: a list object with two elements: data, containing the resulting seasonally
  #' adjusted data frame, and fails, a list of those series that failed the seasonal adjustment process
  #' which inevitably will occur.
  #' 
  #' @param data.path Gives path to data, i.e. \code{'C:/Users/blahblah'} if on Windows. No default.
  #' @param file.name The name of the .dta file holding the CPS data - function works well on Ohio, US data, etc. No default.
  #' @param since.date This is a data passed as a character string i.e. \code{'1990-01-01'}, YEAR-MONTH-DAY that gives the first month of data you are interested in.
  #' @param fields A character vector giving the names of the individual time series user wishes to have seasonally adjusted. Defaults to the basics
  #' @param return.format One of \code{c('DTA', 'CSV')} which gives the format of the file that the resulting seasonally adjusted data will be writted to. Defaults to .DTA
  #' @examples
  #' cps <- SA_EOP(data.path = 'C:/Users/599249/Desktop', file.name = 'cps_all.dta')
  #' cps <- SA_EOP(data.path = 'C:/Users/599249/Desktop', file.name = 'cps_all.dta', since.date = '2000-01-01', return.format = 'DTA')
  #' @export
  
  
  ##This top portion simply checks whether the user's R session has all of
   # our required packages installed. If it doesn't, the user can elect to
   # have the program install the package or not.
  dependencies <- c('data.table', 'lubridate', 'seasonal', 'readstata13', 'tidyr', 'foreign')
  if (!all(dependencies %in% rownames(installed.packages()))){
    response <- readline(paste0(" Package(s) ", 
                dependencies[which(!dependencies %in% rownames(installed.packages()))],
              " Not installed. Enter 'y' if you would like to install these packages"))
  if (substring(response,1,1) %in% c('Y', 'y')){
    needed <- dependencies[which(!dependencies %in% rownames(installed.packages()))]
    
    for (package in needed){
      install.packages(package)
    }
  }
  else{
    stop('This function relies upon data.table, lubridate, seasonal, tidyr, foreign, and readstata13. 
         \n Please install before using. Goodbye.')
  }
    
  }
  
  ##Loads required packages
  library(readstata13)
  library(lubridate)
  library(data.table)
  library(seasonal)
  library(tidyr)
  library(foreign)
  ##Default parameters if user fails to supply them 
   # since.date allows user to subset cps to only capture data since a given date, for speed
   # fields is a vector of the names that the user wishes to have seasonally adjusted
  if(missing(since.date)) since.date <- as.Date('1990-01-01')
  if(missing(fields)) fields <- c('empl', 'hourwage_ft_r', 'earnweek_ft_r', 'hourwage_ft_r_50',
                                  'earnweek_ft_r_50', 'hourwage_ft_r_10', 'earnweek_ft_r_10', 'hourwage_ft_r_90',
                                  'earnweek_ft_r_90')
  if(missing(data.path)) stop('Please give a path to the data file as a string for the data.path param before using')
  if(missing(file.name)) stop('Give the name of the dataset to be seasonally adjusted as a string for the file.name param before using')
  if(missing(return.format)) return.format <- "DTA"
  ##If not missing since.date, convert it to a date
  since.date <- as.Date(since.date)
  ##Read in data from path given from user's data.path, file.name params
  cps <- read.dta13(file.path(data.path, file.name), convert.dates = T) %>%
    as.data.table() %>%
    ## Converting Stata Dates to R dates
    .[ , year := as.character(1960 + (date %/% 12))] %>%
    .[ , month := as.character(date %% 12 + 1)] %>%
    .[ , day := as.character('01')] %>%
    .[ , date := as.Date(paste0(year, '-', month, '-', day))] %>%
    .[date >= since.date] %>%
    .[ , c('date', 'female', 'hs', 'maj_ind', fields), with = F] 
    
  ## R reads in female, hs, and maj_ind as integers, even though in reality
   #  they are simply categorical factors. So this is corrected here
  for (fact in c('female', 'hs', 'maj_ind')){
    set(cps, i = NULL, j = fact, value = factor(cps[[fact]]))
  }
  ## Now we have a cleaned R data.table ready for use -- small problem which
   # we will eventually have to deal with: if data is missing, it is absent
   # from the data.table entirely, rather than filled with NA's. Therefore
   # I generate a full vector of dates for comparing with the possibly 
   # missing labels to identify this problem.
  full <- seq(since.date, cps$date[nrow(cps)], by = '1 month')
  
  ## Since a given time series is defined by the combination of a value of female,
   # a value of hs, a value of maj_ind and an actual series (e.g. real median earnings),
   # we need to subset the full CPS data.table to define a time series object and to
   # adjust it. To do this, I define vectors of all of the possible values of each of these
   # fields.
  hs <- c(0,1,99)
  female <- c(0,1,99)
  ind <- 0:14
  ## Here I initialize a list of length equal to the max number of individual time series
   # that we could possibly have. Some of these elements will be empty if, say, there are no
   # college educated, female miners.
  total_ts <- length(hs) * length(female) * length(ind) * length(fields)
  big.ol.list <- vector('list', length = total_ts)
  ## Ticker keeps count of how many time series we are adding
  ticker <- 1
## Create a giant list of all of the time series that we wish to adjust
  for (educ in hs){
    for (sex in female){
      for (industry in ind){
        for (field in fields){
          # Test if the time series exists, i.e. it is not simply missing from the data
          if (length(cps[hs == educ & female == sex & maj_ind == industry][[field]]) > 0){

          big.ol.list[[ticker]] <- ts(cps[hs == educ & female == sex & maj_ind == industry, c('date', field), with = F] %>%
                                        complete(., date = full) %>% as.data.table() %>% 
                                        setnames(field, paste0(educ, '_', sex, '_', industry, '_', field)) %>% 
                                        .[ , paste0(educ, '_', sex, '_', industry, '_', field), with = F], 
                                                  start = year(since.date), frequency = 12)
          names(big.ol.list)[[ticker]] <- dimnames(big.ol.list[[ticker]])[[2]]
          ticker <- ticker + 1
          
          }
          else{}
        }
      }
    }
  }
  ## Grab only the non-null elements from the big.ol.list
  big.ol.list <- big.ol.list[!sapply(big.ol.list, is.null)]
  ## Seasonally adjust each element in the list
   #  We also keep track of the indexes for which seasonal adjustment failed (for various reasons)
   #  and then replace these failed runs with the original time series
  big.ol.list.SA <- lapply(big.ol.list, 
                        FUN = function(e) {tryCatch({seas(e, x11 = "", na.action = na.x13)$data[ , 1]},
                        error = function(e){cat(paste(e), conditionMessage(e), '\n')})})
  ## Indexes that failed
  fails <- sapply(big.ol.list.SA, is.null)
  
  fail.list <- names(big.ol.list)[fails]
  
  ##Replace failures with the original time series
  big.ol.list.SA[fails] <- big.ol.list[fails] 
  ##Set names attribute of the adjusted time series
  names(big.ol.list.SA) <- names(big.ol.list)
  
  ##Complicated: bind all of the series for a given combo of female, hs, and maj_ind
   #  into an individual data table, then we will merge all of these together
  size.frames <- length(fields)  
  breaks <- seq(1, length(big.ol.list), size.frames)
  
  big.ol.list <- vector('list', length(breaks))
  for (i in 1:length(breaks)){
    big.ol.list[[i]] <- as.data.table(big.ol.list.SA[breaks[i]:(breaks[i] + size.frames - 1)]) 
    big.ol.list[[i]] <- big.ol.list[[i]][ , `:=` (hs = strsplit(names(big.ol.list[[i]])[1], split = '_')[[1]][1],
                 female = strsplit(names(big.ol.list[[i]])[1], split = '_')[[1]][2],
                 maj_ind = strsplit(names(big.ol.list[[i]])[1], split = '_')[[1]][3])] %>%
      setnames(., old = setdiff(names(big.ol.list[[i]]), c('hs', 'female', 'maj_ind')),
               new = fields) %>%
      lapply(., as.numeric, simplify = F)
  }
  ## Bind all of these individual data.tables together into a data.table and get result
   #  into our familiar format
  cps <- rbindlist(big.ol.list) %>%
    .[ , c('female', 'hs', 'maj_ind', fields), with = F] %>%
    .[ , date := full] %>%
    .[order(date, female, hs, maj_ind)] 
  for (fact in c('female', 'hs', 'maj_ind')){
    set(cps, i = NULL, j = fact, value = factor(cps[[fact]]))
  }
  ## Spit out resulting data.table in user's chosen file format into whatever file path passed
   #  as data.path
  if (return.format %in% c('CSV', 'csv')){
    write.csv(cps, file = paste0(data.path, '/', file.name, '_', 'SA.csv'), row.names = F)
  }
  else if (return.format %in% c('dta', 'DTA')){
    write.dta(cps, file = paste0(data.path, '/', file.name, '_', 'SA.dta'), version = 11)
  }

  cps <- list(data = cps, fails = fail.list)
    
  return(cps)
}

  
  
