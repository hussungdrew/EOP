SA_EOP <- function(data.path, file.name, since.date, fields, return.format){
  
  #' @title Import and seasonally adjust CPS data for the EOP
  #' @description Imports and seasonally adjusts Dr. Even's CPS data, for making nicer plots.
  #' User can simply call this function and have all the necessary series to recreate http://fsb.muohio.edu/eop/ home page plots. Note:
  #' must pass \code{data.path} and \code{file.name} for base operation. Note: As you are likely adjusting upwards
  #' of 1000 individual time series, please allow lots of computing time. By lots of time, we're talking run and
  #' go grab some coffee and a snack and then come back and check and see that it still needs more time.
  #' 
  #' @param data.path Gives path to data, i.e. \code{'C:/Users/blahblah'} if on Windows. No default.
  #' @param file.name The name of the .dta file holding the CPS data - function works well on Ohio, US data, etc. No default.
  #' @param since.date This is a data passed as a character string i.e. \code{'1990-01-01'}, YEAR-MONTH-DAY that gives the first month of data you are interested in.
  #' @param fields A character vector giving the names of the individual time series user wishes to have seasonally adjusted. Defaults to the basics
  #' @param return.format One of \code{c('DTA', 'CSV')} which gives the format of the file that the resulting seasonally adjusted data will be writted to. Defaults to .DTA
  #' @examples
  #' cps <- SA_EOP(data.path = 'C:/Users/599249/Desktop', file.name = 'cps_all.dta')
  #' cps <- SA_EOP(data.path = 'C:/Users/599249/Desktop', file.name = 'cps_all.dta', since.date = '2000-01-01', return.format = 'DTA')
  
  
  ##This top portion simply checks whether the user's R session has all of
   # our required packages installed. If it doesn't, the user can elect to
   # have the program install the package or not.
  dependencies <- c('data.table', 'lubridate', 'seasonal', 'readstata13', 'tidyr')
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
    stop('This function relies upon data.table, lubridate, seasonal, and readstata13. 
         \n Please install before using. Goodbye.')
  }
    
  }
  
  ##Loads required packages
  library(readstata13)
  library(lubridate)
  library(data.table)
  library(seasonal)
  ##Default parameters if user fails to supply them 
   # since.date allows user to subset cps to only capture data since a given date, for speed
   # fields is a vector of the names that the user wishes to have seasonally adjusted
  if(missing(since.date)) since.date <- as.Date('1990-01-01')
  if(missing(fields)) fields <- c('empl', 'hourwage_ft_r', 'earnweek_ft_r', 'hourwage_ft_r_50',
                                  'earnweek_ft_r_50', 'hourwage_ft_r_10', 'earnweek_ft_r_10', 'hourwage_ft_r_90',
                                  'earnweek_ft_r_90')
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
                                        complete(., date = full) %>% as.data.table() %>%  .[ , paste(field), with = F], 
                                                  start = year(since.date), frequency = 12)
          # names(big.ol.list)[[ticker]] <- paste0(educ, '_', sex, '_', industry, '_', field)
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
  big.ol.list <- lapply(big.ol.list, 
                        FUN = function(e) {tryCatch({seas(e, x11 = "", na.action = na.x13)},
                        error = function(e){})})
  
  return(cps)
}

  
  
