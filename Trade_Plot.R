trade_plot <- function(download.new, cols, title, subtitle, ...){
  #' @title Replicate bilateral trade plot given on EOP home page
  #' @description Creates the clustered bar chart of US Bilateral Goods Trade Balance by trading partner
  #' that is shown on the EOP home page. Mainly an auxiliary function to keep the home page updated, but 
  #' can also supply own list of countries, control months that are plotted, etc. 
  #' @param download.new Same parameter from \code{get_bilatTrade()}, controls whether a new Census Bureau dataset
  #' is downloaded or if user wishes to navigate to preexisting one.
  #' @param cols This is a named vector giving \code{'Country' = 'color'} value pairs. In case you wish to choose a different
  #' set of countries than the default, those that are included on the EOP home page. Defaults to 
  #' \code{c('Brazil' = '#00cc66', 'Canada' = '#0040ff', 'China' = '#1ab2ff',
  #' Germany' = '#b3b3b3', 'Japan' = '#666666', 'Mexico' = '#53a9c6')}
  #' @param title An alternate title for the plot
  #' @param subtitle An alternate subtitle for the plot
  #' @param ... Parameters that get passed to get_bilatTrade(), i.e. \code{dest.path, file.name, countries, since.date}
  #' @examples 
  #' trade <- trade_plot(download.new = T)
  #' trade <- trade_plot(download.new = F, cols = c('Brazil' = '#00cc66', 'Canada' = '#0040ff'), countries = c('Brazil', 'Canada'))
  #' trade <- trade_plot(download.new = T, title = 'Hello,', subtitle = 'World')
  #' @export

  library(ggplot2)
  library(data.table)
  
  today.date <- gsub(pattern = '-', replacement = '', as.character(Sys.Date())) 
  schedule <- Census_Release()
  stat.month <- paste0(month.name[month(Sys.Date())], ' ', year(Sys.Date()))
  
  if(missing(cols)) cols <- c('Brazil' = '#00cc66', 'Canada' = '#0040ff', 'China' = '#1ab2ff',
                              'Germany' = '#b3b3b3', 'Japan' = '#666666', 'Mexico' = '#53a9c6')
  if(missing(download.new)) download.new <- T
  if(missing(title)) title <- 'US Bilateral Goods Trade Balance, by Trading Partner'
  if(missing(subtitle)) subtitle <- 'Past 6 Months of Data, Seasonally Adjusted'

  if(download.new == T){
    bilat <- get_bilatTrade(download.new = download.new, ...)
  }
  else if (download.new == F){
    bilat <- get_bilatTrade(download.new = download.new, ...)
  }
  
  levels <- bilat[!duplicated(month.name), month.name]
    
  trade_plot <- ggplot(data = bilat[ , month.name := factor(month.name, levels = levels, ordered = T)],
                       mapping = aes(x = month.name, y = `Net Goods Trade`/1000, fill = Country)) + 
    geom_bar(stat = 'identity', position = 'dodge', colour = '#c2d6d6') + 
    scale_fill_manual(values = cols) + 
    labs(x = 'Month', y = 'Net Goods Trade (Thousands)') + 
    facet_grid(~Country) + 
    ggtitle(label = title, subtitle = subtitle) + 
    theme(panel.spacing = unit(0.1, 'lines'),
          axis.text.x = element_text(angle = 90, vjust = 0.25),
          axis.ticks.x = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          rect = element_rect(colour = 'black')) + 
    geom_hline(yintercept = 0)
  
  return(trade_plot)
}

  
