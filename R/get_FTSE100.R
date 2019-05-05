#' Function to download the current components of the FTSE100 index from Wikipedia
#'
#' This function scrapes the stocks that constitute the FTSE100 index from the wikipedia page at https://en.wikipedia.org/wiki/FTSE_100_Index#List_of_FTSE_100_companies.
#'
#' @inheritParams BatchGetSymbols
#'
#' @return A dataframe that includes a column with the list of tickers of companies that belong to the FTSE100 index
#' @export
#' @import rvest
#' @examples
#' \dontrun{
#' df.FTSE100 <- get_FTSE100()
#' print(df.FTSE100$tickers)
#' }
get_FTSE100 <- function(do.cache = TRUE,
                             cache.folder = 'BGS_Cache'){

  if (do.cache) {
    # check if cache folder exists, create if not
    ifelse(!dir.exists(cache.folder), dir.create(file.path(cache.folder)),'')
    cache.file <- file.path(cache.folder, paste0('FTSE100_Composition_', Sys.Date(), '.rds') )

    # check if file exists
    flag <- file.exists(cache.file)

    if (flag) {
      df.FTSE100 <- readRDS(cache.file)
      return(df.FTSE100)
    }
  }

  my.url <- 'https://en.wikipedia.org/wiki/FTSE_100_Index'

  read_html <- 0 # fix for global variable nagging from BUILD
  my.xpath <- '//*[@id="mw-content-text"]/div/table[2]' # old xpath
  my.xpath <- '//*[@id="constituents"]'
  df.FTSE100 <- my.url %>%
    read_html() %>%
    html_nodes(xpath = my.xpath) %>%
    html_table()

  df.FTSE100 <- df.FTSE100[[1]]

  colnames(df.FTSE100) <- c('company','ticker','ICB.sector')

  if (do.cache) {
    saveRDS(df.FTSE100, cache.file)
  }

  return(df.FTSE100)
}
