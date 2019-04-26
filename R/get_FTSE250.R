#' Function to download the current components of the FTSE250 index from Wikipedia
#'
#' This function scrapes the stocks that constitute the FTSE250 index from the wikipedia page at https://en.wikipedia.org/wiki/FTSE_250_Index.
#'
#' @inheritParams BatchGetSymbols
#'
#' @return A dataframe that includes a column with the list of tickers of companies that belong to the FTSE250 index
#' @export
#' @import rvest
#' @examples
#' \dontrun{
#' df.FTSE250 <- get_FTSE250()
#' print(df.FTSE250$tickers)
#' }
get_FTSE250 <- function(do.cache = TRUE,
                             cache.folder = 'BGS_Cache'){

  if (do.cache) {
    # check if cache folder exists, create if not
    ifelse(!dir.exists(cache.folder), dir.create(file.path(cache.folder)),'')
    cache.file <- file.path(cache.folder, paste0('FTSE250_Composition_', Sys.Date(), '.rds') )

    # check if file exists
    flag <- file.exists(cache.file)

    if (flag) {
      df.FTSE250 <- readRDS(cache.file)
      return(df.FTSE250)
    }
  }

  my.url <- 'https://en.wikipedia.org/wiki/FTSE_250_Index'

  read_html <- 0 # fix for global variable nagging from BUILD
  my.xpath <- '//*[@id="mw-content-text"]/div/table[2]' # old xpath
  my.xpath <- '//*[@id="constituents"]'
  df.FTSE250 <- my.url %>%
    read_html() %>%
    html_nodes(xpath = my.xpath) %>%
    html_table()

  df.FTSE250 <- df.FTSE250[[1]]

  colnames(df.FTSE250) <- c('company','tickers')

  if (do.cache) {
    saveRDS(df.FTSE250, cache.file)
  }

  return(df.FTSE250)
}
