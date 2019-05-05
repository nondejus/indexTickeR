#' Function to download the current components of the IBEX 35 index from Wikipedia
#'
#' This function scrapes the stocks that constitute the IBEX 35 index from the wikipedia page at https://en.wikipedia.org/wiki/IBEX_35.
#'
#' @inheritParams BatchGetSymbols
#'
#' @return A dataframe that includes a column with the list of tickers of companies that belong to the IBEX35 index
#' @export
#' @import rvest
#' @examples
#' \dontrun{
#' df.IBEX35 <- get_IBEX35()
#' print(df.IBEX35$tickers)
#' }
get_IBEX35 <- function(do.cache = TRUE,
                       cache.folder = 'BGS_Cache'){

  if (do.cache) {
    # check if cache folder exists, create if not
    ifelse(!dir.exists(cache.folder), dir.create(file.path(cache.folder)),'')
    cache.file <- file.path(cache.folder, paste0('IBEX35_Composition_', Sys.Date(), '.rds') )

    # check if file exists
    flag <- file.exists(cache.file)

    if (flag) {
      df.IBEX35 <- readRDS(cache.file)
      return(df.IBEX35)
    }
  }

  my.url <- 'https://en.wikipedia.org/wiki/IBEX_35'

  read_html <- 0 # fix for global variable nagging from BUILD
  my.xpath <- '//*[@id="mw-content-text"]/div/table[2]' # old xpath
  df.IBEX35 <- my.url %>%
    read_html() %>%
    html_nodes(xpath = my.xpath) %>%
    html_table()

  df.IBEX35 <- df.IBEX35[[1]]

  colnames(df.IBEX35) <- c('ticker','company', 'headquarters', 'sector', 'ISIN', 'weighting')
  df.IBEX35$weighting <- as.numeric(gsub('\\,', '.', df.IBEX35$weighting))

  if (do.cache) {
    saveRDS(df.IBEX35, cache.file)
  }

  return(df.IBEX35)
}
