#' Function to download the current components of the MDAX index from Wikipedia
#'
#' This function scrapes the stocks that constitute the MDAX index from the wikipedia page at https://en.wikipedia.org/wiki/MDAX.
#'
#' @inheritParams BatchGetSymbols
#'
#' @return A dataframe that includes a column with the list of tickers of companies that belong to the MDAX index
#' @export
#' @import rvest
#' @examples
#' \dontrun{
#' df.MDAX <- get_MDAX()
#' print(df.MDAX$tickers)
#' }
get_MDAX <- function(do.cache = TRUE,
                       cache.folder = 'BGS_Cache'){

  if (do.cache) {
    # check if cache folder exists, create if not
    ifelse(!dir.exists(cache.folder), dir.create(file.path(cache.folder)),'')
    cache.file <- file.path(cache.folder, paste0('MDAX_Composition_', Sys.Date(), '.rds') )

    # check if file exists
    flag <- file.exists(cache.file)

    if (flag) {
      df.MDAX <- readRDS(cache.file)
      return(df.MDAX)
    }
  }

  my.url <- 'https://en.wikipedia.org/wiki/MDAX'

  read_html <- 0 # fix for global variable nagging from BUILD
  my.xpath <- '//*[@id="mw-content-text"]/div/table[2]' # old xpath
  df.MDAX <- my.url %>%
    read_html() %>%
    html_nodes(xpath = my.xpath) %>%
    html_table()

  df.MDAX <- df.MDAX[[1]]

  colnames(df.MDAX) <- c('company','ticker', 'sector')

  if (do.cache) {
    saveRDS(df.MDAX, cache.file)
  }

  return(df.MDAX)
}
