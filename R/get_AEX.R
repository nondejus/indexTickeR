#' Function to download the current components of the Amsterdam Exchange Index (AEX) index from Wikipedia
#'
#' This function scrapes the stocks that constitute the AEX index from the wikipedia page at https://en.wikipedia.org/wiki/AEX_index#Composition.
#'
#' @inheritParams BatchGetSymbols
#'
#' @return A dataframe that includes a column with the list of tickers of companies that belong to the AEX index
#' @export
#' @import rvest
#' @examples
#' \dontrun{
#' df.AEX <- get_AEX()
#' print(df.AEX$tickers)
#' }
get_AEX <- function(do.cache = TRUE,
                             cache.folder = 'BGS_Cache'){

    if (do.cache) {
        # check if cache folder exists, create if not
        ifelse(!dir.exists(cache.folder), dir.create(file.path(cache.folder)),'')
        cache.file <- file.path(cache.folder, paste0('AEX_Composition_', Sys.Date(), '.rds') )

        # check if file exists
        flag <- file.exists(cache.file)

        if (flag) {
            df.AEX <- readRDS(cache.file)
            return(df.AEX)
        }
    }

    my.url <- 'https://en.wikipedia.org/wiki/AEX_index'

    read_html <- 0 # fix for global variable nagging from BUILD
    my.xpath <- '//*[@id="mw-content-text"]/div/table[2]'
    df.AEX <- my.url %>%
        read_html() %>%
        html_nodes(xpath = my.xpath) %>%
        html_table()

    df.AEX <- df.AEX[[1]]

    colnames(df.AEX) <- c('company','ICB.sector','ticker','index_weight')

    if (do.cache) {
        saveRDS(df.AEX, cache.file)
    }

    return(df.AEX)
}
