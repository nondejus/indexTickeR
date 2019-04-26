#' Function to download the current components of the BEL 20 index from Wikipedia
#'
#' This function scrapes the stocks that constitute the BEL20 index from the wikipedia page at https://en.wikipedia.org/wiki/BEL_20#Constituents.
#'
#' @inheritParams BatchGetSymbols
#'
#' @return A dataframe that includes a column with the list of tickers of companies that belong to the BEL20 index
#' @export
#' @import rvest
#' @examples
#' \dontrun{
#' df.BEL20 <- get_BEL20()
#' print(df.BEL20$tickers)
#' }
get_BEL20 <- function(do.cache = TRUE,
                             cache.folder = 'BGS_Cache'){

    if (do.cache) {
        # check if cache folder exists, create if not
        ifelse(!dir.exists(cache.folder), dir.create(file.path(cache.folder)),'')
        cache.file <- file.path(cache.folder, paste0('BEL20_Composition_', Sys.Date(), '.rds') )

        # check if file exists
        flag <- file.exists(cache.file)

        if (flag) {
            df.BEL20 <- readRDS(cache.file)
            return(df.BEL20)
        }
    }

    my.url <- 'https://en.wikipedia.org/wiki/BEL_20#Constituents'

    read_html <- 0 # fix for global variable nagging from BUILD
    my.xpath <- '//*[@id="mw-content-text"]/div/table[2]'
    df.BEL20 <- my.url %>%
        read_html() %>%
        html_nodes(xpath = my.xpath) %>%
        html_table()

    df.BEL20 <- df.BEL20[[1]]

    colnames(df.BEL20) <- c('company','ICB.sector','ticker','index_weight_percent')

    if (do.cache) {
        saveRDS(df.BEL20, cache.file)
    }

    return(df.BEL20)
}
